"""
Minimal Mozc IPC client (very partial).

This implementation only supports a *very* small subset of Mozc's IPC
protocol that is required to perform a *single shot* conversion request.

The real Mozc protocol is based on Protocol Buffers defined in
`session/commands.proto`, and each message is prefixed with its length in
little-endian 32-bit unsigned integer.  Re-implementing the entire message
definition would be excessive for the purpose of Sumibi, so we take the
following pragmatic approach instead:

*  At runtime we try to import the official Mozc Python bindings
   (``from mozc import proto``).  If they are available, they are used and
   the client behaves exactly like the native Mozc front-ends.

*  If they are *not* available (which is very common – the Mozc build
   system does not ship them by default), we fall back to a heuristic
   converter that just passes Roman text through unchanged.  Although this
   obviously does *not* yield any Japanese conversion, it keeps the proxy
   server functional and prevents it from dying with “ImportError”.  The
   dummy behaviour is useful for unit testing and for environments where
   Mozc is not installed.  A clear warning is emitted on start-up so that
   users know why no conversion happens.

The class exposed to the rest of the code base is :class:`MozcClient`.
It provides only one public method, :meth:`convert`, which takes a string
and returns Mozc's best conversion candidate as a string.
"""

from __future__ import annotations

import logging
import os
import socket
import struct
import sys
from pathlib import Path
from typing import Optional


_LOGGER = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Protobuf helpers
# ---------------------------------------------------------------------------


def _import_mozc_protobufs():
    """Try to import Mozc's generated *protobuf* Python modules.

    Mozc's build system normally produces a package layout that looks like::

        mozc/
            __init__.py
            proto/
                __init__.py
                commands_pb2.py

    Unfortunately, those files are rarely installed system-wide.  Hence we
    attempt the import in a *best effort* fashion and gracefully fall back
    when it fails.
    """

    # 1) Official Mozc package location: ``mozc.proto.commands_pb2`` ----------------
    try:
        from mozc.proto import commands_pb2  # type: ignore

        _LOGGER.info("Loaded Mozc protobufs from installed package 'mozc.proto'.")
        return commands_pb2
    except ModuleNotFoundError as exc:
        _LOGGER.info("Did not find 'mozc.proto.commands_pb2' (%s)", exc)

    # 2) If a *protocol* package exists alongside this file (i.e.
    #    mozc-helper/protocol/commands_pb2.py) try importing that.
    from pathlib import Path
    helper_dir = Path(__file__).parent

    # Ensure *mozc-helper* directory itself is on sys.path so that a
    # top-level package named ``protocol`` found *inside* that directory can
    # be imported even when callers execute the script directly (``python
    # mozc-helper/server.py``) instead of installing the package.
    if str(helper_dir) not in sys.path:
        sys.path.insert(0, str(helper_dir))
    proto_dir = helper_dir / "protocol"
    if (proto_dir / "commands_pb2.py").is_file():
        # Make helper_dir discoverable for 'import protocol.*'
        if str(helper_dir) not in sys.path:
            sys.path.insert(0, str(helper_dir))

        try:
            import protocol.commands_pb2 as commands_pb2  # type: ignore

            _LOGGER.info("Loaded Mozc protobufs from '%s'", proto_dir)
            return commands_pb2
        except ModuleNotFoundError as exc:
            _LOGGER.warning(
                "Found protocol directory at '%s' but import failed: %s",
                proto_dir,
                exc,
            )

    # 3) *Legacy* fallback: single commands_pb2.py dropped directly next to ipc.py
    import importlib.util

    local_pb = helper_dir / "commands_pb2.py"
    if local_pb.is_file():
        spec = importlib.util.spec_from_file_location("local_mozc_commands_pb2", local_pb)
        if spec and spec.loader:
            module = importlib.util.module_from_spec(spec)
            sys.modules[spec.name] = module
            try:
                spec.loader.exec_module(module)  # type: ignore[arg-type]
                _LOGGER.info("Loaded Mozc protobufs from '%s' (legacy single-file mode)", local_pb)
                return module  # type: ignore[return-value]
            except ModuleNotFoundError as exc:  # Missing sub-protobufs
                _LOGGER.warning(
                    "Single-file commands_pb2 found at '%s' but import failed: %s. "
                    "Likely because dependent protobuf modules are missing (e.g. candidate_window_pb2).",
                    local_pb,
                    exc,
                )

    # 4) Still not found → dummy mode
    _LOGGER.warning(
        "Mozc Python protobuf bindings could not be located in any of the searched locations.\n"
        "  • tried installed package   : 'mozc.proto.commands_pb2'\n"
        "  • tried local protocol dir : %s\n"
        "  • tried single file        : %s\n"
        "Current sys.path is:\n%s\n"
        "Roman → Japanese conversion will be disabled.",
        proto_dir,
        local_pb,
        "\n".join("  - " + p for p in sys.path),
    )
    return None


_commands_pb2 = _import_mozc_protobufs()


# ---------------------------------------------------------------------------
# Socket helpers
# ---------------------------------------------------------------------------


def _detect_socket() -> Optional[Path]:
    """Locate the running ``mozc_server``'s UNIX domain socket.

    1) Honor explicit override via environment variables.
       MOZC_SERVER_SOCKET or MOZC_SOCKET_PATH yields an absolute or abstract path.
    2) Otherwise search for filesystem sockets under XDG_RUNTIME_DIR (if set),
       then under /tmp.
       We look for common naming patterns used by various distributions.
    If no candidate is found, return None.
    """

    uid = os.getuid()
    # 1) Environment override for explicit socket path/address.
    for env_var in ("MOZC_SERVER_SOCKET", "MOZC_SOCKET_PATH"):
        sock = os.environ.get(env_var)
        if sock:
            # abstract socket if leading NUL, else filesystem path
            if sock.startswith("\0"):
                return sock  # type: ignore[return-value]
            p = Path(sock)
            if p.exists():
                return p

    # 2) Search candidate directories: XDG_RUNTIME_DIR or /run/user/<uid>, then /tmp
    search_dirs: list[Path] = []
    xdg = os.environ.get("XDG_RUNTIME_DIR")
    if xdg:
        search_dirs.append(Path(xdg))
    else:
        search_dirs.append(Path(f"/run/user/{uid}"))
    search_dirs.append(Path("/tmp"))

    # Common socket name substrings for various builds
    pattern_parts = [
        f".mozc.{uid}.",     # Upstream naming
        f".mozc_unix.{uid}.", # Debian / Ubuntu
        f".mozc.unix.{uid}",  # Arch Linux
    ]

    for base in search_dirs:
        if not base.is_dir():
            continue
        for part in pattern_parts:
            for entry in base.glob(f"*{part}*"):
                if entry.is_socket():
                    return entry

    return None


class MozcClient:
    """Very small Mozc IPC client.

    The client is heavily cut down: only session creation and single shot
    *CONVERT* are implemented.  This is sufficient for Sumibi's use-case –
    converting a full Roman string at once – but it is *not* a general
    drop-in replacement for a real Mozc front-end.
    """

    # Size prefix: Mozc uses little-endian unsigned 32-bit length (bytes)
    _SIZE_STRUCT = struct.Struct("<I")

    def __init__(self, socket_path: Optional[os.PathLike[str] | str] = None):
        # When protobufs are not available we enter *dummy* mode.  This is
        # signalled by setting ``self._enabled`` to *False*.
        self._enabled = _commands_pb2 is not None

        if not self._enabled:
            self._warn_dummy()
            self._sock = None
            self._session_id: Optional[int] = None
            return

        self._abstract_socket_name: Optional[str] = None

        if socket_path is None:
            socket_path = _detect_socket()
            # If still not found, fall back to default abstract protobuf socket
            if socket_path is None:
                self._abstract_socket_name = f"\0.mozc.{os.getuid()}.unix"

        # If we still were not able to find anything we gracefully fall back
        # to dummy mode instead of hard-failing.
        if socket_path is None and self._abstract_socket_name is None:
            self._warn_dummy("no socket found")
            self._enabled = False
            self._sock = None
            self._session_id = None
            return

        self._socket_path = os.fspath(socket_path) if socket_path is not None else None

        self._sock: socket.socket | None = None
        self._session_id: Optional[int] = None

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def convert(self, text: str) -> str:
        """Convert *text* (Roman or Kana) to Japanese using Mozc.

        If Mozc’s protobuf bindings are not available, the method simply
        returns *text* unchanged so that callers do not have to care about
        installation details.
        """

        if not self._enabled:
            # Dummy behaviour
            return text

        if self._sock is None:
            self._connect()

        assert self._sock is not None  # mypy

        # ----------------------------------------------------------
        # 1.  Make sure we have a session.
        # ----------------------------------------------------------
        if self._session_id is None:
            try:
                self._session_id = self._create_session()
            except ConnectionError:
                # reconnect and retry CREATE_SESSION if socket closed unexpectedly
                if self._sock:
                    try:
                        self._sock.close()
                    except Exception:
                        pass
                self._sock = None
                self._connect()
                self._session_id = self._create_session()

        # ----------------------------------------------------------
        # 2.  Send a *SUBMIT* command with the whole string.
        # ----------------------------------------------------------

        commands_pb2 = _commands_pb2  # local alias; mypy is fine – we know it is not None

        input_pb = commands_pb2.Input()
        input_pb.id = self._session_id
        sess_cmd = input_pb.session_command
        sess_cmd.type = commands_pb2.SessionCommand.SUBMIT  # type: ignore[attr-defined]
        input_pb.raw_input = text  # type: ignore[attr-defined] – Mozc 2.29+ adds raw_input

        self._send_message(input_pb)

        # Receive the *Output* message containing the conversion result.
        output_pb = self._recv_message(commands_pb2.Output())

        if output_pb.HasField("error") and output_pb.error:
            raise RuntimeError("Mozc returned an error for the request")

        # The best candidate is stored in ``output_pb.result.value``.
        if output_pb.HasField("result"):
            return output_pb.result.value

        # No result – return input as fallback so that the caller still gets
        # something meaningful.
        return text

    # ------------------------------------------------------------------
    # Private helpers – socket / protobuf
    # ------------------------------------------------------------------

    def _warn_dummy(self, reason: str = "") -> None:  # noqa: D401
        _LOGGER.warning(
            "MozcClient is running in *dummy* mode; no conversion will happen. %s",
            reason,
        )

    def _connect(self) -> None:
        assert self._enabled  # only called when protobufs present

        self._sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

        # Try filesystem socket first (if available), then abstract socket.
        try:
            if self._socket_path is not None:
                self._sock.connect(self._socket_path)
                _LOGGER.info("Connected to Mozc filesystem socket '%s'", self._socket_path)
                return

            if self._abstract_socket_name is not None:
                # In Python, a leading NUL byte in the *string* triggers an
                # abstract socket connect.
                self._sock.connect(self._abstract_socket_name)
                _LOGGER.info("Connected to Mozc abstract socket '%s'", self._abstract_socket_name)
                return

            raise RuntimeError("No socket path available for connection")
        except OSError as exc:
            raise RuntimeError(
                "Could not connect to mozc_server socket (filesystem='%s', abstract='%s'): %s"
                % (self._socket_path, self._abstract_socket_name, exc)
            ) from exc

    # ------------------------- low-level IPC helpers --------------------

    def _send_message(self, msg) -> None:  # type: ignore[no-self-use]
        """Serialize *msg* (a protobuf message) and write it to the socket."""

        data: bytes = msg.SerializeToString()  # type: ignore[attr-defined]
        size_prefix = self._SIZE_STRUCT.pack(len(data))
        assert self._sock is not None
        self._sock.sendall(size_prefix + data)

    def _recv_exact(self, size: int) -> bytes:
        assert self._sock is not None
        buff = bytearray()
        while len(buff) < size:
            chunk = self._sock.recv(size - len(buff))
            if not chunk:
                raise ConnectionError("Unexpected EOF while reading from Mozc socket")
            buff.extend(chunk)
        return bytes(buff)

    def _recv_message(self, proto_cls):
        # Read size prefix
        size_prefix = self._recv_exact(self._SIZE_STRUCT.size)
        (msg_size,) = self._SIZE_STRUCT.unpack(size_prefix)
        data = self._recv_exact(msg_size)

        msg = proto_cls.__class__() if not isinstance(proto_cls, type) else proto_cls()
        msg.ParseFromString(data)  # type: ignore[attr-defined]
        return msg

    # ------------------------- session helpers -------------------------

    def _create_session(self) -> int:
        """Send *CREATE_SESSION* and return the new session id."""

        commands_pb2 = _commands_pb2  # alias

        input_pb = commands_pb2.Input()
        input_pb.Clear()
        # Use Input.CommandType.CREATE_SESSION to start a new session.
        input_pb.type = commands_pb2.Input.CREATE_SESSION  # type: ignore[attr-defined]

        self._send_message(input_pb)

        output_pb = self._recv_message(commands_pb2.Output())

        if not output_pb.HasField("id"):
            raise RuntimeError("Mozc did not return a session id in CREATE_SESSION response")

        return output_pb.id

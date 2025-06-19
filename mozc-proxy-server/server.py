"""HTTP proxy server for Mozc.

Usage (default):
    python mozc-helper/server.py [--listen 127.0.0.1] [--port 8000]

Endpoints:
    POST /convert  {"text": "romaji string"}

Response example on success::

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8

    {"text": "漢字かな混じり文"}

Error example (400)::

    {"error": "request must be JSON with a 'text' field"}

Implementation details:
    *  The server is intentionally *dependency-free*; it only relies on the
       Python standard library so that it can run in minimal environments
       (e.g. containers, CI) without extra installation steps.
    *  The I/O is blocking and single-threaded.  Mozc conversions are fast
       enough that this is usually no problem.  If you need better
       concurrency, put a reverse proxy such as **gunicorn** or
       **uvicorn** in front of this script.
"""

from __future__ import annotations

import argparse
import importlib
import json
import logging
import sys
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from typing import ClassVar


# ---------------------------------------------------------------------------
# Import MozcClient – works whether the file is executed as script or module
# ---------------------------------------------------------------------------


def _import_mozc_client():  # noqa: D401
    """Import *MozcClient* regardless of how we were invoked.

    Two common invocation patterns exist in practice:

    1. ``python -m mozc_helper.server`` – *module* execution, package
       context is present → simple relative import works.
    2. ``python mozc-helper/server.py`` – executed *as script* from an
       *arbitrary* working directory; no package context → fall back to
       absolute import by *path*.
    """

    try:
        # 1.  Try relative import (works for ``python -m …``)
        from .ipc import MozcClient  # type: ignore

        return MozcClient
    except (ImportError, ValueError):
        # 2.  Fallback: load ipc.py directly via its file *path*.
        this_file = Path(__file__).resolve()
        ipc_path = this_file.parent / "ipc.py"

        spec = importlib.util.spec_from_file_location("mozc_helper_ipc", ipc_path)
        if spec and spec.loader:
            module = importlib.util.module_from_spec(spec)
            sys.modules[spec.name] = module
            spec.loader.exec_module(module)  # type: ignore[arg-type]
            return module.MozcClient  # type: ignore[attr-defined]

        # Should never reach here – raise for clarity.
        raise ImportError("Cannot import MozcClient – spec loader unavailable")


MozcClient = _import_mozc_client()


_LOGGER = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Request handler
# ---------------------------------------------------------------------------


class _RequestHandler(BaseHTTPRequestHandler):
    """`BaseHTTPRequestHandler` subclass handling */convert* requests."""

    # MozcClient is relatively expensive to set up because it needs to
    # create a session.  Re-use a single instance for the whole lifetime of
    # the process.
    _client: ClassVar[MozcClient] = MozcClient()

    server_version = "MozcProxy/0.1"

    # Silence logging from the base class – we handle it ourselves.
    def log_message(self, fmt: str, *args) -> None:  # noqa: D401 (match BaseHTTPRequestHandler API)
        _LOGGER.info("%s - %s", self.address_string(), fmt % args)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _send_json(self, obj, status: HTTPStatus = HTTPStatus.OK) -> None:  # noqa: D401
        data = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(status.value)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    # ------------------------------------------------------------------
    # HTTP verbs
    # ------------------------------------------------------------------

    def do_POST(self) -> None:  # noqa: D401  (match BaseHTTPRequestHandler API)
        if self.path.rstrip("/") != "/convert":
            self._send_json({"error": "unknown endpoint"}, HTTPStatus.NOT_FOUND)
            return

        # Content-Length header is mandatory for POST requests.
        try:
            length = int(self.headers.get("Content-Length", "0"))
        except ValueError:
            self._send_json({"error": "Missing or invalid Content-Length header"}, HTTPStatus.LENGTH_REQUIRED)
            return

        raw_body = self.rfile.read(length)

        try:
            body = json.loads(raw_body.decode("utf-8"))
        except json.JSONDecodeError:
            self._send_json({"error": "request body must be valid JSON"}, HTTPStatus.BAD_REQUEST)
            return

        if not isinstance(body, dict) or "text" not in body:
            self._send_json({"error": "request must be JSON with a 'text' field"}, HTTPStatus.BAD_REQUEST)
            return

        text = body["text"]
        if not isinstance(text, str):
            self._send_json({"error": "'text' must be a string"}, HTTPStatus.BAD_REQUEST)
            return

        try:
            converted = self._client.convert(text)
        except Exception as exc:  # pylint: disable=broad-except  – we want to shield all errors from the client
            _LOGGER.exception("Error while converting text via Mozc: %s", exc)
            self._send_json({"error": str(exc)}, HTTPStatus.INTERNAL_SERVER_ERROR)
            return

        self._send_json({"text": converted})

    # Fallback for methods that we do not implement (GET, PUT, ...)
    def do_GET(self) -> None:  # noqa: D401
        self._send_json({"error": "only POST /convert is supported"}, HTTPStatus.METHOD_NOT_ALLOWED)


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------


def _parse_args() -> argparse.Namespace:  # noqa: D401
    parser = argparse.ArgumentParser(description="Mozc HTTP proxy (JSON)")
    parser.add_argument("--listen", default="127.0.0.1", help="address to bind to (default: 127.0.0.1)")
    parser.add_argument("--port", type=int, default=8000, help="TCP port (default: 8000)")
    parser.add_argument("--log-level", default="INFO", choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"], help="root logger level")
    return parser.parse_args()


def main() -> None:  # noqa: D401
    args = _parse_args()

    logging.basicConfig(
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        level=getattr(logging, args.log_level.upper()),
    )

    _LOGGER.info("Starting Mozc HTTP proxy on %s:%d", args.listen, args.port)
    server = HTTPServer((args.listen, args.port), _RequestHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        _LOGGER.info("Shutting down on Ctrl-C…")


if __name__ == "__main__":
    main()

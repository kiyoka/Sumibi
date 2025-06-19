"""Sumibi *Mozc helper* package.

The package provides two public building blocks:

1. :class:`mozc_helper.ipc.MozcClient` – a **very small** Mozc IPC client.
2. :mod:`mozc_helper.server` – a dependency-free HTTP JSON proxy exposing
   the converter via ``POST /convert``.

Both modules are intentionally self-contained so that they can run on a
typical system without additional Python packages.  If the official Mozc
protobuf bindings are available in ``PYTHONPATH`` the client will use them
automatically; otherwise it will fall back to a no-op dummy that just
returns the original text.
"""

from .ipc import MozcClient  # re-export for convenience

__all__ = ["MozcClient"]

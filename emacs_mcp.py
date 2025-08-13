#!/usr/bin/env python3
"""
MCP server for interacting with Emacs via emacsclient.

This server provides access to evaluate Emacs Lisp expressions via emacsclient
in the currently visible Emacs buffer. You can evaluate code or capture visible
text content from the active Emacs window.

This is the main entry point that imports from the modular structure.
"""

import asyncio
from mcp_server import main


if __name__ == "__main__":
    asyncio.run(main())
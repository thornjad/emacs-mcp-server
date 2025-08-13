"""Main entry point for the Emacs MCP server."""

import asyncio

from .server import mcp


if __name__ == "__main__":
    # Run the FastMCP server directly
    asyncio.run(mcp.run())

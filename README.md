# Emacs MCP Server

A simple Model Context Protocol (MCP) server that enables Claude to control and interact with Emacs remotely.

## Overview

This is a lightweight, single-file MCP server providing three tools for Emacs interaction:

- **emacs_eval**: Evaluate Emacs Lisp expressions and return results
- **emacs_get_visible_text**: Retrieve text currently visible in the active Emacs window  
- **emacs_get_context**: Get contextual information about the current Emacs state

## Files

- `emacs_mcp_server.py` - Main server implementation (~140 lines)
- `test_emacs_mcp_server.py` - Complete test suite
- `pyproject.toml` - Project configuration

## Prerequisites

- Python 3.12+
- UV package manager
- Running Emacs instance with server mode enabled
- `emacsclient` available in PATH

## Quick Start

```bash
# Install dependencies
uv sync --group dev

# Start Emacs server (if not already running)
emacs --daemon

# Run the MCP server
uv run python emacs_mcp_server.py

# Or install and use the script
uv sync && uv run emacs-mcp-server
```

## Testing

```bash
# Run all tests
uv run pytest test_emacs_mcp_server.py

# Run only unit tests (no Emacs required)
uv run pytest test_emacs_mcp_server.py -m "not integration"

# Run integration tests (requires running Emacs)
uv run pytest test_emacs_mcp_server.py -m integration
```

## How It Works

The server uses `subprocess` to call `emacsclient --eval <expression>` and parses the results. Each tool is a simple async function that:

1. Constructs an Emacs Lisp expression
2. Executes it via `emacsclient`
3. Returns structured JSON responses

## Example Usage

```python
# Evaluate simple expressions
await emacs_eval("(+ 2 3)")  # Returns: {"success": true, "result": "5"}

# Get buffer context
await emacs_get_context()    # Returns buffer info as JSON

# Get visible text
await emacs_get_visible_text()  # Returns currently visible content
```

## License

MIT License
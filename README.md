# Emacs MCP Server

An MCP (Model Context Protocol) server that allows Claude Code and other MCP clients to control and interact with Emacs via `emacsclient`.

## Features

- **Non-interactive Emacs control**: Execute Emacs Lisp expressions without user interaction
- **Buffer visibility**: Get text content currently visible in Emacs windows  
- **Context awareness**: Access buffer information, modes, cursor position, and more
- **Real-time integration**: Works with your active Emacs session and open files

## Requirements

- Python 3.12+
- Emacs with server mode enabled (`M-x server-start` or add `(server-start)` to your init file)
- `emacsclient` accessible in PATH

## Installation

```bash
# Clone the repository
git clone <repo-url>
cd emacs-mcp-server-claude-opus-plan

# Install dependencies
uv sync
```

## Files

- `server.py` - Single file containing the complete MCP server
- `test.py` - Test script to validate functionality
- `pyproject.toml` - Project configuration

## Usage

### As MCP Server

```bash
uv run emacs-mcp-server
# or directly:
python server.py
```

### Testing

```bash
python test.py
```

### Available Tools

1. **emacs_eval**: Execute Emacs Lisp expressions
   - Input: Emacs Lisp expression string
   - Output: Evaluation result

2. **emacs_get_visible_text**: Get visible buffer content
   - Output: Text currently visible in active Emacs window

3. **emacs_get_context**: Get Emacs state information
   - Output: JSON with buffer name, mode, cursor position, region info, etc.

### Example Tool Usage

```python
# Evaluate expression
emacs_eval("(+ 2 3)")  # Returns: "5"

# Get current buffer name
emacs_eval("(buffer-name)")  # Returns: "README.md"

# Get visible text
emacs_get_visible_text()  # Returns current window content

# Get context
emacs_get_context()  # Returns JSON with buffer/mode/position info
```

## Configuration with Claude Code

Add to your MCP client configuration:

```json
{
  "mcpServers": {
    "emacs": {
      "command": "uv",
      "args": ["run", "emacs-mcp-server"],
      "cwd": "/path/to/emacs-mcp-server-claude-opus-plan"
    }
  }
}
```

## Error Handling

- Returns error messages when Emacs server is not running
- Handles timeouts for long-running expressions (30s limit)
- Graceful failure when `emacsclient` is not available

## Limitations

- Requires Emacs server to be running (`M-x server-start`)
- No support for interactive commands
- 30-second timeout on expressions
- Only works with accessible `emacsclient` binary
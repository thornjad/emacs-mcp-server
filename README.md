# Emacs MCP Server

A Model Context Protocol (MCP) server that enables programmatic control and introspection of Emacs instances. This server provides tools for evaluating Emacs Lisp expressions, retrieving visible text content, and gathering contextual information about Emacs state.

## Features

- **Non-interactive operation**: Server operates without user intervention
- **Real-time state access**: Retrieve current Emacs state and content
- **Lisp evaluation**: Execute arbitrary Emacs Lisp expressions
- **Buffer introspection**: Access buffer list and current buffer information

## Prerequisites

- Python 3.12 or higher
- Emacs with `emacsclient` available
- Running Emacs server (see setup instructions below)

## Installation

### Using UV (recommended)

```bash
# Clone the repository
git clone <repository-url>
cd emacs-mcp-server-cursor-ide

# Install with UV
uv sync

# Run the server
uv run emacs-mcp-server
```

### Using pip

```bash
pip install -e .
emacs-mcp-server
```

## Emacs Server Setup

The MCP server requires a running Emacs server to function. You can start one in several ways:

### Option 1: Start daemon from command line
```bash
emacs --daemon
```

### Option 2: Start server from within Emacs
In your Emacs session, run:
```elisp
M-x server-start
```

### Option 3: Auto-start in your Emacs configuration
Add this to your `~/.emacs.d/init.el` or equivalent:
```elisp
(server-start)
```

## MCP Tools

The server provides three main tools:

### 1. `emacs_eval`
Evaluate Emacs Lisp expressions and return results.

**Parameters:**
- `expression` (string): Emacs Lisp expression to evaluate
- `timeout` (optional, int): Timeout in seconds (default: 5)

**Example:**
```json
{
  "expression": "(+ 1 2 3)",
  "timeout": 5
}
```

### 2. `emacs_get_visible_text`
Get text currently visible in the Emacs window.

**Parameters:**
- `buffer_name` (optional, string): Specific buffer name (default: current buffer)

**Example:**
```json
{
  "buffer_name": "README.md"
}
```

### 3. `emacs_get_context`
Get contextual information about Emacs state.

**Parameters:** None

**Returns JSON with:**
- Current buffer name
- Major mode
- Point position
- Mark position (if set)
- Window start/end positions
- Available buffers list
- Current working directory

## Usage Examples

### Basic Evaluation
```python
# Evaluate a simple expression
result = await emacs_eval("(+ 1 2 3)")  # Returns "6"

# Get current buffer name
buffer = await emacs_eval("(buffer-name)")  # Returns current buffer name
```

### Buffer Interaction
```python
# Get visible text from current buffer
text = await emacs_get_visible_text()

# Get text from specific buffer
text = await emacs_get_visible_text("init.el")

# Get comprehensive context
context = await emacs_get_context()
```

### Advanced Operations
```python
# Navigate to a specific line
await emacs_eval("(goto-line 42)")

# Insert text at cursor
await emacs_eval('(insert "Hello, world!")')

# Save current buffer
await emacs_eval("(save-buffer)")
```

## Error Handling

The server provides clear error messages for common issues:

- **Emacs server not running**: Instructions to start the server
- **Invalid expressions**: Syntax errors and evaluation failures
- **Timeouts**: Long-running operations that exceed the timeout
- **Missing buffers**: When specified buffers don't exist

## Development

### Running Tests
```bash
uv run pytest
```

### Code Formatting
```bash
uv run black src/ tests/
uv run ruff check src/ tests/
```

## Architecture

The server uses `emacsclient --eval` to communicate with a running Emacs daemon, providing:

- **Performance**: No process startup overhead
- **Real State Access**: Interacts with actual Emacs session
- **Stateful Operations**: Can build context across multiple tool calls
- **User Workflow Integration**: Works with the user's actual running Emacs instance

## Security Considerations

- Input validation prevents malformed Lisp expressions
- Timeout enforcement prevents infinite loops
- Server only accepts local connections (not exposed to network)

## Troubleshooting

### "Emacs server is not running"
Start the Emacs server using one of the methods in the setup section.

### "emacsclient command not found"
Ensure Emacs is installed and `emacsclient` is in your PATH.

### Connection refused
Check that the Emacs server is running and accessible:
```bash
emacsclient --eval "t"
```

## License

MIT License - see LICENSE file for details.

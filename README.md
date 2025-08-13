# Emacs MCP Server

A Model Context Protocol (MCP) server that provides programmatic control of Emacs from external applications, particularly AI agents. This server acts as a bridge between MCP clients and Emacs, allowing remote execution of Emacs Lisp expressions and retrieval of Emacs state information.

## Features

- **Execute Emacs Lisp**: Run arbitrary non-interactive Emacs Lisp expressions
- **Get Visible Text**: Retrieve text currently visible in the active Emacs window
- **Get Context**: Obtain comprehensive information about Emacs state (buffer, mode, position, etc.)
- **Error Handling**: Graceful handling of Emacs connectivity and execution errors
- **Standard MCP Protocol**: Compatible with any MCP client

## Requirements

- Python 3.12+
- Emacs with `emacsclient` available
- Running Emacs server (see setup instructions below)

## Installation

### Using UV (recommended)

```bash
# Clone the repository
git clone <repository-url>
cd emacs-mcp-server-amp

# Install with UV
uv install

# Run the server
uv run emacs-mcp-server
```

### Manual Installation

```bash
pip install mcp
python emacs_mcp_server.py
```

## Setup

### 1. Start Emacs Server

Before using the MCP server, you need to start the Emacs server:

**Option A: From within Emacs**
```
M-x server-start
```

**Option B: Start Emacs with server**
```bash
emacs --daemon
```

**Option C: Add to Emacs config**
Add this to your `.emacs` or `init.el`:
```elisp
(server-start)
```

### 2. Test Connection

Verify that `emacsclient` can connect to your Emacs instance:
```bash
emacsclient --eval "(+ 1 1)"
```

This should return `2`.

## Usage

The server provides three MCP tools:

### 1. emacs_eval

Execute an Emacs Lisp expression and return the result.

**Parameters:**
- `expression` (string): The Emacs Lisp expression to evaluate

**Example:**
```json
{
  "name": "emacs_eval",
  "arguments": {
    "expression": "(buffer-name)"
  }
}
```

### 2. emacs_get_visible_text

Get the text currently visible in the active Emacs window.

**Parameters:** None

**Example:**
```json
{
  "name": "emacs_get_visible_text",
  "arguments": {}
}
```

### 3. emacs_get_context

Get comprehensive contextual information about the current Emacs state.

**Parameters:** None

**Returns:**
- Current buffer name and file path
- Major mode and active minor modes
- Cursor position (line/column/point)
- Buffer modification status
- Mark activity status
- Buffer size and window boundaries
- List of all open buffers

**Example:**
```json
{
  "name": "emacs_get_context",
  "arguments": {}
}
```

## Example Integration

Here's how you might use this with an MCP client:

```python
import asyncio
from mcp import ClientSession

async def example():
    async with ClientSession("emacs-mcp-server") as session:
        # Get current context
        context = await session.call_tool("emacs_get_context")
        print("Current Emacs state:", context)
        
        # Execute some Emacs Lisp
        result = await session.call_tool("emacs_eval", {
            "expression": "(point)"
        })
        print("Current point:", result)
        
        # Get visible text
        text = await session.call_tool("emacs_get_visible_text")
        print("Visible text:", text)
```

## Testing

Run the included test suite:

```bash
uv run python test_server.py
```

The tests verify:
- Basic imports and functionality
- Context formatting
- Error handling
- Tool definitions
- Live integration (if Emacs server is running)

## Architecture

This implementation uses a **single-file architecture** for simplicity:

- **`emacs_mcp_server.py`** - Complete server implementation (~350 lines)
  - MCP server setup and tool handlers
  - Emacs communication functions
  - Context parsing and formatting
  - All functionality in one file for easy understanding

- **`test_server.py`** - Comprehensive test suite
  - Unit tests and integration tests
  - Live testing with actual Emacs instance

This simplified architecture makes the codebase easy to understand, modify, and deploy as a single unit.

## Troubleshooting

### "Emacs server not running" error

**Solution:** Start the Emacs server as described in the setup section.

### "emacsclient not found" error

**Solution:** Make sure Emacs is installed and `emacsclient` is in your PATH.

### Connection timeout errors

**Solution:** 
- Check that Emacs server is running
- Verify `emacsclient --eval "t"` works from command line
- Check for firewall or permission issues

### JSON parsing errors in context

**Solution:** This usually indicates an older Emacs version. The server includes fallback handling for this case.

## Security Considerations

- The server executes arbitrary Emacs Lisp expressions
- Only use with trusted MCP clients
- Consider sandboxing in production environments
- Input sanitization is limited to prevent code injection

## License

See LICENSE file for details.

## Related Projects

- [Model Context Protocol](https://github.com/modelcontextprotocol/python-sdk) - The underlying protocol
- [Emacs](https://www.gnu.org/software/emacs/) - The text editor this server controls

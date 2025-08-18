## Emacs MCP Server

A small MCP server that lets Claude Code and Cursor talk to your running Emacs via `emacsclient`.

### Requirements
- **Emacs** with the server enabled (e.g., add `(server-start)` to your init, or run `emacs --daemon`).
- **Python 3.12**.

### Quick install (recommended)
Install and run with uvx:
```bash
uvx --from git+https://github.com/thornjad/emacs-mcp-server.git emacs-mcp-server --smoke
```

Run the server (for MCP clients that launch it for you, see config below; you can also run it manually to test). It will fail fast if Emacs isn't reachable:
```bash
uvx --from git+https://github.com/thornjad/emacs-mcp-server.git emacs-mcp-server
```

### Alternative install (development)
For development or if you need to modify the code:
```bash
brew install uv
uv venv -p 3.12
uv sync
uv run emacs-mcp-server --smoke
```

### Configuration: Claude Code CLI
Use the `claude mcp add` command to register this server with the CLI.

- uvx (recommended):
```bash
claude mcp add --scope user --transport stdio \
  emacs uvx --from git+https://github.com/thornjad/emacs-mcp-server.git emacs-mcp-server
```

- Development (uv):
```bash
claude mcp add --scope user --transport stdio \
  emacs uv run emacs-mcp-server
```

Confirm and manage:
```bash
claude mcp list
claude mcp get emacs
```

### Configuration: Cursor
Cursor also supports MCP servers.

1) Open Cursor Settings and find the MCP Servers section.
2) Add a new custom server:
   - **Name**: `emacs`
   - **Command** and **Args**: same options as described above for Claude Code
   - **Environment** (optional): same as above
   - **Stdio**: enabled

After saving, ask Cursor to read the visible text in your current Emacs window or to evaluate a small form to confirm everything works.

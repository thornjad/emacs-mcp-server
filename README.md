## Emacs MCP Server

A small MCP server that lets Claude Code and Cursor talk to your running Emacs via `emacsclient`.

### Requirements
- **Emacs** with the server enabled (e.g., add `(server-start)` to your init, or run `emacs --daemon`).
- **emacsclient** on your `PATH` (usually included with Emacs). If it is not, you can point to it with the `EMACSCLIENT` environment variable.
- **Python 3.12**.

### Quick install (recommended)
macOS:
```bash
brew install uv
uv venv -p 3.12
uv sync
```

Verify your Emacs connection:
```bash
uv run emacs-mcp-server --smoke
```

Run the server (for MCP clients that launch it for you, see config below; you can also run it manually to test). It will fail fast if Emacs isn’t reachable:
```bash
uv run emacs-mcp-server
```

### Alternative install (pipx)
If you prefer a standalone binary-like install:
```bash
pipx install .
# Then verify
emacs-mcp-server --smoke
```
If `emacs-mcp-server` is not found after installation, ensure your pipx bin directory is on `PATH` and/or run `which emacs-mcp-server` to get the absolute path for use in client config.

### Configuration: Claude Code (VS Code)
Claude Code can launch MCP servers for you.

1) In VS Code, open Settings and search for “Model Context Protocol Servers” under the Claude extension.
2) Add a new server with the following fields:
   - **Name**: `emacs`
   - **Command** (pick one):
     - If using uv: `uv`
     - If using pipx or a venv: full path from `which emacs-mcp-server`
   - **Args** (choose one set):
     - If using uv: `run`, `emacs-mcp-server`
     - If using a direct binary: leave empty
   - **Environment** (optional):
     - `EMACSCLIENT`: path to `emacsclient` if it’s not on `PATH`
     - `EMACSCLIENT_TIMEOUT`: seconds, e.g. `5.0`
   - **Stdio**: enabled (default for MCP servers)

Tip: You can test quickly by asking Claude to list open buffers or evaluate a simple Emacs Lisp form after adding the server.

### Configuration: Cursor
Cursor also supports MCP servers.

1) Open Cursor Settings and find the MCP Servers section.
2) Add a new server:
   - **Name**: `emacs`
   - **Command** and **Args**: same options as described above for Claude Code
   - **Environment** (optional): same as above
   - **Stdio**: enabled

After saving, ask Cursor to read the visible text in your current Emacs window or to evaluate a small form to confirm everything works.

### Troubleshooting
- **“emacsclient not found”**: Ensure Emacs is installed and the server is running. If needed, set `EMACSCLIENT` to the absolute path of `emacsclient`.
- **Timeouts**: Increase `EMACSCLIENT_TIMEOUT` or launch with `--timeout <seconds>`.

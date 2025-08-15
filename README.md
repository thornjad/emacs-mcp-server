## Emacs MCP Server

A small MCP server that lets Claude Code and Cursor talk to your running Emacs via `emacsclient`.

### Requirements
- **Emacs** with the server enabled (e.g., add `(server-start)` to your init, or run `emacs --daemon`).
- **emacsclient** on your `PATH` (usually included with Emacs).
- **Python 3.12**.

### Quick install (recommended)
macOS:
```bash
brew install uv
uv venv -p 3.12
uv sync
```

[Optional] Verify your Emacs connection:
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

### Configuration: Claude Code CLI
Use the `claude mcp add` command to register this server with the CLI.

- uv (recommended):
```bash
claude mcp add --scope user --transport stdio \
  -e EMACSCLIENT_TIMEOUT=5.0 \
  emacs uv run emacs-mcp-server
```

- Direct binary (pipx/venv):
```bash
claude mcp add --scope user --transport stdio \
  -e EMACSCLIENT_TIMEOUT=5.0 \
  emacs "$(which emacs-mcp-server)"
```

Confirm and manage:
```bash
claude mcp list
claude mcp get emacs
claude mcp remove emacs   # to remove later
```

Notes:
- Keep `--transport stdio` (this server speaks stdio).
- Ensure `emacsclient` is on your `PATH` (this server always runs `emacsclient`).
- The server fails fast at startup if Emacs isn’t reachable; start Emacs (and its server) first.

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

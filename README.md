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
Claude Code’s CLI can load MCP servers from simple JSON specs. Create a server spec named `emacs.json` with the command you prefer (uv or a direct binary):

```json
{
  "name": "emacs",
  "command": "uv",
  "args": ["run", "emacs-mcp-server"],
  "env": {
    "EMACSCLIENT": "emacsclient",
    "EMACSCLIENT_TIMEOUT": "5.0"
  },
  "stdio": true
}
```

Place this file in the CLI’s MCP servers directory, then restart the CLI:
- macOS: `~/Library/Application Support/claude/mcp/servers/emacs.json` (also try `.../Claude/...` depending on build)
- Linux: `~/.config/claude/mcp/servers/emacs.json` (also try `~/.config/Claude/...`)

Notes:
- If you installed with pipx or a venv, set `"command"` to the absolute path from `which emacs-mcp-server` and remove the `args` field.
- Ensure `stdio` is `true` (the CLI uses stdio to talk to MCP servers).
- After adding, the CLI should list the Emacs tools; try listing buffers or evaluating a small form to verify.

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

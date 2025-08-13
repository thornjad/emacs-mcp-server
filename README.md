# Emacs MCP Server

Single-file MCP server for Emacs using `fastmcp` with robust async subprocesses and base64+JSON transport.

## Tools
- `emacs_eval(expr: string)`
  - Response: `{ success: true, result: string }` or `{ success: false, error: string }`
- `emacs_get_visible_text()`
  - Response: `{ success: true, text: string, start: number, end: number }` or error envelope
- `emacs_get_context()`
  - Response: `{ success: true, context: { buffer_name, buffer_file_name, major_mode, point, line, column, modified, narrowed, window_start, window_end, buffers, project_root|null } }` or error
- `emacs_list_buffers()`
  - Response: `{ success: true, buffers: Array<{ name: string, file: string|null, modified: boolean, current: boolean }> }` or error

## Transport
- Structured payloads are emitted from Emacs via `json-encode` and base64; Python decodes and parses for reliability across quotes, backslashes, newlines, and non‑ASCII.

## Configuration
- Flags:
  - `--strict-startup`: fail fast if Emacs is unreachable (default warns)
  - `--timeout <seconds>`: override default timeout (also `EMACSCLIENT_TIMEOUT` env)
- Environment:
  - `EMACSCLIENT`: path to `emacsclient` (default: `emacsclient` on PATH)
  - `EMACSCLIENT_TIMEOUT`: default timeout seconds (default: 5.0)

## Examples
```bash
uv run emacs-mcp-server --strict-startup --timeout 10
```

## Notes
- Startup check retries once briefly to smooth transient failures.
- Visible text includes bounds so callers can correlate to buffer positions.

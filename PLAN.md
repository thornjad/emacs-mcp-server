## Emacs MCP Server (Python 3.12, uv, fastmcp)

### Overall goal
Build a Model Context Protocol (MCP) server that controls Emacs non‑interactively via `emacsclient`. The server will expose three tools to an MCP client:
- emacs_eval: evaluate an Emacs Lisp expression and return the result
- emacs_get_visible_text: return the text currently visible in the selected Emacs window
- emacs_get_context: return contextual information about Emacs state (buffer, file, mode, point, line/column, modified, buffers list, etc.)

This should run locally without Docker, be packaged with `uv`, and use the `fastmcp` library for the MCP server.

### Requirements
- Python: 3.12 (runtime and development)
- Packaging: `uv` (PEP 621 `pyproject.toml`)
- MCP framework: `fastmcp` (Python library)
- Emacs integration: `emacsclient` with an Emacs server running
  - Requires `emacsclient` on PATH (or configurable via `EMACSCLIENT` env var)
  - Emacs server running (e.g., `(server-start)` in your Emacs init or start Emacs with `--daemon`)
- No Docker required

### Design choices
- Use `emacsclient -e` to evaluate Emacs Lisp safely and synchronously.
- For structured data (e.g., context, visible text), have Emacs serialize to JSON and base64‑encode the string to avoid escape/encoding issues with `emacsclient -e` output. Python will base64‑decode and parse JSON.
- Keep the tool surface minimal and deterministic (non‑interactive commands only). Interactive commands are out of scope.
- Return friendly error messages when Emacs is unreachable or `emacsclient` is not available.

### Tools (contract)
- emacs_eval
  - input: `{ expr: string }` (Emacs Lisp expression)
  - output: `{ result: string }` (the printed representation from Emacs; `prin1` semantics)
  - behavior: pass expression to Emacs and return the printed value or an error string

- emacs_get_visible_text
  - input: `{}` (no arguments)
  - output: `{ text: string, start: number, end: number }`
  - behavior: from the selected window, return visible text `(buffer-substring-no-properties (window-start) (window-end nil t))` plus the character positions

- emacs_get_context
  - input: `{}` (no arguments)
  - output (example):
    ```json
    {
      "buffer_name": "init.el",
      "buffer_file_name": "/Users/me/.emacs.d/init.el",
      "major_mode": "emacs-lisp-mode",
      "point": 1234,
      "line": 42,
      "column": 7,
      "modified": true,
      "narrowed": false,
      "window_start": 1100,
      "window_end": 1400,
      "project_root": "/Users/me/src/project",  // when available
      "buffers": ["init.el", "*scratch*", "README.md"]
    }
    ```
  - behavior: gather values from the current buffer/window; where possible detect project via `(project-current)`

### Implementation plan
1. Initialize uv project
   - Create `pyproject.toml` with Python 3.12 requirement and dependency on `fastmcp`
   - Add console script entry point: `emacs-mcp-server = "emacs_mcp_server:main"`
2. Source layout (single file)
   - `emacs_mcp_server.py` at repo root containing both the FastMCP server and the Emacs bridge helpers
3. Emacs bridge (inside the same file)
   - Resolve `emacsclient` binary (env `EMACSCLIENT` or default `emacsclient`)
   - Implement `evaluate(expr: str)` returning printed Lisp result
   - Implement base64+JSON helpers for structured results and functions for visible text and context
4. Tools via fastmcp (in the same file)
   - Register the three tools and map them to the bridge helpers
   - Provide robust error messages when Emacs is not reachable
5. CLI and run
   - `emacs-mcp-server` entrypoint calls `main()` in `emacs_mcp_server.py`
   - Document environment requirements (starting Emacs server, etc.)
6. Lightweight testing
   - Provide a simple `--smoke` CLI flag or a small internal `smoke()` function callable via `uv run python -c "import emacs_mcp_server as m; m.smoke()"`

### Improvement plan (post-review)
- Non-blocking subprocess: replace blocking `subprocess.run` with `asyncio.create_subprocess_exec` and `asyncio.wait_for`
- Optional startup health check: warn by default; fail-fast with `--strict-startup`
- Timeout configuration: `--timeout` CLI flag and `EMACSCLIENT_TIMEOUT` env with sane defaults
- Response envelope: add top-level `success: bool` across all tools; keep domain fields on success, `{error}` on failure
- Context `project_root`: implement via `(project-current)` when available, or remove from description
- Window fallback: keep visible-frame selection; add selected-window fallback if detection fails
- Tests: unit tests with mocks; optional integration tests guarded by marker
- Docs: switch `pyproject.toml` readme to `README.md`; document flags, env, and response shapes

### Progress
- [x] Single-file layout chosen and documented
- [x] `pyproject.toml` created with Python 3.12 and `fastmcp` dependency; console script configured
- [x] Implemented `emacs_mcp_server.py` with Emacs bridge (via `emacsclient`) and base64+JSON helpers
- [x] Implemented tools: `emacs_eval`, `emacs_get_visible_text`, `emacs_get_context`
- [x] Added `--smoke` CLI for quick verification
- [x] Manual verification against a running Emacs server
  - `emacs_eval` returns Lisp results
  - `emacs_get_visible_text` returns visible region and bounds
  - `emacs_get_context` returns `buffer_name`, `buffer_file_name`, and correct `line` for the selected window and buffer
- [x] Non-blocking subprocess (async)
- [x] Optional startup health check (`--strict-startup`)
- [x] Configurable timeouts (`--timeout`, `EMACSCLIENT_TIMEOUT`)
- [x] Response envelope with `success`
- [x] Context `project_root` implemented (when available via `(project-current)`) and described
- [x] Window fallback path
- [x] Unit tests (mocked) and optional integration tests (integration optional)
- [ ] Docs updated (`README.md`, flags, response shapes)

### Additional improvement plan (review follow-ups)
- Documentation enhancements:
  - Expand README with response schema examples and common tool usage
  - Add configuration guide for CLI flags and environment variables
- Code quality:
  - Standardize docstrings and error message formats
  - Ensure public functions have precise type hints
- Functional:
  - New tool `emacs_list_buffers` returning buffers with name/file/modified/current
  - Enhance project root detection: fall back to `vc-root-dir`/`.git` if `(project-current)` is unavailable
  - Optional: multi-window targeting parameters (defer)
- Robustness:
  - Startup connection check: brief retry for transient failures
  - Ensure subprocess cleanup on all error paths (timeouts already kill the process)

### Additional progress
- [x] README expanded (response schema, examples, configuration)
- [x] Docstrings and error formats standardized
- [x] Added tool: `emacs_list_buffers`
- [x] Improved project root detection fallbacks
- [x] Startup check retry
- [ ] Multi-window targeting (deferred)

### Run instructions (after implementation)
- Ensure Python 3.12 and `uv` are installed
- Install deps and run:
  ```bash
  uv venv -p 3.12
  uv sync
  uv run emacs-mcp-server
  ```
- Ensure Emacs server is running in your environment (e.g., in Emacs: `(server-start)`; or `emacs --daemon`).

### Error handling & edge cases
- If `emacsclient` is missing: return a structured error hinting to install Emacs or adjust `EMACSCLIENT` path
- If Emacs server is not running: return a structured error suggesting `(server-start)` or `emacs --daemon`
- If evaluation fails: include stderr/stdout from `emacsclient` and the attempted expression (truncated)

### Acceptance criteria
- Project builds and runs with `uv` on Python 3.12
- Server exposes the three tools with the specified names and behaviors
- `emacs_eval` correctly returns printed results for Lisp expressions
- `emacs_get_visible_text` returns the currently visible text in the selected window
- `emacs_get_context` returns buffer/mode/point/line/column/modified/buffers and related info
- No Docker required; a local Emacs + `emacsclient` is sufficient
- Minimal smoke testing is possible without heavy test suites

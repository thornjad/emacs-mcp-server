from __future__ import annotations

import argparse
import asyncio
import base64
import contextlib
import json
import os
import subprocess
import sys
from dataclasses import dataclass
from typing import Any, Dict, Tuple, List, Optional

from fastmcp import FastMCP

__version__ = "0.1.0"


class EmacsError(RuntimeError):
    """Raised when an Emacs interaction fails in a recoverable way."""
    pass


def _resolve_emacsclient() -> str:
    return os.environ.get("EMACSCLIENT", "emacsclient")


# Default timeout can be overridden via env and CLI
_DEFAULT_TIMEOUT_SECONDS: float = float(os.environ.get("EMACSCLIENT_TIMEOUT", "5.0"))


def _get_timeout_seconds(override: float | None = None) -> float:
    if override is not None and override > 0:
        return override
    return _DEFAULT_TIMEOUT_SECONDS


def _run_emacsclient_eval(expr: str, timeout_seconds: float = 5.0) -> Tuple[str, str, int]:
    """Synchronous fallback runner (used only in smoke or legacy paths)."""
    command = [_resolve_emacsclient(), "-e", expr]
    try:
        proc = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
            timeout=timeout_seconds,
            text=True,
        )
    except FileNotFoundError as exc:
        raise EmacsError(
            "emacsclient not found. Install Emacs or set EMACSCLIENT env var to its path."
        ) from exc
    except subprocess.TimeoutExpired as exc:
        raise EmacsError("emacsclient request timed out") from exc

    stdout = proc.stdout.strip()
    stderr = proc.stderr.strip()
    return stdout, stderr, proc.returncode


async def _run_emacsclient_eval_async(
    expr: str, timeout_seconds: Optional[float] = None
) -> Tuple[str, str, int]:
    """Run emacsclient asynchronously and return (stdout, stderr, returncode)."""
    command = [_resolve_emacsclient(), "-e", expr]
    try:
        process = await asyncio.create_subprocess_exec(
            *command,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
    except FileNotFoundError as exc:
        raise EmacsError(
            "emacsclient not found. Install Emacs or set EMACSCLIENT env var to its path."
        ) from exc

    timeout = _get_timeout_seconds(timeout_seconds)
    try:
        stdout_b, stderr_b = await asyncio.wait_for(process.communicate(), timeout=timeout)
    except asyncio.TimeoutError as exc:
        with contextlib.suppress(ProcessLookupError):
            process.kill()
        raise EmacsError("emacsclient request timed out") from exc

    stdout = (stdout_b or b"").decode("utf-8", errors="replace").strip()
    stderr = (stderr_b or b"").decode("utf-8", errors="replace").strip()
    return stdout, stderr, process.returncode


def evaluate(expr: str, timeout_seconds: float = 5.0) -> str:
    stdout, stderr, code = _run_emacsclient_eval(expr, timeout_seconds)
    if code != 0 or (stderr and not stdout):
        raise EmacsError(f"Emacs eval error: {stderr or stdout}")
    return stdout


async def evaluate_async(expr: str, timeout_seconds: Optional[float] = None) -> str:
    stdout, stderr, code = await _run_emacsclient_eval_async(expr, timeout_seconds)
    if code != 0 or (stderr and not stdout):
        raise EmacsError(f"Emacs eval error: {stderr or stdout}")
    return stdout


@dataclass
class VisibleText:
    text: str
    start: int
    end: int


def _eval_b64_json(elisp_data_expr: str, timeout_seconds: float = 5.0) -> Any:
    elisp = f"""
    (let* ((data {elisp_data_expr})
           (json-str (progn (require 'json) (json-encode data)))
           (b64 (base64-encode-string json-str t)))
      b64)
    """.strip()

    stdout, stderr, code = _run_emacsclient_eval(elisp, timeout_seconds)
    if code != 0 or (stderr and not stdout):
        raise EmacsError(f"Emacs JSON eval error: {stderr or stdout}")

    if stdout.startswith('"') and stdout.endswith('"'):
        emacs_string = stdout[1:-1]
    else:
        emacs_string = stdout

    try:
        json_bytes = base64.b64decode(emacs_string)
        return json.loads(json_bytes)
    except Exception as exc:
        raise EmacsError(f"Failed to decode Emacs JSON: {exc}") from exc


def get_visible_text(timeout_seconds: float = 5.0) -> VisibleText:
    elisp_alist = """
      (let* ((ws (window-start))
             (we (window-end nil t))
             (beg (max (point-min) ws))
             (end (min (point-max) we))
             (beg (min beg end))
             (end (max beg end))
             (s (buffer-substring-no-properties beg end)))
        (list (cons 'text s)
              (cons 'start beg)
              (cons 'end end)))
    """.strip()

    data = _eval_b64_json(elisp_alist, timeout_seconds)
    return VisibleText(text=data["text"], start=int(data["start"]), end=int(data["end"]))


def get_context(timeout_seconds: float = 5.0) -> Dict[str, Any]:
    elisp_alist = """
      (condition-case err
          (let* ((win
                  (or
                   (catch 'found
                     (when (fboundp 'visible-frame-list)
                       (dolist (fr (visible-frame-list))
                         (let ((w (frame-selected-window fr)))
                           (when (and w (window-live-p w)
                                      (not (minibufferp (window-buffer w))))
                             (throw 'found w)))))
                     nil)
                   (and (window-live-p (selected-window)) (selected-window))))
                 (buf (and win (window-buffer win)))
                 (bn (and buf (buffer-name buf)))
                 (bf (and buf (ignore-errors (buffer-file-name buf))))
                 (mm (and buf (ignore-errors (format "%s" (buffer-local-value 'major-mode buf)))))
                 (pt (and win (window-point win)))
                 (line (and pt (with-current-buffer buf (line-number-at-pos pt))))
                 (col (and win (with-selected-window win (current-column))))
                 (mod (and buf (buffer-modified-p buf)))
                 (nar (and buf (with-current-buffer buf (buffer-narrowed-p))))
                 (ws (and win (with-selected-window win (window-start))))
                 (we (and win (with-selected-window win (window-end nil t))))
                 (bufs (mapcar #'buffer-name (buffer-list)))
                 (proj-root
                  (and (fboundp 'project-current)
                       (ignore-errors
                         (let ((proj (project-current nil)))
                           (when (and proj (fboundp 'project-root))
                             (project-root proj)))))))
            (list (cons 'buffer_name bn)
                  (cons 'buffer_file_name bf)
                  (cons 'major_mode mm)
                  (cons 'point pt)
                  (cons 'line line)
                  (cons 'column col)
                  (cons 'modified mod)
                  (cons 'narrowed nar)
                  (cons 'window_start ws)
                  (cons 'window_end we)
                  (cons 'buffers bufs)
                  (cons 'project_root proj-root)))
        (error (list (cons 'error (error-message-string err)))))
    """.strip()

    return _eval_b64_json(elisp_alist, timeout_seconds)


async def _eval_b64_json_async(elisp_data_expr: str, timeout_seconds: Optional[float] = None) -> Any:
    elisp = f"""
    (let* ((data {elisp_data_expr})
           (json-str (progn (require 'json) (json-encode data)))
           (b64 (base64-encode-string json-str t)))
      b64)
    """.strip()

    stdout, stderr, code = await _run_emacsclient_eval_async(elisp, timeout_seconds)
    if code != 0 or (stderr and not stdout):
        raise EmacsError(f"Emacs JSON eval error: {stderr or stdout}")

    if stdout.startswith('"') and stdout.endswith('"'):
        emacs_string = stdout[1:-1]
    else:
        emacs_string = stdout

    try:
        json_bytes = base64.b64decode(emacs_string)
        return json.loads(json_bytes)
    except Exception as exc:
        raise EmacsError(f"Failed to decode Emacs JSON: {exc}") from exc


async def get_visible_text_async(timeout_seconds: Optional[float] = None) -> VisibleText:
    elisp_alist = """
      (let* ((ws (window-start))
             (we (window-end nil t))
             (beg (max (point-min) ws))
             (end (min (point-max) we))
             (beg (min beg end))
             (end (max beg end))
             (s (buffer-substring-no-properties beg end)))
        (list (cons 'text s)
              (cons 'start beg)
              (cons 'end end)))
    """.strip()

    data = await _eval_b64_json_async(elisp_alist, timeout_seconds)
    return VisibleText(text=data["text"], start=int(data["start"]), end=int(data["end"]))


async def get_context_async(timeout_seconds: Optional[float] = None) -> Dict[str, Any]:
    elisp_alist = """
      (condition-case err
          (let* ((win
                  (or
                   (catch 'found
                     (when (fboundp 'visible-frame-list)
                       (dolist (fr (visible-frame-list))
                         (let ((w (frame-selected-window fr)))
                           (when (and w (window-live-p w)
                                      (not (minibufferp (window-buffer w))))
                             (throw 'found w)))))
                     nil)
                   (and (window-live-p (selected-window)) (selected-window))))
                 (buf (and win (window-buffer win)))
                 (bn (and buf (buffer-name buf)))
                 (bf (and buf (ignore-errors (buffer-file-name buf))))
                 (mm (and buf (ignore-errors (format "%s" (buffer-local-value 'major-mode buf)))))
                 (pt (and win (window-point win)))
                 (line (and pt (with-current-buffer buf (line-number-at-pos pt))))
                 (col (and win (with-selected-window win (current-column))))
                 (mod (and buf (buffer-modified-p buf)))
                 (nar (and buf (with-current-buffer buf (buffer-narrowed-p))))
                 (ws (and win (with-selected-window win (window-start))))
                 (we (and win (with-selected-window win (window-end nil t))))
                 (bufs (mapcar #'buffer-name (buffer-list)))
                 (proj-root
                  (and (fboundp 'project-current)
                       (ignore-errors
                         (let ((proj (project-current nil)))
                           (when (and proj (fboundp 'project-root))
                             (project-root proj)))))))
            (list (cons 'buffer_name bn)
                  (cons 'buffer_file_name bf)
                  (cons 'major_mode mm)
                  (cons 'point pt)
                  (cons 'line line)
                  (cons 'column col)
                  (cons 'modified mod)
                  (cons 'narrowed nar)
                  (cons 'window_start ws)
                  (cons 'window_end we)
                  (cons 'buffers bufs)
                  (cons 'project_root proj-root)))
        (error (list (cons 'error (error-message-string err)))))
    """.strip()

    return await _eval_b64_json_async(elisp_alist, timeout_seconds)


@app.tool(
    name="emacs_list_buffers",
    description="List open buffers with name, file path (if any), modified flag, and whether it is the current buffer.",
)
async def emacs_list_buffers() -> Dict[str, Any]:
    """Return a list of open buffers and their basic metadata.

    Response: { success: bool, buffers?: Array<{ name: str, file: str|None, modified: bool, current: bool }>, error?: str }
    """
    elisp = """
      (let* ((cur (buffer-name))
             (items (mapcar (lambda (b)
                              (with-current-buffer b
                                (list (cons 'name (buffer-name b))
                                      (cons 'file (ignore-errors (buffer-file-name b)))
                                      (cons 'modified (buffer-modified-p b))
                                      (cons 'current (string= (buffer-name b) cur)))))
                            (buffer-list))))
        items)
    """.strip()
    try:
        items = await _eval_b64_json_async(elisp, _get_timeout_seconds(None))
        # Convert any Emacs nil file to None in Python occurs via JSON null already
        return {"success": True, "buffers": items}
    except EmacsError as exc:
        return {"success": False, "error": str(exc)}


app = FastMCP(name="emacs-mcp-server")


@app.tool(
    name="emacs_eval",
    description="Evaluate an Emacs Lisp expression and return its printed result.",
)
async def emacs_eval(expr: str) -> Dict[str, Any]:
    try:
        result = await evaluate_async(expr, None)
        return {"success": True, "result": result}
    except EmacsError as exc:
        return {"success": False, "error": str(exc)}


@app.tool(
    name="emacs_get_visible_text",
    description="Get the text currently visible in the selected Emacs window.",
)
async def emacs_get_visible_text() -> Dict[str, Any]:
    try:
        # Prefer async path for consistency
        vt = await get_visible_text_async(_get_timeout_seconds(None))
        return {"success": True, "text": vt.text, "start": vt.start, "end": vt.end}
    except EmacsError as exc:
        return {"success": False, "error": str(exc)}


@app.tool(
    name="emacs_get_context",
    description=(
        "Get contextual information about the Emacs state: buffer name, file name, mode, point, line/column, modified, narrowed, window start/end, project root, and list of buffers."
    ),
)
async def emacs_get_context() -> Dict[str, Any]:
    try:
        ctx = await get_context_async(_get_timeout_seconds(None))
        return {"success": True, "context": ctx}
    except EmacsError as exc:
        return {"success": False, "error": str(exc)}


def smoke() -> None:
    print(f"emacs-mcp-server version: {__version__}")
    try:
        print("Evaluating (+ 1 2 3) via emacsclient...")
        res = asyncio.run(evaluate_async("(+ 1 2 3)", None))
        print("Result:", res)

        print("\nVisible text snippet...")
        vt = get_visible_text(_get_timeout_seconds(None))
        preview = vt.text.replace("\n", "\\n")
        if len(preview) > 80:
            preview = preview[:80] + "..."
        print(json.dumps({"start": vt.start, "end": vt.end, "textPreview": preview}, indent=2))

        print("\nContext...")
        ctx = get_context(_get_timeout_seconds(None))
        print(json.dumps(ctx, indent=2))
    except EmacsError as exc:
        print("Encountered EmacsError:", exc)


async def _check_emacs_connection(timeout_seconds: Optional[float] = None) -> bool:
    try:
        stdout, stderr, code = await _run_emacsclient_eval_async("t", timeout_seconds)
        return code == 0
    except EmacsError:
        return False


def main() -> None:
    parser = argparse.ArgumentParser(description="Emacs MCP Server")
    parser.add_argument("--smoke", action="store_true", help="Run a quick smoke check and exit")
    parser.add_argument("--strict-startup", action="store_true", help="Fail fast if Emacs is not reachable at startup")
    parser.add_argument("--timeout", type=float, default=None, help="Timeout (seconds) for emacsclient operations; overrides EMACSCLIENT_TIMEOUT")
    args = parser.parse_args()

    # Apply CLI timeout override
    global _DEFAULT_TIMEOUT_SECONDS
    if args.timeout is not None and args.timeout > 0:
        _DEFAULT_TIMEOUT_SECONDS = args.timeout

    if args.smoke:
        smoke()
        return

    # Optional startup connectivity check
    # Optional startup connectivity check with brief retry
    ok = asyncio.run(_check_emacs_connection(_get_timeout_seconds(None)))
    if not ok:
        # Retry once after a short delay to smooth transient startup issues
        try:
            asyncio.run(asyncio.sleep(0.2))
        except RuntimeError:
            pass
        ok = asyncio.run(_check_emacs_connection(_get_timeout_seconds(None)))
    if not ok:
        msg = (
            "Warning: cannot connect to Emacs. Ensure Emacs is running, the server is started, and emacsclient is on PATH."
        )
        print(msg, file=sys.stderr)
        if args.strict_startup:
            sys.exit(1)

    app.run()


if __name__ == "__main__":
    main()

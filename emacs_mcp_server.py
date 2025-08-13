from __future__ import annotations

import argparse
import base64
import json
import os
import subprocess
from dataclasses import dataclass
from typing import Any, Dict, Tuple

from fastmcp import FastMCP

__version__ = "0.1.0"


class EmacsError(RuntimeError):
    pass


def _resolve_emacsclient() -> str:
    return os.environ.get("EMACSCLIENT", "emacsclient")


def _run_emacsclient_eval(expr: str, timeout_seconds: float = 5.0) -> Tuple[str, str, int]:
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


def evaluate(expr: str, timeout_seconds: float = 5.0) -> str:
    stdout, stderr, code = _run_emacsclient_eval(expr, timeout_seconds)
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
                 (bufs (mapcar #'buffer-name (buffer-list))))
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
                  (cons 'buffers bufs)))
        (error (list (cons 'error (error-message-string err)))))
    """.strip()

    return _eval_b64_json(elisp_alist, timeout_seconds)


app = FastMCP(name="emacs-mcp-server")


@app.tool(
    name="emacs_eval",
    description="Evaluate an Emacs Lisp expression and return its printed result.",
)
async def emacs_eval(expr: str) -> Dict[str, Any]:
    try:
        result = evaluate(expr)
        return {"result": result}
    except EmacsError as exc:
        return {"error": str(exc)}


@app.tool(
    name="emacs_get_visible_text",
    description="Get the text currently visible in the selected Emacs window.",
)
async def emacs_get_visible_text() -> Dict[str, Any]:
    try:
        vt = get_visible_text()
        return {"text": vt.text, "start": vt.start, "end": vt.end}
    except EmacsError as exc:
        return {"error": str(exc)}


@app.tool(
    name="emacs_get_context",
    description=(
        "Get contextual information about the Emacs state: buffer name, file name, mode, point, line/column, modified, narrowed, window start/end, project root, and list of buffers."
    ),
)
async def emacs_get_context() -> Dict[str, Any]:
    try:
        ctx = get_context()
        return ctx
    except EmacsError as exc:
        return {"error": str(exc)}


def smoke() -> None:
    print(f"emacs-mcp-server version: {__version__}")
    try:
        print("Evaluating (+ 1 2 3) via emacsclient...")
        res = evaluate("(+ 1 2 3)")
        print("Result:", res)

        print("\nVisible text snippet...")
        vt = get_visible_text()
        preview = vt.text.replace("\n", "\\n")
        if len(preview) > 80:
            preview = preview[:80] + "..."
        print(json.dumps({"start": vt.start, "end": vt.end, "textPreview": preview}, indent=2))

        print("\nContext...")
        ctx = get_context()
        print(json.dumps(ctx, indent=2))
    except EmacsError as exc:
        print("Encountered EmacsError:", exc)


def main() -> None:
    parser = argparse.ArgumentParser(description="Emacs MCP Server")
    parser.add_argument("--smoke", action="store_true", help="Run a quick smoke check and exit")
    args = parser.parse_args()

    if args.smoke:
        smoke()
        return

    app.run()


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Emacs MCP Server - Control Emacs via MCP protocol using emacsclient."""

import subprocess
import json
from fastmcp import FastMCP


class EmacsError(Exception):
    """Exception raised when Emacs operations fail."""
    pass


def call_emacsclient(expression: str) -> str:
    """Execute an Emacs Lisp expression via emacsclient.

    Args:
        expression: The Emacs Lisp expression to evaluate

    Returns:
        The result as a string

    Raises:
        EmacsError: If the expression fails to execute or Emacs is unavailable
    """
    try:
        result = subprocess.run(
            ["emacsclient", "--eval", expression],
            capture_output=True,
            text=True,
            timeout=30,
            check=True
        )
        return result.stdout.strip()
    except subprocess.TimeoutExpired:
        raise EmacsError("Emacs evaluation timed out after 30 seconds")
    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip()
        if "server file" in error_msg or "connect" in error_msg:
            raise EmacsError("Emacs server not running. Start Emacs with M-x server-start")
        else:
            raise EmacsError(f"Emacs evaluation failed: {error_msg}")
    except FileNotFoundError:
        raise EmacsError("emacsclient not found. Please ensure Emacs is installed and in PATH")
    except Exception as e:
        raise EmacsError(f"Failed to communicate with Emacs: {str(e)}")


def is_emacs_available() -> bool:
    """Check if Emacs is available via emacsclient."""
    try:
        call_emacsclient("(+ 1 1)")
        return True
    except EmacsError:
        return False


# Create FastMCP server
mcp = FastMCP("emacs-mcp-server")


@mcp.tool()
def emacs_eval(expression: str) -> str:
    """Evaluate a Emacs Lisp expression in the currently visible Emacs buffer and return the result.

    Many Emacs Lisp expressions mutate state in Emacs and simply return 't' to indicate
    success rather than returning the changed state. After executing expressions that may
    change state, use emacs_get_context or emacs_get_visible_text to check the resulting
    state changes.

    Args:
        expression: Emacs Lisp expression to evaluate

    Returns:
        The result of evaluating the expression as a string
    """
    if not is_emacs_available():
        return "Error: Emacs is not available. Please ensure Emacs server is running."

    try:
        return call_emacsclient(expression)
    except EmacsError as e:
        return f"Error: {str(e)}"


@mcp.tool()
def emacs_get_visible_text() -> str:
    """Get the text content currently visible in the active Emacs window.

    Returns:
        The visible text content
    """
    if not is_emacs_available():
        return "Error: Emacs is not available. Please ensure Emacs server is running."

    try:
        expression = """
        (with-selected-window (selected-window)
          (with-current-buffer (window-buffer (selected-window))
            (let* ((start (window-start (selected-window)))
                   (end (window-end (selected-window) t))
                   (end-safe (min end (point-max))))
              (if (> start end-safe)
                  ""
                (buffer-substring-no-properties start end-safe)))))
        """

        result = call_emacsclient(expression)

        # Clean up the result - remove quotes if it's a string literal
        if result.startswith('"') and result.endswith('"'):
            return result[1:-1]
        return result
    except EmacsError as e:
        return f"Error: {str(e)}"


@mcp.tool()
def emacs_get_context() -> str:
    """Get contextual information about the currently visible Emacs buffer.

    Provides information about buffer name, mode, point position, etc.

    Returns:
        JSON string containing context information including buffer name,
        mode, point position, region information, and window bounds
    """
    if not is_emacs_available():
        return "Error: Emacs is not available. Please ensure Emacs server is running."

    try:
        # Get all context information in one go to reduce emacsclient calls
        context_expr = """
        (with-selected-window (selected-window)
          (with-current-buffer (window-buffer (selected-window))
            (json-encode
             (list
              :buffer (list
                       :name (buffer-name)
                       :file (buffer-file-name)
                       :size (point-max))
              :mode (list
                     :major (symbol-name major-mode))
              :position (list
                         :point (point)
                         :line (line-number-at-pos)
                         :column (current-column))
              :region (list
                       :active (region-active-p)
                       :mark (if (region-active-p) (mark) nil)
                       :text (if (region-active-p)
                                (buffer-substring-no-properties (region-beginning) (region-end))
                              nil))
              :window (list
                       :start (window-start (selected-window))
                       :end (window-end (selected-window)))))))
        """

        result = call_emacsclient(context_expr)

        # If json-encode isn't available, fall back to manual construction
        if result.startswith("Error:") or "json-encode" in result:
            # Fallback: get individual pieces with proper targeting
            buffer_name = call_emacsclient("(with-selected-window (selected-window) (buffer-name))")
            buffer_file = call_emacsclient("(with-selected-window (selected-window) (buffer-file-name))")
            major_mode = call_emacsclient("(with-selected-window (selected-window) (symbol-name major-mode))")
            point = call_emacsclient("(with-selected-window (selected-window) (point))")
            line_num = call_emacsclient("(with-selected-window (selected-window) (line-number-at-pos))")
            column = call_emacsclient("(with-selected-window (selected-window) (current-column))")
            point_max = call_emacsclient("(with-selected-window (selected-window) (point-max))")
            window_start = call_emacsclient("(window-start (selected-window))")
            window_end = call_emacsclient("(window-end (selected-window))")
            region_active = call_emacsclient("(with-selected-window (selected-window) (region-active-p))")

            # Clean up string results
            if buffer_name.startswith('"') and buffer_name.endswith('"'):
                buffer_name = buffer_name[1:-1]
            if buffer_file == "nil":
                buffer_file = None
            elif buffer_file.startswith('"') and buffer_file.endswith('"'):
                buffer_file = buffer_file[1:-1]
            if major_mode.startswith('"') and major_mode.endswith('"'):
                major_mode = major_mode[1:-1]

            context = {
                "buffer": {
                    "name": buffer_name,
                    "file": buffer_file,
                    "size": int(point_max) if point_max.isdigit() else 0
                },
                "mode": {
                    "major": major_mode
                },
                "position": {
                    "point": int(point) if point.isdigit() else 0,
                    "line": int(line_num) if line_num.isdigit() else 0,
                    "column": int(column) if column.isdigit() else 0
                },
                "region": {
                    "active": region_active == "t",
                    "mark": None,
                    "text": None
                },
                "window": {
                    "start": int(window_start) if window_start.isdigit() else 0,
                    "end": int(window_end) if window_end.isdigit() else 0
                }
            }

            return json.dumps(context, indent=2)

        # Handle Emacs' double-encoded JSON output
        try:
            # First, try to parse as double-encoded JSON (common with json-encode)
            if result.startswith('"') and result.endswith('"'):
                # Remove outer quotes and try to parse the JSON string
                json_string = json.loads(result)
                parsed = json.loads(json_string)
            else:
                # Try direct parsing
                parsed = json.loads(result)

            # Convert plist format to dictionary if needed
            if isinstance(parsed, list) and len(parsed) % 2 == 0:
                # Convert plist to dictionary
                context = {}
                for i in range(0, len(parsed), 2):
                    key = parsed[i].lstrip(':') if isinstance(parsed[i], str) else str(parsed[i])
                    value = parsed[i + 1]
                    context[key] = value
                parsed = context

            return json.dumps(parsed, indent=2)

        except json.JSONDecodeError:
            # If JSON parsing fails completely, return raw result
            return result

    except EmacsError as e:
        return f"Error: {str(e)}"
    except Exception as e:
        return f"Error: Failed to get Emacs context: {str(e)}"


def main():
    """Main entry point for the server."""
    mcp.run(transport="stdio")


if __name__ == "__main__":
    main()
#!/usr/bin/env python3
"""
MCP server for interacting with Emacs via emacsclient.

This server provides access to evaluate Emacs Lisp expressions via emacsclient
in the currently visible Emacs buffer. You can evaluate code or capture visible
text content from the active Emacs window.
"""

import base64
import binascii
import json
import subprocess
import sys
import shlex
from typing import Any, Dict, Optional

from fastmcp import FastMCP


def run_emacsclient(expression: str, timeout: int = 30) -> subprocess.CompletedProcess:
    """
    Run emacsclient with the given expression safely.

    Args:
        expression: The Emacs Lisp expression to evaluate
        timeout: Timeout in seconds

    Returns:
        CompletedProcess result
    """
    cmd = ['emacsclient', '--eval', expression]
    return subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        check=True,
        timeout=timeout
    )


def parse_emacs_result(stdout: str) -> str:
    """
    Parse emacsclient stdout, handling both JSON objects and quoted strings.

    Args:
        stdout: Raw stdout from emacsclient

    Returns:
        Cleaned result string (JSON objects returned as-is, strings unescaped)
    """
    result = stdout.strip()

    # Handle JSON objects from Emacs (json-encode output)
    if result.startswith('{') and result.endswith('}'):
        try:
            # Validate JSON and return as-is for structured data
            json.loads(result)
            return result
        except json.JSONDecodeError:
            # Fall through to string handling if not valid JSON
            pass

    # Handle Emacs string literals - remove outer quotes and unescape
    if len(result) >= 2 and result.startswith('"') and result.endswith('"'):
        # Unescape the string content
        inner = result[1:-1]
        inner = inner.replace('\\"', '"')
        inner = inner.replace('\\\\', '\\')
        inner = inner.replace('\\n', '\n')
        inner = inner.replace('\\t', '\t')
        return inner

    return result


def is_emacs_server_running() -> bool:
    """
    Check if Emacs server is running and accessible.

    Returns:
        True if Emacs server is running, false otherwise
    """
    try:
        subprocess.run(
            ['emacsclient', '--eval', 't'],
            capture_output=True,
            text=True,
            check=True,
            timeout=5
        )
        return True
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError):
        return False


def create_server_not_running_error() -> str:
    """
    Creates a standard error message for when the Emacs server is not running.

    Returns:
        Error message string
    """
    return "Error: Emacs server is not running. Please start it with M-x server-start in Emacs."


# Create the FastMCP server
mcp = FastMCP("emacs-mcp", version="0.1.0")

# Add server instructions
mcp.server_info = {
    "name": "emacs-mcp",
    "version": "0.1.0",
    "instructions": (
        "This server provides access to evaluate Emacs Lisp expressions via emacsclient "
        "in the currently visible Emacs buffer. You can evaluate code or capture visible "
        "text content from the active Emacs window. Make sure Emacs is running with server "
        "mode enabled (M-x server-start). Note that many Emacs Lisp expressions modify state "
        "but only return 't' to indicate success. When using emacs_eval to run such commands, "
        "follow up with emacs_get_context or emacs_get_visible_text to verify the actual state changes. "
        "Example pattern: First run emacs_eval with your command, then run emacs_get_context to check the new state."
    )
}


@mcp.tool()
def emacs_eval(expression: str) -> str:
    """
    Evaluate a Lisp expression in the currently visible Emacs buffer and return the result.
    Many Emacs Lisp expressions mutate state in Emacs and simply return 't' to indicate
    success rather than returning the changed state. After executing expressions that may
    change state, use emacs_get_context or emacs_get_visible_text to check the resulting
    state changes.

    Args:
        expression: Emacs Lisp expression to evaluate

    Returns:
        The result of the evaluation or error message
    """
    try:
        # Check if Emacs server is running
        if not is_emacs_server_running():
            return create_server_not_running_error()

        # Wrap the expression to target the visible buffer
        wrapped_expr = f"(with-current-buffer (window-buffer (selected-window)) {expression})"

        result = run_emacsclient(wrapped_expr)
        return parse_emacs_result(result.stdout)

    except subprocess.CalledProcessError as e:
        stderr = e.stderr.strip() if e.stderr else ""
        stdout = e.stdout.strip() if e.stdout else ""
        error_msg = stderr or stdout or "Unknown error"
        return f"Error evaluating expression: {error_msg}"
    except subprocess.TimeoutExpired:
        return "Error evaluating expression: Command timed out after 30 seconds."
    except Exception as e:
        return f"Error evaluating expression: {str(e)}"


@mcp.tool()
def emacs_get_visible_text() -> str:
    """
    Get the text content currently visible in the active Emacs window
    with metadata as structured JSON data including position information.

    Returns:
        JSON string with visible text and metadata or error message
    """
    try:
        # Check if Emacs server is running
        if not is_emacs_server_running():
            return create_server_not_running_error()

        # Enhanced Lisp code to get visible text with metadata as JSON
        # Use base64 encoding for text content to avoid JSON escaping issues
        visible_text_expression = """(with-current-buffer (window-buffer (selected-window))
  (let ((start (window-start))
        (end (window-end nil t)))
    (if (and start end)
        (let ((text (buffer-substring-no-properties start end)))
          (json-encode 
            (list (cons 'visible_text_base64 (base64-encode-string text t))
                  (cons 'start_pos start)
                  (cons 'end_pos end)
                  (cons 'start_line (line-number-at-pos start))
                  (cons 'end_line (line-number-at-pos end))
                  (cons 'total_chars (- end start))
                  (cons 'window_height (window-height))
                  (cons 'window_width (window-width)))))
      (json-encode 
        (list (cons 'visible_text_base64 "")
              (cons 'error "No visible text found"))))))"""

        result = run_emacsclient(visible_text_expression)
        json_result = parse_emacs_result(result.stdout)
        
        # If we got JSON with base64 encoded text, decode it
        try:
            data = json.loads(json_result)
            if 'visible_text_base64' in data and data['visible_text_base64']:
                decoded_text = base64.b64decode(data['visible_text_base64']).decode('utf-8')
                data['visible_text'] = decoded_text
                del data['visible_text_base64']
                return json.dumps(data)
        except (json.JSONDecodeError, base64.binascii.Error, UnicodeDecodeError):
            pass
        
        return json_result

    except subprocess.CalledProcessError as e:
        stderr = e.stderr.strip() if e.stderr else ""
        stdout = e.stdout.strip() if e.stdout else ""
        error_msg = stderr or stdout or "Unknown error"
        
        # If json-encode is not available, fall back to simple text
        if "json-encode" in error_msg or "void-function" in error_msg:
            return _get_visible_text_fallback()
        
        return f"Error getting visible text: {error_msg}"
    except subprocess.TimeoutExpired:
        return "Error getting visible text: Command timed out after 30 seconds."
    except Exception as e:
        return f"Error getting visible text: {str(e)}"


def _get_visible_text_fallback() -> str:
    """
    Fallback visible text function using simple text if json-encode is unavailable.
    
    Returns:
        Plain text content of visible area
    """
    try:
        # Fallback to original simple text approach
        visible_text_expression = """(with-current-buffer (window-buffer (selected-window))
  (let ((start (window-start))
        (end (window-end nil t)))
    (if (and start end)
        (buffer-substring-no-properties start end)
      "No visible text found")))"""

        result = run_emacsclient(visible_text_expression)
        return parse_emacs_result(result.stdout)
        
    except Exception as e:
        return f"Error getting visible text (fallback): {str(e)}"


@mcp.tool()
def emacs_get_context() -> str:
    """
    Get contextual information about the currently visible Emacs buffer
    as structured JSON data including name, mode, point position, file path, etc.

    Returns:
        JSON string with buffer context information or error message
    """
    try:
        # Check if Emacs server is running
        if not is_emacs_server_running():
            return create_server_not_running_error()

        # Lisp code to get detailed context as structured JSON
        context_expression = """(with-current-buffer (window-buffer (selected-window))
  (json-encode 
    (list (cons 'buffer_name (buffer-name))
          (cons 'mode (symbol-name major-mode))
          (cons 'point (point))
          (cons 'line_number (line-number-at-pos))
          (cons 'column (current-column))
          (cons 'file_path (or buffer-file-name nil))
          (cons 'modified (buffer-modified-p))
          (cons 'total_lines (count-lines (point-min) (point-max)))
          (cons 'buffer_size (buffer-size))
          (cons 'narrowed (not (= (point-min) 1)))
          (cons 'read_only buffer-read-only))))"""

        result = run_emacsclient(context_expression)
        return parse_emacs_result(result.stdout)

    except subprocess.CalledProcessError as e:
        stderr = e.stderr.strip() if e.stderr else ""
        stdout = e.stdout.strip() if e.stdout else ""
        error_msg = stderr or stdout or "Unknown error"
        
        # If json-encode is not available, fall back to string format
        if "json-encode" in error_msg or "void-function" in error_msg:
            return _get_context_fallback()
        
        return f"Error getting Emacs context: {error_msg}"
    except subprocess.TimeoutExpired:
        return "Error getting Emacs context: Command timed out after 30 seconds."
    except Exception as e:
        return f"Error getting Emacs context: {str(e)}"


def _get_context_fallback() -> str:
    """
    Fallback context function using string format if json-encode is unavailable.
    
    Returns:
        Formatted string with context information
    """
    try:
        # Fallback to original string-based approach
        context_expression = """(with-current-buffer (window-buffer (selected-window))
  (format "Buffer: %s\nMode: %s\nPoint: %d\nLine Number: %d\nColumn: %d\nFile: %s\nModified: %s\nTotal Lines: %d"
          (buffer-name)
          major-mode
          (point)
          (line-number-at-pos)
          (current-column)
          (or buffer-file-name "Not visiting a file")
          (if (buffer-modified-p) "Yes" "No")
          (count-lines (point-min) (point-max))))"""

        result = run_emacsclient(context_expression)
        return parse_emacs_result(result.stdout)
        
    except Exception as e:
        return f"Error getting Emacs context (fallback): {str(e)}"


def main():
    """Main entry point for the server."""
    # Check if Emacs server is running on startup
    if not is_emacs_server_running():
        print("Warning: Emacs server does not appear to be running.", file=sys.stderr)
        print("Please start the Emacs server with M-x server-start", file=sys.stderr)

    # Run the server
    mcp.run()


if __name__ == "__main__":
    main()


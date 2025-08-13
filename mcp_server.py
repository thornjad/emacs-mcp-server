#!/usr/bin/env python3
"""
MCP server implementation for Emacs control using FastMCP.
"""

import asyncio
import json
import subprocess
import sys
from typing import Optional

from fastmcp import FastMCP

from .emacs_client import (
    run_emacsclient,
    parse_emacs_result, 
    is_emacs_server_running,
    create_server_not_running_error
)


# Create the FastMCP server
mcp = FastMCP("emacs-mcp", version="0.2.0")

# Add server instructions
mcp.server_info = {
    "name": "emacs-mcp",
    "version": "0.2.0",
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
async def emacs_eval(expression: str, timeout: int = 5) -> str:
    """
    Evaluate a Lisp expression in the currently visible Emacs buffer and return the result.
    Many Emacs Lisp expressions mutate state in Emacs and simply return 't' to indicate
    success rather than returning the changed state. After executing expressions that may
    change state, use emacs_get_context or emacs_get_visible_text to check the resulting
    state changes.

    Args:
        expression: Emacs Lisp expression to evaluate
        timeout: Timeout in seconds (default: 5)

    Returns:
        The result of the evaluation or error message
    """
    try:
        # Check if Emacs server is running
        if not await is_emacs_server_running():
            return create_server_not_running_error()

        # Wrap the expression to target the visible buffer
        wrapped_expr = f"(with-current-buffer (window-buffer (selected-window)) {expression})"

        stdout, stderr, returncode = await run_emacsclient(wrapped_expr, timeout)
        
        if returncode != 0:
            error_msg = stderr.strip() or stdout.strip() or "Unknown error"
            return f"Error evaluating expression: {error_msg}"
            
        return parse_emacs_result(stdout)

    except subprocess.TimeoutExpired:
        return f"Error evaluating expression: Command timed out after {timeout} seconds."
    except Exception as e:
        return f"Error evaluating expression: {str(e)}"


@mcp.tool()
async def emacs_get_visible_text(timeout: int = 10) -> str:
    """
    Get the text content currently visible in the active Emacs window
    with metadata as structured JSON data including position information.

    Args:
        timeout: Timeout in seconds (default: 10)

    Returns:
        JSON string with visible text and metadata or error message
    """
    try:
        # Check if Emacs server is running
        if not await is_emacs_server_running():
            return create_server_not_running_error()

        # Get visible text and metadata separately, then use Python's JSON handling
        # This avoids JSON escaping issues by letting Python handle the text encoding
        
        # First get the metadata
        metadata_expression = """(with-current-buffer (window-buffer (selected-window))
  (let ((start (window-start))
        (end (window-end nil t)))
    (json-encode 
      (list (cons 'start_pos start)
            (cons 'end_pos end)
            (cons 'start_line (line-number-at-pos start))
            (cons 'end_line (line-number-at-pos end))
            (cons 'total_chars (- end start))
            (cons 'window_height (window-height))
            (cons 'window_width (window-width))))))"""
        
        # Get the raw text content
        text_expression = """(with-current-buffer (window-buffer (selected-window))
  (let ((start (window-start))
        (end (window-end nil t)))
    (if (and start end)
        (buffer-substring-no-properties start end)
      "")))"""
        
        # Get metadata and text separately
        metadata_stdout, metadata_stderr, metadata_returncode = await run_emacsclient(metadata_expression, timeout)
        text_stdout, text_stderr, text_returncode = await run_emacsclient(text_expression, timeout)
        
        if metadata_returncode != 0 or text_returncode != 0:
            error_msg = metadata_stderr or text_stderr or "Unknown error"
            if "json-encode" in error_msg or "void-function" in error_msg:
                return await _get_visible_text_fallback(timeout)
            return f"Error getting visible text: {error_msg}"
        
        metadata_json = parse_emacs_result(metadata_stdout)
        visible_text = parse_emacs_result(text_stdout)
        
        try:
            # Parse metadata JSON and add the text content
            metadata = json.loads(metadata_json)
            metadata['visible_text'] = visible_text
            
            # Let Python's json.dumps handle all the text escaping properly
            return json.dumps(metadata, ensure_ascii=False)
            
        except json.JSONDecodeError:
            # Fallback if metadata parsing fails
            return json.dumps({
                "visible_text": visible_text,
                "error": "Failed to parse metadata"
            }, ensure_ascii=False)

    except subprocess.TimeoutExpired:
        return f"Error getting visible text: Command timed out after {timeout} seconds."
    except Exception as e:
        return f"Error getting visible text: {str(e)}"


async def _get_visible_text_fallback(timeout: int = 10) -> str:
    """
    Fallback visible text function using simple text if json-encode is unavailable.
    
    Args:
        timeout: Timeout in seconds
        
    Returns:
        JSON string with just the text content
    """
    try:
        # Fallback to simple text approach, but still return as JSON
        visible_text_expression = """(with-current-buffer (window-buffer (selected-window))
  (let ((start (window-start))
        (end (window-end nil t)))
    (if (and start end)
        (buffer-substring-no-properties start end)
      "No visible text found")))"""

        stdout, stderr, returncode = await run_emacsclient(visible_text_expression, timeout)
        
        if returncode != 0:
            return json.dumps({
                "visible_text": "",
                "error": f"Fallback failed: {stderr or 'Unknown error'}"
            }, ensure_ascii=False)
            
        visible_text = parse_emacs_result(stdout)
        
        # Return as JSON using Python's proper escaping
        return json.dumps({
            "visible_text": visible_text,
            "fallback": True
        }, ensure_ascii=False)
        
    except Exception as e:
        return json.dumps({
            "visible_text": "",
            "error": f"Error getting visible text (fallback): {str(e)}"
        }, ensure_ascii=False)


@mcp.tool()
async def emacs_get_context(timeout: int = 5) -> str:
    """
    Get contextual information about the currently visible Emacs buffer
    as structured JSON data including name, mode, point position, file path, etc.

    Args:
        timeout: Timeout in seconds (default: 5)

    Returns:
        JSON string with buffer context information or error message
    """
    try:
        # Check if Emacs server is running
        if not await is_emacs_server_running():
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
          (cons 'narrowed (and (> (point-min) 1) (< (point-max) (buffer-size))))
          (cons 'read_only buffer-read-only))))"""

        stdout, stderr, returncode = await run_emacsclient(context_expression, timeout)
        
        if returncode != 0:
            error_msg = stderr.strip() or stdout.strip() or "Unknown error"
            # If json-encode is not available, fall back to string format
            if "json-encode" in error_msg or "void-function" in error_msg:
                return await _get_context_fallback(timeout)
            return f"Error getting Emacs context: {error_msg}"
            
        return parse_emacs_result(stdout)

    except subprocess.TimeoutExpired:
        return f"Error getting Emacs context: Command timed out after {timeout} seconds."
    except Exception as e:
        return f"Error getting Emacs context: {str(e)}"


async def _get_context_fallback(timeout: int = 5) -> str:
    """
    Fallback context function using string format if json-encode is unavailable.
    
    Args:
        timeout: Timeout in seconds
        
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

        stdout, stderr, returncode = await run_emacsclient(context_expression, timeout)
        
        if returncode != 0:
            return f"Error getting Emacs context (fallback): {stderr or 'Unknown error'}"
            
        return parse_emacs_result(stdout)
        
    except Exception as e:
        return f"Error getting Emacs context (fallback): {str(e)}"


async def main():
    """Main entry point for the server."""
    # Check if Emacs server is running on startup
    if not await is_emacs_server_running():
        print("Warning: Emacs server does not appear to be running.", file=sys.stderr)
        print("Please start the Emacs server with M-x server-start", file=sys.stderr)

    # Run the server
    await mcp.run()


if __name__ == "__main__":
    asyncio.run(main())
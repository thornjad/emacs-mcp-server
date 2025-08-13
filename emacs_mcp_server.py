#!/usr/bin/env python3
"""
Emacs MCP Server

A simple MCP server that provides three tools for interacting with Emacs:
- emacs_eval: Evaluate Emacs Lisp expressions
- emacs_get_visible_text: Get visible window content
- emacs_get_context: Get buffer context information

Requires a running Emacs instance with server mode enabled.
"""

import asyncio
import json
import logging
import subprocess
import sys
from typing import Any, Dict

from fastmcp import FastMCP

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler(sys.stderr)]
)

logger = logging.getLogger(__name__)

# Initialize MCP server
mcp = FastMCP("Emacs MCP Server")


async def run_emacsclient(args: list[str], timeout: int = 10) -> tuple[bool, str, str]:
    """Run emacsclient with given arguments.
    
    Returns:
        (success, stdout, stderr)
    """
    try:
        result = subprocess.run(
            ["emacsclient"] + args,
            capture_output=True,
            text=True,
            timeout=timeout
        )
        return result.returncode == 0, result.stdout.strip(), result.stderr.strip()
    except subprocess.TimeoutExpired:
        return False, "", f"Command timed out after {timeout} seconds"
    except Exception as e:
        return False, "", str(e)


async def _emacs_eval(expression: str) -> Dict[str, Any]:
    """Evaluate a given Emacs Lisp expression and return the result.
    
    Args:
        expression: The Emacs Lisp expression to evaluate
        
    Returns:
        Dictionary with success status and result or error
    """
    if not expression.strip():
        return {"success": False, "error": "Expression cannot be empty"}
    
    logger.info(f"Evaluating: {expression}")
    
    success, stdout, stderr = await run_emacsclient(["--eval", expression])
    
    if success:
        return {"success": True, "result": stdout}
    else:
        error = stderr or "Unknown error"
        logger.error(f"Evaluation failed: {error}")
        return {"success": False, "error": error}


async def _emacs_get_visible_text() -> Dict[str, Any]:
    """Get the text content currently visible in the active Emacs window.
    
    Returns:
        Dictionary with success status and visible text or error
    """
    logger.info("Getting visible text")
    
    expression = '''
    (let ((start (window-start))
          (end (window-end)))
      (buffer-substring start end))
    '''
    
    success, stdout, stderr = await run_emacsclient(["--eval", expression])
    
    if success:
        return {"success": True, "visible_text": stdout}
    else:
        error = stderr or "Failed to get visible text"
        logger.error(f"Get visible text failed: {error}")
        return {"success": False, "error": error}


async def _emacs_get_context() -> Dict[str, Any]:
    """Get contextual information about the currently visible Emacs buffer.
    
    Returns:
        Dictionary with success status and context information or error
    """
    logger.info("Getting context")
    
    expression = '''
    (json-encode 
     (list 
      (cons "buffer_name" (buffer-name))
      (cons "major_mode" (symbol-name major-mode))
      (cons "point" (point))
      (cons "mark" (if mark-active (mark) nil))
      (cons "buffer_size" (buffer-size))
      (cons "window_start" (window-start))
      (cons "window_end" (window-end))
      (cons "buffer_list" (mapcar 'buffer-name (buffer-list)))
      (cons "current_line" (line-number-at-pos))
      (cons "current_column" (current-column))
      (cons "buffer_modified" (buffer-modified-p))
      (cons "buffer_file_name" (buffer-file-name))))
    '''
    
    success, stdout, stderr = await run_emacsclient(["--eval", expression])
    
    if success:
        try:
            # Parse JSON response (remove surrounding quotes)
            context_data = json.loads(stdout.strip('"'))
            return {"success": True, "context": context_data}
        except json.JSONDecodeError as e:
            error = f"Failed to parse context JSON: {str(e)}"
            logger.error(error)
            return {"success": False, "error": error}
    else:
        error = stderr or "Failed to get context"
        logger.error(f"Get context failed: {error}")
        return {"success": False, "error": error}


async def check_emacs_connection() -> bool:
    """Check if Emacs is available and responsive."""
    success, _, _ = await run_emacsclient(["--eval", "t"])
    return success


# Register tools with MCP
@mcp.tool()
async def emacs_eval(expression: str) -> Dict[str, Any]:
    """Evaluate a given Emacs Lisp expression and return the result."""
    return await _emacs_eval(expression)


@mcp.tool()
async def emacs_get_visible_text() -> Dict[str, Any]:
    """Get the text content currently visible in the active Emacs window."""
    return await _emacs_get_visible_text()


@mcp.tool()
async def emacs_get_context() -> Dict[str, Any]:
    """Get contextual information about the currently visible Emacs buffer."""
    return await _emacs_get_context()


async def main():
    """Main entry point."""
    logger.info("Starting Emacs MCP Server")
    
    # Check Emacs connection
    if not await check_emacs_connection():
        logger.error("Cannot connect to Emacs. Please ensure:")
        logger.error("1. Emacs is running")
        logger.error("2. Emacs server is started (M-x server-start or --daemon)")
        logger.error("3. emacsclient is available in PATH")
        sys.exit(1)
    
    logger.info("Emacs connection successful")
    
    try:
        await mcp.run()
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
    except Exception as e:
        logger.error(f"Server error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    asyncio.run(main())
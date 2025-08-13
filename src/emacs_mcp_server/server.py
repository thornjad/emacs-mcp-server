"""FastMCP server implementation for Emacs control."""

import json
from typing import Any

from fastmcp import FastMCP
from pydantic import Field

from .emacs_client import EmacsClient, EmacsClientError
from .models import EmacsEvalRequest, EmacsGetVisibleTextRequest, EmacsContextInfo


# initialize MCP server
mcp = FastMCP("Emacs MCP Server")

# global Emacs client instance
emacs_client = EmacsClient()


@mcp.tool()
async def emacs_eval(
    expression: str = Field(..., description="Emacs Lisp expression to evaluate"),
    timeout: int = Field(5, description="Timeout in seconds (default: 5)")
) -> str:
    """Evaluate Emacs Lisp expressions and return results.
    
    This tool allows you to execute arbitrary Emacs Lisp code in the running
    Emacs instance. Use it to interact with buffers, execute commands, or
    query Emacs state.
    
    Args:
        expression: The Emacs Lisp expression to evaluate
        timeout: Maximum time to wait for evaluation (default: 5 seconds)
        
    Returns:
        String representation of the evaluation result or error message
    """
    try:
        result = await emacs_client.eval_expression(expression, timeout)
        return result
    except EmacsClientError as e:
        return f"Error: {str(e)}"


@mcp.tool()
async def emacs_get_visible_text(
    buffer_name: str = Field(None, description="Specific buffer name (default: current buffer)")
) -> str:
    """Get text currently visible in the Emacs window.
    
    This tool retrieves the text content that is currently visible in the
    Emacs window, which corresponds to what the user can see on screen.
    
    Args:
        buffer_name: Optional specific buffer name to get text from.
                    If not provided, uses the current buffer.
        
    Returns:
        The visible text content as a string
    """
    try:
        result = await emacs_client.get_visible_text(buffer_name)
        return result
    except EmacsClientError as e:
        return f"Error: {str(e)}"


@mcp.tool()
async def emacs_get_context() -> str:
    """Get contextual information about the current Emacs state.
    
    This tool provides comprehensive information about the current state
    of Emacs, including buffer information, cursor position, and available
    buffers.
    
    Returns:
        JSON string containing context information including:
        - Current buffer name
        - Major mode
        - Point position (cursor)
        - Mark position (if set)
        - Window start/end positions
        - List of available buffers
        - Current working directory
    """
    try:
        context = await emacs_client.get_context()
        # return as JSON string for MCP compatibility
        return json.dumps(context.model_dump(), indent=2)
    except EmacsClientError as e:
        return f"Error: {str(e)}"


def create_server() -> FastMCP:
    """Create and return the configured MCP server instance."""
    return mcp

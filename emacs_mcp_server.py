#!/usr/bin/env python3
"""
Emacs MCP Server

A Model Context Protocol server that provides programmatic control of Emacs.
Consolidated single-file implementation.
"""

import asyncio
import subprocess
import json
import sys
from typing import Any, Dict
import logging

from mcp.server.models import InitializationOptions
from mcp.server import NotificationOptions, Server
from mcp.types import Resource, Tool, TextContent, ImageContent, EmbeddedResource
import mcp.types as types

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("emacs-mcp-server")

# Create server instance
server = Server("emacs-mcp-server")


class EmacsError(Exception):
    """Exception raised when Emacs operations fail."""
    pass


async def run_emacsclient(expression: str, timeout: int = 10) -> str:
    """
    Execute an Emacs Lisp expression using emacsclient.
    
    Args:
        expression: The Emacs Lisp expression to evaluate
        timeout: Timeout in seconds
        
    Returns:
        The result of the expression as a string
        
    Raises:
        EmacsError: If the command fails or times out
    """
    try:
        # Use emacsclient with -e to execute the expression in the selected frame
        cmd = ["emacsclient", "-e", expression]
        logger.debug(f"Running command: {cmd}")
        
        process = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        
        try:
            stdout, stderr = await asyncio.wait_for(
                process.communicate(), 
                timeout=timeout
            )
        except asyncio.TimeoutError:
            process.kill()
            await process.wait()
            raise EmacsError(f"Command timed out after {timeout} seconds")
        
        if process.returncode != 0:
            error_msg = stderr.decode('utf-8').strip()
            if "server file" in error_msg or "connect" in error_msg:
                raise EmacsError("Emacs server not running. Start Emacs with M-x server-start")
            else:
                raise EmacsError(f"emacsclient failed: {error_msg}")
        
        result = stdout.decode('utf-8').strip()
        logger.debug(f"Command result: {result}")
        return result
        
    except FileNotFoundError:
        raise EmacsError("emacsclient not found. Is Emacs installed?")
    except Exception as e:
        if isinstance(e, EmacsError):
            raise
        raise EmacsError(f"Unexpected error: {str(e)}")


async def eval_elisp(expression: str) -> str:
    """Evaluate an Emacs Lisp expression and return the result."""
    return await run_emacsclient(expression)


async def get_visible_text() -> str:
    """Get the text currently visible in the active Emacs window."""
    expression = """
(with-selected-window (selected-window)
  (with-current-buffer (window-buffer (selected-window))
    (let ((start (window-start (selected-window)))
          (end (min (window-end (selected-window) t) (point-max))))
      (if (and start end (<= start end) (<= end (point-max)))
          (buffer-substring start end)
        (buffer-string)))))
"""
    return await run_emacsclient(expression)


async def get_context() -> Dict[str, Any]:
    """Get comprehensive context about the current Emacs state."""
    # Build a comprehensive Emacs Lisp expression to gather context
    # Make sure we get info from the selected window and buffer
    expression = """
(with-selected-window (selected-window)
  (with-current-buffer (window-buffer (selected-window))
    (json-encode
     (list
      :buffer-name (buffer-name)
      :file-name (buffer-file-name)
      :major-mode (symbol-name major-mode)
      :minor-modes (mapcar #'symbol-name (bound-and-true-p minor-mode-list))
      :point (point)
      :line-number (line-number-at-pos)
      :column-number (current-column)
      :mark-active (if mark-active t nil)
      :buffer-modified (if (buffer-modified-p) t nil)
      :buffer-size (buffer-size)
      :window-start (window-start (selected-window))
      :window-end (window-end (selected-window) t)
      :buffer-list (mapcar #'buffer-name (buffer-list))))))
"""
    
    result = await run_emacsclient(expression)
    
    try:
        # The result from json-encode is double-encoded (a JSON string containing JSON)
        # First decode gets us the JSON string, second decode gets us the actual data
        json_string = json.loads(result)
        context_data = json.loads(json_string)
        
        # If it's already a dictionary (newer Emacs JSON format), return it
        if isinstance(context_data, dict):
            # Convert keys to remove colons and handle kebab-case to underscore
            context = {}
            for key, value in context_data.items():
                clean_key = key.lstrip(':') if key.startswith(':') else key
                # Replace hyphens with underscores for consistency  
                clean_key = clean_key.replace('-', '_')
                context[clean_key] = value
            return context
        
        # If it's a list (plist format), convert to dictionary  
        elif isinstance(context_data, list) and len(context_data) % 2 == 0:
            context = {}
            for i in range(0, len(context_data), 2):
                key = context_data[i].lstrip(':')  # Remove colon prefix
                value = context_data[i + 1]
                context[key] = value
            return context
        
        else:
            raise ValueError("Unexpected JSON structure")
            
    except (json.JSONDecodeError, IndexError, ValueError) as e:
        logger.error(f"Failed to parse context JSON: {e}")
        # Fallback: return basic info
        return {
            "error": "Failed to parse full context",
            "raw_result": result
        }


async def check_emacs_available() -> bool:
    """Check if Emacs server is available."""
    try:
        await run_emacsclient("t")
        return True
    except EmacsError:
        return False


def format_context(context: Dict[str, Any]) -> str:
    """Format the context dictionary into a readable string."""
    if "error" in context:
        return f"Error getting context: {context['error']}\nRaw result: {context.get('raw_result', 'N/A')}"
    
    lines = []
    lines.append("=== Emacs Context ===")
    
    # Buffer information
    buffer_name = context.get('buffer_name', 'N/A')
    file_name = context.get('file_name', 'N/A')
    lines.append(f"Buffer: {buffer_name}")
    if file_name and file_name != 'N/A' and file_name is not None:
        lines.append(f"File: {file_name}")
    
    # Position information
    line_num = context.get('line_number', 'N/A')
    col_num = context.get('column_number', 'N/A')
    point = context.get('point', 'N/A')
    lines.append(f"Position: Line {line_num}, Column {col_num} (Point: {point})")
    
    # Mode information
    major_mode = context.get('major_mode', 'N/A')
    lines.append(f"Major Mode: {major_mode}")
    
    minor_modes = context.get('minor_modes', [])
    if minor_modes:
        lines.append(f"Minor Modes: {', '.join(minor_modes[:10])}{'...' if len(minor_modes) > 10 else ''}")
    
    # Buffer state
    modified = context.get('buffer_modified', False)
    mark_active = context.get('mark_active', False)
    buffer_size = context.get('buffer_size', 'N/A')
    
    lines.append(f"Buffer Size: {buffer_size} characters")
    lines.append(f"Modified: {'Yes' if modified else 'No'}")
    lines.append(f"Mark Active: {'Yes' if mark_active else 'No'}")
    
    # Window information
    window_start = context.get('window_start', 'N/A')
    window_end = context.get('window_end', 'N/A')
    lines.append(f"Window Range: {window_start} - {window_end}")
    
    # Buffer list
    buffer_list = context.get('buffer_list', [])
    if buffer_list:
        lines.append(f"Open Buffers ({len(buffer_list)}): {', '.join(buffer_list[:5])}{'...' if len(buffer_list) > 5 else ''}")
    
    return '\n'.join(lines)


@server.list_tools()
async def handle_list_tools() -> list[Tool]:
    """List available tools. Each tool allows interaction with Emacs."""
    return [
        Tool(
            name="emacs_eval",
            description="Execute an Emacs Lisp expression and return the result",
            inputSchema={
                "type": "object",
                "properties": {
                    "expression": {
                        "type": "string",
                        "description": "The Emacs Lisp expression to evaluate"
                    }
                },
                "required": ["expression"]
            }
        ),
        Tool(
            name="emacs_get_visible_text",
            description="Get the text currently visible in the active Emacs window",
            inputSchema={
                "type": "object",
                "properties": {},
                "required": []
            }
        ),
        Tool(
            name="emacs_get_context",
            description="Get contextual information about the current Emacs state including buffer, mode, point position, etc.",
            inputSchema={
                "type": "object",
                "properties": {},
                "required": []
            }
        )
    ]


@server.call_tool()
async def handle_call_tool(name: str, arguments: dict[str, Any] | None) -> list[types.TextContent | types.ImageContent | types.EmbeddedResource]:
    """Handle tool calls for Emacs operations."""
    if arguments is None:
        arguments = {}

    # Check if Emacs is available
    if not await check_emacs_available():
        return [
            types.TextContent(
                type="text",
                text="Error: Emacs server not available. Please start Emacs and run M-x server-start"
            )
        ]

    try:
        if name == "emacs_eval":
            expression = arguments.get("expression", "")
            if not expression:
                return [
                    types.TextContent(
                        type="text",
                        text="Error: No expression provided"
                    )
                ]
            
            result = await eval_elisp(expression)
            return [
                types.TextContent(
                    type="text",
                    text=result
                )
            ]
            
        elif name == "emacs_get_visible_text":
            result = await get_visible_text()
            return [
                types.TextContent(
                    type="text",
                    text=result
                )
            ]
            
        elif name == "emacs_get_context":
            context = await get_context()
            # Format context as readable text
            formatted_context = format_context(context)
            return [
                types.TextContent(
                    type="text",
                    text=formatted_context
                )
            ]
            
        else:
            return [
                types.TextContent(
                    type="text",
                    text=f"Error: Unknown tool '{name}'"
                )
            ]
            
    except EmacsError as e:
        return [
            types.TextContent(
                type="text",
                text=f"Emacs Error: {str(e)}"
            )
        ]
    except Exception as e:
        logger.error(f"Unexpected error in tool '{name}': {e}")
        return [
            types.TextContent(
                type="text",
                text=f"Error: {str(e)}"
            )
        ]


async def async_main():
    """Async main entry point for the server."""
    # Run the server using stdio transport
    from mcp.server.stdio import stdio_server
    
    async with stdio_server() as (read_stream, write_stream):
        await server.run(
            read_stream,
            write_stream,
            InitializationOptions(
                server_name="emacs-mcp-server",
                server_version="0.1.0",
                capabilities=server.get_capabilities(
                    notification_options=NotificationOptions(),
                    experimental_capabilities={},
                ),
            )
        )


def main():
    """Main entry point for the server."""
    asyncio.run(async_main())


if __name__ == "__main__":
    main()

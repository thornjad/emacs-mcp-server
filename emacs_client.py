#!/usr/bin/env python3
"""
Core Emacs communication client using emacsclient.
"""

import asyncio
import json
import subprocess
from typing import Tuple


async def run_emacsclient(expression: str, timeout: int = 30) -> Tuple[str, str, int]:
    """
    Run emacsclient with the given expression safely using async subprocess.

    Args:
        expression: The Emacs Lisp expression to evaluate
        timeout: Timeout in seconds

    Returns:
        Tuple of (stdout, stderr, returncode)
    """
    cmd = ['emacsclient', '--eval', expression]
    
    process = await asyncio.create_subprocess_exec(
        *cmd,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE
    )
    
    try:
        stdout, stderr = await asyncio.wait_for(
            process.communicate(), timeout=timeout
        )
        return stdout.decode('utf-8'), stderr.decode('utf-8'), process.returncode
        
    except asyncio.TimeoutError:
        process.kill()
        await process.wait()
        raise subprocess.TimeoutExpired(cmd, timeout)


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


async def is_emacs_server_running() -> bool:
    """
    Check if Emacs server is running and accessible.

    Returns:
        True if Emacs server is running, false otherwise
    """
    try:
        stdout, stderr, returncode = await run_emacsclient('t', timeout=5)
        return returncode == 0
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError, asyncio.TimeoutError):
        return False


def create_server_not_running_error() -> str:
    """
    Creates a detailed error message for when the Emacs server is not running.

    Returns:
        Error message string with troubleshooting steps
    """
    return """Error: Emacs server is not running.

Troubleshooting steps:
1. Start Emacs server: M-x server-start (in Emacs) or 'emacs --daemon' (command line)  
2. Check if emacsclient is installed: run 'emacsclient --version'
3. Verify Emacs is running: check process list for 'emacs'
4. Try manual test: 'emacsclient --eval "t"' should return 't'

For more help, see: https://www.emacswiki.org/emacs/EmacsAsDaemon"""
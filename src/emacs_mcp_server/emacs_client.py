"""Emacs client for communicating with running Emacs daemon via emacsclient."""

import asyncio
import json
import subprocess
from typing import Any, Optional

from .models import EmacsContextInfo


class EmacsClientError(Exception):
    """Exception raised when Emacs client operations fail."""
    pass


class EmacsClient:
    """Client for communicating with Emacs daemon via emacsclient."""
    
    def __init__(self, client_cmd: str = "emacsclient"):
        """Initialize Emacs client.
        
        Args:
            client_cmd: Command to use for emacsclient (default: "emacsclient")
        """
        self.client_cmd = client_cmd
    
    async def eval_expression(self, expression: str, timeout: int = 5) -> str:
        """Evaluate an Emacs Lisp expression.
        
        Args:
            expression: Emacs Lisp expression to evaluate
            timeout: Timeout in seconds
            
        Returns:
            String representation of the evaluation result
            
        Raises:
            EmacsClientError: If evaluation fails or times out
        """
        cmd = [self.client_cmd, "--eval", expression]
        
        try:
            process = await asyncio.create_subprocess_exec(
                *cmd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await asyncio.wait_for(
                process.communicate(), timeout=timeout
            )
            
            if process.returncode != 0:
                error_msg = stderr.decode('utf-8').strip()
                if "emacs: can't connect to server" in error_msg.lower():
                    raise EmacsClientError(
                        "Emacs server is not running. Start with 'emacs --daemon' "
                        "or run '(server-start)' in your Emacs session."
                    )
                raise EmacsClientError(f"Emacs evaluation failed: {error_msg}")
            
            return stdout.decode('utf-8').strip()
            
        except asyncio.TimeoutError:
            raise EmacsClientError(f"Emacs evaluation timed out after {timeout} seconds")
        except FileNotFoundError:
            raise EmacsClientError(
                "emacsclient command not found. Please ensure Emacs is installed "
                "and emacsclient is in your PATH."
            )
    
    async def get_visible_text(self, buffer_name: Optional[str] = None) -> str:
        """Get text currently visible in the Emacs window.
        
        Args:
            buffer_name: Specific buffer name (default: current buffer)
            
        Returns:
            Visible text content as string
        """
        if buffer_name:
            # switch to specific buffer and get visible text
            expression = f'''
            (with-current-buffer "{buffer_name}"
              (buffer-substring (window-start) (window-end)))
            '''
        else:
            # get visible text from current buffer
            expression = "(buffer-substring (window-start) (window-end))"
        
        return await self.eval_expression(expression)
    
    async def get_context(self) -> EmacsContextInfo:
        """Get contextual information about current Emacs state.
        
        Returns:
            EmacsContextInfo object with current state information
        """
        # build a comprehensive lisp expression to gather all context from the selected window
        expression = '''
        (json-encode
         (list
          :current-buffer (buffer-name (window-buffer (selected-window)))
          :major-mode (with-current-buffer (window-buffer (selected-window)) (symbol-name major-mode))
          :point-position (with-current-buffer (window-buffer (selected-window)) (point))
          :mark-position (with-current-buffer (window-buffer (selected-window)) (if mark-active (mark) nil))
          :window-start (window-start (selected-window))
          :window-end (window-end (selected-window))
          :available-buffers (mapcar #'buffer-name (buffer-list))
          :current-working-directory (with-current-buffer (window-buffer (selected-window)) default-directory)))
        '''
        
        result_json = await self.eval_expression(expression)
        
        try:
            # the result might be double-encoded as a JSON string
            if result_json.startswith('"') and result_json.endswith('"'):
                # remove outer quotes and unescape
                result_json = json.loads(result_json)
            data = json.loads(result_json)
            return EmacsContextInfo(
                current_buffer=data["current-buffer"],
                major_mode=data["major-mode"],
                point_position=data["point-position"],
                mark_position=data.get("mark-position"),
                window_start=data["window-start"],
                window_end=data["window-end"],
                available_buffers=data["available-buffers"],
                current_working_directory=data["current-working-directory"]
            )
        except (json.JSONDecodeError, KeyError) as e:
            raise EmacsClientError(f"Failed to parse Emacs context data: {e}")
    
    async def is_server_running(self) -> bool:
        """Check if Emacs server is running.
        
        Returns:
            True if server is running, False otherwise
        """
        try:
            await self.eval_expression("t", timeout=2)
            return True
        except EmacsClientError:
            return False

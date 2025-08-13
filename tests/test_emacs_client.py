"""Tests for Emacs client functionality."""

import pytest
from unittest.mock import AsyncMock, patch
import asyncio

from emacs_mcp_server.emacs_client import EmacsClient, EmacsClientError
from emacs_mcp_server.models import EmacsContextInfo


class TestEmacsClient:
    """Test cases for EmacsClient."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.client = EmacsClient()
    
    @pytest.mark.asyncio
    async def test_eval_expression_success(self):
        """Test successful expression evaluation."""
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            # mock successful process
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate.return_value = (b"6\n", b"")
            mock_subprocess.return_value = mock_process
            
            result = await self.client.eval_expression("(+ 1 2 3)")
            assert result == "6"
    
    @pytest.mark.asyncio
    async def test_eval_expression_server_not_running(self):
        """Test error when Emacs server is not running."""
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            # mock server not running error
            mock_process = AsyncMock()
            mock_process.returncode = 1
            mock_process.communicate.return_value = (b"", b"emacs: can't connect to server")
            mock_subprocess.return_value = mock_process
            
            with pytest.raises(EmacsClientError) as exc_info:
                await self.client.eval_expression("(+ 1 2 3)")
            
            assert "Emacs server is not running" in str(exc_info.value)
    
    @pytest.mark.asyncio
    async def test_eval_expression_timeout(self):
        """Test timeout handling."""
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            mock_process = AsyncMock()
            # simulate timeout
            mock_process.communicate.side_effect = asyncio.TimeoutError()
            mock_subprocess.return_value = mock_process
            
            with pytest.raises(EmacsClientError) as exc_info:
                await self.client.eval_expression("(sleep 10)", timeout=1)
            
            assert "timed out" in str(exc_info.value)
    
    @pytest.mark.asyncio
    async def test_eval_expression_command_not_found(self):
        """Test error when emacsclient command is not found."""
        with patch('asyncio.create_subprocess_exec') as mock_subprocess:
            mock_subprocess.side_effect = FileNotFoundError()
            
            with pytest.raises(EmacsClientError) as exc_info:
                await self.client.eval_expression("(+ 1 2 3)")
            
            assert "emacsclient command not found" in str(exc_info.value)
    
    @pytest.mark.asyncio
    async def test_get_visible_text_default_buffer(self):
        """Test getting visible text from current buffer."""
        with patch.object(self.client, 'eval_expression') as mock_eval:
            mock_eval.return_value = "Hello, world!"
            
            result = await self.client.get_visible_text()
            
            assert result == "Hello, world!"
            mock_eval.assert_called_once_with("(buffer-substring (window-start) (window-end))")
    
    @pytest.mark.asyncio
    async def test_get_visible_text_specific_buffer(self):
        """Test getting visible text from specific buffer."""
        with patch.object(self.client, 'eval_expression') as mock_eval:
            mock_eval.return_value = "Buffer content"
            
            result = await self.client.get_visible_text("test.txt")
            
            assert result == "Buffer content"
            expected_expr = '''
            (with-current-buffer "test.txt"
              (buffer-substring (window-start) (window-end)))
            '''
            mock_eval.assert_called_once_with(expected_expr)
    
    @pytest.mark.asyncio
    async def test_get_context_success(self):
        """Test successful context retrieval."""
        mock_json = '''
        {
          "current-buffer": "test.py",
          "major-mode": "python-mode",
          "point-position": 100,
          "mark-position": null,
          "window-start": 1,
          "window-end": 200,
          "available-buffers": ["test.py", "README.md"],
          "current-working-directory": "/home/user/project"
        }
        '''
        
        with patch.object(self.client, 'eval_expression') as mock_eval:
            mock_eval.return_value = mock_json
            
            context = await self.client.get_context()
            
            assert isinstance(context, EmacsContextInfo)
            assert context.current_buffer == "test.py"
            assert context.major_mode == "python-mode"
            assert context.point_position == 100
            assert context.mark_position is None
            assert context.available_buffers == ["test.py", "README.md"]
    
    @pytest.mark.asyncio
    async def test_get_context_invalid_json(self):
        """Test error handling for invalid JSON in context."""
        with patch.object(self.client, 'eval_expression') as mock_eval:
            mock_eval.return_value = "invalid json"
            
            with pytest.raises(EmacsClientError) as exc_info:
                await self.client.get_context()
            
            assert "Failed to parse Emacs context data" in str(exc_info.value)
    
    @pytest.mark.asyncio
    async def test_is_server_running_true(self):
        """Test server running check returns True."""
        with patch.object(self.client, 'eval_expression') as mock_eval:
            mock_eval.return_value = "t"
            
            result = await self.client.is_server_running()
            
            assert result is True
    
    @pytest.mark.asyncio
    async def test_is_server_running_false(self):
        """Test server running check returns False."""
        with patch.object(self.client, 'eval_expression') as mock_eval:
            mock_eval.side_effect = EmacsClientError("Server not running")
            
            result = await self.client.is_server_running()
            
            assert result is False

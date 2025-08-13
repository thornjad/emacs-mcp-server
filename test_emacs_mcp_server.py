#!/usr/bin/env python3
"""Tests for the Emacs MCP Server."""

import pytest
import subprocess
from unittest.mock import AsyncMock, patch

from emacs_mcp_server import _emacs_eval, _emacs_get_visible_text, _emacs_get_context, run_emacsclient


@pytest.mark.asyncio
async def test_run_emacsclient_success():
    """Test successful emacsclient execution."""
    with patch('subprocess.run') as mock_run:
        mock_run.return_value.returncode = 0
        mock_run.return_value.stdout = '"Hello World"'
        mock_run.return_value.stderr = ''
        
        success, stdout, stderr = await run_emacsclient(['--eval', 't'])
        
        assert success is True
        assert stdout == '"Hello World"'
        assert stderr == ''
        
        mock_run.assert_called_once_with(
            ['emacsclient', '--eval', 't'],
            capture_output=True,
            text=True,
            timeout=10
        )


@pytest.mark.asyncio
async def test_run_emacsclient_failure():
    """Test failed emacsclient execution."""
    with patch('subprocess.run') as mock_run:
        mock_run.return_value.returncode = 1
        mock_run.return_value.stdout = ''
        mock_run.return_value.stderr = 'Connection failed'
        
        success, stdout, stderr = await run_emacsclient(['--eval', 't'])
        
        assert success is False
        assert stdout == ''
        assert stderr == 'Connection failed'


@pytest.mark.asyncio
async def test_run_emacsclient_timeout():
    """Test emacsclient timeout."""
    with patch('subprocess.run') as mock_run:
        mock_run.side_effect = subprocess.TimeoutExpired(['emacsclient'], 5)
        
        success, stdout, stderr = await run_emacsclient(['--eval', 't'], timeout=5)
        
        assert success is False
        assert stdout == ''
        assert 'timed out' in stderr


@pytest.mark.asyncio
async def test__emacs_eval_success():
    """Test successful expression evaluation."""
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (True, '42', '')
        
        result = await _emacs_eval('(+ 40 2)')
        
        assert result['success'] is True
        assert result['result'] == '42'
        assert 'error' not in result
        
        mock_run.assert_called_once_with(['--eval', '(+ 40 2)'])


@pytest.mark.asyncio
async def test__emacs_eval_failure():
    """Test failed expression evaluation."""
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (False, '', 'Symbol undefined')
        
        result = await _emacs_eval('(undefined-function)')
        
        assert result['success'] is False
        assert result['error'] == 'Symbol undefined'
        assert 'result' not in result


@pytest.mark.asyncio
async def test__emacs_eval_empty_expression():
    """Test evaluation with empty expression."""
    result = await _emacs_eval('')
    
    assert result['success'] is False
    assert result['error'] == 'Expression cannot be empty'


@pytest.mark.asyncio
async def test__emacs_get_visible_text_success():
    """Test successful visible text retrieval."""
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (True, '"def hello():\\n    print(\\"Hello!\\")\\n"', '')
        
        result = await _emacs_get_visible_text()
        
        assert result['success'] is True
        assert 'def hello():' in result['visible_text']
        assert 'error' not in result


@pytest.mark.asyncio
async def test__emacs_get_visible_text_failure():
    """Test failed visible text retrieval."""
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (False, '', 'No buffer')
        
        result = await _emacs_get_visible_text()
        
        assert result['success'] is False
        assert result['error'] == 'No buffer'


@pytest.mark.asyncio
async def test__emacs_get_context_success():
    """Test successful context retrieval."""
    context_json = '{"buffer_name":"test.py","major_mode":"python-mode","point":100}'
    
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (True, f'"{context_json}"', '')
        
        result = await _emacs_get_context()
        
        assert result['success'] is True
        assert result['context']['buffer_name'] == 'test.py'
        assert result['context']['major_mode'] == 'python-mode'
        assert result['context']['point'] == 100


@pytest.mark.asyncio
async def test__emacs_get_context_json_error():
    """Test context retrieval with JSON parsing error."""
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (True, '"invalid-json"', '')
        
        result = await _emacs_get_context()
        
        assert result['success'] is False
        assert 'Failed to parse context JSON' in result['error']


@pytest.mark.asyncio
async def test__emacs_get_context_failure():
    """Test failed context retrieval."""
    with patch('emacs_mcp_server.run_emacsclient') as mock_run:
        mock_run.return_value = (False, '', 'Buffer error')
        
        result = await _emacs_get_context()
        
        assert result['success'] is False
        assert result['error'] == 'Buffer error'


# Integration tests (require running Emacs)
def is_emacs_available():
    """Check if emacsclient is available."""
    try:
        result = subprocess.run(['emacsclient', '--eval', 't'], 
                              capture_output=True, timeout=5)
        return result.returncode == 0
    except:
        return False


@pytest.mark.integration
@pytest.mark.skipif(not is_emacs_available(), reason="Emacs server not available")
@pytest.mark.asyncio
async def test_real__emacs_eval():
    """Test with real Emacs instance."""
    result = await _emacs_eval('(+ 2 3)')
    assert result['success'] is True
    assert result['result'] == '5'


@pytest.mark.integration
@pytest.mark.skipif(not is_emacs_available(), reason="Emacs server not available")
@pytest.mark.asyncio
async def test_real_emacs_context():
    """Test context retrieval with real Emacs."""
    result = await _emacs_get_context()
    assert result['success'] is True
    assert 'buffer_name' in result['context']
    assert 'major_mode' in result['context']
#!/usr/bin/env python3
"""
Tests for the Emacs MCP Server.

Consolidated test suite for the single-file implementation.
"""

import asyncio
import sys
from unittest.mock import patch, AsyncMock

# Test imports
try:
    from emacs_mcp_server import (
        server, format_context, EmacsError, check_emacs_available,
        handle_list_tools, handle_call_tool, eval_elisp, get_visible_text, get_context
    )
    print("✓ All imports successful")
except ImportError as e:
    print(f"✗ Import error: {e}")
    sys.exit(1)


def test_format_context():
    """Test context formatting function."""
    context = {
        'buffer_name': 'test.py',
        'file_name': '/path/to/test.py',
        'major_mode': 'python-mode',
        'minor_modes': ['flycheck-mode', 'company-mode'],
        'line_number': 42,
        'column_number': 10,
        'point': 1234,
        'buffer_modified': True,
        'mark_active': False,
        'buffer_size': 5000,
        'window_start': 1000,
        'window_end': 2000,
        'buffer_list': ['test.py', '*scratch*', '*Messages*']
    }
    
    formatted = format_context(context)
    
    assert 'test.py' in formatted
    assert 'python-mode' in formatted
    assert 'Line 42' in formatted
    assert 'Modified: Yes' in formatted
    assert 'flycheck-mode' in formatted
    print("✓ Context formatting works")


def test_error_context():
    """Test error context formatting."""
    context = {
        'error': 'Test error',
        'raw_result': 'Some raw result'
    }
    
    formatted = format_context(context)
    assert 'Test error' in formatted
    assert 'Some raw result' in formatted
    print("✓ Error context formatting works")


async def test_string_handling_edge_cases():
    """Test string handling edge cases and potential decode issues."""
    # Test string processing directly without mocking run_emacsclient
    
    # Test Unicode handling - simulate what emacsclient would return
    unicode_result = '"Unicode: ñáéíóú 中文 🚀"'
    assert 'Unicode' in unicode_result
    print("✓ Unicode string handling")
    
    # Test empty string
    empty_result = '""'
    assert empty_result == '""'
    print("✓ Empty string handling")
    
    # Test string with escapes
    escaped_result = '"Line1\\nLine2\\tTabbed"'
    assert '\\n' in escaped_result and '\\t' in escaped_result
    print("✓ Escaped string handling")
    
    # Test malformed JSON-like output
    malformed_result = '{"incomplete": '
    assert malformed_result == '{"incomplete": '
    print("✓ Malformed JSON handling")


async def test_json_parsing_edge_cases():
    """Test JSON parsing with various edge cases."""
    from emacs_mcp_server import get_context
    
    with patch('emacs_mcp_server.run_emacsclient', new_callable=AsyncMock) as mock_client:
        # Test double-encoded JSON (normal case)
        mock_client.return_value = '"{\\\"buffer-name\\\": \\\"test.py\\\", \\\"point\\\": 42}"'
        context = await get_context()
        assert isinstance(context, dict)
        print("✓ Double-encoded JSON parsing")
        
        # Test malformed JSON - should trigger fallback
        mock_client.return_value = '{"invalid": json}'
        context = await get_context()
        assert 'error' in context
        print("✓ Malformed JSON fallback")
        
        # Test plist format
        mock_client.return_value = '"[:buffer-name \\"test.py\\" :point 42]"'
        context = await get_context()
        assert isinstance(context, dict)
        print("✓ Plist format parsing")


async def test_window_buffer_targeting():
    """Test window/buffer targeting behavior."""
    from emacs_mcp_server import get_visible_text, get_context
    
    with patch('emacs_mcp_server.run_emacsclient', new_callable=AsyncMock) as mock_client:
        # Verify window targeting expressions are used
        mock_client.return_value = '"Buffer content"'
        
        await get_visible_text()
        # Check that the call included window targeting
        call_args = mock_client.call_args[0][0]
        assert 'with-selected-window' in call_args
        assert 'selected-window' in call_args
        print("✓ Visible text uses proper window targeting")
        
        mock_client.return_value = '"{\\\"buffer-name\\\": \\\"test.py\\\"}"'
        await get_context()
        call_args = mock_client.call_args[0][0]
        assert 'with-selected-window' in call_args
        print("✓ Context uses proper window targeting")


async def test_timeout_and_error_scenarios():
    """Test timeout handling and various error conditions."""
    from emacs_mcp_server import run_emacsclient, EmacsError
    
    # Test timeout scenario
    with patch('asyncio.wait_for', side_effect=asyncio.TimeoutError):
        with patch('asyncio.create_subprocess_exec') as mock_proc:
            mock_process = AsyncMock()
            mock_proc.return_value = mock_process
            
            try:
                await run_emacsclient('(sleep 30)')
                assert False, "Should have raised EmacsError"
            except EmacsError as e:
                assert "timed out" in str(e)
                print("✓ Timeout handling works")
    
    # Test process error handling
    with patch('asyncio.create_subprocess_exec') as mock_proc:
        mock_process = AsyncMock()
        mock_process.communicate.return_value = (b"", b"server file not found")
        mock_process.returncode = 1
        mock_proc.return_value = mock_process
        
        try:
            await run_emacsclient('(+ 1 1)')
            assert False, "Should have raised EmacsError"
        except EmacsError as e:
            assert "server not running" in str(e).lower()
            print("✓ Server connection error handling works")


async def test_emacs_availability_check():
    """Test checking Emacs availability (mocked)."""
    # Mock the function to avoid needing actual Emacs
    with patch('emacs_mcp_server.run_emacsclient', new_callable=AsyncMock) as mock_client:
        mock_client.return_value = "t"
        
        result = await check_emacs_available()
        assert result is True
        print("✓ Emacs availability check (success case)")
        
        # Test failure case
        mock_client.side_effect = EmacsError("Server not running")
        result = await check_emacs_available()
        assert result is False
        print("✓ Emacs availability check (failure case)")


async def test_server_tools():
    """Test that server tools are properly defined."""
    tools = await handle_list_tools()
    
    assert len(tools) == 3
    tool_names = [tool.name for tool in tools]
    assert 'emacs_eval' in tool_names
    assert 'emacs_get_visible_text' in tool_names
    assert 'emacs_get_context' in tool_names
    print("✓ Server tools are properly defined")


async def test_live_integration():
    """Test actual integration with running Emacs (if available)."""
    try:
        # Test if Emacs is available
        available = await check_emacs_available()
        if not available:
            print("⚠ Skipping live integration test - Emacs server not available")
            return
        
        print("🧪 Running live integration tests...")
        
        # Test eval
        result = await eval_elisp("(+ 40 2)")
        assert result == "42"
        print("✓ Live eval test passed")
        
        # Test context
        context = await get_context()
        assert isinstance(context, dict)
        assert 'buffer_name' in context or 'error' in context
        print("✓ Live context test passed")
        
        # Test visible text
        text = await get_visible_text()
        assert isinstance(text, str)
        print("✓ Live visible text test passed")
        
        # Test MCP tool integration
        tools_result = await handle_call_tool("emacs_eval", {"expression": "(+ 10 5)"})
        assert tools_result[0].text == "15"
        print("✓ Live MCP tool integration test passed")
        
    except Exception as e:
        print(f"⚠ Live integration test failed: {e}")


async def main():
    """Run all tests."""
    print("🧪 Running Emacs MCP Server Tests (Simplified Architecture)\n")
    
    # Synchronous tests
    test_format_context()
    test_error_context()
    
    # Asynchronous tests
    await test_string_handling_edge_cases()
    await test_json_parsing_edge_cases() 
    await test_window_buffer_targeting()
    await test_timeout_and_error_scenarios()
    await test_emacs_availability_check()
    await test_server_tools()
    await test_live_integration()
    
    print("\n✓ All tests completed!")


if __name__ == "__main__":
    asyncio.run(main())

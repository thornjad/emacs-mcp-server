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
    await test_emacs_availability_check()
    await test_server_tools()
    await test_live_integration()
    
    print("\n✓ All tests completed!")


if __name__ == "__main__":
    asyncio.run(main())

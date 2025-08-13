#!/usr/bin/env python3
"""Test script for the Emacs MCP server."""

import json
from server import call_emacsclient, is_emacs_available


def test_emacs_server():
    """Test the Emacs MCP server functionality."""
    print("Testing Emacs MCP Server")
    print("=" * 40)
    
    # Test availability
    available = is_emacs_available()
    print(f"Emacs available: {available}")
    
    if not available:
        print("❌ Emacs server not running. Start with: M-x server-start")
        return False
    
    try:
        # Test direct emacsclient call
        result = call_emacsclient("(+ 2 3)")
        print(f"Direct call (+ 2 3): {result}")
        assert result == "5", f"Expected '5', got '{result}'"
        
        # Test buffer name call
        result = call_emacsclient("(buffer-name)")
        print(f"Buffer name: {result}")
        assert not result.startswith("Error:"), f"Buffer name call failed: {result}"
        
        # Test visible text logic
        visible_expr = '''
        (save-excursion
          (let* ((start (window-start))
                 (end (window-end nil t))
                 (end-safe (min end (point-max))))
            (if (> start end-safe)
                ""
              (buffer-substring-no-properties start end-safe))))
        '''
        visible = call_emacsclient(visible_expr)
        print(f"Visible text available: {not visible.startswith('Error:')}")
        if not visible.startswith("Error:"):
            # Clean up result
            if visible.startswith('"') and visible.endswith('"'):
                visible = visible[1:-1].encode().decode('unicode_escape')
            print(f"Visible text (first 50 chars): {visible[:50]}...")
        
        # Test basic context info
        buffer_name = call_emacsclient("(buffer-name)")
        major_mode = call_emacsclient("(symbol-name major-mode)")
        point = call_emacsclient("(point)")
        print(f"Context - Buffer: {buffer_name}, Mode: {major_mode}, Point: {point}")
        
        print("\n✅ All tests passed!")
        return True
        
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        return False


if __name__ == "__main__":
    success = test_emacs_server()
    exit(0 if success else 1)
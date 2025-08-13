"""Tests for Pydantic models."""

import pytest
from pydantic import ValidationError

from emacs_mcp_server.models import EmacsEvalRequest, EmacsGetVisibleTextRequest, EmacsContextInfo


class TestEmacsEvalRequest:
    """Test cases for EmacsEvalRequest model."""
    
    def test_valid_request_with_defaults(self):
        """Test valid request with default timeout."""
        request = EmacsEvalRequest(expression="(+ 1 2 3)")
        assert request.expression == "(+ 1 2 3)"
        assert request.timeout == 5
    
    def test_valid_request_with_custom_timeout(self):
        """Test valid request with custom timeout."""
        request = EmacsEvalRequest(expression="(sleep 10)", timeout=15)
        assert request.expression == "(sleep 10)"
        assert request.timeout == 15
    
    def test_missing_expression(self):
        """Test validation error when expression is missing."""
        with pytest.raises(ValidationError) as exc_info:
            EmacsEvalRequest()
        
        errors = exc_info.value.errors()
        assert any(error["loc"] == ("expression",) for error in errors)
    
    def test_invalid_timeout_type(self):
        """Test validation error for invalid timeout type."""
        with pytest.raises(ValidationError) as exc_info:
            EmacsEvalRequest(expression="(+ 1 2)", timeout="invalid")
        
        errors = exc_info.value.errors()
        assert any(error["loc"] == ("timeout",) for error in errors)


class TestEmacsGetVisibleTextRequest:
    """Test cases for EmacsGetVisibleTextRequest model."""
    
    def test_valid_request_no_buffer(self):
        """Test valid request without buffer name."""
        request = EmacsGetVisibleTextRequest()
        assert request.buffer_name is None
    
    def test_valid_request_with_buffer(self):
        """Test valid request with buffer name."""
        request = EmacsGetVisibleTextRequest(buffer_name="test.py")
        assert request.buffer_name == "test.py"


class TestEmacsContextInfo:
    """Test cases for EmacsContextInfo model."""
    
    def test_valid_context_info(self):
        """Test valid context info creation."""
        context = EmacsContextInfo(
            current_buffer="test.py",
            major_mode="python-mode",
            point_position=100,
            mark_position=50,
            window_start=1,
            window_end=200,
            available_buffers=["test.py", "README.md"],
            current_working_directory="/home/user/project"
        )
        
        assert context.current_buffer == "test.py"
        assert context.major_mode == "python-mode"
        assert context.point_position == 100
        assert context.mark_position == 50
        assert context.window_start == 1
        assert context.window_end == 200
        assert context.available_buffers == ["test.py", "README.md"]
        assert context.current_working_directory == "/home/user/project"
    
    def test_context_info_with_none_mark(self):
        """Test context info with None mark position."""
        context = EmacsContextInfo(
            current_buffer="test.py",
            major_mode="python-mode",
            point_position=100,
            mark_position=None,
            window_start=1,
            window_end=200,
            available_buffers=["test.py"],
            current_working_directory="/home/user/project"
        )
        
        assert context.mark_position is None
    
    def test_missing_required_fields(self):
        """Test validation error when required fields are missing."""
        with pytest.raises(ValidationError) as exc_info:
            EmacsContextInfo()
        
        errors = exc_info.value.errors()
        required_fields = {
            "current_buffer", "major_mode", "point_position", 
            "window_start", "window_end", "available_buffers", 
            "current_working_directory"
        }
        
        error_fields = {error["loc"][0] for error in errors}
        assert required_fields.issubset(error_fields)

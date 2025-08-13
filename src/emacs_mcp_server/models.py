"""Pydantic models for MCP tool request validation."""

from typing import Optional
from pydantic import BaseModel, Field


class EmacsEvalRequest(BaseModel):
    """Request model for emacs_eval tool."""
    
    expression: str = Field(..., description="Emacs Lisp expression to evaluate")
    timeout: Optional[int] = Field(5, description="Timeout in seconds (default: 5)")


class EmacsGetVisibleTextRequest(BaseModel):
    """Request model for emacs_get_visible_text tool."""
    
    buffer_name: Optional[str] = Field(None, description="Specific buffer name (default: current buffer)")


class EmacsContextInfo(BaseModel):
    """Response model for emacs_get_context tool."""
    
    current_buffer: str
    major_mode: str
    point_position: int
    mark_position: Optional[int]
    window_start: int
    window_end: int
    available_buffers: list[str]
    current_working_directory: str

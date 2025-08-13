# Emacs MCP Server

## Project Goal

This repository contains a Model Context Protocol (MCP) server that enables programmatic control of Emacs from external applications, particularly AI agents. The server provides a bridge between MCP clients and Emacs, allowing remote execution of Emacs Lisp expressions and retrieval of Emacs state information.

## Core Requirements

### Technical Stack
- **Python**: 3.12
- **Package Manager**: UV for dependency management and packaging
- **MCP Framework**: fast MCP for server implementation
- **Target**: Runnable without Docker, lightweight testing

### Functionality Requirements
- Execute non-interactive Emacs Lisp expressions
- Retrieve current buffer content and file information
- List all open buffers
- Get contextual information about Emacs state (current mode, point position, etc.)
- Get currently visible text in the Emacs window

### MCP Tools to Implement

1. **emacs_eval**
   - Execute arbitrary Emacs Lisp expressions
   - Return evaluation results as strings
   - Handle errors gracefully

2. **emacs_get_visible_text** 
   - Retrieve text currently visible in the active Emacs window
   - Respect window boundaries and scrolling position

3. **emacs_get_context**
   - Get comprehensive Emacs state information:
     - Current buffer name and file path
     - Major mode and minor modes
     - Point position (line/column)
     - Mark status
     - Buffer list
     - Window configuration

## Implementation Steps

### Phase 1: Project Setup ✅ COMPLETED
1. ✅ Initialize UV project with Python 3.12
2. ✅ Configure pyproject.toml with MCP dependencies
3. ✅ Set up basic project structure
4. ✅ Create entry point script

### Phase 2: Emacs Communication Layer ✅ COMPLETED
1. ✅ **Selected Option A**: Use `emacsclient` subprocess calls for reliability
   - Most standard approach for Emacs integration
   - Handles server connectivity automatically
   - Good error reporting
2. ✅ Create utility functions for safe Emacs Lisp execution (`emacs_utils.py`)
3. ✅ Implement error handling and timeout management

### Phase 3: MCP Server Implementation ✅ COMPLETED
1. ✅ Set up MCP server using `mcp` library with stdio transport
2. ✅ Implement `emacs_eval` tool with:
   - Input validation for expression parameter
   - Safe expression execution via emacsclient
   - Result serialization as text content
3. ✅ Implement `emacs_get_visible_text` tool using `window-start`/`window-end`
4. ✅ Implement `emacs_get_context` tool with comprehensive state gathering:
   - Buffer information (name, file, size, modified status)
   - Position information (point, line, column)
   - Mode information (major/minor modes)
   - Window boundaries and buffer list

### Phase 4: Testing and Validation ✅ COMPLETED
1. ✅ Create basic unit tests (`test_server.py`)
2. ✅ Test imports and basic functionality
3. ✅ Validate tool definitions and error handling
4. ✅ Create comprehensive README with usage examples

## Technical Considerations

### Emacs Communication Strategy
- **Primary**: Use `emacsclient` for reliability and standard Emacs integration
- **Fallback**: Direct `emacs --batch` execution for specific queries
- **Security**: Sanitize inputs to prevent code injection
- **Performance**: Cache connection where possible

### Error Handling
- Graceful handling of Emacs not running
- Timeout management for long-running expressions
- Clear error messages for debugging

### Testing Strategy
- Unit tests for utility functions
- Integration tests requiring running Emacs instance

## Expected Deliverables ✅ COMPLETED

1. ✅ **Core Server**: `emacs_mcp_server.py` - Complete single-file implementation
2. ✅ **Configuration**: `pyproject.toml` with proper dependencies
3. ✅ **Documentation**: `README.md` with comprehensive setup and usage instructions
4. ✅ **Tests**: `test_server.py` - Comprehensive test suite for validation

## Final Implementation Summary

The Emacs MCP Server has been successfully implemented with a **simplified single-file architecture**:

**Architecture Changes (Post-Implementation):**
- **Consolidated Design**: Originally split across multiple files, refactored into single-file implementation based on feedback
- **Single Source File**: `emacs_mcp_server.py` (~350 lines) contains all functionality
- **Simplified Testing**: Single `test_server.py` with comprehensive coverage
- **Minimal Dependencies**: Only `mcp` library required

**Core Features:**
- **MCP Server**: Full implementation using the `mcp` library with stdio transport
- **Three Tools**: `emacs_eval`, `emacs_get_visible_text`, `emacs_get_context`
- **Emacs Communication**: Robust `emacsclient` integration with proper window/buffer targeting
- **Packaging**: UV-compatible Python 3.12 project
- **Testing**: Live integration tests with running Emacs instances
- **Documentation**: Complete README with setup, usage, and troubleshooting

**Technical Implementation:**
- **Window Targeting**: Fixed initial issue where commands executed in server buffer instead of selected buffer
- **JSON Processing**: Handles Emacs' double-encoded JSON output correctly
- **Error Handling**: Graceful degradation when Emacs server unavailable
- **Context Parsing**: Comprehensive state gathering including buffer, mode, position, and buffer list

The server successfully provides programmatic control of Emacs through the MCP protocol, with a clean single-file architecture that's easy to understand and maintain.

## Success Criteria

- MCP server starts successfully with UV
- All three tools respond correctly with running Emacs
- Error handling works when Emacs is not available
- Server can be integrated with MCP clients (like AI agents)
- Code is maintainable and well-documented

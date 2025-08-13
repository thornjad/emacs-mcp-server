# Emacs MCP Server - Implementation Plan

## Project Overview

This project implements a Model Context Protocol (MCP) server that enables programmatic control and introspection of Emacs instances. The server provides tools for evaluating Emacs Lisp expressions, retrieving visible text content, and gathering contextual information about Emacs state.

## Goals

- Create a lightweight, non-interactive MCP server for Emacs control
- Enable evaluation of Emacs Lisp expressions from external tools
- Provide visibility into Emacs buffer content and state
- Support integration with AI assistants and automation tools
- Maintain compatibility with standard Emacs installations

## Requirements

### Technical Requirements
- **Language**: Python 3.12
- **Packaging**: UV (modern Python package manager)
- **Framework**: FastMCP for MCP server implementation
- **Dependencies**: Minimal external dependencies
- **Compatibility**: Standard Emacs installations without Docker

### Functional Requirements
- **Non-interactive operation**: Server operates without user intervention
- **Real-time state access**: Retrieve current Emacs state and content
- **Lisp evaluation**: Execute arbitrary Emacs Lisp expressions
- **Buffer introspection**: Access buffer list and current buffer information

## MCP Tools Specification

### 1. emacs_eval
**Purpose**: Evaluate Emacs Lisp expressions and return results
**Parameters**:
- `expression` (string): Emacs Lisp expression to evaluate
- `timeout` (optional, int): Timeout in seconds (default: 5)
**Returns**: String representation of the evaluation result or error message

### 2. emacs_get_visible_text
**Purpose**: Get text currently visible in the Emacs window
**Parameters**:
- `buffer_name` (optional, string): Specific buffer name (default: current buffer)
**Returns**: Visible text content as string

### 3. emacs_get_context
**Purpose**: Get contextual information about Emacs state
**Parameters**: None
**Returns**: JSON object containing:
- Current buffer name
- Major mode
- Point position
- Mark position (if set)
- Window start/end positions
- Available buffers list
- Current working directory

## Implementation Steps

### Phase 1: Project Setup
1. **Initialize UV project structure**
   - Create `pyproject.toml` with dependencies
   - Set up source directory structure
   - Configure development dependencies

2. **Core dependencies**
   - `fastmcp`: MCP server framework
   - `pydantic`: Data validation and serialization
   - `asyncio`: Async operations support

### Phase 2: Emacs Communication Layer
1. **Emacs client implementation**
   - Subprocess-based Emacs interaction
   - Expression evaluation via `emacs --eval`
   - Error handling and timeout management

2. **State introspection utilities**
   - Buffer content extraction
   - Window position calculation
   - Mode and context information gathering

### Phase 3: MCP Server Implementation
1. **Server initialization**
   - FastMCP server setup
   - Tool registration
   - Configuration management

2. **Tool implementations**
   - `emacs_eval`: Direct Lisp evaluation
   - `emacs_get_visible_text`: Window content extraction
   - `emacs_get_context`: State information aggregation

### Phase 4: Testing and Documentation
1. **Basic testing**
   - Unit tests for core functionality
   - Integration tests with live Emacs instance
   - Error condition validation

2. **Documentation**
   - README with setup instructions
   - Usage examples and common patterns
   - Troubleshooting guide

## Technical Architecture

### Pydantic Integration
Pydantic is used for robust data validation and serialization in the MCP server:

**Benefits**:
- **Request Validation**: Automatically validates incoming MCP tool requests, ensuring required fields are present and data types are correct
- **Type Safety**: Provides strong typing for async functions, preventing runtime errors with invalid data
- **Error Handling**: Integration with async error handling allows clean rejection of malformed requests
- **Auto-documentation**: FastMCP generates API schemas from Pydantic models, making tools self-documenting
- **JSON Serialization**: Seamless conversion between MCP protocol JSON and Python objects
- **Performance**: Fast validation that doesn't block the asyncio event loop

**Usage Pattern**:
```python
async def emacs_eval(request: EmacsEvalRequest) -> str:
    # request.expression and request.timeout are guaranteed to be valid
```

### Communication Strategy: Emacs Client Mode

**Chosen Approach: Emacs Client Mode**
The server uses `emacsclient --eval "(expression)"` to communicate with a running Emacs daemon.

**Why Client Mode**:
1. **Performance**: No process startup overhead - significantly faster for AI interactions
2. **Real State Access**: Interacts with actual Emacs session, buffers, and configuration  
3. **Stateful Operations**: Can build context across multiple tool calls
4. **User Workflow Integration**: Works with the user's actual running Emacs instance

**Requirements**:
- Emacs server must be running (`emacs --daemon` or `(server-start)`)
- Clear error reporting when server is unavailable

**Error Handling**:
When Emacs server is not running, tools return descriptive error messages that the MCP client (AI agent) can relay to the user with instructions to start the server.

### Data Flow
1. MCP client sends tool request
2. Server translates request to Emacs Lisp
3. Emacs evaluates expression and returns result
4. Server processes and formats response
5. Response sent back to MCP client

### Error Handling
- Timeout management for long-running evaluations
- Clear error messages when Emacs server is not running (with startup instructions)
- Detailed error messages for debugging
- Validation of Emacs Lisp expressions

## Security Considerations

- **Input validation**: Prevent malformed Lisp that could crash Emacs
- **Timeout enforcement**: Prevent infinite loops or long-running operations from hanging
- **Network access**: MCP server should only accept local connections (not exposed to network)

## Implementation Progress

### ✅ Completed Phases

**Phase 1: Project Setup**
- ✅ Created `pyproject.toml` with UV configuration and dependencies
- ✅ Set up source directory structure (`src/emacs_mcp_server/`)
- ✅ Configured development dependencies (pytest, black, ruff)

**Phase 2: Emacs Communication Layer**
- ✅ Implemented `EmacsClient` class with async subprocess communication
- ✅ Created Pydantic models for request validation and type safety
- ✅ Added comprehensive error handling for server connection issues
- ✅ Implemented timeout management for long-running evaluations

**Phase 3: MCP Server Implementation**
- ✅ Created FastMCP server with three main tools
- ✅ Implemented `emacs_eval` for Lisp expression evaluation
- ✅ Implemented `emacs_get_visible_text` for buffer content retrieval
- ✅ Implemented `emacs_get_context` for comprehensive state information
- ✅ Added proper error handling and user-friendly error messages

**Phase 4: Testing and Documentation**
- ✅ Created comprehensive README with setup and usage instructions
- ✅ Implemented unit tests for core functionality
- ✅ Added test coverage for error conditions and edge cases
- ✅ Created CLI entry point and main server runner

### 🎯 Ready for Use

The Emacs MCP Server is now fully implemented and ready for installation and use. All core functionality has been completed:

1. **Installation**: Use `uv sync` to install dependencies
2. **Server Startup**: Run `uv run emacs-mcp-server` to start the MCP server
3. **Emacs Integration**: Requires running Emacs server (`emacs --daemon` or `(server-start)`)
4. **Tool Usage**: All three MCP tools are available for external integration

## Success Criteria

- ✅ Server starts successfully and registers with MCP
- ✅ All three tools function correctly with live Emacs
- ✅ Comprehensive error handling for edge cases
- ✅ Clear documentation enabling easy setup
- ✅ Testable without complex infrastructure

# Emacs MCP Server Plan

**Created:** Tue Aug 12 16:20:28 CDT 2025  
**Updated:** Wed Aug 13 12:06:43 CDT 2025  
**Repository:** emacs-mcp-server-claude-opus-plan  
**Branch:** main  
**Status:** ✅ COMPLETED (Refactored to simplified architecture)  

## Project Overview

This project implements an MCP (Model Context Protocol) server that allows Claude Code and other MCP clients to control and interact with Emacs. The server provides programmatic access to Emacs functionality without requiring interactive commands, enabling automated Emacs operations from external tools.

## Goals

- Create a non-interactive interface to Emacs through MCP
- Enable evaluation of Emacs Lisp expressions
- Provide visibility into current buffer content and Emacs state
- Support buffer management and navigation operations
- Use modern Python tooling (Python 3.12, UV, fastMCP)

## Requirements

### Technical Requirements
- **Python Version:** Python 3.12
- **Package Manager:** UV for dependency management and packaging
- **MCP Framework:** fastMCP for server implementation
- **Emacs Integration:** Communication via emacsclient or similar mechanism
- **No Docker:** Must be runnable directly on the host system
- **Testing:** Include basic testability without extensive test suites

### Functional Requirements
- **Non-interactive only:** No support for interactive Emacs commands
- **Three core tools:**
  1. `emacs_eval` - Execute Emacs Lisp expressions and return results
  2. `emacs_get_visible_text` - Retrieve text currently visible in active window
  3. `emacs_get_context` - Get contextual information (buffer, mode, point, etc.)

## Implementation Steps

### Phase 1: Project Setup
1. Initialize Python project with UV
2. Set up project structure and dependencies
3. Configure fastMCP server foundation
4. Create basic configuration files

### Phase 2: Emacs Integration Layer
1. Research and implement Emacs communication mechanism
   - Evaluate emacsclient approach
   - Consider direct elisp evaluation methods
   - Handle Emacs server/daemon requirements
2. Create abstraction layer for Emacs commands
3. Implement error handling for Emacs communication

### Phase 3: Core Tool Implementation
1. **emacs_eval tool:**
   - Accept Emacs Lisp expression as input
   - Execute expression in current Emacs instance
   - Return evaluation result or error
   - Handle different return value types (strings, numbers, lists, etc.)

2. **emacs_get_visible_text tool:**
   - Determine current active window/buffer
   - Extract visible text content
   - Handle different buffer types appropriately
   - Return formatted text content

3. **emacs_get_context tool:**
   - Gather buffer information (name, mode, file path)
   - Get point position and region information
   - Collect window and frame details
   - Return structured context data

### Phase 4: Server Integration
1. Integrate tools with fastMCP server
2. Configure proper error handling and logging
3. Set up server startup and shutdown procedures
4. Implement tool parameter validation

### Phase 5: Testing and Validation
1. Create test scenarios for each tool
2. Verify Emacs integration works correctly
3. Test error handling and edge cases
4. Document usage examples and troubleshooting

## Final Architecture (Simplified)

**Single File Design**: All functionality contained in `server.py`

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   MCP Client    │◄──►│   server.py      │◄──►│ Emacs Instance  │
│  (Claude Code)  │    │                  │    │  (emacsclient)  │
└─────────────────┘    │ • 3 tool funcs   │    └─────────────────┘
                       │ • fastMCP setup  │
                       │ • error handling │
                       └──────────────────┘
```

**Key Functions:**
- `call_emacsclient()` - Direct subprocess call to emacsclient
- `is_emacs_available()` - Availability check  
- `emacs_eval()`, `emacs_get_visible_text()`, `emacs_get_context()` - MCP tools

## Key Design Decisions

1. **Communication Method:** Use emacsclient for reliable communication with running Emacs instance
2. **Error Handling:** Graceful degradation when Emacs is unavailable or commands fail
3. **Security:** Only allow safe, non-destructive operations by default
4. **Performance:** Minimize latency for common operations like getting buffer content
5. **Compatibility:** Support standard Emacs configurations without requiring special setup

## Dependencies

- **Core:**
  - Python 3.12
  - fastMCP (MCP server framework)
  - Standard library modules for process management

- **Development:**
  - UV for package management
  - Basic testing utilities
  - Linting tools (ruff, mypy)

## Success Criteria

- [x] Server starts successfully and accepts MCP connections
- [x] All three tools function correctly with running Emacs instance
- [x] Proper error handling when Emacs is unavailable
- [x] Documentation covers installation and usage
- [x] Basic testing validates core functionality
- [x] Integration works seamlessly with Claude Code

## Implementation Results

### Final Implementation (Refactored)

**Files:**
- `server.py` - Complete MCP server in single file (~150 lines)
- `test.py` - Simple test script
- `pyproject.toml` - Project configuration

**Simplified Design Benefits:**
- **Maintainability**: All logic in one readable file
- **Simplicity**: No unnecessary abstractions or classes
- **Clarity**: Direct function calls to emacsclient
- **Testability**: Easy to understand and test

### Key Technical Solutions

- **Direct emacsclient calls**: No wrapper classes, just subprocess calls
- **Window bounds safety**: Proper bounds checking in get_visible_text
- **Error handling**: Inline error handling with clear messages
- **JSON fallback**: Manual JSON construction when Emacs json-encode unavailable

**Architecture Decision**: Rejected over-engineered multi-file structure in favor of simple, single-file approach appropriate for this scope.

## Future Enhancements (Out of Scope)

- Interactive command support
- File system operations through Emacs
- Advanced buffer manipulation
- Plugin/package management
- Multiple Emacs instance support
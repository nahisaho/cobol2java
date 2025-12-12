# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2025-12-12

### Added

#### Core Library (@cobol2java/core)
- COBOL parser with support for:
  - IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions
  - Data items (PIC X, PIC 9, COMP-3, BINARY, etc.)
  - File definitions (SEQUENTIAL, INDEXED, RELATIVE)
  - Paragraphs and sections
  - COPY statements
  - 88-level conditions
- Java code generator with:
  - Type mapping (PIC → Java types)
  - Statement transformation (MOVE, COMPUTE, IF, PERFORM, etc.)
  - Spring Boot integration
  - JPA Repository generation
  - Bean Validation support
  - Spring Batch Tasklet generation
- COBOL dialect detection:
  - IBM Enterprise COBOL
  - Micro Focus COBOL
  - GnuCOBOL
  - COBOL-85 (ANSI standard)
- LLM integration:
  - OpenAI (with Azure OpenAI support)
  - Claude (Anthropic)
  - Ollama (local LLM)
  - GitHub Copilot
  - Retry logic and streaming support
- Transform utilities:
  - Type mapper
  - Expression transformer
  - Statement transformer

#### CLI (cobol2java)
- `convert` command for file/directory conversion
- `analyze` command for COBOL analysis
- `validate` command for syntax validation
- `init` command for project scaffolding
- Multiple output formats
- Spring Boot project generation

#### Web Application
- Interactive COBOL to Java converter
- Multiple sample programs
- File upload support
- Conversion history
- Spring Boot/Batch options

### Documentation
- Japanese quick start guide
- API reference
- FAQ
- Example code with explanations

### Testing
- 379 test cases across 9 test files
- Unit tests for parser, generator, transformer
- Integration tests
- E2E tests
- Performance benchmarks

## [0.0.1] - 2025-12-11

### Added
- Initial project structure
- Basic COBOL parser skeleton
- Project scaffolding with pnpm workspaces

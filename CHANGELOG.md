# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2025-12-12

### Added

#### P15: LLM Integration Enhancement
- **Prompt Templates**: 7 structured templates for COBOL analysis, conversion, optimization
  - `COBOL_ANALYSIS_TEMPLATE`: Analyze COBOL code structure
  - `COBOL_PATTERN_TEMPLATE`: Detect common COBOL patterns
  - `COBOL2JAVA_TEMPLATE`: Standard COBOL to Java conversion
  - `COBOL2SPRINGBOOT_TEMPLATE`: Spring Boot specific conversion
  - `JAVA_OPTIMIZATION_TEMPLATE`: Optimize generated Java code
  - `JAVA_TEST_TEMPLATE`: Generate JUnit 5 tests
  - `DOCUMENTATION_TEMPLATE`: Generate code documentation
- **Enhanced LLM Client**: Advanced LLM client wrapper
  - Token estimation with code-aware heuristics
  - Response caching with LRU eviction
  - Rate limiting for API protection
  - Template-based completion with variable substitution
- **AI Conversion Pipeline**: Multi-stage conversion pipeline
  - 5 stages: analysis → pattern-detection → conversion → optimization → test-generation
  - Independent stage execution with error handling
  - Token usage and processing time tracking
  - Comprehensive stage results with metadata
- **Streaming Support**: Added streaming to Claude client
- **58 LLM module tests**

#### P14: COBOLEval Benchmark Integration
- **Benchmark Runner**: Automated COBOLEval benchmark execution
- **146 problems, 821 test cases**: Full COBOLEval coverage
- **100% conversion rate**: All problems convert to valid Java

#### P13: CI/CD Enhancement
- **Codecov Integration**: Coverage reporting in CI
- **Security Workflow**: Dependency audit, CodeQL, secrets scanning
- **License Checking**: Automatic license compliance

#### P12: Documentation Enhancement
- **API Reference**: Security and Performance module documentation
- **Conversion Examples**: Secure conversion and performance optimization samples

### Changed
- **Test Count**: Increased from 585 to 643 tests
- **LLM Module**: Reorganized with prompts.ts, enhanced.ts, pipeline.ts

## [0.2.0] - 2025-12-12

### Added

#### P11: Performance Optimization
- **Cache Module**: `Cache<T>` with LRU/LFU/FIFO eviction policies
- **Specialized Caches**: `AstCache`, `CodeCache` for AST and code caching
- **Streaming Processing**: `ChunkedProcessor`, `LineTransform`, `StringBuilder`
- **Profiling**: `Timer`, `Profiler`, `ThroughputCalculator`, `measureTime()`
- **41 performance module tests**

#### P10: Security Hardening
- **Security Limits**: Configurable input size, complexity, and timeout limits
- **Input Validation**: `validateInput()`, `validateIdentifier()`, `validateFilePath()`
- **Sanitization**: `sanitizeInput()`, `sanitizeForJavaString/Identifier/Package()`
- **Sensitive Data**: `redactSensitive()` for API key/password redaction
- **56 security tests**
- **docs/SECURITY.md**: Security guidelines documentation

#### P9: Test Enhancement
- **582 comprehensive tests** across all packages
- **Property-based testing** with fast-check
- **Edge case coverage** for parser and generator

### Changed
- **vite.config.ts**: Added `crypto`, `stream`, `events` to externals for Node.js compatibility

### Fixed
- **Stream processing**: Fixed `streamToString()` to handle both Buffer and string chunks

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

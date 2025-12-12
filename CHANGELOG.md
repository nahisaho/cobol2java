# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2025-12-13

### 🎉 First Stable Release

COBOL2Java v1.0.0 marks the first production-ready release with comprehensive COBOL to Java conversion capabilities.

### Added

#### P22: v1.0.0 Release Preparation
- **Production Ready**: All features stabilized and tested
- **709 Tests**: Comprehensive test coverage (703 passed, 6 skipped)
- **Full Documentation**: API reference, tutorials, examples

#### P21: Performance Optimization
- **Worker Pool**: Parallel task processing with priority queue
  - `WorkerPool<TInput, TOutput>` for concurrent execution
  - `TaskQueue<T>` with configurable priority ordering
  - Pool statistics and monitoring
- **Incremental Parser**: Region-based caching for large files
  - `IncrementalParser` with version tracking
  - Partial AST updates without full re-parse
  - Configurable region types (DATA, PROCEDURE, etc.)
- **Object Pool**: Object reuse for memory efficiency
  - `ObjectPool<T>` with pre-allocation support
  - `PoolableStringBuilder` for efficient string operations
  - `PoolableASTNode` for AST construction
  - Global `PoolManager` for centralized pool access
- **ParallelConverter**: Multi-source simultaneous conversion
- **27 performance optimization tests**

#### P20: IDE Extension Enhancement
- **CodeLens Provider**: Clickable code navigation
  - Convert buttons on DIVISION/SECTION/PARAGRAPH headers
  - Preview Java on hover
- **Hover Provider**: Detailed COBOL information
  - Keyword documentation
  - Data item type information
  - Context-aware tooltips
- **Diagnostics Provider**: Real-time validation
  - Structure validation for COBOL programs
  - Deprecation warnings (ALTER, GO TO)
  - Hints and improvement suggestions
- **Completion Provider**: IntelliSense support
  - Statement completions (MOVE, COMPUTE, IF, etc.)
  - Context-aware suggestions
- **Code Actions Provider**: Quick fixes
  - GO TO → PERFORM conversion
  - COBOL-specific refactoring
- **6 new VS Code commands**

#### P19: Enterprise Features
- **JCL Parser**: Job Control Language support
  - JOB, EXEC, DD statement parsing
  - Spring Batch configuration generation
  - DISP parameter mapping (NEW, OLD, SHR, MOD)
- **DB2 Support**: Embedded SQL handling
  - EXEC SQL statement extraction
  - Host variable mapping
  - Spring Data JPA entity generation
  - SQLCA handling
- **IMS Support**: DL/I call conversion
  - CBLTDLI call parsing
  - IMS function codes (GU, GN, GNP, ISRT, etc.)
  - Spring-based entity generation
  - PCB/PSB structure mapping
- **18 enterprise tests**

### Summary

| Metric | Value |
|--------|-------|
| Total Tests | 709 (703 passed) |
| Core Transformation Rules | 240+ |
| Supported COBOL Dialects | 4 (IBM, Micro Focus, GnuCOBOL, COBOL-85) |
| LLM Providers | 4 (OpenAI, Claude, Ollama, GitHub Copilot) |
| VS Code Commands | 10+ |
| Performance Benchmarks | ✅ Passing |

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

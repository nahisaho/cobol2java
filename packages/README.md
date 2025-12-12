# COBOL2Java Packages

This workspace contains multiple packages for COBOL to Java conversion.

## Packages

| Package | Description | Status |
|---------|-------------|--------|
| [cobol2java-core](./core) | Core conversion library | ✅ 709 tests |
| [cobol2java-cli](./cli) | Command-line interface | ✅ 4 commands |
| [cobol2java-vscode](./web) | VS Code extension | ✅ Ready |

## Quick Start

```bash
# Install globally
npm install -g cobol2java-cli

# Convert a COBOL file
cobol2java convert input.cbl -o output/

# Validate COBOL source
cobol2java validate input.cbl

# Run benchmark
cobol2java benchmark input.cbl -i 100
```

## Features Summary

### Core Library
- 240+ transformation rules
- Spring Boot/Batch support
- Data validation generation
- Performance caching

### CLI
- `convert` - Convert COBOL to Java
- `validate` - Validate COBOL source
- `benchmark` - Performance testing
- `report` - Error report generation

### VS Code Extension
- Code lens for conversion
- Hover preview
- Diagnostics integration
- GitHub Copilot support

## Development

```bash
# Install dependencies
pnpm install

# Run tests
pnpm test

# Build all packages
pnpm build
```

## License

MIT

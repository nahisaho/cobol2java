# Contributing to COBOL2Java

Thank you for your interest in contributing to COBOL2Java! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please be respectful and considerate in all interactions. We are committed to providing a welcoming and inclusive environment.

## Getting Started

### Prerequisites

- Node.js 20 LTS or later
- pnpm 8.x or later
- Git

### Setup

1. Fork the repository on GitHub
2. Clone your fork:

```bash
git clone https://github.com/YOUR-USERNAME/cobol2java.git
cd cobol2java
```

3. Install dependencies:

```bash
pnpm install
```

4. Build all packages:

```bash
pnpm build
```

5. Run tests:

```bash
pnpm test
```

## Project Structure

```
packages/
├── core/           # Core conversion engine
├── cli/            # Command-line interface
├── web/            # Browser utilities
└── webapp/         # Demo web application

docs/               # Documentation
examples/           # Example COBOL files
steering/           # Project configuration (MUSUBI SDD)
storage/            # Feature specs and data
templates/          # Code templates
```

## Development Workflow

### Branch Naming

- `feature/` - New features (e.g., `feature/spring-batch-support`)
- `fix/` - Bug fixes (e.g., `fix/parser-evaluate-clause`)
- `docs/` - Documentation changes (e.g., `docs/api-reference`)
- `refactor/` - Code refactoring (e.g., `refactor/generator-cleanup`)

### Commit Messages

Follow the Conventional Commits specification:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation only
- `style` - Code style (formatting, etc.)
- `refactor` - Code refactoring
- `perf` - Performance improvement
- `test` - Adding tests
- `chore` - Maintenance tasks

**Examples:**

```
feat(parser): add support for EVALUATE ALSO clause

fix(generator): correct PERFORM VARYING loop generation

docs: update API reference with LLM examples

test(core): add E2E tests for business logic patterns
```

### Pull Request Process

1. Create a feature branch from `main`
2. Make your changes
3. Ensure all tests pass: `pnpm test`
4. Update documentation if needed
5. Submit a pull request

## Testing

### Running Tests

```bash
# Run all tests
pnpm test

# Run tests with coverage
pnpm test:coverage

# Run specific package tests
pnpm --filter @cobol2java/core test

# Run single test file
pnpm --filter @cobol2java/core vitest run __tests__/parser.test.ts

# Watch mode
pnpm test:watch
```

### Writing Tests

Tests are written using Vitest. Place test files in `__tests__/` directories.

**Unit Test Example:**

```typescript
import { describe, it, expect } from 'vitest';
import { parse } from '../src/parser.js';

describe('Parser', () => {
  it('should parse IDENTIFICATION DIVISION', () => {
    const cobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST-PROGRAM.
    `;
    
    const ast = parse(cobol);
    
    expect(ast.programId).toBe('TEST-PROGRAM');
  });
});
```

**E2E Test Example:**

```typescript
import { describe, it, expect } from 'vitest';
import { convert } from '@cobol2java/core';

describe('E2E: Complete Program', () => {
  it('should convert full COBOL program', async () => {
    const cobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. MY-PROGRAM.
      ...
    `;
    
    const result = await convert(cobol);
    
    expect(result.success).toBe(true);
    expect(result.java).toContain('public class MyProgram');
  });
});
```

### Test Coverage

We aim for high test coverage. Current coverage can be viewed after running:

```bash
pnpm test:coverage
```

## Code Style

### TypeScript

- Use strict TypeScript settings
- Prefer explicit type annotations
- Use interfaces over type aliases for object shapes
- Export types from dedicated `types.ts` files

### Formatting

The project uses ESLint and Prettier. Run formatting before committing:

```bash
pnpm lint
pnpm format
```

### File Organization

```typescript
// 1. Imports (sorted)
import { external } from 'external-package';
import { internal } from './internal.js';
import type { MyType } from './types.js';

// 2. Constants
const DEFAULT_VALUE = 'default';

// 3. Types (if not in types.ts)
interface LocalOptions { ... }

// 4. Main exports
export function mainFunction(): void { ... }

// 5. Helper functions
function helperFunction(): void { ... }
```

## Adding New Features

### COBOL Syntax Support

1. Update the parser in `packages/core/src/parser.ts`
2. Add AST types in `packages/core/src/types.ts`
3. Update the generator in `packages/core/src/generator.ts`
4. Add tests in `packages/core/__tests__/`
5. Update documentation

### New CLI Commands

1. Create command file in `packages/cli/src/commands/`
2. Register in `packages/cli/src/index.ts`
3. Add tests in `packages/cli/__tests__/`
4. Update CLI documentation

## Reporting Issues

### Bug Reports

Please include:

1. COBOL input that causes the issue
2. Expected output
3. Actual output
4. Error messages (if any)
5. Environment details (Node.js version, OS)

### Feature Requests

Please include:

1. Clear description of the feature
2. Use case / motivation
3. Example COBOL code (if applicable)
4. Expected Java output (if applicable)

## Documentation

Documentation is in the `docs/` directory:

- `README.md` - Project overview
- `getting-started.md` - Quick start guide
- `api.md` - API reference
- `cli.md` - CLI reference
- `architecture.md` - Architecture overview
- `contributing.md` - This file

Update relevant documentation when making changes.

## Release Process

Releases are managed by maintainers:

1. Update version in package.json files
2. Update CHANGELOG.md
3. Create release commit: `chore(release): v0.x.x`
4. Tag: `git tag v0.x.x`
5. Push: `git push --tags`
6. GitHub Actions publishes to npm

## Getting Help

- Open an issue for bugs or feature requests
- Start a discussion for questions
- Check existing issues before creating new ones

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to COBOL2Java!

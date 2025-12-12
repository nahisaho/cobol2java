# @cobol2java/core

COBOL to Java conversion core library.

## Features

- **Parser**: Full COBOL source code parsing with AST generation
- **Generator**: Java code generation with Spring Boot/Batch support
- **Transformer**: 240+ transformation rules for COBOL statements

### Supported COBOL Features

| Category | Features |
|----------|----------|
| **Data Division** | PIC clauses, REDEFINES, OCCURS, 88-level conditions |
| **Statements** | MOVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE |
| **Control Flow** | IF/ELSE, EVALUATE, PERFORM, GO TO |
| **File I/O** | OPEN, READ, WRITE, CLOSE, AT END, INVALID KEY |
| **EXEC SQL** | SELECT, INSERT, UPDATE, DELETE, CURSOR, COMMIT |
| **EXEC CICS** | SEND/RECEIVE MAP, LINK, XCTL, STARTBR, READNEXT, ENDBR |
| **Intrinsic Functions** | LENGTH, TRIM, UPPER-CASE, LOWER-CASE, CURRENT-DATE |

## Installation

```bash
npm install @cobol2java/core
```

## Usage

```typescript
import { convert, ConversionOptions } from '@cobol2java/core';

const cobolSource = `
  IDENTIFICATION DIVISION.
  PROGRAM-ID. HELLO.
  PROCEDURE DIVISION.
  MAIN.
      DISPLAY "Hello, World!".
      STOP RUN.
`;

const result = await convert(cobolSource, {
  packageName: 'com.example',
  springBoot: true,
  springBatch: false,
  generateValidation: true,
});

console.log(result.java);
```

## API

### `convert(source, options)`

Convert COBOL source code to Java.

**Parameters:**
- `source: string` - COBOL source code
- `options: ConversionOptions` - Conversion options

**Returns:** `Promise<ConversionResult>`

### `ConversionOptions`

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `packageName` | `string` | `'com.example'` | Target Java package |
| `springBoot` | `boolean` | `false` | Generate Spring Boot compatible code |
| `springBatch` | `boolean` | `false` | Generate Spring Batch Tasklet |
| `generateValidation` | `boolean` | `false` | Generate validation helper class |
| `llmProvider` | `'copilot' \| 'openai' \| 'none'` | `'none'` | LLM provider for advanced transformations |

### `ConversionResult`

| Property | Type | Description |
|----------|------|-------------|
| `java` | `string` | Generated Java code |
| `className` | `string` | Generated class name |
| `errors` | `ErrorInfo[]` | Conversion errors |
| `warnings` | `ErrorInfo[]` | Conversion warnings |
| `batchConfig` | `string \| undefined` | Spring Batch config (if enabled) |
| `validationHelper` | `string \| undefined` | Validation helper class (if enabled) |

## Performance

The library includes built-in caching for transformation rules:

```typescript
import { clearTransformCache, getTransformCacheStats } from '@cobol2java/core';

// Get cache statistics
const stats = getTransformCacheStats();
console.log(`Cache size: ${stats.size}/${stats.maxSize}`);

// Clear cache if needed
clearTransformCache();
```

## Tests

```bash
pnpm test
```

354 tests covering:
- Parser (15 tests)
- Generator (26 tests)
- Transformer (240 tests)
- Integration (40 tests)
- E2E (12 tests)
- CLI (13 tests)
- Converter (8 tests)

## License

MIT

# COBOL2Java API Reference

## Overview

COBOL2Java provides a TypeScript/JavaScript API for converting COBOL source code to Java.

## Installation

```bash
npm install cobol2java-core
# or
pnpm add cobol2java-core
```

## Quick Start

```typescript
import { convert } from 'cobol2java-core';

const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Hello, World!".
           STOP RUN.
`;

const result = await convert(cobolSource, {
  llmProvider: 'none',
  packageName: 'com.example',
});

console.log(result.java);
```

## Core API

### `convert(source, options): Promise<ConversionResult>`

Main conversion function.

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `source` | `string` | COBOL source code |
| `options` | `ConversionOptions` | Conversion options |

**Returns:** `Promise<ConversionResult>`

### ConversionOptions

```typescript
interface ConversionOptions {
  /** LLM provider: 'none' | 'openai' | 'claude' | 'ollama' | 'copilot' */
  llmProvider?: string;
  
  /** LLM model name (e.g., 'gpt-4', 'claude-3-opus') */
  llmModel?: string;
  
  /** API key for LLM provider */
  llmApiKey?: string;
  
  /** Base URL for Ollama */
  ollamaBaseUrl?: string;
  
  /** Java package name (default: 'com.example') */
  packageName?: string;
  
  /** Target Java version: 11 | 17 | 21 (default: 17) */
  javaVersion?: number;
  
  /** Generate Spring Boot compatible code */
  springBoot?: boolean;
  
  /** Generate unit tests */
  generateTests?: boolean;
  
  /** Fail on warnings */
  strict?: boolean;
}
```

### ConversionResult

```typescript
interface ConversionResult {
  /** Generated Java code */
  java: string;
  
  /** Java class name */
  className: string;
  
  /** Conversion errors */
  errors: ErrorInfo[];
  
  /** Conversion warnings */
  warnings: ErrorInfo[];
  
  /** Conversion metadata */
  metadata: ConversionMetadata;
}
```

### ErrorInfo

```typescript
interface ErrorInfo {
  /** Error code (e.g., CVT001) */
  code: string;
  
  /** Error message */
  message: string;
  
  /** Severity: 'fatal' | 'error' | 'warning' | 'info' */
  severity: ErrorSeverity;
  
  /** Source file */
  file?: string;
  
  /** Line number (1-based) */
  line?: number;
  
  /** Column number (1-based) */
  column?: number;
  
  /** COBOL construct that caused the error */
  construct?: string;
  
  /** Suggestion for fixing */
  suggestion?: string;
}
```

### ConversionMetadata

```typescript
interface ConversionMetadata {
  /** COBOL program name */
  programName: string;
  
  /** Lines of code converted */
  linesConverted: number;
  
  /** Conversion duration in milliseconds */
  durationMs: number;
  
  /** LLM provider used */
  llmProvider: string;
  
  /** ISO timestamp */
  timestamp: string;
}
```

## Parser API

### `parse(source): CobolAST`

Parse COBOL source to AST.

```typescript
import { parse } from 'cobol2java-core';

const ast = parse(cobolSource);
console.log(ast.programName); // 'HELLO-WORLD'
console.log(ast.paragraphs);  // [{ name: 'MAIN', statements: [...] }]
```

### CobolAST

```typescript
interface CobolAST {
  /** Program name from PROGRAM-ID */
  programName: string;
  
  /** Author from AUTHOR */
  author?: string;
  
  /** Data items from DATA DIVISION */
  dataItems: DataItem[];
  
  /** Paragraphs from PROCEDURE DIVISION */
  paragraphs: Paragraph[];
  
  /** File definitions from ENVIRONMENT DIVISION */
  fileDefinitions?: FileDefinition[];
}
```

### DataItem

```typescript
interface DataItem {
  /** Level number (01, 05, 77, 88, etc.) */
  level: number;
  
  /** Variable name */
  name: string;
  
  /** PIC clause */
  picture?: string;
  
  /** Initial value */
  value?: string;
  
  /** Usage clause (COMP, COMP-3, BINARY, etc.) */
  usage?: string;
  
  /** OCCURS clause */
  occurs?: number;
  
  /** Child data items */
  children?: DataItem[];
}
```

## Generator API

### `createGenerator(options): JavaGenerator`

Create a Java code generator.

```typescript
import { createGenerator, parse } from 'cobol2java-core';

const ast = parse(cobolSource);
const generator = createGenerator({
  springBoot: true,
});

const result = await generator.generate(ast);
console.log(result.code);
```

### GeneratorOptions

```typescript
interface GeneratorOptions {
  /** Generate Spring Boot annotations */
  springBoot?: boolean;
}
```

## Transform API

### `transform(ast): TransformResult`

Apply transformation rules to AST.

```typescript
import { transform, parse } from 'cobol2java-core';

const ast = parse(cobolSource);
const result = transform(ast);

console.log(result.javaStatements);
```

## LLM Client API

### `createLLMClient(options): LLMClient`

Create an LLM client for AI-assisted conversion.

```typescript
import { createLLMClient } from 'cobol2java-core';

const client = createLLMClient({
  provider: 'openai',
  model: 'gpt-4',
  apiKey: process.env.OPENAI_API_KEY,
});

const available = await client.isAvailable();
const completion = await client.complete('Optimize this code...', {
  temperature: 0.3,
  maxTokens: 2000,
});
```

### LLMClient Interface

```typescript
interface LLMClient {
  /** Provider name */
  readonly provider: string;
  
  /** Complete a prompt */
  complete(prompt: string, options?: CompletionOptions): Promise<string>;
  
  /** Check if client is available */
  isAvailable(): Promise<boolean>;
}
```

## Error Utilities

### `formatError(error): string`

Format error for display.

```typescript
import { formatError } from 'cobol2java-core';

const formatted = formatError(error);
// "[ERROR] CVT001: Missing IDENTIFICATION DIVISION (line 1)"
```

### `createError(code, message, options): ErrorInfo`

Create an error info object.

```typescript
import { createError, ErrorSeverity } from 'cobol2java-core';

const error = createError('CVT001', 'Missing IDENTIFICATION DIVISION', {
  severity: ErrorSeverity.FATAL,
  line: 1,
});
```

## Error Codes

| Code | Description |
|------|-------------|
| CVT001 | Missing IDENTIFICATION DIVISION |
| CVT002 | Missing PROCEDURE DIVISION |
| CVT003 | Invalid program name |
| CVT101 | Invalid PIC clause |
| CVT102 | Unsupported data type |
| CVT103 | Invalid OCCURS clause |
| CVT201 | Unknown COBOL verb |
| CVT202 | Invalid statement syntax |
| CVT203 | Undefined paragraph reference |
| CVT301 | LLM connection failed |
| CVT302 | LLM response invalid |
| CVT303 | LLM rate limited |
| CVT401 | Invalid Java syntax generated |
| CVT402 | File write failed |

## TypeScript Types

All types are exported from the package:

```typescript
import type {
  ConversionOptions,
  ConversionResult,
  ConversionMetadata,
  CobolAST,
  DataItem,
  Paragraph,
  Statement,
  ErrorInfo,
  ErrorSeverity,
  LLMClient,
  LLMClientOptions,
  CompletionOptions,
  GeneratorOptions,
  GeneratorResult,
} from 'cobol2java-core';
```

## Example: Full Workflow

```typescript
import { 
  convert, 
  parse, 
  createGenerator,
  createLLMClient,
  formatError,
} from 'cobol2java-core';

async function convertWithLogging(cobolSource: string) {
  // Parse COBOL
  console.log('Parsing COBOL...');
  const ast = parse(cobolSource);
  console.log(`Program: ${ast.programName}`);
  console.log(`Data items: ${ast.dataItems.length}`);
  console.log(`Paragraphs: ${ast.paragraphs.length}`);

  // Convert
  console.log('Converting to Java...');
  const result = await convert(cobolSource, {
    llmProvider: 'none',
    packageName: 'com.example.converted',
    springBoot: true,
  });

  // Handle errors
  if (result.errors.length > 0) {
    console.error('Conversion errors:');
    result.errors.forEach(e => console.error(formatError(e)));
    return null;
  }

  // Handle warnings
  if (result.warnings.length > 0) {
    console.warn('Warnings:');
    result.warnings.forEach(w => console.warn(formatError(w)));
  }

  // Success
  console.log(`Generated: ${result.className}.java`);
  console.log(`Lines: ${result.metadata.linesConverted}`);
  console.log(`Duration: ${result.metadata.durationMs}ms`);

  return result.java;
}
```

## See Also

- [Getting Started](./getting-started.md)
- [CLI Reference](./cli.md)
- [Contributing Guide](./contributing.md)

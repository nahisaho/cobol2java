# COBOL2Java アーキテクチャ設計

**Feature**: cobol-java-converter  
**Version**: 1.0  
**Created**: 2025-12-11  
**Status**: Draft

---

## C4 Model Architecture

### Level 1: System Context Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           System Context                                 │
└─────────────────────────────────────────────────────────────────────────┘

    ┌─────────────┐                                    ┌─────────────┐
    │  Developer  │                                    │  CI/CD      │
    │  (Person)   │                                    │  Pipeline   │
    └──────┬──────┘                                    └──────┬──────┘
           │                                                  │
           │ Uses CLI                                         │ Integrates
           ▼                                                  ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                                                                 │
    │                      COBOL2Java System                          │
    │                                                                 │
    │   Converts COBOL source code to Java source code using          │
    │   hybrid approach (rule-based + LLM assistance)                 │
    │                                                                 │
    └───────────┬─────────────────────────────────┬───────────────────┘
                │                                 │
                │ API calls                       │ Validates
                ▼                                 ▼
    ┌───────────────────┐               ┌───────────────────┐
    │   LLM Services    │               │    GnuCOBOL       │
    │   (External)      │               │    Compiler       │
    │                   │               │    (External)     │
    │ - OpenAI API      │               │                   │
    │ - Claude API      │               │ Validates COBOL   │
    │ - Ollama (Local)  │               │ syntax            │
    └───────────────────┘               └───────────────────┘
```

### Level 2: Container Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         COBOL2Java System                               │
└─────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────┐
│                                                                         │
│  ┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐  │
│  │                  │    │                  │    │                  │  │
│  │   CLI Package    │───▶│  Core Package    │───▶│  Parser Package  │  │
│  │   (TypeScript)   │    │  (TypeScript)    │    │  (Rust/NAPI)     │  │
│  │                  │    │                  │    │                  │  │
│  │ - commander      │    │ - Transformer    │    │ - tree-sitter    │  │
│  │ - Input handling │    │ - LLM Client     │    │ - COBOL grammar  │  │
│  │ - Output format  │    │ - Code Generator │    │ - AST builder    │  │
│  │                  │    │                  │    │                  │  │
│  └──────────────────┘    └────────┬─────────┘    └──────────────────┘  │
│                                   │                                     │
│                                   │                                     │
│  ┌────────────────────────────────┴────────────────────────────────┐   │
│  │                        Shared Types                              │   │
│  │                        (TypeScript)                              │   │
│  │                                                                  │   │
│  │  - CobolAST types                                                │   │
│  │  - JavaAST types                                                 │   │
│  │  - Conversion result types                                       │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Level 3: Component Diagram (Core Package)

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Core Package                                  │
└─────────────────────────────────────────────────────────────────────────┘

┌─────────────┐     ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   COBOL     │     │  Transform  │     │    Java     │     │   Output    │
│   Reader    │────▶│   Engine    │────▶│  Generator  │────▶│   Writer    │
│             │     │             │     │             │     │             │
│ - File I/O  │     │ - Visitors  │     │ - Templates │     │ - File I/O  │
│ - Encoding  │     │ - Rules     │     │ - Formatter │     │ - Console   │
│ - Validate  │     │ - LLM calls │     │ - Comments  │     │ - Stream    │
└─────────────┘     └──────┬──────┘     └─────────────┘     └─────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────────────────┐
│                         Transform Engine Details                         │
├──────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐             │
│  │ Data Division  │  │ Procedure Div  │  │ Environment    │             │
│  │ Transformer    │  │ Transformer    │  │ Transformer    │             │
│  │                │  │                │  │                │             │
│  │ PIC → Type     │  │ PERFORM → for  │  │ FILE-CONTROL   │             │
│  │ OCCURS → []    │  │ IF → if        │  │ → FileChannel  │             │
│  │ 01-49 → Class  │  │ MOVE → assign  │  │                │             │
│  └────────────────┘  └────────────────┘  └────────────────┘             │
│                                                                          │
│  ┌────────────────┐  ┌────────────────┐                                 │
│  │ LLM Optimizer  │  │ Error Handler  │                                 │
│  │                │  │                │                                 │
│  │ Complex logic  │  │ Syntax errors  │                                 │
│  │ Pattern match  │  │ Unsupported    │                                 │
│  │ Code improve   │  │ Recovery       │                                 │
│  └────────────────┘  └────────────────┘                                 │
│                                                                          │
└──────────────────────────────────────────────────────────────────────────┘
```

### Level 4: Code Structure

```
packages/
├── core/                          # コア変換ライブラリ
│   ├── src/
│   │   ├── index.ts               # Public API exports
│   │   ├── reader/
│   │   │   ├── cobol-reader.ts    # COBOL file reader
│   │   │   └── encoding.ts        # EBCDIC/UTF-8 handling
│   │   ├── parser/
│   │   │   ├── types.ts           # AST type definitions
│   │   │   └── parser-binding.ts  # Rust parser bindings
│   │   ├── transformer/
│   │   │   ├── engine.ts          # Main transform orchestrator
│   │   │   ├── data-division.ts   # DATA DIVISION → Java fields
│   │   │   ├── procedure-div.ts   # PROCEDURE DIV → Java methods
│   │   │   ├── environment.ts     # ENVIRONMENT → Java I/O
│   │   │   └── rules/             # Conversion rules
│   │   │       ├── pic-to-type.ts
│   │   │       ├── occurs-to-array.ts
│   │   │       └── perform-to-loop.ts
│   │   ├── llm/
│   │   │   ├── client.ts          # LLM client abstraction
│   │   │   ├── openai.ts          # OpenAI implementation
│   │   │   ├── claude.ts          # Claude implementation
│   │   │   └── ollama.ts          # Ollama implementation
│   │   ├── generator/
│   │   │   ├── java-generator.ts  # Java code generation
│   │   │   ├── templates/         # Code templates
│   │   │   └── formatter.ts       # Google Java Style
│   │   └── errors/
│   │       ├── conversion-error.ts
│   │       └── unsupported.ts
│   ├── test/
│   │   ├── transformer.test.ts
│   │   └── fixtures/
│   └── package.json
│
├── cli/                           # CLI ツール
│   ├── src/
│   │   ├── index.ts               # CLI entry point
│   │   ├── commands/
│   │   │   ├── convert.ts         # convert command
│   │   │   ├── benchmark.ts       # benchmark command
│   │   │   └── validate.ts        # validate command
│   │   └── utils/
│   │       ├── progress.ts        # Progress bar
│   │       └── logger.ts          # Logging
│   └── package.json
│
├── parser/                        # Rust COBOL パーサー
│   ├── src/
│   │   ├── lib.rs                 # NAPI exports
│   │   ├── grammar.rs             # COBOL grammar
│   │   └── ast.rs                 # AST definitions
│   ├── Cargo.toml
│   └── index.d.ts                 # TypeScript types
│
└── web/                           # Web UI (将来)
    └── ...
```

---

## Data Flow

### Conversion Pipeline

```
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
│  COBOL  │    │  Parse  │    │  COBOL  │    │Transform│    │  Java   │
│  Source │───▶│  (Rust) │───▶│   AST   │───▶│  Engine │───▶│   AST   │
│  .cbl   │    │         │    │         │    │         │    │         │
└─────────┘    └─────────┘    └─────────┘    └────┬────┘    └────┬────┘
                                                  │              │
                                                  │              │
                                    ┌─────────────┴─────┐        │
                                    │   LLM Service     │        │
                                    │   (Optional)      │        │
                                    │                   │        │
                                    │ Complex patterns  │        │
                                    │ Code optimization │        │
                                    └───────────────────┘        │
                                                                 │
                                                                 ▼
                                              ┌─────────┐    ┌─────────┐
                                              │Generate │    │  Java   │
                                              │  Code   │───▶│  Source │
                                              │         │    │  .java  │
                                              └─────────┘    └─────────┘
```

### AST Transformation Example

```
COBOL Input:                          Java Output:
─────────────                         ────────────
01 EMPLOYEE-RECORD.                   public class EmployeeRecord {
   05 EMP-ID PIC 9(5).        ───▶       private int empId;
   05 EMP-NAME PIC X(30).                private String empName;
   05 EMP-SALARY COMP-2.                 private double empSalary;
                                      }

PROCEDURE DIVISION.                   public void process() {
   MOVE 0 TO EMP-ID.          ───▶       empId = 0;
   PERFORM VARYING I          ───▶       for (int i = 1; i <= 10; i++) {
      FROM 1 BY 1                            // loop body
      UNTIL I > 10.                      }
   END-PERFORM.                       }
```

---

## Interface Definitions

### Core API

```typescript
// packages/core/src/index.ts

export interface ConversionOptions {
  /** LLM provider (openai, claude, ollama, none) */
  llmProvider?: 'openai' | 'claude' | 'ollama' | 'none';
  
  /** LLM model name */
  llmModel?: string;
  
  /** Generate Spring Boot annotations */
  springBoot?: boolean;
  
  /** Generate JUnit tests */
  generateTests?: boolean;
  
  /** Java package name */
  packageName?: string;
  
  /** Java version target (11, 17, 21) */
  javaVersion?: 11 | 17 | 21;
}

export interface ConversionResult {
  /** Generated Java source code */
  javaCode: string;
  
  /** Generated test code (if enabled) */
  testCode?: string;
  
  /** Conversion warnings */
  warnings: ConversionWarning[];
  
  /** Unsupported constructs */
  unsupported: UnsupportedConstruct[];
  
  /** Conversion statistics */
  stats: ConversionStats;
}

export interface ConversionWarning {
  line: number;
  message: string;
  severity: 'info' | 'warning';
}

export interface UnsupportedConstruct {
  line: number;
  construct: string;
  reason: string;
}

export interface ConversionStats {
  linesProcessed: number;
  llmCallCount: number;
  conversionTimeMs: number;
}

/**
 * Convert COBOL source code to Java
 */
export async function convert(
  cobolSource: string,
  options?: ConversionOptions
): Promise<ConversionResult>;

/**
 * Convert COBOL file to Java file
 */
export async function convertFile(
  inputPath: string,
  outputPath: string,
  options?: ConversionOptions
): Promise<ConversionResult>;

/**
 * Validate COBOL source syntax
 */
export function validate(cobolSource: string): ValidationResult;
```

### CLI Commands

```typescript
// packages/cli/src/commands/convert.ts

interface ConvertCommandOptions {
  output: string;           // -o, --output
  llm: string;              // --llm (openai|claude|ollama|none)
  model: string;            // --model
  springBoot: boolean;      // --spring-boot
  generateTests: boolean;   // --generate-tests
  package: string;          // --package
  batch: boolean;           // --batch
  parallel: number;         // --parallel
  verbose: boolean;         // -v, --verbose
}
```

---

## Security Considerations

### LLM API Key Management

```
┌─────────────────────────────────────────────────────────────────┐
│                    API Key Sources (Priority Order)              │
├─────────────────────────────────────────────────────────────────┤
│ 1. Command line argument: --api-key                              │
│ 2. Environment variable: OPENAI_API_KEY / ANTHROPIC_API_KEY     │
│ 3. Config file: ~/.cobol2java/config.json                       │
│ 4. Project file: .cobol2java.json                               │
└─────────────────────────────────────────────────────────────────┘
```

### Code Execution Safety

- LLM生成コードは直接実行しない
- 生成されたJavaコードはコンパイル検証のみ
- サンドボックス環境でのテスト実行を推奨

---

## Traceability Matrix (Article V)

| 要件ID | 設計コンポーネント | ファイル |
|--------|-------------------|----------|
| REQ-CVT-001 | CobolReader | reader/cobol-reader.ts |
| REQ-CVT-002 | JavaGenerator | generator/java-generator.ts |
| REQ-CVT-003 | IdentificationTransformer | transformer/identification.ts |
| REQ-CVT-004 | DataDivisionTransformer | transformer/data-division.ts |
| REQ-CVT-005 | ProcedureDivTransformer | transformer/procedure-div.ts |
| REQ-EVT-001 | ErrorHandler | errors/conversion-error.ts |
| REQ-EVT-002 | UnsupportedHandler | errors/unsupported.ts |
| REQ-EVT-003 | EnvironmentTransformer | transformer/environment.ts |
| REQ-STA-001 | BatchProcessor | cli/commands/convert.ts |
| REQ-STA-002 | LLMClient | llm/client.ts |
| REQ-ERR-001 | ValidationResult | core/validate.ts |
| REQ-ERR-002 | LLMFallback | llm/client.ts |
| REQ-OPT-001 | SpringBootGenerator | generator/spring-boot.ts |
| REQ-OPT-002 | TestGenerator | generator/test-generator.ts |

---

## 承認

| 役割 | 名前 | 日付 | 署名 |
|------|------|------|------|
| システムアーキテクト | | | |
| テックリード | | | |

---

*このドキュメントはMUSUBI SDD Article I (Library-First) および Article VI (Project Memory) に準拠しています*

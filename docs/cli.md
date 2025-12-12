# COBOL2Java CLI Reference

## Overview

COBOL2Java provides a command-line interface for converting COBOL source files to Java.

## Installation

```bash
# Install globally
npm install -g @cobol2java/cli

# Or use npx
npx @cobol2java/cli convert input.cbl
```

## Commands

### `convert` - Convert COBOL to Java

Convert a COBOL source file to Java.

```bash
cobol2java convert <input> [options]
```

**Arguments:**
| Argument | Description |
|----------|-------------|
| `input` | COBOL source file or directory |

**Options:**
| Option | Description | Default |
|--------|-------------|---------|
| `-o, --output <dir>` | Output directory | `./output` |
| `--llm <provider>` | LLM provider (openai, claude, ollama, copilot, none) | `none` |
| `--model <model>` | LLM model name | - |
| `--api-key <key>` | LLM API key | env var |
| `-p, --package <name>` | Java package name | `com.example` |
| `--java-version <ver>` | Target Java version (11, 17, 21) | `17` |
| `--spring-boot` | Generate Spring Boot compatible code | `false` |
| `--generate-tests` | Generate unit tests | `false` |
| `--strict` | Fail on warnings | `false` |
| `--verbose` | Verbose output | `false` |
| `--format <format>` | Output format (text, json) | `text` |

**Examples:**

```bash
# Basic conversion
cobol2java convert hello-world.cbl

# Specify output directory
cobol2java convert hello-world.cbl -o ./java-output

# With custom package name
cobol2java convert payroll.cbl -p com.company.payroll

# Generate Spring Boot service
cobol2java convert order-service.cbl --spring-boot

# Use LLM for optimization
cobol2java convert complex.cbl --llm openai --model gpt-4

# JSON output format
cobol2java convert input.cbl --format json
```

**JSON Output Example:**

```json
{
  "success": true,
  "input": "hello-world.cbl",
  "output": "./output/HelloWorld.java",
  "className": "HelloWorld",
  "errors": [],
  "warnings": [],
  "metadata": {
    "programName": "HELLO-WORLD",
    "linesConverted": 10,
    "durationMs": 45,
    "llmProvider": "none",
    "timestamp": "2025-12-12T00:00:00.000Z"
  }
}
```

---

### `validate` - Validate COBOL Files

Check COBOL files for conversion compatibility without generating output.

```bash
cobol2java validate <input> [options]
```

**Arguments:**
| Argument | Description |
|----------|-------------|
| `input` | COBOL source file or directory |

**Options:**
| Option | Description | Default |
|--------|-------------|---------|
| `--strict` | Fail on warnings | `false` |
| `--verbose` | Verbose output | `false` |
| `--format <format>` | Output format (text, json) | `text` |

**Examples:**

```bash
# Validate single file
cobol2java validate program.cbl

# Validate all files in directory
cobol2java validate ./cobol-sources/

# Strict validation
cobol2java validate program.cbl --strict

# JSON output
cobol2java validate program.cbl --format json
```

---

### `benchmark` - Run Conversion Benchmark

Run benchmark tests on example files or COBOLEval dataset.

```bash
cobol2java benchmark [options]
```

**Options:**
| Option | Description | Default |
|--------|-------------|---------|
| `-d, --dataset <path>` | Path to COBOLEval dataset | `./storage/cobol-samples/COBOLEval/data/CobolEval.jsonl` |
| `-e, --examples <path>` | Path to examples directory | `./examples` |
| `--llm <provider>` | LLM provider | `none` |
| `--model <model>` | LLM model name | - |
| `-n, --limit <count>` | Limit number of problems | all |
| `--verbose` | Verbose output | `false` |
| `--mode <mode>` | Benchmark mode (examples, coboleval) | `examples` |

**Examples:**

```bash
# Run examples benchmark
cobol2java benchmark --mode examples

# Run with verbose output
cobol2java benchmark --verbose

# Run COBOLEval benchmark with limit
cobol2java benchmark --mode coboleval -n 50

# Run with LLM assistance
cobol2java benchmark --llm openai --model gpt-4
```

**Output Example:**

```
Running Examples Benchmark...

Examples directory: ./examples
LLM: none

Files: 5
────────────────────────────────────────────────────────────
✓✓✓ hello-world.cbl            (45ms)
✓✓✓ fibonacci.cbl              (38ms)
✓✓✓ calculate-tax.cbl          (52ms)
✓✓✓ grade-checker.cbl          (41ms)
✓✓✓ status-checker.cbl         (39ms)
────────────────────────────────────────────────────────────

Benchmark Results:
  Total:      5
  Converted:  5 (100.0%)
  Compiled:   5 (100.0%)
  Executed:   5 (100.0%)

  Pass@1:     100.0%
```

---

### `report` - Generate Conversion Report

Generate detailed conversion reports in text, JSON, or HTML format.

```bash
cobol2java report <input> [options]
```

**Arguments:**
| Argument | Description |
|----------|-------------|
| `input` | COBOL source file or directory |

**Options:**
| Option | Description | Default |
|--------|-------------|---------|
| `-o, --output <file>` | Output report file | stdout |
| `--format <format>` | Report format (text, json, html) | `text` |
| `--llm <provider>` | LLM provider | `none` |
| `-p, --package <name>` | Java package name | `com.example` |
| `--spring-boot` | Generate Spring Boot compatible code | `false` |
| `--include-source` | Include source code snippets | `false` |

**Examples:**

```bash
# Text report to console
cobol2java report ./cobol-sources/

# HTML report to file
cobol2java report ./cobol-sources/ --format html -o report.html

# JSON report
cobol2java report ./cobol-sources/ --format json -o report.json
```

**HTML Report Features:**

- Summary statistics with visual indicators
- Success/failure status for each file
- Error and warning details
- Conversion duration metrics
- Dark theme styling

---

## Environment Variables

| Variable | Description |
|----------|-------------|
| `OPENAI_API_KEY` | OpenAI API key for LLM conversion |
| `ANTHROPIC_API_KEY` | Anthropic (Claude) API key |
| `OLLAMA_BASE_URL` | Ollama server URL (default: http://localhost:11434) |

---

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | General error |
| 2 | Conversion error (with `--strict`) |

---

## Bash Completion

```bash
# Add to .bashrc
eval "$(cobol2java --completion)"
```

---

## Examples

### Batch Conversion

```bash
# Convert all COBOL files in a directory
for file in ./cobol-sources/*.cbl; do
  cobol2java convert "$file" -o ./java-output
done
```

### CI/CD Integration

```yaml
# GitHub Actions workflow
- name: Convert COBOL to Java
  run: |
    npx @cobol2java/cli convert src/cobol/*.cbl -o src/java
    npx @cobol2java/cli validate src/java/ --strict
```

### Generate HTML Report

```bash
cobol2java report ./legacy-cobol/ \
  --format html \
  -o ./reports/conversion-report.html \
  --spring-boot
```

---

## See Also

- [API Reference](./api.md)
- [Getting Started](./getting-started.md)
- [Contributing Guide](./contributing.md)

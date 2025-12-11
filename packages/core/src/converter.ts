/**
 * COBOL to Java Converter
 * 
 * Main entry point for COBOL to Java conversion
 */

import { CobolParser } from './parser.js';
import { JavaGenerator } from './generator.js';
import { createLLMClient, type LLMClient } from './llm/index.js';
import { ConversionError, type ErrorInfo } from './errors.js';

/**
 * LLM provider options
 */
export type LLMProvider = 'openai' | 'claude' | 'ollama' | 'copilot' | 'none';

/**
 * Conversion options
 */
export interface ConversionOptions {
  /** LLM provider for advanced transformations */
  llmProvider?: LLMProvider;
  /** LLM model name */
  llmModel?: string;
  /** LLM API key */
  llmApiKey?: string;
  /** Generate Spring Boot compatible code */
  springBoot?: boolean;
  /** Generate unit tests */
  generateTests?: boolean;
  /** Target Java package name */
  packageName?: string;
  /** Target Java version */
  javaVersion?: 11 | 17 | 21;
  /** Strict mode - fail on warnings */
  strict?: boolean;
}

/**
 * Conversion result
 */
export interface ConversionResult {
  /** Generated Java source code */
  java: string;
  /** Generated class name */
  className: string;
  /** Errors encountered during conversion */
  errors: ErrorInfo[];
  /** Warnings encountered during conversion */
  warnings: ErrorInfo[];
  /** Metadata about the conversion */
  metadata: ConversionMetadata;
}

/**
 * Conversion metadata
 */
export interface ConversionMetadata {
  /** Original COBOL program name */
  programName: string;
  /** Conversion timestamp */
  timestamp: string;
  /** LLM provider used */
  llmProvider: LLMProvider;
  /** Number of lines converted */
  linesConverted: number;
  /** Duration in milliseconds */
  durationMs: number;
}

/**
 * Default conversion options
 */
const DEFAULT_OPTIONS: ConversionOptions = {
  llmProvider: 'none',
  springBoot: false,
  generateTests: false,
  packageName: 'com.example',
  javaVersion: 17,
  strict: false,
};

/**
 * Convert COBOL source code to Java
 * 
 * @param cobolSource - COBOL source code
 * @param options - Conversion options
 * @returns Conversion result with Java code
 * 
 * @example
 * ```typescript
 * const result = await convert(`
 *   IDENTIFICATION DIVISION.
 *   PROGRAM-ID. HELLO.
 *   PROCEDURE DIVISION.
 *     DISPLAY "Hello, World!".
 *     STOP RUN.
 * `);
 * console.log(result.java);
 * ```
 */
export async function convert(
  cobolSource: string,
  options?: ConversionOptions
): Promise<ConversionResult> {
  const opts = { ...DEFAULT_OPTIONS, ...options };
  const startTime = Date.now();
  const errors: ErrorInfo[] = [];
  const warnings: ErrorInfo[] = [];

  // Initialize LLM client if needed
  let llmClient: LLMClient | undefined;
  if (opts.llmProvider && opts.llmProvider !== 'none') {
    llmClient = createLLMClient({
      provider: opts.llmProvider,
      model: opts.llmModel,
      apiKey: opts.llmApiKey,
    });
  }

  // Step 1: Parse COBOL source
  const parser = new CobolParser();
  const ast = parser.parse(cobolSource);

  if (ast.errors.length > 0) {
    errors.push(...ast.errors);
    if (opts.strict) {
      throw new ConversionError('Parse errors in strict mode', errors);
    }
  }

  // Step 2: Generate Java code
  const generator = new JavaGenerator({
    packageName: opts.packageName!,
    javaVersion: opts.javaVersion!,
    springBoot: opts.springBoot!,
    llmClient,
  });

  const generated = await generator.generate(ast);
  warnings.push(...generated.warnings);

  const durationMs = Date.now() - startTime;

  return {
    java: generated.code,
    className: generated.className,
    errors,
    warnings,
    metadata: {
      programName: ast.programName || 'Unknown',
      timestamp: new Date().toISOString(),
      llmProvider: opts.llmProvider!,
      linesConverted: cobolSource.split('\n').length,
      durationMs,
    },
  };
}

/**
 * @cobol2java/core
 * 
 * Core library for COBOL to Java conversion
 */

export { convert, type ConversionOptions, type ConversionResult } from './converter.js';
export { CobolParser, type CobolAst } from './parser.js';
export { JavaGenerator, type GeneratorOptions } from './generator.js';
export { createLLMClient, type LLMClient, type LLMProvider } from './llm/index.js';
export * from './errors.js';
export * from './transform/index.js';
export * from './dialect/index.js';
export * from './batch/index.js';
export * from './diff/index.js';
export * from './javadoc/index.js';
export * from './security/index.js';
export * from './performance/index.js';

// Benchmark module is Node.js only (uses fs/promises)
// Import directly from '@cobol2java/core/benchmark' for CLI usage
export type {
  CobolEvalProblem,
  TestCase,
  ResultType,
  EvalResult,
  BenchmarkSummary,
  BenchmarkReport,
  BenchmarkOptions,
} from './benchmark/types.js';

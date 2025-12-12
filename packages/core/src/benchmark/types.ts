/**
 * COBOLEval Benchmark Types
 * Based on HumanEval-style evaluation for COBOL
 */

/**
 * Test case result type
 */
export type ResultType = 'Bool' | 'Int' | 'Float' | 'String' | 'List';

/**
 * Single test case
 */
export interface TestCase {
  /** COBOL test program source */
  test: string;
  /** Expected result */
  result: {
    value: string;
    type_: ResultType;
  };
}

/**
 * COBOLEval problem definition
 */
export interface CobolEvalProblem {
  /** Unique task identifier (e.g., "HumanEval/0") */
  task_id: string;
  /** COBOL prompt/skeleton code */
  prompt: string;
  /** Entry point function name */
  entry_point: string;
  /** Canonical solution (Python reference) */
  canonical_solution: string;
  /** Test cases */
  tests: TestCase[];
}

/**
 * Evaluation result for a single problem
 */
export interface EvalResult {
  task_id: string;
  passed: boolean;
  total_tests: number;
  passed_tests: number;
  conversion_success: boolean;
  java_code?: string;
  errors?: string[];
  duration_ms: number;
}

/**
 * Benchmark summary
 */
export interface BenchmarkSummary {
  total_problems: number;
  conversion_success: number;
  conversion_rate: number;
  passed_problems: number;
  pass_rate: number;
  total_tests: number;
  passed_tests: number;
  test_pass_rate: number;
  duration_ms: number;
  timestamp: string;
}

/**
 * Full benchmark report
 */
export interface BenchmarkReport {
  summary: BenchmarkSummary;
  results: EvalResult[];
  errors: Array<{ task_id: string; error: string }>;
}

/**
 * Benchmark options
 */
export interface BenchmarkOptions {
  /** Path to CobolEval.jsonl file */
  dataPath: string;
  /** Maximum problems to evaluate (for quick testing) */
  limit?: number;
  /** Enable verbose output */
  verbose?: boolean;
  /** Timeout per problem in ms */
  timeoutMs?: number;
  /** Output file path for report */
  outputPath?: string;
}

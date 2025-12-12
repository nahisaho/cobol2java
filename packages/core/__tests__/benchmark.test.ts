/**
 * COBOLEval Benchmark Tests
 */

import { describe, it, expect, beforeAll } from 'vitest';
import { existsSync } from 'fs';
import { resolve } from 'path';
import {
  loadCobolEval,
  evaluateProblem,
  runBenchmark,
  formatReport,
  type CobolEvalProblem,
} from '../src/benchmark/index.js';

const COBOL_EVAL_PATH = resolve(
  process.cwd(),
  '../../storage/cobol-samples/COBOLEval/data/CobolEval.jsonl'
);

const datasetExists = existsSync(COBOL_EVAL_PATH);

describe('COBOLEval Benchmark', () => {
  describe('loadCobolEval', () => {
    it.skipIf(!datasetExists)('should load COBOLEval dataset', async () => {
      const problems = await loadCobolEval(COBOL_EVAL_PATH);
      
      expect(problems).toBeDefined();
      expect(Array.isArray(problems)).toBe(true);
      expect(problems.length).toBe(146);
    });

    it.skipIf(!datasetExists)('should parse problem structure correctly', async () => {
      const problems = await loadCobolEval(COBOL_EVAL_PATH);
      const first = problems[0]!;
      
      expect(first.task_id).toBe('HumanEval/0');
      expect(first.prompt).toContain('IDENTIFICATION DIVISION');
      expect(first.entry_point).toBe('has_close_elements');
      expect(first.tests).toHaveLength(4);
      expect(first.tests[0]!.result.type_).toBe('Bool');
    });
  });

  describe('evaluateProblem', () => {
    it.skipIf(!datasetExists)('should evaluate a problem', async () => {
      const problems = await loadCobolEval(COBOL_EVAL_PATH);
      const problem = problems[0]!;
      
      const result = evaluateProblem(problem);
      
      expect(result.task_id).toBe('HumanEval/0');
      expect(typeof result.passed).toBe('boolean');
      expect(typeof result.conversion_success).toBe('boolean');
      expect(result.duration_ms).toBeGreaterThanOrEqual(0);
    });

    it('should handle malformed COBOL gracefully', async () => {
      const badProblem: CobolEvalProblem = {
        task_id: 'test/bad',
        prompt: 'THIS IS NOT VALID COBOL',
        entry_point: 'bad_program',
        canonical_solution: '',
        tests: [{ test: '', result: { value: 'true', type_: 'Bool' } }],
      };
      
      const result = await evaluateProblem(badProblem);
      
      // Should not throw, but may have errors
      expect(result.task_id).toBe('test/bad');
      expect(typeof result.passed).toBe('boolean');
    });
  });

  describe('runBenchmark', () => {
    it.skipIf(!datasetExists)('should run benchmark with limit', async () => {
      const report = await runBenchmark({
        dataPath: COBOL_EVAL_PATH,
        limit: 5,
        verbose: false,
      });
      
      expect(report.summary.total_problems).toBe(5);
      expect(report.results).toHaveLength(5);
      expect(report.summary.duration_ms).toBeGreaterThan(0);
      expect(report.summary.timestamp).toBeDefined();
    });

    it.skipIf(!datasetExists)('should calculate metrics correctly', async () => {
      const report = await runBenchmark({
        dataPath: COBOL_EVAL_PATH,
        limit: 10,
        verbose: false,
      });
      
      const { summary } = report;
      
      // Rates should be between 0 and 1
      expect(summary.conversion_rate).toBeGreaterThanOrEqual(0);
      expect(summary.conversion_rate).toBeLessThanOrEqual(1);
      expect(summary.pass_rate).toBeGreaterThanOrEqual(0);
      expect(summary.pass_rate).toBeLessThanOrEqual(1);
      expect(summary.test_pass_rate).toBeGreaterThanOrEqual(0);
      expect(summary.test_pass_rate).toBeLessThanOrEqual(1);
    });
  });

  describe('formatReport', () => {
    it('should format report as string', () => {
      const report = {
        summary: {
          total_problems: 146,
          conversion_success: 120,
          conversion_rate: 0.822,
          passed_problems: 100,
          pass_rate: 0.685,
          total_tests: 584,
          passed_tests: 500,
          test_pass_rate: 0.856,
          duration_ms: 5000,
          timestamp: '2025-12-12T00:00:00.000Z',
        },
        results: [],
        errors: [],
      };
      
      const formatted = formatReport(report);
      
      expect(formatted).toContain('COBOLEval Benchmark Report');
      expect(formatted).toContain('146');
      expect(formatted).toContain('82.2');
      expect(formatted).toContain('Conversion Success');
    });

    it('should include error summary when errors exist', () => {
      const report = {
        summary: {
          total_problems: 10,
          conversion_success: 8,
          conversion_rate: 0.8,
          passed_problems: 8,
          pass_rate: 0.8,
          total_tests: 40,
          passed_tests: 32,
          test_pass_rate: 0.8,
          duration_ms: 1000,
          timestamp: '2025-12-12T00:00:00.000Z',
        },
        results: [],
        errors: [
          { task_id: 'HumanEval/5', error: 'Parse error at line 10' },
          { task_id: 'HumanEval/8', error: 'Unknown statement' },
        ],
      };
      
      const formatted = formatReport(report);
      
      expect(formatted).toContain('Errors (2)');
      expect(formatted).toContain('HumanEval/5');
      expect(formatted).toContain('HumanEval/8');
    });
  });

  describe('Performance', () => {
    it.skipIf(!datasetExists)('should process problems efficiently', async () => {
      const startTime = Date.now();
      
      await runBenchmark({
        dataPath: COBOL_EVAL_PATH,
        limit: 20,
        verbose: false,
      });
      
      const duration = Date.now() - startTime;
      
      // Should process 20 problems in under 5 seconds
      expect(duration).toBeLessThan(5000);
    });
  });
});

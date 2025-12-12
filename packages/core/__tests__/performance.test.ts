/**
 * Performance Tests for COBOL2Java
 * 
 * Tests conversion performance with various file sizes
 */

import { describe, it, expect } from 'vitest';
import { CobolParser, JavaGenerator } from '../src/index.js';

/**
 * Generate COBOL program of specified size
 */
function generateLargeCobol(dataItems: number, procedures: number): string {
  const lines: string[] = [
    '       IDENTIFICATION DIVISION.',
    '       PROGRAM-ID. PERFTEST.',
    '       DATA DIVISION.',
    '       WORKING-STORAGE SECTION.',
  ];

  // Generate data items
  for (let i = 1; i <= dataItems; i++) {
    lines.push(`       01 WS-ITEM-${i.toString().padStart(4, '0')}.`);
    lines.push(`          05 WS-FIELD-A-${i} PIC X(10).`);
    lines.push(`          05 WS-FIELD-B-${i} PIC 9(5).`);
    lines.push(`          05 WS-FIELD-C-${i} PIC 9(7)V99.`);
  }

  lines.push('       PROCEDURE DIVISION.');
  lines.push('       MAIN-PROC.');

  // Generate procedures
  for (let i = 1; i <= procedures; i++) {
    lines.push(`           MOVE "DATA${i}" TO WS-FIELD-A-1.`);
    lines.push(`           COMPUTE WS-FIELD-B-1 = ${i} * 10.`);
    lines.push(`           IF WS-FIELD-B-1 > 100`);
    lines.push(`               DISPLAY "Large value: " WS-FIELD-B-1`);
    lines.push(`           END-IF.`);
  }

  lines.push('           STOP RUN.');

  return lines.join('\n');
}

/**
 * Measure execution time
 */
function measureTime<T>(fn: () => T): { result: T; durationMs: number } {
  const start = performance.now();
  const result = fn();
  const durationMs = performance.now() - start;
  return { result, durationMs };
}

describe('Performance Tests', () => {
  describe('Parser Performance', () => {
    it('should parse small program (10 items, 5 statements) under 50ms', () => {
      const source = generateLargeCobol(10, 5);
      const parser = new CobolParser();
      
      const { result, durationMs } = measureTime(() => parser.parse(source));
      
      expect(result).toBeDefined();
      expect(result.dataItems.length).toBeGreaterThan(0);
      expect(durationMs).toBeLessThan(50);
      console.log(`Small program parse time: ${durationMs.toFixed(2)}ms`);
    });

    it('should parse medium program (50 items, 25 statements) under 100ms', () => {
      const source = generateLargeCobol(50, 25);
      const parser = new CobolParser();
      
      const { result, durationMs } = measureTime(() => parser.parse(source));
      
      expect(result).toBeDefined();
      expect(durationMs).toBeLessThan(100);
      console.log(`Medium program parse time: ${durationMs.toFixed(2)}ms`);
    });

    it('should parse large program (100 items, 50 statements) under 200ms', () => {
      const source = generateLargeCobol(100, 50);
      const parser = new CobolParser();
      
      const { result, durationMs } = measureTime(() => parser.parse(source));
      
      expect(result).toBeDefined();
      expect(durationMs).toBeLessThan(200);
      console.log(`Large program parse time: ${durationMs.toFixed(2)}ms`);
    });

    it('should parse very large program (200 items, 100 statements) under 500ms', () => {
      const source = generateLargeCobol(200, 100);
      const parser = new CobolParser();
      
      const { result, durationMs } = measureTime(() => parser.parse(source));
      
      expect(result).toBeDefined();
      expect(durationMs).toBeLessThan(500);
      console.log(`Very large program parse time: ${durationMs.toFixed(2)}ms`);
    });
  });

  describe('Generator Performance', () => {
    it('should generate Java for small AST under 50ms', async () => {
      const source = generateLargeCobol(10, 5);
      const parser = new CobolParser();
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      
      const start = performance.now();
      const result = await generator.generate(ast);
      const durationMs = performance.now() - start;
      
      expect(result).toBeDefined();
      expect(durationMs).toBeLessThan(50);
      console.log(`Small program generation time: ${durationMs.toFixed(2)}ms`);
    });

    it('should generate Java for medium AST under 100ms', async () => {
      const source = generateLargeCobol(50, 25);
      const parser = new CobolParser();
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      
      const start = performance.now();
      const result = await generator.generate(ast);
      const durationMs = performance.now() - start;
      
      expect(result).toBeDefined();
      expect(durationMs).toBeLessThan(100);
      console.log(`Medium program generation time: ${durationMs.toFixed(2)}ms`);
    });

    it('should generate Java for large AST under 200ms', async () => {
      const source = generateLargeCobol(100, 50);
      const parser = new CobolParser();
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      
      const start = performance.now();
      const result = await generator.generate(ast);
      const durationMs = performance.now() - start;
      
      expect(result).toBeDefined();
      expect(durationMs).toBeLessThan(200);
      console.log(`Large program generation time: ${durationMs.toFixed(2)}ms`);
    });
  });

  describe('Memory Efficiency', () => {
    it('should not significantly increase memory for large programs', async () => {
      // Get baseline memory
      const baselineHeap = process.memoryUsage().heapUsed;
      
      // Generate and parse/generate large program
      const source = generateLargeCobol(100, 50);
      const parser = new CobolParser();
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      
      // Force garbage collection hint
      void result.code.length; // Access to prevent optimization
      
      const currentHeap = process.memoryUsage().heapUsed;
      const memoryIncreaseMB = (currentHeap - baselineHeap) / (1024 * 1024);
      
      // Should use less than 50MB additional memory
      expect(memoryIncreaseMB).toBeLessThan(50);
      console.log(`Memory increase: ${memoryIncreaseMB.toFixed(2)}MB`);
    });
  });

  describe('Throughput', () => {
    it('should parse and generate at least 50 small programs per second', async () => {
      const source = generateLargeCobol(10, 5);
      const iterations = 50;
      
      const start = performance.now();
      for (let i = 0; i < iterations; i++) {
        const parser = new CobolParser();
        const ast = parser.parse(source);
        const generator = new JavaGenerator({
          packageName: 'com.example',
          javaVersion: 17,
          springBoot: false,
          springBatch: false,
        });
        await generator.generate(ast);
      }
      const durationMs = performance.now() - start;
      
      const throughput = (iterations / durationMs) * 1000;
      expect(throughput).toBeGreaterThan(50);
      console.log(`Throughput: ${throughput.toFixed(2)} conversions/second`);
    });
  });

  describe('Scaling', () => {
    it('should scale linearly with input size', async () => {
      const sizes = [10, 20, 40];
      const times: number[] = [];
      
      for (const size of sizes) {
        const source = generateLargeCobol(size, size);
        const start = performance.now();
        const parser = new CobolParser();
        const ast = parser.parse(source);
        const generator = new JavaGenerator({
          packageName: 'com.example',
          javaVersion: 17,
          springBoot: false,
          springBatch: false,
        });
        await generator.generate(ast);
        const durationMs = performance.now() - start;
        times.push(durationMs);
      }
      
      // Check that doubling size doesn't more than triple time (allowing for overhead)
      const ratio = times[2] / times[0];
      expect(ratio).toBeLessThan(10); // Should be roughly linear, allowing some overhead
      console.log(`Scaling ratio (4x size): ${ratio.toFixed(2)}x time`);
    });
  });
});

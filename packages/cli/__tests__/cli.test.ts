/**
 * CLI Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { execSync } from 'node:child_process';
import { existsSync, rmSync, readFileSync } from 'node:fs';
import { join } from 'node:path';

const CLI_PATH = join(__dirname, '../dist/bin/cobol2java.js');
const EXAMPLES_PATH = join(__dirname, '../../../examples');
const OUTPUT_PATH = join(__dirname, '../test-output');

function runCli(args: string): { stdout: string; stderr: string; exitCode: number } {
  try {
    const stdout = execSync(`node ${CLI_PATH} ${args}`, {
      encoding: 'utf-8',
      cwd: join(__dirname, '../../..'),
    });
    return { stdout, stderr: '', exitCode: 0 };
  } catch (error: unknown) {
    const err = error as { stdout?: string; stderr?: string; status?: number };
    return {
      stdout: err.stdout || '',
      stderr: err.stderr || '',
      exitCode: err.status || 1,
    };
  }
}

describe('CLI', () => {
  beforeEach(() => {
    // Clean test output
    if (existsSync(OUTPUT_PATH)) {
      rmSync(OUTPUT_PATH, { recursive: true });
    }
  });

  afterEach(() => {
    // Clean test output
    if (existsSync(OUTPUT_PATH)) {
      rmSync(OUTPUT_PATH, { recursive: true });
    }
  });

  describe('--help', () => {
    it('displays help information', () => {
      const { stdout, exitCode } = runCli('--help');
      expect(exitCode).toBe(0);
      expect(stdout).toContain('cobol2java');
      expect(stdout).toContain('convert');
      expect(stdout).toContain('validate');
      expect(stdout).toContain('benchmark');
    });
  });

  describe('--version', () => {
    it('displays version', () => {
      const { stdout, exitCode } = runCli('--version');
      expect(exitCode).toBe(0);
      expect(stdout).toMatch(/\d+\.\d+\.\d+/);
    });
  });

  describe('convert command', () => {
    it('converts COBOL file to Java', () => {
      const { stdout, exitCode } = runCli(
        `convert ${EXAMPLES_PATH}/hello-world.cbl -o ${OUTPUT_PATH}`
      );
      expect(exitCode).toBe(0);
      expect(stdout).toContain('Generated');
      expect(existsSync(join(OUTPUT_PATH, 'HelloWorld.java'))).toBe(true);
    });

    it('generates valid Java code', () => {
      runCli(`convert ${EXAMPLES_PATH}/hello-world.cbl -o ${OUTPUT_PATH}`);
      const javaCode = readFileSync(join(OUTPUT_PATH, 'HelloWorld.java'), 'utf-8');
      expect(javaCode).toContain('public class HelloWorld');
      expect(javaCode).toContain('public void execute()');
      expect(javaCode).toContain('public static void main');
    });

    it('includes package declaration', () => {
      runCli(`convert ${EXAMPLES_PATH}/hello-world.cbl -o ${OUTPUT_PATH} -p com.test`);
      const javaCode = readFileSync(join(OUTPUT_PATH, 'HelloWorld.java'), 'utf-8');
      expect(javaCode).toContain('package com.test');
    });

    it('shows verbose output', () => {
      const { stdout } = runCli(
        `convert ${EXAMPLES_PATH}/hello-world.cbl -o ${OUTPUT_PATH} --verbose`
      );
      expect(stdout).toContain('Metadata');
      expect(stdout).toContain('Program:');
      expect(stdout).toContain('Lines:');
      expect(stdout).toContain('Duration:');
    });

    it('supports JSON output format', () => {
      const { stdout, exitCode } = runCli(
        `convert ${EXAMPLES_PATH}/hello-world.cbl -o ${OUTPUT_PATH} --format json`
      );
      expect(exitCode).toBe(0);
      const result = JSON.parse(stdout);
      expect(result.success).toBe(true);
      expect(result.className).toBe('HelloWorld');
    });
  });

  describe('validate command', () => {
    it('validates correct COBOL file', () => {
      const { stdout, exitCode } = runCli(
        `validate ${EXAMPLES_PATH}/hello-world.cbl`
      );
      expect(exitCode).toBe(0);
      expect(stdout).toContain('No errors found');
    });

    it('validates all example files', () => {
      const exampleFiles = ['hello-world.cbl', 'fibonacci.cbl', 'calculate-tax.cbl'];
      for (const file of exampleFiles) {
        const { exitCode } = runCli(`validate ${EXAMPLES_PATH}/${file}`);
        expect(exitCode).toBe(0);
      }
    });
  });

  describe('convert all examples', () => {
    it('converts fibonacci.cbl', () => {
      const { exitCode } = runCli(
        `convert ${EXAMPLES_PATH}/fibonacci.cbl -o ${OUTPUT_PATH}`
      );
      expect(exitCode).toBe(0);
      const javaCode = readFileSync(join(OUTPUT_PATH, 'Fibonacci.java'), 'utf-8');
      expect(javaCode).toContain('public class Fibonacci');
      expect(javaCode).toContain('while'); // PERFORM UNTIL -> while loop
    });

    it('converts calculate-tax.cbl', () => {
      const { exitCode } = runCli(
        `convert ${EXAMPLES_PATH}/calculate-tax.cbl -o ${OUTPUT_PATH}`
      );
      expect(exitCode).toBe(0);
      expect(existsSync(join(OUTPUT_PATH, 'CalculateTax.java'))).toBe(true);
    });

    it('converts grade-checker.cbl', () => {
      const { exitCode } = runCli(
        `convert ${EXAMPLES_PATH}/grade-checker.cbl -o ${OUTPUT_PATH}`
      );
      expect(exitCode).toBe(0);
      expect(existsSync(join(OUTPUT_PATH, 'GradeChecker.java'))).toBe(true);
    });

    it('converts status-checker.cbl', () => {
      const { exitCode } = runCli(
        `convert ${EXAMPLES_PATH}/status-checker.cbl -o ${OUTPUT_PATH}`
      );
      expect(exitCode).toBe(0);
      expect(existsSync(join(OUTPUT_PATH, 'StatusChecker.java'))).toBe(true);
    });
  });
});

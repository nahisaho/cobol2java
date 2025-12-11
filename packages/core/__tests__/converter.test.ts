/**
 * Core library tests
 */

import { describe, it, expect } from 'vitest';
import { convert, CobolParser, createLLMClient } from '../src/index.js';

describe('convert', () => {
  it('converts simple COBOL program to Java', async () => {
    const cobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO.
      PROCEDURE DIVISION.
        DISPLAY "Hello, World!".
        STOP RUN.
    `;

    const result = await convert(cobol, { llmProvider: 'none' });

    expect(result.java).toContain('public class Hello');
    expect(result.className).toBe('Hello');
    expect(result.metadata.programName).toBe('HELLO');
  });

  it('generates Spring Boot code with option', async () => {
    const cobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. CALCULATOR.
      PROCEDURE DIVISION.
        STOP RUN.
    `;

    const result = await convert(cobol, {
      llmProvider: 'none',
      springBoot: true,
      packageName: 'com.myapp',
    });

    expect(result.java).toContain('@Service');
    expect(result.java).toContain('package com.myapp;');
  });

  it('returns warnings for incomplete COBOL', async () => {
    const cobol = 'INVALID COBOL CODE';

    const result = await convert(cobol, { llmProvider: 'none' });

    // Parser reports missing divisions as errors
    expect(result.errors.length).toBeGreaterThan(0);
  });
});

describe('CobolParser', () => {
  it('parses IDENTIFICATION DIVISION', () => {
    const parser = new CobolParser();
    const cobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. MYPROGRAM.
    `;

    const ast = parser.parse(cobol);

    expect(ast.type).toBe('program');
    expect(ast.programName).toBe('MYPROGRAM');
  });

  it('reports missing PROCEDURE DIVISION', () => {
    const parser = new CobolParser();
    const cobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
    `;

    const ast = parser.parse(cobol);

    expect(ast.errors.some(e => e.code === 'CVT002')).toBe(true);
  });
});

describe('LLM Client', () => {
  it('creates noop client for none provider', () => {
    const client = createLLMClient({ provider: 'none' });

    expect(client.provider).toBe('none');
  });

  it('noop client is always available', async () => {
    const client = createLLMClient({ provider: 'none' });

    expect(await client.isAvailable()).toBe(true);
  });

  it('noop client returns empty string', async () => {
    const client = createLLMClient({ provider: 'none' });

    const result = await client.complete('test prompt');

    expect(result).toBe('');
  });
});

/**
 * Full Pipeline E2E Tests
 * 
 * Tests the complete conversion pipeline from COBOL source to Java output
 */

import { describe, it, expect } from 'vitest';
import { CobolParser, JavaGenerator } from '../src/index.js';

describe('Full Conversion Pipeline', () => {
  describe('Basic Program Conversion', () => {
    it('should convert simple COBOL to valid Java', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".
       PROCEDURE DIVISION.
       MAIN-PROC.
           DISPLAY WS-MESSAGE.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      // Verify structure
      expect(result.code).toContain('com.example');
      expect(result.code).toContain('class');
      expect(result.className).toBe('Hello');
    });

    it('should handle multiple data items', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIDATA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(30).
       01 WS-AGE PIC 9(3).
       01 WS-SALARY PIC 9(7)V99.
       01 WS-ACTIVE PIC X VALUE 'Y'.
       PROCEDURE DIVISION.
       MAIN-PROC.
           DISPLAY "Done".
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      expect(result.code).toContain('class');
      expect(result.className).toBe('Multidata');
    });

    it('should generate numeric handling with BigDecimal', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMBERS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMOUNT PIC 9(9)V99.
       01 WS-RATE PIC V9999.
       01 WS-TOTAL PIC 9(11)V99.
       PROCEDURE DIVISION.
       CALC-TOTAL.
           COMPUTE WS-TOTAL = WS-AMOUNT * WS-RATE.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      expect(result.code).toContain('BigDecimal');
    });
  });

  describe('Spring Boot Generation', () => {
    it('should generate Spring Service annotation', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTSVC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUST-ID PIC 9(10).
       PROCEDURE DIVISION.
       GET-CUSTOMER.
           DISPLAY WS-CUST-ID.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example.customer',
        javaVersion: 17,
        springBoot: true,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      expect(result.code).toContain('@Service');
      expect(result.code).toContain('com.example.customer');
    });

    it('should generate Spring annotation when specified', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTCTL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESPONSE PIC X(100).
       PROCEDURE DIVISION.
       HANDLE-REQUEST.
           DISPLAY WS-RESPONSE.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: true,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      // Should have Spring annotations
      expect(result.code).toContain('@Service');
    });
  });

  describe('Group Data Item Conversion', () => {
    it('should convert group items', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GROUPS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ADDRESS.
          05 WS-STREET PIC X(50).
          05 WS-CITY PIC X(30).
          05 WS-STATE PIC X(2).
          05 WS-ZIP PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      expect(result.code).toContain('class');
    });
  });

  describe('Procedure Division Conversion', () => {
    it('should convert paragraphs to methods', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. METHODS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(5) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM INIT-DATA.
           PERFORM PROCESS-DATA.
           PERFORM CLEANUP.
           STOP RUN.
       INIT-DATA.
           MOVE 0 TO WS-COUNTER.
       PROCESS-DATA.
           ADD 1 TO WS-COUNTER.
           DISPLAY WS-COUNTER.
       CLEANUP.
           DISPLAY "Done".
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);

      // Should have method definitions
      expect(result.code).toContain('void');
    });
  });

  describe('Error Handling', () => {
    it('should handle invalid COBOL gracefully', () => {
      const invalidCobol = `
       THIS IS NOT VALID COBOL
       RANDOM TEXT HERE
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(invalidCobol)).not.toThrow();
    });

    it('should handle empty source', () => {
      const parser = new CobolParser();
      expect(() => parser.parse('')).not.toThrow();
    });

    it('should handle minimal COBOL', async () => {
      const minimalCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIMAL.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(minimalCobol);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      expect(result.code).toContain('class');
      expect(result.className).toBe('Minimal');
    });
  });

  describe('COBOL-85 Constructs', () => {
    it('should convert EVALUATE statement', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CODE PIC X(2).
       01 WS-DESC PIC X(20).
       PROCEDURE DIVISION.
       CHECK-CODE.
           EVALUATE WS-CODE
               WHEN 'AA'
                   MOVE "Active" TO WS-DESC
               WHEN 'IN'
                   MOVE "Inactive" TO WS-DESC
               WHEN OTHER
                   MOVE "Unknown" TO WS-DESC
           END-EVALUATE.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      expect(result.code).toContain('class');
      expect(result.className).toBe('Evaltest');
    });

    it('should convert PERFORM with VARYING', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOPTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-IDX PIC 9(3).
       01 WS-TOTAL PIC 9(7).
       PROCEDURE DIVISION.
       LOOP-PROC.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 100
               ADD WS-IDX TO WS-TOTAL
           END-PERFORM.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      expect(result.code).toContain('class');
      expect(result.className).toBe('Looptest');
    });
  });

  describe('File I/O Conversion', () => {
    it('should convert file definitions', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILETEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-REC PIC X(80).
       WORKING-STORAGE SECTION.
       01 WS-EOF PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       READ-FILE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-REC
               AT END SET WS-EOF TO TRUE.
           CLOSE INPUT-FILE.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      expect(result.code).toContain('class');
      expect(result.className).toBe('Filetest');
    });
  });

  describe('Complex Data Types', () => {
    it('should handle COMP fields', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BINARY-NUM PIC S9(9) COMP.
       01 WS-PACKED-NUM PIC S9(9)V99 COMP-3.
       01 WS-DISPLAY-NUM PIC S9(9)V99.
       PROCEDURE DIVISION.
       MAIN-PROC.
           MOVE 12345 TO WS-BINARY-NUM.
           MOVE 12345.67 TO WS-PACKED-NUM.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      expect(result.code).toContain('BigDecimal');
    });

    it('should handle OCCURS with index', async () => {
      const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAYTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ITEM OCCURS 50 TIMES INDEXED BY IDX.
             10 WS-CODE PIC X(5).
             10 WS-DESC PIC X(20).
       PROCEDURE DIVISION.
       MAIN-PROC.
           SET IDX TO 1.
           MOVE "TEST" TO WS-CODE(IDX).
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      expect(result.code).toContain('class');
      expect(result.className).toBe('Arraytest');
    });
  });
});

describe('Conversion Options', () => {
  it('should apply all options correctly', async () => {
    const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
    `;

    const parser = new CobolParser();
    const ast = parser.parse(source);
    const generator = new JavaGenerator({
      packageName: 'com.example.test',
      javaVersion: 17,
      springBoot: true,
      springBatch: false,
    });
    const result = await generator.generate(ast);

    expect(result.code).toContain('com.example.test');
    expect(result.code).toContain('class');
  });
});

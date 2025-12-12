/**
 * COBOL Dialect Detection Tests
 */

import { describe, it, expect } from 'vitest';
import { detectDialect, getDialectHints } from '../src/dialect/dialect.js';

describe('COBOL Dialect Detection', () => {
  describe('detectDialect', () => {
    it('should detect IBM Enterprise COBOL with EXEC SQL', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SQLPROG.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        EXEC SQL
          INCLUDE SQLCA
        END-EXEC.
        PROCEDURE DIVISION.
          EXEC SQL
            SELECT * FROM CUSTOMERS
          END-EXEC.
          GOBACK.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('ibm');
      expect(result.confidence).toBeGreaterThan(10);
      expect(result.markers.some(m => /EXEC\s+SQL/i.test(m))).toBe(true);
    });

    it('should detect IBM COBOL with EXEC CICS', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CICSPROG.
        PROCEDURE DIVISION.
          EXEC CICS
            SEND MAP('CUSTMAP')
          END-EXEC.
          GOBACK.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('ibm');
      expect(result.markers.some(m => /EXEC\s+CICS/i.test(m))).toBe(true);
    });

    it('should detect IBM COBOL with JSON operations', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. JSONPROG.
        PROCEDURE DIVISION.
          JSON GENERATE WS-JSON FROM WS-RECORD.
          GOBACK.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('ibm');
    });

    it('should detect Micro Focus COBOL with $SET', () => {
      const source = `
        $SET SOURCEFORMAT "FREE"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. MFPROG.
        DATA DIVISION.
        SCREEN SECTION.
        01 MAIN-SCREEN.
          05 LINE 1 COL 1 VALUE "Hello".
        PROCEDURE DIVISION.
          DISPLAY MAIN-SCREEN.
          STOP RUN.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('microfocus');
      expect(result.markers.some(m => /\$SET/i.test(m))).toBe(true);
    });

    it('should detect Micro Focus COBOL with SCREEN SECTION', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SCREENPROG.
        DATA DIVISION.
        SCREEN SECTION.
        01 INPUT-SCREEN.
          05 LINE 5 COL 10 VALUE "Name:".
        PROCEDURE DIVISION.
          ACCEPT FROM CONSOLE.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('microfocus');
    });

    it('should detect GnuCOBOL with ALLOCATE', () => {
      const source = `
        >>SOURCE FORMAT FREE
        IDENTIFICATION DIVISION.
        PROGRAM-ID. GNUPROG.
        DATA DIVISION.
        LOCAL-STORAGE SECTION.
        01 WS-PTR POINTER.
        PROCEDURE DIVISION.
          ALLOCATE WS-RECORD RETURNING WS-PTR.
          FREE WS-PTR.
          STOP RUN.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('gnucobol');
    });

    it('should detect GnuCOBOL with exception handling', () => {
      const source = `
        >>SOURCE FORMAT FREE
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TRYPROG.
        DATA DIVISION.
        LOCAL-STORAGE SECTION.
        01 WS-RESULT PIC 9(5).
        PROCEDURE DIVISION.
          ALLOCATE WS-BUFFER.
          TRY
            COMPUTE WS-RESULT = 100 / WS-DIVISOR
          CATCH
            DISPLAY "Division error"
          FINALLY
            DISPLAY "Done"
          END-TRY.
          FREE WS-BUFFER.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('gnucobol');
    });

    it('should default to COBOL-85 for standard code', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. STDPROG.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-NAME PIC X(20).
        PROCEDURE DIVISION.
          MOVE "HELLO" TO WS-NAME.
          DISPLAY WS-NAME.
          STOP RUN.
      `;
      const result = detectDialect(source);
      expect(result.dialect).toBe('cobol85');
    });

    it('should return markers for detected features', () => {
      const source = `
        EXEC SQL
          SELECT CUSTOMER_ID INTO :WS-ID
          FROM CUSTOMERS
        END-EXEC.
        EXEC CICS RETURN END-EXEC.
        GOBACK.
      `;
      const result = detectDialect(source);
      expect(result.markers.length).toBeGreaterThan(0);
    });

    it('should have confidence between 0 and 100', () => {
      const source = `IDENTIFICATION DIVISION. PROGRAM-ID. TEST.`;
      const result = detectDialect(source);
      expect(result.confidence).toBeGreaterThanOrEqual(0);
      expect(result.confidence).toBeLessThanOrEqual(100);
    });
  });

  describe('getDialectHints', () => {
    it('should return IBM-specific hints', () => {
      const hints = getDialectHints('ibm');
      expect(hints).toContain('Use JDBC or JPA for EXEC SQL statements');
      expect(hints.some(h => h.includes('CICS'))).toBe(true);
    });

    it('should return Micro Focus hints', () => {
      const hints = getDialectHints('microfocus');
      expect(hints.some(h => h.includes('SCREEN'))).toBe(true);
    });

    it('should return GnuCOBOL hints', () => {
      const hints = getDialectHints('gnucobol');
      expect(hints.some(h => h.includes('ALLOCATE'))).toBe(true);
    });

    it('should return COBOL-85 hints', () => {
      const hints = getDialectHints('cobol85');
      expect(hints.some(h => h.includes('PERFORM'))).toBe(true);
    });

    it('should return default hints for unknown dialect', () => {
      const hints = getDialectHints('unknown');
      expect(hints.length).toBeGreaterThan(0);
    });
  });
});

import { describe, it, expect } from 'vitest';
import { CobolParser } from '../src/parser.js';

describe('CobolParser', () => {
  const parser = new CobolParser();

  describe('parse', () => {
    it('should parse PROGRAM-ID', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.programName).toBe('TEST-PROGRAM');
      expect(ast.type).toBe('program');
    });

    it('should handle missing IDENTIFICATION DIVISION with warning', () => {
      const source = `
        PROGRAM-ID. TEST.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.errors.some(e => e.code === 'CVT001')).toBe(true);
    });

    it('should parse data items', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DATA-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-NAME PIC X(20).
        01 WS-COUNT PIC 9(5) VALUE 0.
        01 WS-AMOUNT PIC 9(7)V99 VALUE 100.50.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.dataItems.length).toBe(3);
      
      expect(ast.dataItems[0]).toEqual({
        level: 1,
        name: 'WS-NAME',
        pic: 'X(20)',
      });
      
      expect(ast.dataItems[1]).toEqual({
        level: 1,
        name: 'WS-COUNT',
        pic: '9(5)',
        value: '0',
      });
      
      expect(ast.dataItems[2]).toEqual({
        level: 1,
        name: 'WS-AMOUNT',
        pic: '9(7)V99',
        value: '100.50',
      });
    });

    it('should parse paragraphs', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PARA-TEST.
        PROCEDURE DIVISION.
        MAIN-PARA.
            DISPLAY "HELLO"
            PERFORM SUB-PARA
            STOP RUN.
        SUB-PARA.
            DISPLAY "WORLD".
      `;
      const ast = parser.parse(source);

      expect(ast.paragraphs.length).toBe(2);
      expect(ast.paragraphs[0]?.name).toBe('MAIN-PARA');
      expect(ast.paragraphs[0]?.statements).toContain('DISPLAY "HELLO"');
      expect(ast.paragraphs[0]?.statements).toContain('PERFORM SUB-PARA');
      
      expect(ast.paragraphs[1]?.name).toBe('SUB-PARA');
      expect(ast.paragraphs[1]?.statements.some(s => s.includes('DISPLAY "WORLD"'))).toBe(true);
    });

    it('should parse various data item formats', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. FORMAT-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SIMPLE PIC 9.
        01 WS-DECIMAL PIC 9V99 VALUE 0.10.
        01 WS-STRING PIC X(30) VALUE "HELLO WORLD".
        01 WS-LARGE PIC 9(15).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.dataItems.length).toBe(4);
      expect(ast.dataItems[0]?.pic).toBe('9');
      expect(ast.dataItems[1]?.pic).toBe('9V99');
      expect(ast.dataItems[1]?.value).toBe('0.10');
      expect(ast.dataItems[2]?.pic).toBe('X(30)');
      expect(ast.dataItems[2]?.value).toBe('HELLO WORLD');
      expect(ast.dataItems[3]?.pic).toBe('9(15)');
    });

    it('should skip comments', () => {
      const source = `
        IDENTIFICATION DIVISION.
       * This is a comment
        PROGRAM-ID. COMMENT-TEST.
       * Another comment
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "TEST"
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.programName).toBe('COMMENT-TEST');
      expect(ast.paragraphs.length).toBe(1);
    });

    it('should parse statements with variables', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. STMT-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-A PIC 9(5).
        01 WS-B PIC 9(5).
        PROCEDURE DIVISION.
        MAIN.
            MOVE 10 TO WS-A
            ADD 5 TO WS-A
            SUBTRACT 2 FROM WS-B
            DISPLAY WS-A
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const mainPara = ast.paragraphs[0];
      expect(mainPara?.statements).toContain('MOVE 10 TO WS-A');
      expect(mainPara?.statements).toContain('ADD 5 TO WS-A');
      expect(mainPara?.statements).toContain('SUBTRACT 2 FROM WS-B');
      expect(mainPara?.statements).toContain('DISPLAY WS-A');
    });
  });
});

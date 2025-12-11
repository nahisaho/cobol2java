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

    it('should parse REDEFINES clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. REDEFINES-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DATE PIC 9(8).
        01 WS-DATE-R REDEFINES WS-DATE.
           05 WS-YEAR PIC 9(4).
           05 WS-MONTH PIC 9(2).
           05 WS-DAY PIC 9(2).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const redefinesItem = ast.dataItems.find(d => d.name === 'WS-DATE-R');
      expect(redefinesItem).toBeDefined();
      expect(redefinesItem?.redefines).toBe('WS-DATE');
    });

    it('should parse OCCURS with INDEXED BY', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. OCCURS-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-TABLE.
           05 WS-ENTRY OCCURS 10 INDEXED BY WS-IDX.
              10 WS-NAME PIC X(20).
              10 WS-VALUE PIC 9(5).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const occursItem = ast.dataItems.find(d => d.name === 'WS-ENTRY');
      expect(occursItem).toBeDefined();
      expect(occursItem?.occurs).toBe(10);
      expect(occursItem?.indexed).toContain('WS-IDX');
    });

    it('should parse SELECT...ASSIGN statement', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. FILE-TEST.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT CUSTOMER-FILE ASSIGN TO "CUSTFILE.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CUST-ID
                   FILE STATUS IS WS-FILE-STATUS.
        DATA DIVISION.
        FILE SECTION.
        FD CUSTOMER-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           LABEL RECORDS ARE STANDARD.
        WORKING-STORAGE SECTION.
        01 WS-FILE-STATUS PIC XX.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.fileDefinitions).toBeDefined();
      expect(ast.fileDefinitions.length).toBe(1);
      
      const fileDef = ast.fileDefinitions[0]!;
      expect(fileDef.selectName).toBe('CUSTOMER-FILE');
      expect(fileDef.assignTo).toBe('CUSTFILE.DAT');
      expect(fileDef.organization).toBe('INDEXED');
      expect(fileDef.accessMode).toBe('DYNAMIC');
      expect(fileDef.recordKey).toBe('CUST-ID');
      expect(fileDef.fileStatus).toBe('WS-FILE-STATUS');
    });

    it('should parse simple SELECT...ASSIGN for sequential file', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SEQ-FILE-TEST.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO "INPUT.TXT".
        DATA DIVISION.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.fileDefinitions.length).toBe(1);
      
      const fileDef = ast.fileDefinitions[0]!;
      expect(fileDef.selectName).toBe('INPUT-FILE');
      expect(fileDef.assignTo).toBe('INPUT.TXT');
    });
  });
});

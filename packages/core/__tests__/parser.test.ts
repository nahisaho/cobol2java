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

    it('should parse IDENTIFICATION DIVISION extended paragraphs', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. IDENT-TEST.
        AUTHOR. John Smith.
        INSTALLATION. Main Office.
        DATE-WRITTEN. 2025-12-12.
        DATE-COMPILED. 2025-12-12.
        SECURITY. Confidential.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.identificationInfo).toBeDefined();
      expect(ast.identificationInfo?.programId).toBe('IDENT-TEST');
      expect(ast.identificationInfo?.author).toBe('John Smith');
      expect(ast.identificationInfo?.installation).toBe('Main Office');
      expect(ast.identificationInfo?.dateWritten).toBe('2025-12-12');
      expect(ast.identificationInfo?.dateCompiled).toBe('2025-12-12');
      expect(ast.identificationInfo?.security).toBe('Confidential');
    });

    it('should parse SPECIAL-NAMES paragraph', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SPECIAL-TEST.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA
            CURRENCY SIGN IS "EUR".
        DATA DIVISION.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.specialNames).toBeDefined();
      expect(ast.specialNames?.decimalPointIsComma).toBe(true);
      expect(ast.specialNames?.currencySign).toBe('EUR');
    });

    it('should parse COPY statement', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COPY-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
            COPY CUSTOMER-COPY.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.copyStatements).toBeDefined();
      expect(ast.copyStatements.length).toBeGreaterThanOrEqual(1);
      const copyStmt = ast.copyStatements.find(c => c.copybook === 'CUSTOMER-COPY');
      expect(copyStmt).toBeDefined();
    });

    it('should parse COPY with REPLACING', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COPY-REPLACE-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
            COPY MASTER-COPY REPLACING ==:PREFIX:== BY ==WS-==.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      expect(ast.copyStatements.length).toBeGreaterThanOrEqual(1);
      const copyStmt = ast.copyStatements.find(c => c.copybook === 'MASTER-COPY');
      expect(copyStmt).toBeDefined();
      expect(copyStmt?.replacing).toBeDefined();
      expect(copyStmt?.replacing?.length).toBeGreaterThanOrEqual(1);
      expect(copyStmt?.replacing?.[0]?.from).toBe(':PREFIX:');
      expect(copyStmt?.replacing?.[0]?.to).toBe('WS-');
    });
  });

  describe('DATA DIVISION enhancements', () => {
    it('should parse USAGE clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. USAGE-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-BINARY-NUM PIC 9(9) USAGE BINARY.
        01 WS-COMP-NUM PIC 9(5) COMP.
        01 WS-PACKED PIC S9(7)V99 COMP-3.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const binaryItem = ast.dataItems.find(d => d.name === 'WS-BINARY-NUM');
      expect(binaryItem?.usage).toBe('BINARY');

      const compItem = ast.dataItems.find(d => d.name === 'WS-COMP-NUM');
      expect(compItem?.usage).toBe('COMP');

      const packedItem = ast.dataItems.find(d => d.name === 'WS-PACKED');
      expect(packedItem?.usage).toBe('COMP-3');
    });

    it('should parse SYNCHRONIZED clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SYNC-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SYNC-DATA PIC 9(9) SYNC.
        01 WS-SYNC-LEFT PIC X(10) SYNCHRONIZED LEFT.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const syncItem = ast.dataItems.find(d => d.name === 'WS-SYNC-DATA');
      expect(syncItem?.synchronized).toBe('RIGHT');

      const syncLeftItem = ast.dataItems.find(d => d.name === 'WS-SYNC-LEFT');
      expect(syncLeftItem?.synchronized).toBe('LEFT');
    });

    it('should parse JUSTIFIED clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. JUST-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-JUST-DATA PIC X(20) JUST RIGHT.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const justItem = ast.dataItems.find(d => d.name === 'WS-JUST-DATA');
      expect(justItem?.justified).toBe('RIGHT');
    });

    it('should parse BLANK WHEN ZERO clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. BLANK-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-AMOUNT PIC ZZZ9.99 BLANK WHEN ZERO.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const blankItem = ast.dataItems.find(d => d.name === 'WS-AMOUNT');
      expect(blankItem?.blankWhenZero).toBe(true);
    });

    it('should parse SIGN clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SIGN-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SIGNED PIC S9(5) SIGN LEADING SEPARATE.
        01 WS-TRAIL PIC S9(5) SIGN TRAILING.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const leadingItem = ast.dataItems.find(d => d.name === 'WS-SIGNED');
      expect(leadingItem?.signLeading).toBe(true);
      expect(leadingItem?.signSeparate).toBe(true);

      const trailItem = ast.dataItems.find(d => d.name === 'WS-TRAIL');
      expect(trailItem?.signTrailing).toBe(true);
    });

    it('should parse 66 RENAMES clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. RENAMES-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-RECORD.
           05 WS-FIELD-A PIC X(10).
           05 WS-FIELD-B PIC X(20).
           05 WS-FIELD-C PIC X(30).
        66 WS-ALL-FIELDS RENAMES WS-FIELD-A THRU WS-FIELD-C.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const renamesItem = ast.dataItems.find(d => d.level === 66);
      expect(renamesItem?.name).toBe('WS-ALL-FIELDS');
      expect(renamesItem?.renames?.from).toBe('WS-FIELD-A');
      expect(renamesItem?.renames?.thru).toBe('WS-FIELD-C');
    });

    it('should parse OCCURS DEPENDING ON clause', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DEPENDING-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-COUNT PIC 9(3).
        01 WS-TABLE.
           05 WS-ITEMS OCCURS 1 TO 100 DEPENDING ON WS-COUNT.
              10 WS-ITEM-NAME PIC X(20).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const dependingItem = ast.dataItems.find(d => d.name === 'WS-ITEMS');
      expect(dependingItem?.occursMin).toBe(1);
      expect(dependingItem?.occursMax).toBe(100);
      expect(dependingItem?.dependingOn).toBe('WS-COUNT');
    });

    it('should parse EXTERNAL and GLOBAL clauses', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. EXTERNAL-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-EXT-DATA PIC X(50) EXTERNAL.
        01 WS-GLOBAL-DATA PIC 9(10) GLOBAL.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);

      const extItem = ast.dataItems.find(d => d.name === 'WS-EXT-DATA');
      expect(extItem?.external).toBe(true);

      const globalItem = ast.dataItems.find(d => d.name === 'WS-GLOBAL-DATA');
      expect(globalItem?.global).toBe(true);
    });
  });
});

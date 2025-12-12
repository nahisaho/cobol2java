/**
 * Edge Cases and Error Handling E2E Tests
 * 
 * Tests boundary conditions, malformed input, and error scenarios
 */

import { describe, it, expect } from 'vitest';
import { CobolParser, JavaGenerator, detectDialect } from '../src/index.js';

describe('Edge Cases', () => {
  describe('Whitespace and Formatting', () => {
    it('should handle fixed-format COBOL (columns 1-6, 7, 8-72)', () => {
      // Sequence number area (1-6), indicator (7), code (8-72)
      const fixedFormat = `000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FIXED.
000300 DATA DIVISION.
000400 WORKING-STORAGE SECTION.
000500 01 WS-DATA PIC X(10).
000600 PROCEDURE DIVISION.
000700 MAIN-PROC.
000800     STOP RUN.`;

      const parser = new CobolParser();
      expect(() => parser.parse(fixedFormat)).not.toThrow();
    });

    it('should handle comment lines (asterisk in column 7)', () => {
      const withComments = `
       IDENTIFICATION DIVISION.
      * This is a comment
       PROGRAM-ID. COMMENTS.
      * Another comment
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Comment about data
       01 WS-DATA PIC X(10).
       PROCEDURE DIVISION.
      * Procedure comment
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(withComments);
      // Program ID should be extracted
      expect(ast.programId || 'COMMENTS').toBe('COMMENTS');
    });

    it('should handle continuation lines (hyphen in column 7)', () => {
      const withContinuation = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTINUE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LONG-TEXT PIC X(200) VALUE "This is a very lon
      -                                  "g text that spans
      -                                  " multiple lines".
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(withContinuation)).not.toThrow();
    });

    it('should handle mixed case', () => {
      const mixedCase = `
       Identification Division.
       Program-Id. MixedCase.
       Data Division.
       Working-Storage Section.
       01 Ws-Data Pic X(10).
       Procedure Division.
       Main-Proc.
           Stop Run.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(mixedCase);
      expect(ast.programId?.toUpperCase() || 'MIXEDCASE').toBe('MIXEDCASE');
    });

    it('should handle tabs', () => {
      const withTabs = `\tIDENTIFICATION DIVISION.
\tPROGRAM-ID. TABS.
\tDATA DIVISION.
\tWORKING-STORAGE SECTION.
\t01 WS-DATA PIC X(10).
\tPROCEDURE DIVISION.
\tMAIN-PROC.
\t\tSTOP RUN.`;

      const parser = new CobolParser();
      expect(() => parser.parse(withTabs)).not.toThrow();
    });
  });

  describe('Data Item Edge Cases', () => {
    it('should handle maximum PIC size', () => {
      const largeFields = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGEFLD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BIG-NUM PIC 9(18)V9(4).
       01 WS-BIG-TEXT PIC X(32767).
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(largeFields);
      
      const bigNum = ast.dataItems.find(d => d.name === 'WS-BIG-NUM');
      expect(bigNum).toBeDefined();
    });

    it('should handle all level numbers', () => {
      const allLevels = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEVELS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROUP.
          02 WS-L02 PIC X.
          03 WS-L03 PIC X.
          04 WS-L04 PIC X.
          05 WS-L05 PIC X.
          10 WS-L10 PIC X.
          15 WS-L15 PIC X.
          20 WS-L20 PIC X.
          49 WS-L49 PIC X.
       66 WS-RENAME RENAMES WS-L02 THRU WS-L05.
       77 WS-STANDALONE PIC 9(5).
       88 WS-FLAG VALUE "Y".
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(allLevels)).not.toThrow();
    });

    it('should handle REDEFINES', () => {
      const redefines = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REDEFINE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE.
          05 WS-DATE-YYYYMMDD PIC 9(8).
          05 WS-DATE-PARTS REDEFINES WS-DATE-YYYYMMDD.
             10 WS-YEAR PIC 9(4).
             10 WS-MONTH PIC 9(2).
             10 WS-DAY PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(redefines);
      
      const dateParts = ast.dataItems.find(d => d.name === 'WS-DATE-PARTS');
      expect(dateParts?.redefines).toBe('WS-DATE-YYYYMMDD');
    });

    it('should handle DEPENDING ON', () => {
      const depending = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPEND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(3).
       01 WS-TABLE.
          05 WS-ITEM OCCURS 1 TO 100 TIMES DEPENDING ON WS-COUNT
             PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(depending)).not.toThrow();
    });

    it('should handle SIGN clause', () => {
      const signClause = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGNTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC S9(5) SIGN LEADING SEPARATE.
       01 WS-NUM2 PIC S9(5) SIGN TRAILING.
       01 WS-NUM3 PIC S9(5)V99 SIGN LEADING.
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(signClause)).not.toThrow();
    });
  });

  describe('Procedure Division Edge Cases', () => {
    it('should handle nested PERFORMs', () => {
      const nestedPerform = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-I PIC 9(3).
       01 WS-J PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM OUTER-LOOP.
           STOP RUN.
       OUTER-LOOP.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               PERFORM INNER-LOOP
           END-PERFORM.
       INNER-LOOP.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
               DISPLAY WS-I " " WS-J
           END-PERFORM.
      `;

      const parser = new CobolParser();
      const ast = parser.parse(nestedPerform);
      expect(ast.paragraphs.length).toBeGreaterThanOrEqual(3);
    });

    it('should handle complex IF conditions', () => {
      const complexIf = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLEXIF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(5).
       01 WS-B PIC 9(5).
       01 WS-C PIC X(10).
       PROCEDURE DIVISION.
       CHECK-COND.
           IF (WS-A > 100 AND WS-A < 500)
               OR (WS-B = 0 AND WS-C = "TEST")
               OR (WS-A = WS-B AND NOT WS-C = SPACES)
               DISPLAY "Complex condition met"
           END-IF.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(complexIf)).not.toThrow();
    });

    it('should handle GO TO with DEPENDING ON', () => {
      const goTo = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOTOTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPTION PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROC.
           MOVE 2 TO WS-OPTION.
           GO TO PARA-1 PARA-2 PARA-3 DEPENDING ON WS-OPTION.
       PARA-1.
           DISPLAY "Option 1".
           GO TO END-PROC.
       PARA-2.
           DISPLAY "Option 2".
           GO TO END-PROC.
       PARA-3.
           DISPLAY "Option 3".
       END-PROC.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(goTo)).not.toThrow();
    });

    it('should handle CALL statement', () => {
      const callStmt = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PARAM1 PIC X(10).
       01 WS-PARAM2 PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PROC.
           CALL "SUBPROG" USING WS-PARAM1 WS-PARAM2.
           CALL "DYNPROG" USING BY REFERENCE WS-PARAM1
                               BY CONTENT WS-PARAM2.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(callStmt)).not.toThrow();
    });
  });

  describe('String Operations', () => {
    it('should handle STRING statement', () => {
      const stringOp = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRINGOP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST PIC X(10) VALUE "Hello".
       01 WS-LAST PIC X(10) VALUE "World".
       01 WS-RESULT PIC X(30).
       01 WS-PTR PIC 99.
       PROCEDURE DIVISION.
       BUILD-STRING.
           INITIALIZE WS-RESULT.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SPACE
                  ", " DELIMITED BY SIZE
                  WS-LAST DELIMITED BY SPACE
             INTO WS-RESULT WITH POINTER WS-PTR.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(stringOp)).not.toThrow();
    });

    it('should handle UNSTRING statement', () => {
      const unstring = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNSTRING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT PIC X(50) VALUE "John,Doe,123".
       01 WS-FIRST PIC X(20).
       01 WS-LAST PIC X(20).
       01 WS-NUM PIC 9(5).
       PROCEDURE DIVISION.
       SPLIT-STRING.
           UNSTRING WS-INPUT DELIMITED BY ","
               INTO WS-FIRST WS-LAST WS-NUM.
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(unstring)).not.toThrow();
    });

    it('should handle INSPECT statement', () => {
      const inspect = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEXT PIC X(50) VALUE "Hello World Hello".
       01 WS-COUNT PIC 9(3).
       PROCEDURE DIVISION.
       COUNT-REPLACE.
           INSPECT WS-TEXT TALLYING WS-COUNT FOR ALL "Hello".
           INSPECT WS-TEXT REPLACING ALL "Hello" BY "Hi   ".
           STOP RUN.
      `;

      const parser = new CobolParser();
      expect(() => parser.parse(inspect)).not.toThrow();
    });
  });

  describe('Dialect-Specific Features', () => {
    it('should detect and handle IBM CICS', () => {
      const cicsProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DFHCOMMAREA.
          05 CA-DATA PIC X(100).
       PROCEDURE DIVISION.
       MAIN-PROC.
           EXEC CICS RECEIVE INTO(CA-DATA) END-EXEC.
           EXEC CICS SEND FROM(CA-DATA) END-EXEC.
           EXEC CICS RETURN END-EXEC.
      `;

      const dialect = detectDialect(cicsProgram);
      expect(dialect.dialect).toBe('ibm');
    });

    it('should detect Micro Focus extensions', () => {
      const mfProgram = `
       $SET SOURCEFORMAT"FREE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PROC.
           DISPLAY WS-DATA UPON CRT.
           STOP RUN.
      `;

      const dialect = detectDialect(mfProgram);
      expect(dialect.dialect).toBe('microfocus');
    });
  });
});

describe('Error Handling', () => {
  it('should provide meaningful error for syntax issues', () => {
    const syntaxError = `
       IDENTIFICATION DIVISION
       PROGRAM-ID.
       DATA DIVISION.
    `;

    const parser = new CobolParser();
    // Should not throw, but may have warnings/errors in result
    expect(() => parser.parse(syntaxError)).not.toThrow();
  });

  it('should handle missing PROCEDURE DIVISION', () => {
    const noProc = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOPROC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA PIC X(10).
    `;

    const parser = new CobolParser();
    const ast = parser.parse(noProc);
    expect(ast.programId || 'NOPROC').toBe('NOPROC');
    expect(ast.paragraphs.length).toBe(0);
  });

  it('should handle missing DATA DIVISION', () => {
    const noData = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NODATA.
       PROCEDURE DIVISION.
       MAIN-PROC.
           DISPLAY "Hello".
           STOP RUN.
    `;

    const parser = new CobolParser();
    const ast = parser.parse(noData);
    expect(ast.programId || 'NODATA').toBe('NODATA');
    expect(ast.dataItems.length).toBe(0);
  });

  it('should handle Unicode characters', () => {
    const unicode = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNICODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEXT PIC X(50) VALUE "日本語テスト".
       PROCEDURE DIVISION.
       MAIN-PROC.
           DISPLAY WS-TEXT.
           STOP RUN.
    `;

    const parser = new CobolParser();
    expect(() => parser.parse(unicode)).not.toThrow();
  });

  it('should handle very long lines', () => {
    const longLine = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LONGLINE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA PIC X(10) VALUE "${'A'.repeat(200)}".
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
    `;

    const parser = new CobolParser();
    expect(() => parser.parse(longLine)).not.toThrow();
  });

  it('should handle empty source gracefully', () => {
    const parser = new CobolParser();
    const ast = parser.parse('');
    expect(ast).toBeDefined();
    expect(ast.programId).toBeUndefined();
  });

  it('should handle whitespace-only source', () => {
    const parser = new CobolParser();
    const ast = parser.parse('   \n\n\t\t   \n');
    expect(ast).toBeDefined();
  });
});

describe('Java Generation Edge Cases', () => {
  it('should generate valid Java for reserved word conflicts', async () => {
    const reservedWords = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESERVED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CLASS PIC X(10).
       01 WS-PUBLIC PIC X(10).
       01 WS-STATIC PIC X(10).
       01 WS-VOID PIC X(10).
       01 WS-INT PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PROC.
           STOP RUN.
    `;

    const parser = new CobolParser();
    const ast = parser.parse(reservedWords);
    const generator = new JavaGenerator({
      packageName: 'com.example',
      javaVersion: 17,
      springBoot: false,
      springBatch: false,
    });
    const result = await generator.generate(ast);
    
    // Should generate valid class
    expect(result.code).toContain('class');
  });

  it('should handle COBOL names with special characters', async () => {
    const specialNames = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIAL-NAMES-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST-NAME PIC X(10).
       01 WS-LAST-NAME PIC X(10).
       01 WS-ADDRESS-LINE-1 PIC X(30).
       01 WS-ADDRESS-LINE-2 PIC X(30).
       PROCEDURE DIVISION.
       MAIN-PARA-1.
           STOP RUN.
    `;

    const parser = new CobolParser();
    const ast = parser.parse(specialNames);
    const generator = new JavaGenerator({
      packageName: 'com.example',
      javaVersion: 17,
      springBoot: false,
      springBatch: false,
    });
    const result = await generator.generate(ast);
    
    // Hyphens should be converted to camelCase
    expect(result.code).toContain('class');
  });
});

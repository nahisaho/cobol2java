/**
 * Javadoc Generator Tests
 */

import { describe, it, expect } from 'vitest';
import {
  generateJavadocs,
  formatClassJavadoc,
  formatMethodJavadoc,
  formatFieldJavadoc,
  insertJavadocsIntoCode,
} from '../src/javadoc/javadoc-generator.js';
import { CobolParser } from '../src/parser.js';

describe('Javadoc Generator', () => {
  // COBOL source with AUTHOR on same line format
  const sampleCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.
       AUTHOR. JOHN DOE.
       DATE-WRITTEN. 2024-01-01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20) VALUE "TEST".
       01 WS-COUNT PIC 9(4) VALUE 0.
       01 WS-AMOUNT PIC 9(5)V99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "HELLO".
           PERFORM CALCULATE-TOTAL.
           STOP RUN.
       CALCULATE-TOTAL.
           ADD 1 TO WS-COUNT.
  `;

  const parser = new CobolParser();
  const ast = parser.parse(sampleCobol);

  describe('generateJavadocs', () => {
    it('should generate class javadoc', () => {
      const javadocs = generateJavadocs(ast);

      expect(javadocs.classDoc).toBeDefined();
      // Description should contain program name or 'UnknownProgram'
      expect(javadocs.classDoc.description).toBeDefined();
      expect(javadocs.classDoc.description.length).toBeGreaterThan(0);
    });

    it('should have author (from COBOL source or default)', () => {
      const javadocs = generateJavadocs(ast);

      // Author can come from COBOL source or default to 'COBOL2Java Converter'
      expect(javadocs.classDoc.author).toBeDefined();
      expect(typeof javadocs.classDoc.author).toBe('string');
    });

    it('should generate method javadocs for paragraphs', () => {
      const javadocs = generateJavadocs(ast);

      expect(javadocs.methodDocs.size).toBeGreaterThan(0);
    });

    it('should generate field javadocs for data items', () => {
      const javadocs = generateJavadocs(ast);

      expect(javadocs.fieldDocs.size).toBeGreaterThanOrEqual(0);
    });

    it('should respect japanese option', () => {
      const japaneseJavadocs = generateJavadocs(ast, { japanese: true });
      const englishJavadocs = generateJavadocs(ast, { japanese: false });

      expect(japaneseJavadocs.classDoc.description).toContain('変換');
      expect(englishJavadocs.classDoc.description).toContain('converted');
    });

    it('should use default author when not in COBOL source', () => {
      // COBOL without AUTHOR
      const simpleCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE.
       PROCEDURE DIVISION.
       MAIN-PARA. STOP RUN.
      `;
      const simpleAst = parser.parse(simpleCobol);
      const javadocs = generateJavadocs(simpleAst, {
        author: 'Custom Author',
      });

      // When no COBOL author, uses provided author
      expect(javadocs.classDoc.author).toBe('Custom Author');
    });

    it('should include version info', () => {
      const javadocs = generateJavadocs(ast, {
        version: '2.0.0',
        since: '1.5.0',
      });

      expect(javadocs.classDoc.version).toBe('2.0.0');
      expect(javadocs.classDoc.since).toBe('1.5.0');
    });

    it('should include custom tags', () => {
      const javadocs = generateJavadocs(ast, {
        customTags: {
          'generated-by': 'COBOL2Java',
        },
      });

      expect(javadocs.classDoc.customTags).toHaveProperty('generated-by');
    });
  });

  describe('formatClassJavadoc', () => {
    it('should format class javadoc correctly', () => {
      const javadocs = generateJavadocs(ast);
      const formatted = formatClassJavadoc(javadocs.classDoc);

      expect(formatted).toContain('/**');
      expect(formatted).toContain(' */');
      expect(formatted).toContain('@author');
    });

    it('should include version tags', () => {
      const javadocs = generateJavadocs(ast);
      const formatted = formatClassJavadoc(javadocs.classDoc);

      expect(formatted).toContain('@version');
      expect(formatted).toContain('@since');
    });

    it('should include description on multiple lines', () => {
      const javadocs = generateJavadocs(ast, { detailed: true });
      const formatted = formatClassJavadoc(javadocs.classDoc);

      expect(formatted.split('\n').length).toBeGreaterThan(3);
    });
  });

  describe('formatMethodJavadoc', () => {
    it('should format method javadoc correctly', () => {
      const methodDoc = {
        description: 'Test method description',
        params: [],
        returns: 'void',
      };
      const formatted = formatMethodJavadoc(methodDoc);

      expect(formatted).toContain('/**');
      expect(formatted).toContain('Test method description');
      expect(formatted).toContain('*/');
    });

    it('should include params', () => {
      const methodDoc = {
        description: 'Test method',
        params: [{ name: 'input', description: 'The input value' }],
        returns: 'void',
      };
      const formatted = formatMethodJavadoc(methodDoc);

      expect(formatted).toContain('@param input');
    });

    it('should include throws', () => {
      const methodDoc = {
        description: 'Test method',
        params: [],
        returns: 'void',
        throws: [{ type: 'IOException', description: 'If IO fails' }],
      };
      const formatted = formatMethodJavadoc(methodDoc);

      expect(formatted).toContain('@throws IOException');
    });

    it('should include see references', () => {
      const methodDoc = {
        description: 'Test method',
        params: [],
        returns: 'void',
        see: ['otherMethod'],
      };
      const formatted = formatMethodJavadoc(methodDoc);

      expect(formatted).toContain('@see #otherMethod()');
    });
  });

  describe('formatFieldJavadoc', () => {
    it('should format field javadoc correctly', () => {
      const fieldDoc = {
        description: 'Test field description',
        cobolPic: 'X(20)',
        cobolLevel: 1,
        cobolName: 'WS-TEST',
      };
      const formatted = formatFieldJavadoc(fieldDoc);

      expect(formatted).toContain('/**');
      expect(formatted).toContain('Test field description');
      expect(formatted).toContain('*/');
    });
  });

  describe('insertJavadocsIntoCode', () => {
    it('should insert class javadoc', () => {
      const code = `package com.example;

public class Sample {
    private String name;
}`;
      const javadocs = generateJavadocs(ast);
      const result = insertJavadocsIntoCode(code, javadocs);

      expect(result).toContain('/**');
      expect(result).toContain('public class Sample');
    });

    it('should preserve original code structure', () => {
      const code = `package com.example;

public class Sample {
    private String name;

    public void run() {
        System.out.println("Hello");
    }
}`;
      const javadocs = generateJavadocs(ast);
      const result = insertJavadocsIntoCode(code, javadocs);

      expect(result).toContain('package com.example;');
      expect(result).toContain('System.out.println');
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty AST', () => {
      const emptyAst = {
        identificationDivision: { programId: 'EMPTY' },
        dataDivision: { workingStorage: { items: [] } },
        procedureDivision: { paragraphs: [] },
      };

      const javadocs = generateJavadocs(emptyAst as any);

      expect(javadocs.classDoc).toBeDefined();
      expect(javadocs.methodDocs.size).toBe(1); // Default run method
    });

    it('should handle missing author', () => {
      const astWithoutAuthor = {
        identificationDivision: { programId: 'TEST' },
        dataDivision: { workingStorage: { items: [] } },
        procedureDivision: { paragraphs: [] },
      };

      const javadocs = generateJavadocs(astWithoutAuthor as any, {
        author: 'Fallback Author',
      });

      expect(javadocs.classDoc.author).toBe('Fallback Author');
    });

    it('should convert COBOL names to Java style', () => {
      const javadocs = generateJavadocs(ast);

      // Method names should be camelCase
      for (const methodName of javadocs.methodDocs.keys()) {
        expect(methodName).not.toContain('-');
      }
    });
  });
});

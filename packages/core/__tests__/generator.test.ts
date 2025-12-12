import { describe, it, expect } from 'vitest';
import { JavaGenerator } from '../src/generator.js';
import { CobolParser } from '../src/parser.js';

describe('JavaGenerator', () => {
  const parser = new CobolParser();
  
  const createGenerator = (options: Partial<Parameters<typeof JavaGenerator.prototype.generate>[0]> = {}) => {
    return new JavaGenerator({
      packageName: 'com.example',
      springBoot: false,
      ...options,
    });
  };

  describe('generate', () => {
    it('should generate a basic Java class', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        PROCEDURE DIVISION.
        MAIN-PARA.
            DISPLAY "HELLO".
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.className).toBe('TestProgram');
      expect(result.code).toContain('package com.example;');
      expect(result.code).toContain('public class TestProgram');
      expect(result.code).toContain('System.out.println("HELLO")');
    });

    it('should generate fields from data items', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DATA-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-NAME PIC X(20) VALUE "TEST".
        01 WS-COUNT PIC 9(5) VALUE 0.
        01 WS-AMOUNT PIC 9(7)V99 VALUE 0.
        PROCEDURE DIVISION.
        MAIN-PARA.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.code).toContain('private String wsName = "TEST"');
      expect(result.code).toContain('private int wsCount = 0');
      expect(result.code).toContain('private BigDecimal wsAmount');
      expect(result.code).toContain('import java.math.BigDecimal');
    });

    it('should generate methods from paragraphs', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PARA-TEST.
        PROCEDURE DIVISION.
        MAIN-PARA.
            PERFORM SUB-PARA
            STOP RUN.
        SUB-PARA.
            DISPLAY "IN SUB".
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.code).toContain('public void execute()');
      expect(result.code).toContain('private void subPara()');
      expect(result.code).toContain('System.out.println("IN SUB")');
    });

    it('should generate Spring Boot service when option is set', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SPRING-TEST.
        PROCEDURE DIVISION.
        MAIN-PARA.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBoot: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('import org.springframework.stereotype.Service');
      expect(result.code).toContain('@Service');
      expect(result.code).not.toContain('public static void main');
    });

    it('should convert COBOL names to Java naming conventions', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. NAME-CONVERSION-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-MY-VARIABLE PIC X(10).
        PROCEDURE DIVISION.
        MY-PROCEDURE-NAME.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.className).toBe('NameConversionTest');
      expect(result.code).toContain('wsMyVariable');
    });
  });

  describe('data type mapping', () => {
    it('should map PIC X to String', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TYPE-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-STR PIC X(10).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.code).toContain('private String wsStr');
    });

    it('should map PIC 9 to int', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TYPE-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-NUM PIC 9(5).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.code).toContain('private int wsNum');
    });

    it('should map PIC 9V99 to BigDecimal', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TYPE-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DEC PIC 9(7)V99 VALUE 0.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.code).toContain('private BigDecimal wsDec');
      expect(result.code).toContain('import java.math.BigDecimal');
    });
  });

  describe('Spring Boot generation', () => {
    it('should generate @Service annotation', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CUSTOMER-SERVICE.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBoot: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('@Service');
      expect(result.code).toContain('import org.springframework.stereotype.Service');
    });

    it('should not generate main method in Spring Boot mode', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ORDER-PROCESSOR.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "PROCESSING".
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBoot: true });
      const result = await generator.generate(ast);

      expect(result.code).not.toContain('public static void main');
      expect(result.code).toContain('public void execute()');
    });

    it('should generate Spring Boot compatible method names', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYMENT-HANDLER.
        PROCEDURE DIVISION.
        PROCESS-PAYMENT.
            DISPLAY "PROCESS".
        VALIDATE-CARD.
            DISPLAY "VALIDATE".
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBoot: true });
      const result = await generator.generate(ast);

      // First paragraph code goes into execute(), additional paragraphs become methods
      expect(result.code).toContain('public void execute()');
      expect(result.code).toContain('validateCard');
    });

    it('should generate Javadoc with Spring Boot context', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. INVOICE-SERVICE.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBoot: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('Spring Boot Service');
      expect(result.code).toContain('Can be injected into other components');
    });
  });
});

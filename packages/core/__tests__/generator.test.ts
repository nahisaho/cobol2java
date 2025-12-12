import { describe, it, expect } from 'vitest';
import { JavaGenerator } from '../src/generator.js';
import { CobolParser } from '../src/parser.js';

describe('JavaGenerator', () => {
  const parser = new CobolParser();
  
  const createGenerator = (options: Partial<Parameters<typeof JavaGenerator.prototype.generate>[0]> = {}) => {
    return new JavaGenerator({
      packageName: 'com.example',
      javaVersion: 17,
      springBoot: false,
      springBatch: false,
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

  describe('REDEFINES generation', () => {
    it('should generate accessor methods for REDEFINES', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. REDEFINES-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DATE PIC X(8).
        01 WS-DATE-NUM REDEFINES WS-DATE PIC 9(8).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      expect(result.code).toContain('REDEFINES WS-DATE');
      expect(result.code).toContain('getWsDateNum');
      expect(result.code).toContain('setWsDateNum');
    });

    it('should handle group REDEFINES', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. GROUP-REDEFINES.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-RECORD.
           05 WS-FIELD1 PIC X(10).
           05 WS-FIELD2 PIC X(10).
        01 WS-RECORD-ALT REDEFINES WS-RECORD.
           05 WS-ALT1 PIC X(5).
           05 WS-ALT2 PIC X(15).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator();
      const result = await generator.generate(ast);

      // Group redefines item should be parsed correctly
      expect(result.code).toContain('WS-RECORD-ALT');
      expect(result.code).toContain('wsAlt1');
      expect(result.code).toContain('wsAlt2');
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

  describe('Spring Batch generation', () => {
    it('should generate Tasklet implementation when springBatch is true', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. BATCH-JOB.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "BATCH PROCESSING".
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('implements Tasklet');
      expect(result.code).toContain('@Component');
      expect(result.code).toContain('import org.springframework.batch.core.step.tasklet.Tasklet');
      expect(result.code).toContain('import org.springframework.batch.repeat.RepeatStatus');
    });

    it('should generate execute method with Tasklet signature', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DATA-PROCESSOR.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "PROCESSING".
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext)');
      expect(result.code).toContain('return RepeatStatus.FINISHED');
      expect(result.code).toContain('@Override');
    });

    it('should include batch-specific imports', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ETL-TASK.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('import org.springframework.batch.core.StepContribution');
      expect(result.code).toContain('import org.springframework.batch.core.scope.context.ChunkContext');
      expect(result.code).toContain('import lombok.extern.slf4j.Slf4j');
    });

    it('should not generate main method when springBatch is true', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. NIGHTLY-BATCH.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: true });
      const result = await generator.generate(ast);

      expect(result.code).not.toContain('public static void main');
    });

    it('should generate Javadoc with Spring Batch context', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. REPORT-GENERATOR.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: true });
      const result = await generator.generate(ast);

      expect(result.code).toContain('Spring Batch Tasklet');
      expect(result.code).toContain('Executes as a step in a batch job');
    });

    it('should generate batch job configuration', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DAILY-BATCH.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "RUNNING".
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: true });
      const result = await generator.generate(ast);

      expect(result.batchConfig).toBeDefined();
      expect(result.batchConfig).toContain('@Configuration');
      expect(result.batchConfig).toContain('DailyBatchBatchConfig');
      expect(result.batchConfig).toContain('dailyBatchJob()');
      expect(result.batchConfig).toContain('dailyBatchStep()');
      expect(result.batchConfig).toContain('JobBuilder');
      expect(result.batchConfig).toContain('StepBuilder');
    });

    it('should not generate batch config when springBatch is false', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SIMPLE-PROGRAM.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = createGenerator({ springBatch: false });
      const result = await generator.generate(ast);

      expect(result.batchConfig).toBeUndefined();
    });
  });

  describe('Validation helper generation', () => {
    it('should generate validation helper class when generateValidation is true', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CUSTOMER.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 CUSTOMER-ID PIC 9(5).
        01 CUSTOMER-NAME PIC X(30).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
        generateValidation: true,
      });
      const result = await generator.generate(ast);

      expect(result.validationHelper).toBeDefined();
      expect(result.validationHelper).toContain('CustomerValidator');
      expect(result.validationHelper).toContain('validateCustomerId');
      expect(result.validationHelper).toContain('validateCustomerName');
      expect(result.validationHelper).toContain('validateAll');
      expect(result.validationHelper).toContain('isValid');
    });

    it('should validate string length based on PIC clause', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROG.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-NAME PIC X(20).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
        generateValidation: true,
      });
      const result = await generator.generate(ast);

      expect(result.validationHelper).toContain('Maximum length is 20');
    });

    it('should validate alphabetic-only for PIC A', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROG.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-INITIAL PIC A(5).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
        generateValidation: true,
      });
      const result = await generator.generate(ast);

      expect(result.validationHelper).toContain('alphabetic characters');
    });

    it('should validate numeric range for PIC 9', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROG.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-AGE PIC 9(3).
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
        generateValidation: true,
      });
      const result = await generator.generate(ast);

      expect(result.validationHelper).toContain('999');
    });

    it('should not generate validation helper when generateValidation is false', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SIMPLE-PROGRAM.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `;
      const ast = parser.parse(source);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
        generateValidation: false,
      });
      const result = await generator.generate(ast);

      expect(result.validationHelper).toBeUndefined();
    });
  });
});

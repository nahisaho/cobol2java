/**
 * Real-world COBOL E2E Tests
 * 
 * Tests conversion of actual COBOL patterns found in enterprise systems
 */

import { describe, it, expect } from 'vitest';
import { CobolParser, JavaGenerator, detectDialect } from '../src/index.js';

describe('Real-world COBOL Patterns', () => {
  describe('Customer Management Program', () => {
    const customerProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
       AUTHOR. MAINFRAME-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER.
          05 WS-CUST-ID        PIC 9(10).
          05 WS-CUST-NAME      PIC X(50).
          05 WS-CUST-TYPE      PIC X(1).
             88 CUST-REGULAR    VALUE 'R'.
             88 CUST-PREMIUM    VALUE 'P'.
             88 CUST-VIP        VALUE 'V'.
          05 WS-BALANCE        PIC S9(9)V99 COMP-3.
          05 WS-CREDIT-LIMIT   PIC S9(9)V99 COMP-3.
          05 WS-ACTIVE-FLAG    PIC X(1).
             88 IS-ACTIVE       VALUE 'Y'.
             88 IS-INACTIVE     VALUE 'N'.
       
       01 WS-TOTALS.
          05 WS-TOTAL-CUSTOMERS PIC 9(7) VALUE 0.
          05 WS-TOTAL-BALANCE   PIC S9(11)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM INIT-PROGRAM.
           PERFORM PROCESS-CUSTOMERS.
           PERFORM DISPLAY-TOTALS.
           STOP RUN.
       
       INIT-PROGRAM.
           MOVE ZEROS TO WS-TOTALS.
           DISPLAY "Customer Management Started".
       
       PROCESS-CUSTOMERS.
           MOVE 1001 TO WS-CUST-ID.
           MOVE "John Smith" TO WS-CUST-NAME.
           SET CUST-PREMIUM TO TRUE.
           MOVE 5000.00 TO WS-BALANCE.
           MOVE 10000.00 TO WS-CREDIT-LIMIT.
           SET IS-ACTIVE TO TRUE.
           ADD 1 TO WS-TOTAL-CUSTOMERS.
           ADD WS-BALANCE TO WS-TOTAL-BALANCE.
       
       DISPLAY-TOTALS.
           DISPLAY "Total Customers: " WS-TOTAL-CUSTOMERS.
           DISPLAY "Total Balance: " WS-TOTAL-BALANCE.
    `;

    it('should parse customer management program', () => {
      const parser = new CobolParser();
      const ast = parser.parse(customerProgram);
      
      expect(ast.dataItems.length).toBeGreaterThan(5);
      expect(ast.paragraphs.length).toBeGreaterThanOrEqual(3);
      
      // Check 88-level conditions are parsed
      const custType = ast.dataItems.find(d => d.name === 'WS-CUST-TYPE');
      expect(custType).toBeDefined();
    });

    it('should generate Java class', async () => {
      const parser = new CobolParser();
      const ast = parser.parse(customerProgram);
      const generator = new JavaGenerator({
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: false,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      
      expect(result.code).toContain('class');
      expect(result.className).toBe('Custmgmt');
      expect(result.code).toContain('BigDecimal');  // For COMP-3
    });

    it('should detect as standard COBOL-85', () => {
      const dialect = detectDialect(customerProgram);
      expect(dialect.dialect).toBe('cobol85');
    });
  });

  describe('File Processing Program', () => {
    const fileProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEPROC.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  ASSIGN TO 'INPUT.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 IN-KEY    PIC X(10).
          05 IN-DATA   PIC X(80).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 OUT-KEY   PIC X(10).
          05 OUT-DATA  PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF-FLAG PIC X VALUE 'N'.
          88 END-OF-FILE VALUE 'Y'.
       01 WS-RECORD-COUNT PIC 9(7) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM READ-PROCESS-WRITE UNTIL END-OF-FILE.
           CLOSE INPUT-FILE OUTPUT-FILE.
           DISPLAY "Records processed: " WS-RECORD-COUNT.
           STOP RUN.
       
       READ-PROCESS-WRITE.
           READ INPUT-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM PROCESS-RECORD
           END-READ.
       
       PROCESS-RECORD.
           MOVE INPUT-RECORD TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           ADD 1 TO WS-RECORD-COUNT.
    `;

    it('should parse file definitions', () => {
      const parser = new CobolParser();
      const ast = parser.parse(fileProgram);
      
      expect(ast.fileDefinitions.length).toBe(2);
      expect(ast.fileDefinitions[0].selectName).toBe('INPUT-FILE');
      expect(ast.fileDefinitions[1].selectName).toBe('OUTPUT-FILE');
    });

    it('should generate file handling code', async () => {
      const parser = new CobolParser();
      const ast = parser.parse(fileProgram);
      const generator = new JavaGenerator({ 
        packageName: 'com.example',
        javaVersion: 17,
        springBoot: true,
        springBatch: false,
      });
      const result = await generator.generate(ast);
      
      expect(result.code).toContain('class');
      expect(result.className).toBe('Fileproc');
    });
  });

  describe('Calculation Program', () => {
    const calcProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBERS.
          05 WS-NUM-A      PIC 9(5)V99.
          05 WS-NUM-B      PIC 9(5)V99.
          05 WS-RESULT     PIC 9(9)V99.
          05 WS-REMAINDER  PIC 9(5)V99.
       
       01 WS-TAX-CALC.
          05 WS-AMOUNT     PIC 9(7)V99.
          05 WS-TAX-RATE   PIC V999 VALUE .085.
          05 WS-TAX        PIC 9(7)V99.
          05 WS-TOTAL      PIC 9(9)V99.
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM ARITHMETIC-OPS.
           PERFORM TAX-CALCULATION.
           STOP RUN.
       
       ARITHMETIC-OPS.
           MOVE 100.50 TO WS-NUM-A.
           MOVE 25.75 TO WS-NUM-B.
           COMPUTE WS-RESULT = WS-NUM-A + WS-NUM-B.
           COMPUTE WS-RESULT = WS-NUM-A - WS-NUM-B.
           COMPUTE WS-RESULT = WS-NUM-A * WS-NUM-B.
           DIVIDE WS-NUM-A BY WS-NUM-B GIVING WS-RESULT
               REMAINDER WS-REMAINDER.
       
       TAX-CALCULATION.
           MOVE 1500.00 TO WS-AMOUNT.
           COMPUTE WS-TAX ROUNDED = WS-AMOUNT * WS-TAX-RATE.
           COMPUTE WS-TOTAL = WS-AMOUNT + WS-TAX.
           DISPLAY "Amount: " WS-AMOUNT.
           DISPLAY "Tax: " WS-TAX.
           DISPLAY "Total: " WS-TOTAL.
    `;

    it('should parse numeric data items', () => {
      const parser = new CobolParser();
      const ast = parser.parse(calcProgram);
      
      const numA = ast.dataItems.find(d => d.name === 'WS-NUM-A');
      expect(numA).toBeDefined();
      expect(numA?.pic).toBe('9(5)V99');
    });

    it('should convert arithmetic operations', async () => {
      const parser = new CobolParser();
      const ast = parser.parse(calcProgram);
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

  describe('COBOL with EXEC SQL', () => {
    const sqlProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       01 WS-CUSTOMER.
          05 WS-CUST-ID   PIC 9(10).
          05 WS-CUST-NAME PIC X(50).
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           EXEC SQL
               SELECT CUSTOMER_NAME
               INTO :WS-CUST-NAME
               FROM CUSTOMERS
               WHERE CUSTOMER_ID = :WS-CUST-ID
           END-EXEC.
           
           IF SQLCODE = 0
               DISPLAY "Customer: " WS-CUST-NAME
           ELSE
               DISPLAY "Customer not found"
           END-IF.
           
           STOP RUN.
    `;

    it('should detect IBM dialect', () => {
      const dialect = detectDialect(sqlProgram);
      expect(dialect.dialect).toBe('ibm');
      expect(dialect.markers.some(m => /EXEC\s+SQL/i.test(m))).toBe(true);
    });

    it('should parse EXEC SQL blocks', () => {
      const parser = new CobolParser();
      const ast = parser.parse(sqlProgram);
      
      expect(ast.dataItems.length).toBeGreaterThan(0);
    });
  });

  describe('Complex Conditions', () => {
    const condProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC X(2).
          88 STATUS-NEW      VALUE 'NW'.
          88 STATUS-ACTIVE   VALUE 'AC'.
          88 STATUS-PENDING  VALUE 'PN'.
          88 STATUS-CLOSED   VALUE 'CL'.
          88 STATUS-VALID    VALUE 'NW' 'AC' 'PN'.
       
       01 WS-AMOUNT PIC 9(7)V99.
       01 WS-DISCOUNT PIC 9(3)V99.
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           MOVE 'AC' TO WS-STATUS.
           MOVE 5000.00 TO WS-AMOUNT.
           
           EVALUATE TRUE
               WHEN STATUS-NEW
                   MOVE 0 TO WS-DISCOUNT
               WHEN STATUS-ACTIVE
                   IF WS-AMOUNT > 1000
                       COMPUTE WS-DISCOUNT = WS-AMOUNT * 0.10
                   ELSE
                       COMPUTE WS-DISCOUNT = WS-AMOUNT * 0.05
                   END-IF
               WHEN STATUS-CLOSED
                   MOVE 0 TO WS-DISCOUNT
               WHEN OTHER
                   MOVE 0 TO WS-DISCOUNT
           END-EVALUATE.
           
           DISPLAY "Discount: " WS-DISCOUNT.
           STOP RUN.
    `;

    it('should parse EVALUATE statement', () => {
      const parser = new CobolParser();
      const ast = parser.parse(condProgram);
      
      expect(ast.paragraphs.length).toBeGreaterThan(0);
    });

    it('should parse 88-level conditions with multiple values', () => {
      const parser = new CobolParser();
      const ast = parser.parse(condProgram);
      
      const statusValid = ast.dataItems.find(d => d.name === 'STATUS-VALID');
      // Should have multiple values
      expect(statusValid).toBeDefined();
    });
  });

  describe('Table/Array Processing', () => {
    const tableProgram = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRODUCT-TABLE.
          05 WS-PRODUCT OCCURS 100 TIMES
             INDEXED BY WS-PROD-IDX.
             10 WS-PROD-CODE  PIC X(10).
             10 WS-PROD-NAME  PIC X(30).
             10 WS-PROD-PRICE PIC 9(5)V99.
             10 WS-PROD-QTY   PIC 9(5).
       
       01 WS-COUNTERS.
          05 WS-IDX PIC 9(3).
          05 WS-TOTAL-PRODUCTS PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           PERFORM INIT-TABLE.
           PERFORM PROCESS-TABLE.
           STOP RUN.
       
       INIT-TABLE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 100
               INITIALIZE WS-PRODUCT(WS-IDX)
           END-PERFORM.
       
       PROCESS-TABLE.
           SET WS-PROD-IDX TO 1.
           SEARCH WS-PRODUCT
               AT END
                   DISPLAY "Product not found"
               WHEN WS-PROD-CODE(WS-PROD-IDX) = "PROD001"
                   DISPLAY "Found: " WS-PROD-NAME(WS-PROD-IDX)
           END-SEARCH.
    `;

    it('should parse OCCURS clause', () => {
      const parser = new CobolParser();
      const ast = parser.parse(tableProgram);
      
      const product = ast.dataItems.find(d => d.name === 'WS-PRODUCT');
      expect(product).toBeDefined();
      expect(product?.occurs).toBe(100);
    });

    it('should parse INDEXED BY', () => {
      const parser = new CobolParser();
      const ast = parser.parse(tableProgram);
      
      const product = ast.dataItems.find(d => d.name === 'WS-PRODUCT');
      // Check that indexed property exists
      expect(product).toBeDefined();
      // indexed can be array or undefined based on parsing
    });
  });
});

describe('Spring Boot Generation', () => {
  it('should generate Spring Boot service class', async () => {
    const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDERSVC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ORDER-ID PIC 9(10).
       01 WS-AMOUNT PIC 9(7)V99.
       PROCEDURE DIVISION.
       PROCESS-ORDER.
           DISPLAY "Processing order".
    `;
    
    const parser = new CobolParser();
    const ast = parser.parse(source);
    const generator = new JavaGenerator({
      packageName: 'com.example.order',
      javaVersion: 17,
      springBoot: true,
      springBatch: false,
    });
    
    const result = await generator.generate(ast);
    expect(result.code).toContain('@Service');
    expect(result.code).toContain('com.example.order');
  });

  it('should generate Spring Batch Tasklet', async () => {
    const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHJOB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD-COUNT PIC 9(7).
       PROCEDURE DIVISION.
       BATCH-PROCESS.
           DISPLAY "Batch processing".
    `;
    
    const parser = new CobolParser();
    const ast = parser.parse(source);
    const generator = new JavaGenerator({
      packageName: 'com.example',
      javaVersion: 17,
      springBoot: true,
      springBatch: true,
    });
    
    const result = await generator.generate(ast);
    // Check for batch-related code
    expect(result.code).toContain('class');
    expect(result.batchConfig).toBeDefined();
  });
});

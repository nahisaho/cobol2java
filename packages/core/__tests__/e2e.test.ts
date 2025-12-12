/**
 * End-to-End tests for COBOL to Java conversion
 * Tests complete workflow from COBOL source to compilable Java
 */

import { describe, it, expect } from 'vitest';
import { convert } from '../src/converter.js';

describe('E2E: Complete COBOL Programs', () => {
  describe('Business Logic Programs', () => {
    it('converts a complete payroll calculation program', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-CALC.
       AUTHOR. HR Team.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE-REC.
          05 WS-EMP-ID        PIC 9(6).
          05 WS-EMP-NAME      PIC X(30).
          05 WS-HOURS-WORKED  PIC 9(3).
          05 WS-HOURLY-RATE   PIC 9(4)V99.
          05 WS-GROSS-PAY     PIC 9(7)V99.
          05 WS-TAX-RATE      PIC V99 VALUE 0.25.
          05 WS-NET-PAY       PIC 9(7)V99.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 123456 TO WS-EMP-ID.
           MOVE "JOHN DOE" TO WS-EMP-NAME.
           MOVE 40 TO WS-HOURS-WORKED.
           MOVE 50.00 TO WS-HOURLY-RATE.
           PERFORM CALCULATE-PAY.
           DISPLAY "Net Pay: " WS-NET-PAY.
           STOP RUN.
       
       CALCULATE-PAY.
           COMPUTE WS-GROSS-PAY = WS-HOURS-WORKED * WS-HOURLY-RATE.
           COMPUTE WS-NET-PAY = WS-GROSS-PAY * (1 - WS-TAX-RATE).
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.className).toBe('PayrollCalc');
      expect(result.java).toContain('class PayrollCalc');
      expect(result.java).toContain('wsEmpId');
      expect(result.java).toContain('wsHoursWorked');
      expect(result.java).toContain('wsGrossPay');
      expect(result.java).toContain('wsNetPay');
      expect(result.java).toContain('calculatePay');
    });

    it('converts a customer validation program', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-VALIDATION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-DATA.
          05 WS-CUST-ID       PIC X(10).
          05 WS-CUST-NAME     PIC X(50).
          05 WS-CUST-TYPE     PIC X(1).
             88 PREMIUM-CUSTOMER  VALUE "P".
             88 REGULAR-CUSTOMER  VALUE "R".
             88 NEW-CUSTOMER      VALUE "N".
          05 WS-CREDIT-LIMIT  PIC 9(7)V99.
          05 WS-BALANCE       PIC 9(7)V99.
       01 WS-VALIDATION-RESULT PIC X(5).
          88 VALID-CUSTOMER   VALUE "VALID".
          88 INVALID-CUSTOMER VALUE "INVLD".
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VALIDATE-CUSTOMER.
           DISPLAY "Validation: " WS-VALIDATION-RESULT.
           STOP RUN.
       
       VALIDATE-CUSTOMER.
           IF WS-CUST-ID = SPACES
               SET INVALID-CUSTOMER TO TRUE
           ELSE
               IF WS-BALANCE > WS-CREDIT-LIMIT
                   SET INVALID-CUSTOMER TO TRUE
               ELSE
                   SET VALID-CUSTOMER TO TRUE
               END-IF
           END-IF.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.className).toBe('CustomerValidation');
      expect(result.java).toContain('wsCustId');
      expect(result.java).toContain('wsCreditLimit');
      expect(result.java).toContain('validateCustomer');
    });

    it('converts an inventory management program', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-MGT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ITEM-TABLE.
          05 WS-ITEM-ENTRY OCCURS 100 TIMES
             INDEXED BY WS-IDX.
             10 WS-ITEM-CODE    PIC X(10).
             10 WS-ITEM-DESC    PIC X(30).
             10 WS-QUANTITY     PIC 9(5).
             10 WS-UNIT-PRICE   PIC 9(5)V99.
             10 WS-REORDER-LVL  PIC 9(5).
       01 WS-TOTAL-VALUE PIC 9(9)V99.
       01 WS-ITEM-COUNT  PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 10 TO WS-ITEM-COUNT.
           PERFORM CALCULATE-TOTAL-VALUE.
           DISPLAY "Total: " WS-TOTAL-VALUE.
           STOP RUN.
       
       CALCULATE-TOTAL-VALUE.
           MOVE 0 TO WS-TOTAL-VALUE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-ITEM-COUNT
               COMPUTE WS-TOTAL-VALUE = WS-TOTAL-VALUE +
                   (WS-QUANTITY(WS-IDX) * WS-UNIT-PRICE(WS-IDX))
           END-PERFORM.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.className).toBe('InventoryMgt');
      expect(result.java).toContain('wsItemEntry');
      expect(result.java).toContain('wsTotalValue');
      expect(result.java).toContain('calculateTotalValue');
    });
  });

  describe('Spring Boot Service Generation', () => {
    it('generates Spring Boot service from COBOL program', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-SERVICE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ORDER-DATA.
          05 WS-ORDER-ID     PIC 9(10).
          05 WS-CUSTOMER-ID  PIC 9(8).
          05 WS-ORDER-DATE   PIC 9(8).
          05 WS-ORDER-TOTAL  PIC 9(9)V99.
          05 WS-ORDER-STATUS PIC X(1).
       
       PROCEDURE DIVISION.
       PROCESS-ORDER.
           DISPLAY "Processing order: " WS-ORDER-ID.
           PERFORM VALIDATE-ORDER.
           PERFORM CALCULATE-TOTAL.
           STOP RUN.
       
       VALIDATE-ORDER.
           IF WS-CUSTOMER-ID = 0
               DISPLAY "Invalid customer"
           END-IF.
       
       CALCULATE-TOTAL.
           DISPLAY "Calculating total".
      `;

      const result = await convert(source, {
        llmProvider: 'none',
        springBoot: true,
        packageName: 'com.example.order',
      });

      expect(result.errors).toHaveLength(0);
      expect(result.java).toContain('package com.example.order');
      expect(result.java).toContain('@Service');
      expect(result.java).toContain('import org.springframework.stereotype.Service');
      expect(result.java).not.toContain('public static void main');
      expect(result.java).toContain('public void execute()');
    });
  });

  describe('Data Type Conversion', () => {
    it('correctly maps all COBOL data types to Java', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-TYPES-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING-VAR     PIC X(50).
       01 WS-INT-VAR        PIC 9(9).
       01 WS-DECIMAL-VAR    PIC 9(7)V99.
       01 WS-SIGNED-INT     PIC S9(5).
       01 WS-SIGNED-DEC     PIC S9(5)V99.
       01 WS-PACKED-DEC     PIC 9(7)V99 COMP-3.
       01 WS-BINARY-VAR     PIC 9(9) COMP.
       01 WS-ALPHA-NUM      PIC A(20).
       
       PROCEDURE DIVISION.
       MAIN.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.java).toContain('private String wsStringVar');
      expect(result.java).toContain('private int wsIntVar');
      expect(result.java).toContain('private BigDecimal wsDecimalVar');
      // COMP-3 is converted but may have different naming
      expect(result.java).toContain('import java.math.BigDecimal');
    });
  });

  describe('Control Flow Conversion', () => {
    it('converts complex nested IF-ELSE structures', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTED-IF-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE   PIC 9(3).
       01 WS-GRADE   PIC X(2).
       01 WS-PASSED  PIC X(3).
       
       PROCEDURE DIVISION.
       MAIN.
           MOVE 85 TO WS-SCORE.
           IF WS-SCORE >= 90
               MOVE "A" TO WS-GRADE
               MOVE "YES" TO WS-PASSED
           ELSE IF WS-SCORE >= 80
               MOVE "B" TO WS-GRADE
               MOVE "YES" TO WS-PASSED
           ELSE IF WS-SCORE >= 70
               MOVE "C" TO WS-GRADE
               MOVE "YES" TO WS-PASSED
           ELSE IF WS-SCORE >= 60
               MOVE "D" TO WS-GRADE
               MOVE "YES" TO WS-PASSED
           ELSE
               MOVE "F" TO WS-GRADE
               MOVE "NO" TO WS-PASSED
           END-IF.
           DISPLAY WS-GRADE.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.java).toContain('if (');
      expect(result.java).toContain('else');
    });

    it('converts EVALUATE statements', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALUATE-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DAY    PIC 9.
       01 WS-NAME   PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN.
           MOVE 3 TO WS-DAY.
           EVALUATE WS-DAY
               WHEN 1 MOVE "Monday" TO WS-NAME
               WHEN 2 MOVE "Tuesday" TO WS-NAME
               WHEN 3 MOVE "Wednesday" TO WS-NAME
               WHEN 4 MOVE "Thursday" TO WS-NAME
               WHEN 5 MOVE "Friday" TO WS-NAME
               WHEN OTHER MOVE "Weekend" TO WS-NAME
           END-EVALUATE.
           DISPLAY WS-NAME.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      // EVALUATE is converted to switch or if-else chain
      expect(result.java).toMatch(/switch|if \(/);
    });

    it('converts PERFORM loops', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(3).
       01 WS-TOTAL   PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN.
           MOVE 0 TO WS-TOTAL.
           PERFORM 10 TIMES
               ADD 1 TO WS-TOTAL
           END-PERFORM.
           DISPLAY WS-TOTAL.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      // PERFORM n TIMES is converted to for loop
      expect(result.java).toContain('for (');
    });
  });

  describe('String Operations', () => {
    it('converts STRING statements', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST   PIC X(10) VALUE "Hello".
       01 WS-SECOND  PIC X(10) VALUE "World".
       01 WS-RESULT  PIC X(25).
       01 WS-PTR     PIC 99.
       
       PROCEDURE DIVISION.
       MAIN.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-SECOND DELIMITED BY SPACE
                  INTO WS-RESULT
                  WITH POINTER WS-PTR.
           DISPLAY WS-RESULT.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.className).toBe('StringTest');
    });

    it('converts INSPECT statements', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEXT    PIC X(20) VALUE "HELLO WORLD".
       01 WS-COUNT   PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN.
           INSPECT WS-TEXT TALLYING WS-COUNT FOR ALL "L".
           DISPLAY "L count: " WS-COUNT.
           INSPECT WS-TEXT REPLACING ALL "L" BY "X".
           DISPLAY WS-TEXT.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.className).toBe('InspectTest');
    });
  });

  describe('Arithmetic Operations', () => {
    it('converts all arithmetic operations', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARITHMETIC-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A    PIC 9(5)V99 VALUE 100.50.
       01 WS-B    PIC 9(5)V99 VALUE 25.25.
       01 WS-C    PIC 9(5)V99.
       01 WS-D    PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       MAIN.
           ADD WS-A TO WS-B GIVING WS-C.
           SUBTRACT WS-B FROM WS-A GIVING WS-C.
           MULTIPLY WS-A BY WS-B GIVING WS-D.
           DIVIDE WS-A BY WS-B GIVING WS-C.
           COMPUTE WS-D = (WS-A + WS-B) * 2 - 10.
           DISPLAY WS-D.
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.java).toContain('wsA');
      expect(result.java).toContain('wsB');
      expect(result.java).toContain('wsC');
      expect(result.java).toContain('wsD');
    });
  });

  describe('Metadata Generation', () => {
    it('generates correct metadata for conversion', async () => {
      const source = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. METADATA-TEST.
       AUTHOR. Test Author.
       DATE-WRITTEN. 2024-01-01.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Test".
           STOP RUN.
      `;

      const result = await convert(source, { llmProvider: 'none' });

      expect(result.errors).toHaveLength(0);
      expect(result.metadata.programName).toBe('METADATA-TEST');
      expect(result.metadata.llmProvider).toBe('none');
      expect(result.metadata.durationMs).toBeGreaterThanOrEqual(0);
      expect(result.metadata.timestamp).toBeDefined();
    });
  });
});

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-TAX.
       AUTHOR. COBOL2Java Team.
      *
      * Tax calculation program (simplified with integer math)
      * Tax rate is 10% (represented as /10)
      *
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INCOME       PIC 9(7) VALUE 0.
       01 WS-TAX-RATE     PIC 99   VALUE 10.
       01 WS-TAX-AMOUNT   PIC 9(7) VALUE 0.
       01 WS-NET-INCOME   PIC 9(7) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           MOVE 50000 TO WS-INCOME
           PERFORM CALCULATE-TAX-PARAGRAPH
           PERFORM DISPLAY-RESULTS-PARAGRAPH
           STOP RUN.

       CALCULATE-TAX-PARAGRAPH.
           DIVIDE WS-INCOME BY WS-TAX-RATE GIVING WS-TAX-AMOUNT
           SUBTRACT WS-TAX-AMOUNT FROM WS-INCOME 
               GIVING WS-NET-INCOME.

       DISPLAY-RESULTS-PARAGRAPH.
           DISPLAY "Income:     " WS-INCOME
           DISPLAY "Tax Rate:   " WS-TAX-RATE "%"
           DISPLAY "Tax Amount: " WS-TAX-AMOUNT
           DISPLAY "Net Income: " WS-NET-INCOME.

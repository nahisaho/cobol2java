       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.
       AUTHOR. COBOL2Java Team.
      *
      * Fibonacci sequence calculator
      *
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-N          PIC 9(2)  VALUE 10.
       01 WS-I          PIC 9(2)  VALUE 0.
       01 WS-FIB-PREV   PIC 9(10) VALUE 0.
       01 WS-FIB-CURR   PIC 9(10) VALUE 1.
       01 WS-FIB-NEXT   PIC 9(10) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Fibonacci sequence (first " WS-N " numbers):"
           PERFORM CALCULATE-FIBONACCI
           STOP RUN.

       CALCULATE-FIBONACCI.
           MOVE 0 TO WS-I
           MOVE 0 TO WS-FIB-PREV
           MOVE 1 TO WS-FIB-CURR
           DISPLAY WS-FIB-PREV
           ADD 1 TO WS-I
           PERFORM UNTIL WS-I >= WS-N
               DISPLAY WS-FIB-CURR
               ADD WS-FIB-PREV TO WS-FIB-CURR GIVING WS-FIB-NEXT
               MOVE WS-FIB-CURR TO WS-FIB-PREV
               MOVE WS-FIB-NEXT TO WS-FIB-CURR
               ADD 1 TO WS-I
           END-PERFORM.

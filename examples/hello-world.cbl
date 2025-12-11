       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. COBOL2Java Team.
      *
      * Simple Hello World program
      *
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".
       01 WS-COUNTER PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Starting program..."
           PERFORM GREET-PARAGRAPH
           DISPLAY "Program complete."
           STOP RUN.

       GREET-PARAGRAPH.
           DISPLAY WS-MESSAGE
           ADD 1 TO WS-COUNTER
           DISPLAY WS-COUNTER.

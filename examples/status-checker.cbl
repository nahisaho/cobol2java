       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATUS-CHECKER.
       AUTHOR. COBOL2Java Team.
      *
      * Status checking program using EVALUATE
      *
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS      PIC 9 VALUE 2.
       01 WS-MESSAGE     PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM CHECK-STATUS-PARAGRAPH
           DISPLAY "Status: " WS-STATUS
           DISPLAY "Message: " WS-MESSAGE
           STOP RUN.

       CHECK-STATUS-PARAGRAPH.
           EVALUATE WS-STATUS
               WHEN 1
                   MOVE "ACTIVE" TO WS-MESSAGE
               WHEN 2
                   MOVE "PENDING" TO WS-MESSAGE
               WHEN 3
                   MOVE "INACTIVE" TO WS-MESSAGE
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-MESSAGE
           END-EVALUATE.

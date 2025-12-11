       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-CHECKER.
       AUTHOR. COBOL2Java Team.
      *
      * Grade checking program with IF/ELSE
      *
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE       PIC 9(3) VALUE 85.
       01 WS-GRADE       PIC X(1) VALUE SPACES.
       01 WS-PASS-FAIL   PIC X(4) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM CHECK-GRADE
           DISPLAY "Score: " WS-SCORE
           DISPLAY "Grade: " WS-GRADE
           DISPLAY "Result: " WS-PASS-FAIL
           STOP RUN.

       CHECK-GRADE.
           IF WS-SCORE >= 90
               MOVE "A" TO WS-GRADE
               MOVE "PASS" TO WS-PASS-FAIL
           ELSE
               IF WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
                   MOVE "PASS" TO WS-PASS-FAIL
               ELSE
                   IF WS-SCORE >= 70
                       MOVE "C" TO WS-GRADE
                       MOVE "PASS" TO WS-PASS-FAIL
                   ELSE
                       IF WS-SCORE >= 60
                           MOVE "D" TO WS-GRADE
                           MOVE "PASS" TO WS-PASS-FAIL
                       ELSE
                           MOVE "F" TO WS-GRADE
                           MOVE "FAIL" TO WS-PASS-FAIL
                       END-IF
                   END-IF
               END-IF
           END-IF.

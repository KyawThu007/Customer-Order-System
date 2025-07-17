       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIM-AGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-AGE-ALPHA         PIC X(10) VALUE SPACES.
       01 WS-AGE-NUMERIC       PIC 9(5) VALUE ZEROS.
       01 WS-AGE-DISPLAY       PIC ZZZZ9.
       01 WS-AGE-TRIMMED       PIC X(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE '   25   ' TO WS-AGE-ALPHA

           *> Convert trimmed alphanumeric to numeric
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-AGE-ALPHA))
               TO WS-AGE-NUMERIC

           *> Format numeric age without leading zeros
           MOVE WS-AGE-NUMERIC TO WS-AGE-DISPLAY

           DISPLAY 'Age (Numeric) :' WS-AGE-DISPLAY
           STOP RUN.

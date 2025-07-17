      ******************************************************************
      * Author     : You
      * Date       : 2025-07-08
      * Purpose    : Write order details to a file on Windows
      * Compiler   : GnuCOBOL on Windows
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE-TO-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINTERFILE ASSIGN TO "C:\\Users\\Public\\output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SAMPLE.

       DATA DIVISION.
       FILE SECTION.
       FD  PRINTERFILE
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINT-REC.

       01  PRINT-REC               PIC X(132).

       WORKING-STORAGE SECTION.
       01  SAMPLE                  PIC XX.

       01  ORDER-ID                PIC X(10) VALUE "ORD000123".
       01  CART-ID1                PIC X(10) VALUE "CART1001".
       01  CART-ID2                PIC X(10) VALUE "CART1002".
       01  TOTAL-AMOUNT            PIC 9(7)V99 VALUE 1200.00.
       01  DISCOUNT                PIC 9(3)V99 VALUE 100.00.
       01  FINAL-AMOUNT            PIC 9(7)V99 VALUE 1100.00.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT PRINTERFILE

           IF SAMPLE NOT = "00"
               DISPLAY "ERROR OPENING FILE: " SAMPLE
               STOP RUN
           END-IF

           MOVE ALL "-" TO PRINT-REC
           WRITE PRINT-REC

           MOVE SPACES TO PRINT-REC
           STRING "ORDER ID     : " DELIMITED BY SIZE
                  ORDER-ID       DELIMITED BY SIZE
                  INTO PRINT-REC
           END-STRING
           WRITE PRINT-REC

           MOVE SPACES TO PRINT-REC
           STRING "CART ID #1   : " DELIMITED BY SIZE
                  CART-ID1       DELIMITED BY SIZE
                  INTO PRINT-REC
           END-STRING
           WRITE PRINT-REC

           MOVE SPACES TO PRINT-REC
           STRING "CART ID #2   : " DELIMITED BY SIZE
                  CART-ID2       DELIMITED BY SIZE
                  INTO PRINT-REC
           END-STRING
           WRITE PRINT-REC

           MOVE SPACES TO PRINT-REC
           STRING "TOTAL AMOUNT : " DELIMITED BY SIZE
                  TOTAL-AMOUNT   DELIMITED BY SIZE
                  INTO PRINT-REC
           END-STRING
           WRITE PRINT-REC

           MOVE SPACES TO PRINT-REC
           STRING "DISCOUNT     : " DELIMITED BY SIZE
                  DISCOUNT       DELIMITED BY SIZE
                  INTO PRINT-REC
           END-STRING
           WRITE PRINT-REC

           MOVE SPACES TO PRINT-REC
           STRING "FINAL AMOUNT : " DELIMITED BY SIZE
                  FINAL-AMOUNT   DELIMITED BY SIZE
                  INTO PRINT-REC
           END-STRING
           WRITE PRINT-REC

           MOVE ALL "-" TO PRINT-REC
           WRITE PRINT-REC

           CLOSE PRINTERFILE

           DISPLAY "Order written to C:\\Users\\Public\\output.txt"

           STOP RUN.

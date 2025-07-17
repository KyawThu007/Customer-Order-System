       IDENTIFICATION DIVISION.
       PROGRAM-ID. HistoryManager.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HistoryFile ASSIGN TO "history.dat"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS H-RelativeKey
               FILE STATUS IS H-File-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  HistoryFile.
       01  History-Record        PIC X(80).

       WORKING-STORAGE SECTION.
       01  H-RelativeKey         PIC 9(4) COMP.
       01  H-File-Status         PIC XX.

       01  WS-Choice             PIC 9.
       01  WS-Done               PIC X VALUE "N".
       01  WS-History-Data       PIC X(80).
       01  WS-Input-Key          PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

           OPEN I-O HistoryFile
           IF H-File-Status NOT = "00"
               DISPLAY "File not found. Creating new file..."
               OPEN OUTPUT HistoryFile
               CLOSE HistoryFile
               OPEN I-O HistoryFile
           END-IF

           PERFORM UNTIL WS-Done = "Y"
               DISPLAY "==========================="
               DISPLAY "1. Insert History Record"
               DISPLAY "2. Read History Record"
               DISPLAY "3. Exit"
               DISPLAY "Enter choice: "
               ACCEPT WS-Choice

               EVALUATE WS-Choice
                   WHEN 1
                       PERFORM INSERT-RECORD
                   WHEN 2
                       PERFORM READ-RECORD
                   WHEN 3
                       MOVE "Y" TO WS-Done
                   WHEN OTHER
                       DISPLAY "Invalid choice."
               END-EVALUATE
           END-PERFORM

           CLOSE HistoryFile
           STOP RUN.

       INSERT-RECORD.
           DISPLAY "Enter relative key (1-9999): "
           ACCEPT H-RelativeKey

           DISPLAY "Enter history message: "
           ACCEPT WS-History-Data

           MOVE WS-History-Data TO History-Record

           WRITE History-Record
               INVALID KEY
                   DISPLAY "Record already exists. Status: "
                   H-File-Status
               NOT INVALID KEY
                   DISPLAY "Record inserted."
           .

       READ-RECORD.
           DISPLAY "Enter relative key to read: "
           ACCEPT H-RelativeKey

           READ HistoryFile
               INVALID KEY
                   DISPLAY "Record not found. Status: " H-File-Status
               NOT INVALID KEY
                   DISPLAY "Record found: " History-Record
           .

      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Item.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ItemFile ASSIGN TO "Item.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS RelativeKey
           FILE STATUS IS File-Status.

           SELECT TranscationFile ASSIGN TO "Transcation.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS RelativeKey
           FILE STATUS IS File-Status.

           SELECT StockFile ASSIGN TO "Stock.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS Stock-RelativeKey
           FILE STATUS IS File-Status.
       DATA DIVISION.
       FILE SECTION.
       FD ItemFile.
       01 Item-Record.
           05 Item-ID PIC 9(5).
           05 Item-Name PIC X(20).
           05 Item-Price PIC 9(5).
       FD TranscationFile.
       01 Transcation-Record.
           05 Transcation-ID PIC 9(5).
           05 Transcation-Item-ID PIC 9(5).
           05 Transcation-Date PIC X(10).
           05 Transcation-Quantity PIC 9(3).
           05 Transcation-Unit-Of-Price PIC 9(10).
       FD StockFile.
       01 Stock-Record.
           05 Stock-ID PIC 9(5).
           05 Stock-Item-ID PIC 9(5).
           05 Stock-Quantity PIC 9(3).
           05 Stock-Unit-Of-Price PIC 9(10).
       WORKING-STORAGE SECTION.
       01 File-Status PIC XX VALUE "00".
       01 EOF PIC X VALUE "Y".
       01 EOFP PIC X VALUE "Y".
       01 User-Choice PIC X.
       01 RelativeKey PIC 9(5).
       01 Stock-RelativeKey PIC 9(5).
       01 Update-Quantity PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM UNTIL EOFP = "N"

               DISPLAY "1.View Item"
               DISPLAY "2.Add Stock"
               DISPLAY "3.View Transcation"
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               EVALUATE User-Choice
                   WHEN "1"
                       PERFORM View-Data
                   WHEN "2"
                       PERFORM Add-Stock
                   WHEN "3"
                       PERFORM View-Transcation
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE

               DISPLAY "More Process (Y/N)?"
               ACCEPT EOFP
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM

            STOP RUN.
       View-Data.
            OPEN INPUT ItemFile
            IF File-Status NOT = "00" THEN
               DISPLAY "Error opening file. Status: " File-Status
               STOP RUN
            END-IF

            DISPLAY "Item Record"
            DISPLAY "--------------------------------------------------"
            DISPLAY "Item-ID                  "
                   "Item-Name"
            DISPLAY "--------------------------------------------------"
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY Item-ID"                    "
                           Item-Name
            END-PERFORM
            CLOSE ItemFile.
       View-Transcation.
            OPEN INPUT TranscationFile
            IF File-Status NOT = "00" THEN
               DISPLAY "Error opening file. Status: " File-Status
               STOP RUN
            END-IF

            DISPLAY "Transcation Record"
            DISPLAY "--------------------------------------------------"
            DISPLAY "Item-ID         "
                   "Date           "
                   "Quantity           "
                   "Unit of Price"
            DISPLAY "--------------------------------------------------"
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ TranscationFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY Transcation-Item-ID"           "
                   Transcation-Date"     "
                   Transcation-Quantity"                "
                   Transcation-Unit-Of-Price
            END-PERFORM
            CLOSE TranscationFile.

       Add-Stock.
            OPEN INPUT ItemFile
            MOVE "Y" TO EOF
            PERFORM UNTIL EOF = "N"

            DISPLAY "Enter Relative Record Number to Add: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY "Item Name: " Item-Name

                   OPEN OUTPUT TranscationFile

                   MOVE 1 TO Transcation-ID
                   MOVE Item-ID TO Transcation-Item-ID
                   DISPLAY "Enter Date: "
                   ACCEPT Transcation-Date
                   DISPLAY "Enter Quantity: "
                   ACCEPT Transcation-Quantity
                   DISPLAY "Enter Unit of Price: "
                   ACCEPT Transcation-Unit-Of-Price

                   WRITE Transcation-Record

                   DISPLAY "Stock Transcation successfully."
                   PERFORM Check-Stock
                   CLOSE TranscationFile
                END-IF

            END-READ
            DISPLAY "Enter new data for (Y/N)?"
            ACCEPT EOF
            MOVE FUNCTION UPPER-CASE(EOF) TO EOF
            END-PERFORM
            CLOSE ItemFile.
       Check-Stock.
            OPEN I-O StockFile
            MOVE Item-ID TO Stock-RelativeKey
            READ StockFile INVALID KEY
               MOVE 1 TO Stock-ID
               MOVE Item-ID TO Stock-Item-ID
               MOVE Transcation-Quantity TO Stock-Quantity
               MOVE Item-Price TO Stock-Unit-Of-Price
               WRITE Stock-Record

               DISPLAY "Stock Added successfully."
            NOT INVALID KEY
               IF File-Status = "00"

                   ADD Transcation-Quantity TO Stock-Quantity
                   GIVING Update-Quantity

                   MOVE Update-Quantity TO Stock-Quantity
                   MOVE Item-Price TO Stock-Unit-Of-Price

                   REWRITE Stock-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY "Stock updated successfully."
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF
            END-READ
            CLOSE StockFile.
       END PROGRAM Item.

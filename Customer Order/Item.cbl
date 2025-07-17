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
       DATA DIVISION.
       FILE SECTION.
       FD ItemFile.
       01 Item-Record.
           05 Item-ID PIC 9(5).
           05 Item-Name PIC X(20).
           05 Item-Price PIC 9(5).
       WORKING-STORAGE SECTION.
       01 File-Status PIC XX VALUE "00".
       01 EOF PIC X VALUE "Y".
       01 EOFP PIC X VALUE "Y".
       01 User-Choice PIC X.
       01 RelativeKey PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN I-O ItemFile
            IF File-Status = "35"
               DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT ItemFile
               CLOSE ItemFile
               OPEN I-O ItemFile
            END-IF.

            IF File-Status NOT = "00"
               DISPLAY "Error opening file. Status: " File-Status
               STOP RUN
            END-IF.
            PERFORM UNTIL EOFP = "N"
               OPEN I-O ItemFile
               DISPLAY "1.View Data"
               DISPLAY "2.Insert Data"
               DISPLAY "3.Update Data"
               DISPLAY "4.Delete Data"
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               EVALUATE User-Choice
                   WHEN "1"
                       PERFORM View-Data
                   WHEN "2"
                       PERFORM Insert-Data
                   WHEN "3"
                       PERFORM Update-Data
                   WHEN "4"
                       PERFORM Delete-Data
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
               CLOSE ItemFile
               DISPLAY "More Process (Y/N)?"
               ACCEPT EOFP
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM
            CLOSE ItemFile
            STOP RUN.
       View-Data.
            IF File-Status NOT = "00" THEN
               DISPLAY "Error opening file. Status: " File-Status
               STOP RUN
            END-IF

            DISPLAY "Item Record"
            DISPLAY "--------------------------------------------------"
            DISPLAY "Item-ID                  "
                   "Item-Name                  "
                   "Item-Price"
            DISPLAY "--------------------------------------------------"
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY Item-ID"                    "
                           Item-Name"                  "
                           Item-Price
            END-PERFORM.
       Insert-Data.
            MOVE "Y" TO EOF
            PERFORM UNTIL EOF = "N"
               DISPLAY "Enter Item ID: "
               ACCEPT Item-ID
               MOVE Item-ID TO RelativeKey
               DISPLAY "Enter Item Name: "
               ACCEPT Item-Name
               DISPLAY "Enter Item Price: "
               ACCEPT Item-Price
               WRITE Item-Record INVALID KEY
               DISPLAY "Error: Unable to write record."
               END-WRITE

               IF File-Status = "00"
                   DISPLAY "Item added successfully."
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF

               DISPLAY "Enter new data for (Y/N)?"
               ACCEPT EOF
               MOVE FUNCTION UPPER-CASE(EOF) TO EOF
            END-PERFORM.
       Update-Data.
            DISPLAY "Enter Relative Record Number to Update: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY "Item Name: " Item-Name
                   DISPLAY "Enter New Item Name: "
                   ACCEPT Item-Name
                   DISPLAY "Enter Item Price: "
                   ACCEPT Item-Price
                   REWRITE Item-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY "Item updated successfully."
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF

            END-READ.

       Delete-Data.
            DISPLAY "Enter Relative Record Number to Delete: "
            ACCEPT RelativeKey

            DELETE ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY "Item deleted successfully."
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
            END-DELETE.
       END PROGRAM Item.

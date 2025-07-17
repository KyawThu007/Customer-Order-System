******************************************************************
      * Discount Table CRUD
      * Fields: Limit, Percent, StartDate, EndDate
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISCOUNT-CRUD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DISCOUNT-FILE ASSIGN TO "DISCOUNTS.DAT"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS WS-REL-KEY
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD DISCOUNT-FILE.
       01 DISCOUNT-RECORD.
           05 LIMIT-AMOUNT   PIC 9(6)V99.
           05 PERCENT        PIC 9(3)V99.
           05 START-DATE     PIC 9(8).
           05 END-DATE       PIC 9(8).

       WORKING-STORAGE SECTION.
       01 WS-REL-KEY        PIC 9(5).
       01 WS-STATUS         PIC XX.
       01 WS-CHOICE         PIC X.
       01 WS-CONTINUE       PIC X VALUE 'Y'.
       01 WS-FREE-KEY       PIC 9(5).

       PROCEDURE DIVISION.

      * ------------------ MAIN LOGIC ------------------
       MAIN-PARA.
           OPEN I-O DISCOUNT-FILE.
           IF WS-STATUS = "35"
               DISPLAY "File not found. Creating..."
               OPEN OUTPUT DISCOUNT-FILE
               CLOSE DISCOUNT-FILE
               OPEN I-O DISCOUNT-FILE
           END-IF.

           PERFORM UNTIL WS-CONTINUE = "N"
               DISPLAY "------------------------------------------"
               DISPLAY "1. View Discounts"
               DISPLAY "2. Add Discount"
               DISPLAY "3. Update Discount"
               DISPLAY "4. Delete Discount"
               DISPLAY "Q. Quit"
               DISPLAY "------------------------------------------"
               DISPLAY "Enter choice: "
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN "1" PERFORM VIEW-DISCOUNTS
                   WHEN "2" PERFORM ADD-DISCOUNT
                   WHEN "3" PERFORM UPDATE-DISCOUNT
                   WHEN "4" PERFORM DELETE-DISCOUNT
                   WHEN "Q" MOVE "N" TO WS-CONTINUE
                   WHEN OTHER DISPLAY "Invalid option. Try again."
               END-EVALUATE
           END-PERFORM.

           CLOSE DISCOUNT-FILE.
           DISPLAY "Done. Bye!"
           STOP RUN.

      * ------------------ VIEW ------------------------
       VIEW-DISCOUNTS.
           DISPLAY "---------------------------------------------"
           DISPLAY "REC#  LIMIT     PERCENT   START     END"
           DISPLAY "---------------------------------------------"

           MOVE 1 TO WS-REL-KEY.

           PERFORM UNTIL WS-REL-KEY > 100
               READ DISCOUNT-FILE
                   INVALID KEY
                       CONTINUE
                   NOT INVALID KEY
                       DISPLAY WS-REL-KEY " | "
                               LIMIT-AMOUNT " | "
                               PERCENT " | "
                               START-DATE " | "
                               END-DATE
               END-READ
               ADD 1 TO WS-REL-KEY
           END-PERFORM.

           MOVE "00" TO WS-STATUS.

      * ------------------ ADD -------------------------
       ADD-DISCOUNT.
           PERFORM FIND-NEXT-FREE-KEY.

           DISPLAY "Enter Limit Amount: "
           ACCEPT LIMIT-AMOUNT.
           DISPLAY "Enter Percent: "
           ACCEPT PERCENT.
           DISPLAY "Enter Start Date (YYYYMMDD): "
           ACCEPT START-DATE.
           DISPLAY "Enter End Date (YYYYMMDD): "
           ACCEPT END-DATE.

           MOVE WS-FREE-KEY TO WS-REL-KEY.

           WRITE DISCOUNT-RECORD INVALID KEY
               DISPLAY "Error writing record."
           END-WRITE.

           IF WS-STATUS = "00"
               DISPLAY "Discount added successfully at slot #"
               WS-REL-KEY
           END-IF.

      * ------------------ UPDATE ----------------------
       UPDATE-DISCOUNT.
           DISPLAY "Enter Record Number to Update: "
           ACCEPT WS-REL-KEY.

           READ DISCOUNT-FILE INVALID KEY
               DISPLAY "Record not found."
               EXIT PARAGRAPH
           END-READ.

           DISPLAY "Current: " LIMIT-AMOUNT " " PERCENT " "
                    START-DATE " " END-DATE.

           DISPLAY "Enter New Limit Amount: "
           ACCEPT LIMIT-AMOUNT.
           DISPLAY "Enter New Percent: "
           ACCEPT PERCENT.
           DISPLAY "Enter New Start Date (YYYYMMDD): "
           ACCEPT START-DATE.
           DISPLAY "Enter New End Date (YYYYMMDD): "
           ACCEPT END-DATE.

           REWRITE DISCOUNT-RECORD INVALID KEY
               DISPLAY "Error rewriting record."
           END-REWRITE.

           DISPLAY "Discount updated.".

      * ------------------ DELETE ----------------------
       DELETE-DISCOUNT.
           DISPLAY "Enter Record Number to Delete: "
           ACCEPT WS-REL-KEY.

           DELETE DISCOUNT-FILE INVALID KEY
               DISPLAY "Record not found."
           END-DELETE.

           DISPLAY "Discount deleted.".

      * ------------------ FIND FREE KEY ---------------
       FIND-NEXT-FREE-KEY.
           MOVE 1 TO WS-FREE-KEY.
           PERFORM UNTIL WS-FREE-KEY > 100
               MOVE WS-FREE-KEY TO WS-REL-KEY
               READ DISCOUNT-FILE
                   INVALID KEY
                       EXIT PERFORM
                   NOT INVALID KEY
                       ADD 1 TO WS-FREE-KEY
               END-READ
           END-PERFORM.

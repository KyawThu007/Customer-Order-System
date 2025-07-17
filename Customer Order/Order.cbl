       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AdminFile ASSIGN TO "admin.txt"
           ORGANIZATION IS SEQUENTIAL.

           SELECT SaleFile ASSIGN TO "sale.txt"
           ORGANIZATION IS SEQUENTIAL.

           SELECT ItemFile ASSIGN TO "Item.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS RelativeKey
           FILE STATUS IS File-Status.


           SELECT HistoryFile ASSIGN TO "history.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS History-ID
            FILE STATUS IS File-Status.


           SELECT ORDER-FILE ASSIGN TO "ORDER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.


       DATA DIVISION.
       FILE SECTION.
       FD AdminFile.
       01 AdminRec.
           05 Admin-ID PIC X(6).
           05 Admin-PW PIC X(6).

       FD SaleFile.
       01 SaleRec.
           05 Sale-ID PIC X(5).
           05 Sale-PW PIC X(6).


       FD ItemFile.
       01 Item-Record.
           05 Item-ID PIC 9(5).
           05 Item-Name PIC X(20).
           05 Item-Price PIC 9(5).
           05 Item-Qty PIC 9(3).




       FD HistoryFile.
       01 History-Record.
           05 History-ID PIC 9(5) VALUE ZEROES.
           05 History-Item-ID PIC 9(5).
           05 History-Date PIC X(10).
           05 History-Quantity PIC 9(3).
           05 History-Unit-Of-Price PIC 9(10).

       FD ORDER-FILE.
       01 ORDER-RECORD.
           05 ORDER-ID PIC 9(5).
           05 CUSTOMER-ID  PIC 9(5).
           05 ORDER-ITEM-ID   PIC 9(5).
           05 ORDER-ITEM-NAME PIC X(15).
           05 ORDER-QTY       PIC 9(3).
           05 TOTAL-AMOUNT      PIC 9(6).
           05 UNIT-OF-PRICE PIC 9(6).
           05 ORDER-DATE PIC XX/XX/XXXX.

       WORKING-STORAGE SECTION.
       01  WS-RESULT        PIC 9(4) VALUE ZERO.
       01 File-Status PIC XX VALUE "00".
       01 EOF PIC X VALUE "Y".
       01 EOFP PIC X VALUE "Y".
       01 User-Choice PIC X.
       01 RelativeKey PIC 9(5).
       01 role PIC X(1).
       01 EF PIC X(1) value "Y".
       01 Admin-Input-ID       PIC X(6).
       01 Admin-Input-Password       PIC X(6).
       01 Sale-Input-ID       PIC X(5).
       01 Sale-Input-Password       PIC X(6).
       01 ACCESSS PIC X(1) value "N".
       01 a PIC X(7) value space.
       01 Update-Quantity PIC 9(3).
       01 MAX-ID            PIC 9(5) VALUE ZERO.
       01 loginalert PIC X(20) .


       01 WS-STATUS         PIC XX.
       01 WS-END            PIC X VALUE "N".
       01 WS-MATCHED        PIC X VALUE "N".
       01 TOTAL-COST     PIC 9(6) VALUE ZERO.
       01 Input-Customer-Id     PIC 9(5).
       01 INPUT-ITEM-ID     PIC X(5).
       01 INPUT-QTY         PIC 9(3).
       01 CONFIRMATION      PIC X(10).



       PROCEDURE DIVISION.
           PERFORM UNTIL EF = "N"
               DISPLAY "Choose Role:"
               DISPLAY "1. Admin"
               DISPLAY "2. Sale"
               ACCEPT Role

               EVALUATE Role
                   WHEN "1"
                       PERFORM Admin
                   WHEN "2"
                       PERFORM Sale
                   WHEN OTHER
                       DISPLAY "Choose 1 or 2 "
               END-EVALUATE

               DISPLAY "Choose Role Again?(Y/N)?"
               ACCEPT EF
               MOVE FUNCTION UPPER-CASE(EF) TO EF
           END-PERFORM
            STOP RUN.

           Admin.
           DISPLAY "============================"
           DISPLAY "       LOGIN PROGRAM       "
           DISPLAY "============================"
           DISPLAY "Enter Admin-Id: "
           ACCEPT Admin-Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Admin-Input-Password

           OPEN INPUT AdminFile
           MOVE "N" TO EF
           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
               NOT AT END
                   IF Admin-ID = Admin-Input-ID
                       AND
                      Admin-PW = Admin-Input-Password
                       MOVE "Y" TO EF
                       MOVE "Y" TO ACCESSS
                       EXIT PERFORM
                   END-IF


           END-PERFORM

           CLOSE AdminFile

           IF ACCESSS = "Y"
               DISPLAY "Login successful. Welcome, " Admin-ID
               OPEN I-O ItemFile

            PERFORM UNTIL EOFP = "N"
               OPEN I-O ItemFile
               DISPLAY "-----------------------------"
               display "Item"
               DISPLAY "-----------------------------"
               DISPLAY "1.View Items"
               DISPLAY "2.Insert Item"
               DISPLAY "3.Update Item"
               DISPLAY "4.Delete Item"
               DISPLAY "-----------------------------"
               DISPLAY ""
               DISPLAY "-----------------------------"
               display "Stock"
               DISPLAY "-----------------------------"
               DISPLAY "5.Stock Fill "
               DISPLAY "-----------------------------"
               DISPLAY ""
               display "Item transaction"
               DISPLAY "-----------------------------"
               DISPLAY "6.View Item Transaction "
               DISPLAY "-----------------------------"
               DISPLAY ""
               DISPLAY "7.Exit"
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
                   WHEN "5"
                       PERFORM Add-Stock
                   WHEN "6"
                       PERFORM View-History
                   WHEN "7"
                       stop run
                   WHEN OTHER
                       DISPLAY "Invalid command. Try again."
               END-EVALUATE
               CLOSE ItemFile
               DISPLAY "More Process (Y/N)?"
               ACCEPT EOFP
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM
            CLOSE ItemFile

           ELSE
               DISPLAY "Login failed. Invalid credentials."
           END-IF.


   1


       View-Data.
            DISPLAY "Item Record"
            DISPLAY "--------------------------------------------------"
            DISPLAY "Item-ID" a
                   "Item-Name" a
                   "Item Quantity" a
                   "Item-Price"

            DISPLAY "--------------------------------------------------"
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY Item-ID a
                           Item-Name a
                           Item-Qty a
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
               DISPLAY "Enter Item Quantity: "
               ACCEPT Item-Qty
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
                   DISPLAY "Enter Item Quantity: "
                   ACCEPT Item-Qty
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
           STOP RUN.


       Add-Stock.
            OPEN I-O ItemFile
            MOVE "Y" TO EOF
            PERFORM UNTIL EOF = "2"

            DISPLAY "Enter Item-Id: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"

                    OPEN I-O HistoryFile
                    IF File-Status = "35"
                        DISPLAY "File does not exist. Creating file..."
                        OPEN OUTPUT HistoryFile
                        CLOSE HistoryFile
                        OPEN I-O HistoryFile


                    END-IF
                   IF History-ID = SPACES
                     MOVE 1 TO History-ID
                   ELSE
                        MOVE "N" TO EOF
                        PERFORM UNTIL EOF = "Y"
                           READ HistoryFile NEXT RECORD
                           AT END
                               MOVE "Y" TO EOF
                           NOT AT END
                               IF HISTORY-ID > MAX-ID
                                MOVE HISTORY-ID TO MAX-ID
                               END-IF
                        END-PERFORM

                   END-IF
                  ADD 1 TO MAX-ID
                   MOVE MAX-ID TO HISTORY-ID

                   DISPLAY "Item Name: " Item-Name

                   MOVE Item-ID TO History-Item-ID
                   DISPLAY "Enter Date: "
                   ACCEPT History-Date
                   DISPLAY "Enter Quantity: "
                   ACCEPT History-Quantity
                   DISPLAY "Enter Unit of Price: "
                   ACCEPT History-Unit-Of-Price

                   WRITE History-Record INVALID KEY
                   DISPLAY "Error: Unable to write record."
                   END-WRITE
                   IF File-Status = "00"
                       DISPLAY "Stock fill successfully."
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                   CLOSE HistoryFile

                   ADD History-Quantity TO Item-Qty
                   GIVING Update-Quantity
                   MOVE Update-Quantity TO Item-Qty

                   REWRITE Item-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY "Stock updated successfully."
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF

                END-IF

            END-READ
            DISPLAY "1.Add more fill Stock"
            DISPLAY "2.Go Back"
            ACCEPT EOF
            MOVE FUNCTION UPPER-CASE(EOF) TO EOF
            END-PERFORM

            CLOSE ItemFile.

       View-History.
            OPEN I-O HistoryFile
            IF File-Status NOT = "00" THEN
               DISPLAY "Error opening file. Status: " File-Status
               STOP RUN
            END-IF

            DISPLAY "History Record"
            DISPLAY "--------------------------------------------------"
            DISPLAY "History-ID         "
                   "Item-ID         "
                   "Date           "
                   "Quantity           "
                   "Unit of Price"
            DISPLAY "--------------------------------------------------"
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ HistoryFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY History-ID"           "
                   History-Item-ID"           "
                   History-Date"     "
                   History-Quantity"                "
                   History-Unit-Of-Price
            END-PERFORM
            CLOSE HistoryFile.


Sale j*> obs

           Sale.
           DISPLAY "============================"
           DISPLAY "       LOGIN PROGRAM       "
           DISPLAY "============================"
           DISPLAY "Enter Sale-ID: "
           ACCEPT Sale-Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Sale-Input-Password

           OPEN INPUT SaleFile
           MOVE "N" TO EF
           PERFORM UNTIL EF = "Y"
               READ SaleFile
                   AT END
                       MOVE "Y" TO EF
               NOT AT END
                   IF Sale-ID = Sale-Input-ID
                       AND
                      Sale-PW = Sale-Input-Password
                      MOVE "Login successful." TO loginalert
                      MOVE "Y" TO EF
                      Perform OrderProcess
                      EXIT PERFORM
                   else
                       move  "Login failed." TO loginalert
                   END-IF

           END-PERFORM
           display loginalert
           CLOSE SaleFile.

           OrderProcess.
              OPEN INPUT ItemFile
           IF WS-STATUS NOT = "00"
               DISPLAY "Cannot open item file. Status: " WS-STATUS
               STOP RUN
           END-IF

           DISPLAY "Available Items:"
           PERFORM SHOW-ITEMS
           CLOSE ItemFile

           DISPLAY "Enter Customer-Id: "
           ACCEPT Input-Customer-Id
           DISPLAY "Enter Item ID: "
           ACCEPT INPUT-ITEM-ID
           DISPLAY "Enter Quantity: "
           ACCEPT INPUT-QTY

           OPEN INPUT ItemFile
           PERFORM CHECK-STOCK
           CLOSE ItemFile

           IF WS-MATCHED = "N"
               DISPLAY "Item ID not found."
               STOP RUN
           END-IF

           DISPLAY "Confirm Order? (Y/N): "
           ACCEPT CONFIRMATION
           IF FUNCTION UPPER-CASE(CONFIRMATION) = "Y"
               OPEN EXTEND ORDER-FILE
               MOVE Input-Customer-Id  TO CUSTOMER-ID
               MOVE INPUT-ITEM-ID   TO ORDER-ITEM-ID
               MOVE ITEM-NAME       TO ORDER-ITEM-NAME
               MOVE INPUT-QTY       TO ORDER-QTY
               MOVE TOTAL-COST   TO TOTAL-AMOUNT
               WRITE ORDER-RECORD
               CLOSE ORDER-FILE

               PERFORM SHOW-INVOICE
           ELSE
               DISPLAY "Order Cancelled."
           END-IF

           STOP RUN.

       SHOW-ITEMS.
           PERFORM UNTIL WS-STATUS = "10"
               READ ItemFile
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       DISPLAY "ID: " ITEM-ID
                       DISPLAY "Name: " ITEM-NAME

                       DISPLAY "-------------------------"
           END-PERFORM.

       CHECK-STOCK.
           MOVE "N" TO WS-MATCHED
           PERFORM UNTIL WS-STATUS = "10"
               READ ItemFile
                   AT END
                       MOVE "10" TO WS-STATUS
                   NOT AT END
                       IF ITEM-ID = INPUT-ITEM-ID
                           MOVE "Y" TO WS-MATCHED
                           IF INPUT-QTY > Item-Qty
                               DISPLAY "Stock Not Enough."
                               STOP RUN
                           ELSE
                               COMPUTE TOTAL-COST=
                               ITEM-PRICE * INPUT-QTY
                               DISPLAY "Stock Available."
                               DISPLAY "Item Name : " ITEM-NAME
                               DISPLAY "Unit Price: " ITEM-PRICE
                               DISPLAY "Total     : " TOTAL-COST
                           END-IF
                       END-IF
           END-PERFORM.

       SHOW-INVOICE.
           DISPLAY "--------- Invoice ---------"
           DISPLAY "Customer-ID : " CUSTOMER-ID
           display "Item-Id :"Item-ID
           DISPLAY "Item-Name     : " ITEM-NAME
           DISPLAY "Quantity : " INPUT-QTY
           DISPLAY "Unit     : " ITEM-PRICE
           DISPLAY "Total Cost    : " TOTAL-COST
           DISPLAY "---------------------------".

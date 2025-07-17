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
           SELECT AdminFile ASSIGN TO "admin.txt"
           ORGANIZATION IS SEQUENTIAL.

           SELECT CasherFile ASSIGN TO "casher.txt"
           ORGANIZATION IS SEQUENTIAL.

           SELECT CategoryFile ASSIGN TO "category.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ItemFile ASSIGN TO "item.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS RelativeKey
           FILE STATUS IS File-Status.

           SELECT HistoryFile ASSIGN TO "history.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS History-ID
            FILE STATUS IS File-Status.

           SELECT DiscountFile ASSIGN TO "discount.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS RelativeKey
           FILE STATUS IS File-Status.

           SELECT CartFile ASSIGN TO "cart.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS Cart-ID
            FILE STATUS IS File-Status.

           SELECT SaleCartFile ASSIGN TO "salecart.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS Sale-Cart-ID
            FILE STATUS IS File-Status.

           SELECT InvoiceFile ASSIGN TO "invoice.dat"
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS Invoice-ID
            FILE STATUS IS File-Status.

       DATA DIVISION.
       FILE SECTION.
       FD AdminFile.
       01 Admin-Record.
           05 Admin-ID PIC X(6).
           05 Admin-PW PIC X(6).

       FD CasherFile.
       01 Casher-Record.
           05 Casher-ID PIC X(6).
           05 Casher-PW PIC X(6).

       FD CategoryFile.
       01 Category-Record.
           05 Category-Name PIC X(12).

       FD ItemFile.
       01 Item-Record.
           05 Item-ID          PIC 9(5).
           05 Item-Name        PIC X(20).
           05 Item-Category    PIC X(12).
           05 Item-Price       PIC 9(5).
           05 Item-Qty         PIC 9(3).

       FD HistoryFile.
       01 History-Record.
           05 History-ID               PIC 9(5) VALUE ZEROES.
           05 History-Item-ID          PIC 9(5).
           05 History-Date             PIC X(10).
           05 History-Quantity         PIC 9(3).
           05 History-Unit-Of-Price    PIC 9(10).

       FD DiscountFile.
       01 Discount-Record.
           05 Discount-ID    PIC 9(5).
           05 Limit-Amount   PIC 9(6).
           05 Percent        PIC 9(2)V99.
           05 Start-Date     PIC 9(8).
           05 End-Date       PIC 9(8).

       FD CartFile.
       01 Cart-Record.
           05 Cart-ID               PIC 9(5) VALUE ZEROES.
           05 Cart-Item-ID          PIC 9(5).
           05 Cart-Quantity         PIC 9(3).
           05 Cart-Unit-Of-Price    PIC 9(10).
       FD SaleCartFile.
       01 Sale-Cart-Record.
           05 Sale-Cart-ID               PIC 9(5) VALUE ZEROES.
           05 Sale-Cart-Item-ID          PIC 9(5).
           05 Sale-Cart-Quantity         PIC 9(3).
           05 Sale-Cart-Unit-Of-Price    PIC 9(10).
       FD InvoiceFile.
       01 Invoice-Record.
           05 Invoice-ID               PIC 9(5).
           05 Invoice-Casher-ID        PIC X(6).
           05 Invoice-Customer-Name    PIC X(20).
           05 Item-ID-List.
               10 Invoice-Sale-Cart-ID    OCCURS 5 TIMES PIC 9(5)
               VALUE ZEROES.
           05 Total-Amount             PIC 9(10).
           05 Discount                 PIC 9(8).
           05 Final-Amount             PIC 9(10).
           05 Invoice-Date             PIC 9(8).
           05 Invoice-Time             PIC X(11).
           05 Invoice-Status           PIC A(9).

       WORKING-STORAGE SECTION.
       01  WS-RESULT           PIC 9(4) VALUE ZERO.
       01 File-Status          PIC XX VALUE "00".
       01 EOF                  PIC X VALUE "Y".
       01 EOFP                 PIC X VALUE "Y".
       01 User-Choice          PIC X(2).
       01 RelativeKey          PIC 9(5).
       01 role                 PIC X(1).
       01 EF                   PIC X(1) VALUE "Y".
       01 End-Program          PIC X(1).
       01 Input-ID             PIC X(6).
       01 Input-Password       PIC X(6).
       01 ACCESSS              PIC X(1) VALUE "N".
       01 Update-Quantity      PIC 9(3).
       01 Discount-Price       PIC 9(10)V99.
       01 Final-Total-Price    PIC 9(10).
       01 MAX-ID               PIC 9(5) VALUE ZERO.
       01 Category-Index-List.
               05 Category-Index-Name    OCCURS 10 TIMES PIC X(12)
               VALUE SPACE.
       01 INPUT-IDX            PIC 9(2).
       01 EFC                  PIC X VALUE "N".
       01 Input-Item-Name      PIC X(20).
       01 Category-Count       PIC 9(3).
       01 Blank-Space.
           05 A1 PIC X(1) VALUE SPACE.
           05 A2 PIC X(2) VALUE SPACE.
           05 A3 PIC X(3) VALUE SPACE.
           05 A4 PIC X(4) VALUE SPACE.
           05 A5 PIC X(5) VALUE SPACE.
           05 A6 PIC X(6) VALUE SPACE.
           05 A7 PIC X(7) VALUE SPACE.
           05 A8 PIC X(8) VALUE SPACE.
           05 A9 PIC X(9) VALUE SPACE.
       01 Blank-Star.
           05 S1 PIC X(1)  VALUE    "+".
           05 S2 PIC X(2)  VALUE    "++".
           05 S3 PIC X(3)  VALUE    "+++".
           05 S4 PIC X(4)  VALUE    "++++".
           05 S5 PIC X(5)  VALUE    "+++++".
           05 S6 PIC X(6)  VALUE    "++++++".
           05 S7 PIC X(7)  VALUE    "+++++++".
           05 S8 PIC X(8)  VALUE    "++++++++".
           05 S9 PIC X(9)  VALUE    "+++++++++".
           05 S10 PIC X(10) VALUE    "++++++++++".
       01 Blank-Hipan.
           05 H1 PIC X(1)  VALUE    "-".
           05 H2 PIC X(2)  VALUE    "--".
           05 H3 PIC X(3)  VALUE    "---".
           05 H4 PIC X(4)  VALUE    "----".
           05 H5 PIC X(5)  VALUE    "-----".
           05 H6 PIC X(6)  VALUE    "------".
           05 H7 PIC X(7)  VALUE    "-------".
           05 H8 PIC X(8)  VALUE    "--------".
           05 H9 PIC X(9)  VALUE    "---------".
           05 H10 PIC X(10) VALUE    "----------".
       01 Date-Time-Format.
           05  WS-TIME-RAW         PIC 9(8).
           05  WS-HH               PIC 99.
           05  WS-MM               PIC 99.
           05  WS-SS               PIC 99.
           05  WS-TOTAL-MINUTES   PIC 9999.
           05  WS-AMPM             PIC X(2).
           05  WS-DISPLAY-HH       PIC 99.
           05  WS-DISPLAY-MM       PIC 99.
       01 Date-Format.
           05 WS-YEAR      PIC 9(4).
           05 WS-MONTH     PIC 99.
           05 WS-DAY       PIC 99.
           01 WS-DATE-OUT  PIC X(10).
       01 Waring-Noti.
           05 ESC             PIC X       VALUE X'1B'.        *> Escape character (ASCII 27)
           05 Blue-On         PIC X(5)    VALUE "[34m".       *> Blue text
           05 Reset-Color     PIC X(4)    VALUE "[0m".
       01 Register-Check.
           05 ID-LEN PIC 9 VALUE 0.
           05 PW-LEN PIC 9 VALUE 0.
           05 Password-Length  PIC 99.
           05 Has-Upper        PIC X VALUE 'N'.
           05 Has-Lower        PIC X VALUE 'N'.
           05 Has-Number       PIC X VALUE 'N'.
           05 Has-Special      PIC X VALUE 'N'.
           05 I                PIC 99.
           05 Curr-Char        PIC X.
           05 Done             PIC X VALUE 'N'.
       01 Flag PIC X VALUE "N".
       01 Cart-Flag PIC X VALUE "N".
       01 Cart-Total-Quantity PIC 9(5).
       01 IDX PIC 9.
       01 Total PIC 9(5).
       01 Temp-ID-List.
               05 Temp-ID    OCCURS 5 TIMES PIC 9(5)
               VALUE ZEROES.
       01 Best-Sell    PIC 9(3)V99.
       01 All-Qty      PIC 9(8)V99.
       01 Each-Qty     PIC 9(8)V99.
       01 EOFB         PIC X VALUE "N".
       01 Low-Stock-Count PIC 9(3).
       01 Low-Stock-Count-Format PIC ZZ9.
       01 Message-Format PIC X(80).
       01 Customer-Name PIC X(80).


       PROCEDURE DIVISION.
           PERFORM Dashborad
           STOP RUN.
       Dashborad.
           MOVE "1" TO End-Program
           PERFORM UNTIL End-Program = "0"
               DISPLAY "Choose Role:"
               DISPLAY "1. Admin"
               DISPLAY "2. Casher"
               DISPLAY "3. Register"
               DISPLAY "0. Exit"
               DISPLAY "Enter Choose Option:"
               ACCEPT Role

               EVALUATE Role
                   WHEN "1"
                       MOVE "N" TO Flag
                       PERFORM Admin
                   WHEN "2"
                       MOVE "N" TO Flag
                       PERFORM Sale
                   WHEN "3"
                       MOVE "N" TO Flag
                       PERFORM Admin-Credential
                   WHEN "0"
                       EXIT PERFORM
                   WHEN OTHER
                       DISPLAY "Choose 1 or 2"
                       MOVE "Y" TO Flag
                       MOVE "1" TO End-Program
               END-EVALUATE
               IF Flag = "N"
               DISPLAY "1.Start Program"
               DISPLAY "0.Exit"
               ACCEPT End-Program
               END-IF
               MOVE FUNCTION UPPER-CASE(End-Program) TO End-Program
           END-PERFORM.
       Admin-Credential.
           DISPLAY "Enter Admin Id: "
           ACCEPT Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Input-Password

           OPEN INPUT AdminFile
           MOVE "N" TO EF
           MOVE "N" TO ACCESSS

           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
                   NOT AT END
                       IF Admin-ID = Input-ID
                          AND Admin-PW = Input-Password
                           MOVE "Y" TO EF
                           MOVE "Y" TO ACCESSS
                       END-IF
               END-READ
           END-PERFORM

           CLOSE AdminFile
           PERFORM Register.
       Register.
           IF ACCESSS = "Y"
               DISPLAY "Register:"
               DISPLAY "1.Admin"
               DISPLAY "2.Casher"
               DISPLAY "0.Go Back"
               DISPLAY "Enter Choose Option:"
               ACCEPT Role
               EVALUATE Role
                   WHEN "1"
                       PERFORM Admin-Register
                   WHEN "2"
                       PERFORM Casher-Register
                   WHEN "0"
                       PERFORM Dashborad
                   WHEN OTHER
                       DISPLAY "Choose 1 or 2 "
               END-EVALUATE
           ELSE
               DISPLAY "Admin Id and Passward incorrect, Try Again!"
               PERFORM Admin-Credential
           END-IF.
       Admin-Register.
           DISPLAY S10 S10 S10 S8
               DISPLAY A9 A2 "REGISTER PROGRAM"
               DISPLAY S10 S10 S10 S8
               DISPLAY "Register Admin Id: "
               ACCEPT Input-ID
               COMPUTE ID-LEN = FUNCTION LENGTH(FUNCTION TRIM(Input-ID))
               IF ID-LEN = 6
                   PERFORM Admin-Check
               ELSE
                   DISPLAY "Admin- ID has 6 ."
                   PERFORM Admin-Register
               END-IF.
       Admin-Check.
           OPEN INPUT AdminFile
           MOVE "N" TO EF
           MOVE "N" TO ACCESSS

           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
                   NOT AT END
                       IF Admin-ID = Input-ID
                           MOVE "Y" TO ACCESSS
                           MOVE "Y" TO EF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE AdminFile

           IF ACCESSS = "Y"
               DISPLAY "Already exists. Please add a new member!"
               PERFORM Admin-Register
           ELSE
               PERFORM Password-Check
               IF Done = "Y"
                   OPEN EXTEND AdminFile
                   MOVE Input-ID TO Admin-ID
                   MOVE Input-Password TO Admin-PW
                   WRITE Admin-Record
                   CLOSE AdminFile

                   DISPLAY "Register successful. Welcome, " Input-ID
               END-IF
           END-IF.
       Password-Check.
           MOVE "N" TO Done
           PERFORM UNTIL Done = 'Y'
           MOVE SPACES TO Input-Password
           MOVE 'N' TO Has-Upper Has-Lower Has-Number Has-Special

           DISPLAY "Register Password: "
                   ACCEPT Input-Password

           COMPUTE Password-Length =
           FUNCTION LENGTH (FUNCTION TRIM (Input-Password TRAILING))

           IF Password-Length NOT = 6
               DISPLAY "Password must be exactly 6 characters long."
               DISPLAY "Please try again..."
           ELSE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > Password-Length
                   MOVE Input-Password(I:1) TO Curr-Char

                   IF Curr-Char >= 'A' AND Curr-Char <= 'Z'
                       MOVE 'Y' TO Has-Upper
                   ELSE IF Curr-Char >= 'a' AND Curr-Char <= 'z'
                       MOVE 'Y' TO Has-Lower
                   ELSE IF Curr-Char >= '0' AND Curr-Char <= '9'
                       MOVE 'Y' TO Has-Number
                   ELSE IF Curr-Char IS EQUAL TO "!" OR "@"
                       OR "#" OR "$" OR "%"
                       MOVE 'Y' TO Has-Special
                   END-IF
               END-PERFORM

               IF Has-Upper = 'Y' AND Has-Lower = 'Y'
                   AND Has-Number = 'Y'
                  AND Has-Special = 'Y'

                   MOVE 'Y' TO Done
               ELSE
                   DISPLAY "Password must contain at least:"
                   IF Has-Upper NOT = 'Y'
                       DISPLAY " - One UPPERCASE letter" END-IF
                   IF Has-Lower NOT = 'Y'
                       DISPLAY " - One lowercase letter" END-IF
                   IF Has-Number NOT = 'Y'
                       DISPLAY " - One digit" END-IF
                   IF Has-Special NOT = 'Y'
                       DISPLAY " - One special character (!@#$%)" END-IF
                   DISPLAY "Please try again..."
               END-IF
           END-IF
           END-PERFORM.
       Casher-Register.
           DISPLAY S10 S10 S10 S8
               DISPLAY A9 A2 "REGISTER PROGRAM"
               DISPLAY S10 S10 S10 S8
               DISPLAY "Register Casher Id:"
              ACCEPT Input-ID
               COMPUTE ID-LEN = FUNCTION LENGTH(FUNCTION TRIM(Input-ID))
               IF ID-LEN = 6
                   PERFORM Casher-Check
               ELSE
                   DISPLAY "Casher-ID has 6 ."
                   PERFORM Casher-Register
               END-IF.
       Casher-Check.
           OPEN INPUT CasherFile
           MOVE "N" TO EF
           MOVE "N" TO ACCESSS

           PERFORM UNTIL EF = "Y"
               READ CasherFile
                   AT END
                       MOVE "Y" TO EF
                   NOT AT END
                       IF Casher-ID = Input-ID
                           MOVE "Y" TO ACCESSS
                           MOVE "Y" TO EF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CasherFile.

           IF ACCESSS = "Y"
               DISPLAY "Already exists. Please add a new member!"
               PERFORM Casher-Register
           ELSE
               PERFORM Password-Check
               IF Done = "Y"
                   OPEN EXTEND CasherFile
                   MOVE Input-ID TO Casher-ID
                   MOVE Input-Password TO Casher-PW
                   WRITE Casher-Record
                   CLOSE CasherFile

                       DISPLAY "Register successful. Welcome, " Input-ID
               END-IF
           END-IF.
       admin.
           DISPLAY S10 S10 S10 S8
           DISPLAY A9 A2 "LOGIN PROGRAM"
           DISPLAY S10 S10 S10 S8
           DISPLAY "Enter Admin Id: "
           ACCEPT Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Input-Password


           OPEN INPUT AdminFile
           MOVE "N" TO ACCESSS
           MOVE "N" TO EF
           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
               NOT AT END
                   IF Admin-ID = Input-ID
                       AND
                      Admin-PW = Input-Password
                       MOVE "Y" TO EF
                       MOVE "Y" TO ACCESSS
                       EXIT PERFORM

                   END-IF

           END-PERFORM

           CLOSE AdminFile

           IF ACCESSS = "Y"
             DISPLAY H10 H10 H10 H8
             DISPLAY "Login successful. Welcome, " Admin-ID
             PERFORM Admin-Process
           ELSE
               DISPLAY "Login failed.Login try again!!!!!"
               PERFORM Admin
           END-IF.

       Admin-Process.
            MOVE "1" TO EOFP
            PERFORM UNTIL EOFP = "0"
               PERFORM Low-Stock-Noti
               DISPLAY S10 S10 S10 S5
               display "Item"
               DISPLAY H10 H10 H10 H5
               DISPLAY "1. View Item"
               DISPLAY "2. Add Item"
               DISPLAY "3. Update Item"
               DISPLAY "4. Delete Item"
               DISPLAY "5. Update Sale Price"
               DISPLAY "6. Best Sale Item"
               DISPLAY ""
               DISPLAY S10 S10 S10 S5
               display "Stock"
               DISPLAY H10 H10 H10 H5
               DISPLAY "7. Low Stock"
               MOVE Low-Stock-Count TO Low-Stock-Count-Format
               STRING "<<<Warning: Stock "
                       Low-Stock-Count-Format DELIMITED BY SIZE
                       ">>>"
                       INTO Message-Format
            DISPLAY ESC Blue-On Message-Format ESC Reset-Color

               DISPLAY "8. Fill Stock"
               DISPLAY "9. View History"
               DISPLAY ""
               DISPLAY S10 S10 S10 S5
               display "Discount"
               DISPLAY H10 H10 H10 H5
               DISPLAY "10. View Discount"
               DISPLAY "11. Add Discount"
               DISPLAY "12. Update Discount"
               DISPLAY "13. Delete Discount"
               DISPLAY ""
               DISPLAY S10 S10 S10 S5
               DISPLAY "0. Exit"
               DISPLAY S10 S10 S10 S5
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               EVALUATE User-Choice
                   WHEN "1"
                       PERFORM View-Item
                       MOVE "1" TO EOFP
                   WHEN "2"
                       PERFORM Add-Item
                       MOVE "1" TO EOFP
                   WHEN "3"
                       PERFORM Update-Item
                       MOVE "1" TO EOFP
                   WHEN "4"
                       PERFORM Delete-Item
                       MOVE "1" TO EOFP
                   WHEN "5"
                       PERFORM Update-Price
                       MOVE "1" TO EOFP
                   WHEN "6"
                       PERFORM Best-Sale-Item
                       MOVE "1" TO EOFP
                   WHEN "7"
                       PERFORM Low-Stock-Item
                       MOVE "1" TO EOFP
                   WHEN "8"
                       PERFORM Add-Stock
                       MOVE "1" TO EOFP
                   WHEN "9"
                       PERFORM View-History
                       MOVE "1" TO EOFP
                   WHEN "10"
                       PERFORM View-Discount
                       MOVE "1" TO EOFP
                   WHEN "11"
                       PERFORM Add-Discount
                       MOVE "1" TO EOFP
                   WHEN "12"
                       PERFORM Update-Discount
                       MOVE "1" TO EOFP
                   WHEN "13"
                       PERFORM Delete-Discount
                       MOVE "1" TO EOFP
                   WHEN "0"
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
      *>          DISPLAY "1.Home"
      *>          DISPLAY "0.Exit"
      *>          ACCEPT EOFP

      *>          MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM.
       sale.
           DISPLAY S10 S10 S10 S8
           DISPLAY A7 "Welcome to order program"
           DISPLAY S10 S10 S10 S8
           DISPLAY "Enter Casher Id: "
           ACCEPT Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Input-Password

           OPEN INPUT CasherFile
           MOVE "N" TO ACCESSS
           MOVE "N" TO EF
           PERFORM UNTIL EF = "Y"
               READ CasherFile
                   AT END
                       MOVE "Y" TO EF
               NOT AT END
                   IF Casher-ID = Input-ID
                       AND
                      Casher-PW = Input-Password
                       MOVE "Y" TO EF
                       MOVE "Y" TO ACCESSS
                       EXIT PERFORM
                   END-IF

           END-PERFORM

           CLOSE CasherFile

           IF ACCESSS = "Y"
             DISPLAY H10 H10 H10 H8
             DISPLAY "Login successful. Welcome, " Casher-ID
             PERFORM Sale-Process
           ELSE
               DISPLAY "Login failed.Login try again!!!!!"
               perform Sale
           END-IF.


       Sale-Process.
           MOVE "1" TO EOFP
            PERFORM UNTIL EOFP = "0"
               PERFORM Low-Stock-Noti
               DISPLAY S10 S10 S10 S5
               display "Casher"
               DISPLAY H10 H10 H10 H5

               DISPLAY "1. New Order"
               DISPLAY "2. Best Seller Item"
               DISPLAY "3. Low Stock Item"
               MOVE Low-Stock-Count TO Low-Stock-Count-Format
               STRING "<<<Warning: Stock "
                       FUNCTION TRIM(Low-Stock-Count-Format)
                       DELIMITED BY SIZE
                       ">>>"
                       INTO Message-Format
            DISPLAY ESC Blue-On Message-Format ESC Reset-Color
               DISPLAY "4. Pending Invoice"
               DISPLAY "5. Completed Invoice"
               DISPLAY "6. Discount"
               DISPLAY ""
               DISPLAY S10 S10 S10 S5
               DISPLAY "0. Exit"
               DISPLAY S10 S10 S10 S5
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               EVALUATE User-Choice
                   WHEN "1"
                       PERFORM Casher-Home
                       MOVE "1" TO EOFP
                   WHEN "2"
                       PERFORM Best-Sale-Item
                       MOVE "1" TO EOFP
                   WHEN "3"
                       PERFORM Low-Stock-Item
                       MOVE "1" TO EOFP
                   WHEN "4"
                       PERFORM Invoice-Process
                       MOVE "1" TO EOFP
                   WHEN "5"
                       PERFORM Completed-Invoice
                       MOVE "1" TO EOFP
                   WHEN "6"
                       PERFORM View-Discount
                       MOVE "1" TO EOFP
                   WHEN "0"
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
      *>          DISPLAY "1. Home"
      *>          DISPLAY "0. Exit"
      *>          ACCEPT EOFP

      *>          MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM.

       Casher-Home.
           PERFORM View-Item
           PERFORM Cart.
       Low-Stock-Item.
            OPEN I-O ItemFile
            DISPLAY S10 S10 S10 S10 S10
            DISPLAY "Low Stock Item Record"
            DISPLAY H10 H10 H10 H10 H10
            DISPLAY "Item-ID"  A5
                    "Name"     A5 A5 A7
                    "Category" A4
                    "Quantity" A5
                    "Price"

            DISPLAY H10 H10 H10 H10 H10
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Item-Qty < 10
                   DISPLAY Item-ID     A7
                           Item-Name   A1
                           Item-Category
                           Item-Qty    A9 A1
                           Item-Price
                   END-IF
            END-PERFORM
            DISPLAY S10 S10 S10 S10 S10
                CLOSE ItemFile.
       View-Item.
           OPEN I-O ItemFile
            DISPLAY S10 S10 S10 S10 S10 S10 S5
            DISPLAY "Item Record"
            DISPLAY H10 H10 H10 H10 H10 H10 H5
            DISPLAY "Item-ID"  A5
                    "Name"     A5 A5 A7
                    "Category" A4
                    "Quantity" A6
                    "Price"

            DISPLAY H10 H10 H10 H10 H10 H10 H5
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Item-Qty < 10
                       DISPLAY Item-ID         A7
                               Item-Name       A1
                               Item-Category
                               Item-Qty        A9 A1
                               Item-Price      A9 A1
                               "<<< Low Item"
                   ELSE
                       DISPLAY Item-ID         A7
                               Item-Name       A1
                               Item-Category
                               Item-Qty        A9 A2
                               Item-Price
                   END-IF
            END-PERFORM
            CLOSE ItemFile
            DISPLAY S10 S10 S10 S10 S10 S10 S5
            MOVE "1" TO EFC
            PERFORM UNTIL EFC = "0"
              DISPLAY "1.Find With Item ID"
              DISPLAY "2.Find With Item Name"
              DISPLAY "3.Find With Category"
              DISPLAY "0.Go Back"
              DISPLAY "Enter Choose Option:"
              ACCEPT User-Choice
              EVALUATE User-Choice
                   WHEN "1"
                       PERFORM Find-Id-Item
                       MOVE "1" TO EFC
                   WHEN "2"
                       PERFORM Find-Item
                       MOVE "1" TO EFC
                   WHEN "3"
                       PERFORM Find-Category-Item
                       MOVE "1" TO EFC
                   WHEN "0"
                       MOVE "0" TO EFC
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
                       MOVE "1" TO EFC
               END-EVALUATE
               MOVE FUNCTION UPPER-CASE(EFC) TO EFC
            END-PERFORM.
       Category-Item.
           OPEN INPUT CategoryFile
            DISPLAY "Category"
            DISPLAY H10 H10 H10 H10 H10 H2
            MOVE "N" TO EFC
            MOVE 1 TO IDX
            PERFORM UNTIL EFC = "Y"
               READ CategoryFile NEXT RECORD
               AT END
                   MOVE "Y" TO EFC
               NOT AT END
                   DISPLAY IDX ". " Category-Name
                   MOVE Category-Name TO Category-Index-Name(IDX)
                   ADD 1 TO IDX
            END-PERFORM
            DISPLAY "Enter Choose Option: "
            ACCEPT INPUT-IDX
            CLOSE CategoryFile.
       Add-Item.
           OPEN I-O ItemFile
            MOVE "1" TO EOF
            PERFORM UNTIL EOF = "0"

               PERFORM Category-Item

               DISPLAY "Enter Item ID: "
               ACCEPT Item-ID
               MOVE Item-ID TO RelativeKey
               DISPLAY "Enter Item Name: "
               ACCEPT Item-Name
               MOVE Category-Index-Name(INPUT-IDX) TO Item-Category
               DISPLAY "Enter Sale Price: "
               ACCEPT Item-Price
               MOVE 0 TO Item-Qty
               WRITE Item-Record INVALID KEY
               DISPLAY "Error: Unable to write record."
               END-WRITE

               IF File-Status = "00"
                   DISPLAY ESC Blue-On
                   "Item added successfully." ESC Reset-Color
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
               DISPLAY "1.Add more data"
               DISPLAY "0.Go Back"
               ACCEPT EOF
               MOVE FUNCTION UPPER-CASE(EOF) TO EOF
            END-PERFORM
                CLOSE ItemFile.
       Update-Item.
           OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Update: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY S10 S10 S10 S10 S10 S10 S5
                   DISPLAY "Item Record"
                   DISPLAY H10 H10 H10 H10 H10 H10 H5
                   DISPLAY "Item-ID"   A5
                            "Name"     A5 A5 A7
                            "Category" A4
                            "Quantity" A6
                            "Price"

                   DISPLAY H10 H10 H10 H10 H10 H10 H5
                   DISPLAY Item-ID         A7
                           Item-Name       A1
                           Item-Category
                           Item-Qty        A9 A2
                           Item-Price
                   DISPLAY S10 S10 S10 S10 S10 S10 S5
                   DISPLAY "Enter New Item Name: "
                   ACCEPT Item-Name
                   REWRITE Item-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Item updated successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF

            END-READ
                CLOSE ItemFile.
       Delete-Item.
           OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Delete: "
            ACCEPT RelativeKey

            DELETE ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY ESC Blue-On
                       "Item deleted successfully." ESC Reset-Color
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
            END-DELETE
                CLOSE ItemFile.
       Update-Price.
           OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Update: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY S10 S10 S10 S10 S10 S10 S5
                   DISPLAY "Item Record"
                   DISPLAY H10 H10 H10 H10 H10 H10 H5
                   DISPLAY "Item-ID"   A5
                            "Name"     A5 A5 A7
                            "Category" A4
                            "Quantity" A6
                            "Price"

                   DISPLAY H10 H10 H10 H10 H10 H10 H5
                   DISPLAY Item-ID         A7
                           Item-Name       A1
                           Item-Category
                           Item-Qty        A9 A2
                           Item-Price
                   DISPLAY S10 S10 S10 S10 S10 S10 S5
                   DISPLAY "Update Price: "
                   ACCEPT Item-Price
                   REWRITE Item-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Item updated successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF

            END-READ
                CLOSE ItemFile.
       Add-Stock.
            OPEN I-O ItemFile
            MOVE "1" TO EOF
            PERFORM UNTIL EOF = "0"

            DISPLAY "Enter Item-ID to Add: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                    DISPLAY S10 S10 S10 S10 S10 S10 S5
                    DISPLAY "Item Record"
                    DISPLAY H10 H10 H10 H10 H10 H10 H5
                    DISPLAY "Item-ID"  A5
                            "Name"     A5 A5 A7
                            "Category" A4
                            "Quantity" A6
                            "Price"

                    DISPLAY H10 H10 H10 H10 H10 H10 H5
                    DISPLAY Item-ID         A7
                            Item-Name       A1
                            Item-Category
                            Item-Qty        A9 A2
                            Item-Price
                    DISPLAY S10 S10 S10 S10 S10 S10 S5

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

                   MOVE Item-ID TO History-Item-ID
                   DISPLAY "Enter Date (YYYYMMDD): "
                   ACCEPT History-Date
                   DISPLAY "Enter Quantity: "
                   ACCEPT History-Quantity
                   DISPLAY "Enter Unit of Price: "
                   ACCEPT History-Unit-Of-Price

                   WRITE History-Record INVALID KEY
                   DISPLAY "Error: Unable to write record."
                   END-WRITE
                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Stock fill successfully." ESC Reset-Color
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
                       DISPLAY ESC Blue-On
                       "Stock updated successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF

                END-IF

            END-READ
            DISPLAY "1.Add more fill Stock"
            DISPLAY "0.Go Back"
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

            DISPLAY H10 H10 H10 H10 H10 H10 H7
            DISPLAY "History Record"
            DISPLAY S10 S10 S10 S10 S10 S10 S7
            DISPLAY "History-ID"   A5
                    "Item-ID"      A5
                    "Date"         A5 A5
                    "Quantity"     A5
                    "Unit of Price"
            DISPLAY S10 S10 S10 S10 S10 S10 S7
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ HistoryFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY History-ID              A9 A1
                           History-Item-ID         A7
                           History-Date            A4
                           History-Quantity        A8 A2
                           History-Unit-Of-Price
            END-PERFORM
            DISPLAY H10 H10 H10 H10 H10 H10 H7
            CLOSE HistoryFile.
       View-Discount.
            OPEN I-O DiscountFile
            DISPLAY H10 H10 H10 H10 H10 H2
            DISPLAY "Discount Record"
            DISPLAY S10 S10 S10 S10 S10 S2
            DISPLAY "Limit Amount"     A5
                    "Percent"          A5
                    "Start Date"       A5
                    "End Date"

            DISPLAY S10 S10 S10 S10 S10 S2
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ DiscountFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   DISPLAY Limit-Amount    A6 A5
                           Percent         A7
                           Start-Date      A7
                           End-Date

            END-PERFORM
            DISPLAY H10 H10 H10 H10 H10 H2
                CLOSE DiscountFile.
       Add-Discount.
            OPEN I-O DiscountFile
            IF File-Status = "35"
               DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT DiscountFile
               CLOSE DiscountFile
               OPEN I-O DiscountFile
            END-IF
            MOVE "1" TO EOF
            PERFORM UNTIL EOF = "0"
               DISPLAY "Enter Discount ID: "
               ACCEPT Discount-ID
               MOVE Discount-ID TO RelativeKey
               DISPLAY "Enter Limit Amount:: "
               ACCEPT Limit-Amount
               DISPLAY "Enter Percent: "
               ACCEPT Percent
               DISPLAY "Enter Start Date (YYYYMMDD): "
               ACCEPT Start-Date
               DISPLAY "Enter End Date (YYYYMMDD): "
               ACCEPT End-Date

               WRITE Discount-Record INVALID KEY
               DISPLAY "Error: Unable to write record."
               END-WRITE

               IF File-Status = "00"
                   DISPLAY ESC Blue-On
                       "Discount added successfully." ESC Reset-Color
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
               DISPLAY "1.Add more data"
               DISPLAY "0.Go Back"
               ACCEPT EOF
               MOVE FUNCTION UPPER-CASE(EOF) TO EOF
            END-PERFORM
                CLOSE DiscountFile.
       Update-Discount.
           OPEN I-O DiscountFile
            DISPLAY "Enter Discount-ID to Update: "
            ACCEPT RelativeKey

            READ DiscountFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                    DISPLAY H10 H10 H10 H10 H10 H2
                    DISPLAY "Discount Record"
                    DISPLAY S10 S10 S10 S10 S10 S2
                    DISPLAY "Limit Amount"     A5
                            "Percent"          A5
                            "Start Date"       A5
                            "End Date"

                    DISPLAY S10 S10 S10 S10 S10 S2
                    DISPLAY Limit-Amount    A6 A5
                            Percent         A7
                            Start-Date      A7
                            End-Date
                    DISPLAY H10 H10 H10 H10 H10 H2


                   DISPLAY "Enter Limit Amount: "
                   ACCEPT Limit-Amount
                   DISPLAY "Enter Percent: "
                   ACCEPT Percent
                   DISPLAY "Enter Start Date (YYYYMMDD): "
                   ACCEPT Start-Date
                   DISPLAY "Enter End Date (YYYYMMDD): "
                   ACCEPT End-Date

                   REWRITE Discount-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Discount updated successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF

            END-READ
                CLOSE DiscountFile.
       Delete-Discount.
           OPEN I-O DiscountFile
            DISPLAY "Enter Discount-ID to Delete: "
            ACCEPT RelativeKey

            DELETE DiscountFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY ESC Blue-On
                       "Discount deleted successfully." ESC Reset-Color
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
            END-DELETE
                CLOSE DiscountFile.
       Cart.
            MOVE "1" TO EOFP
            PERFORM UNTIL EOFP = "0"
              PERFORM Buy-Cart
              DISPLAY "1.Add Cart With Item ID"
              DISPLAY "2.Add Cart With Item Name"
              DISPLAY "3.Add Cart With Category"
              DISPLAY "4.Buy"
              DISPLAY "5.Update Quantity"
              DISPLAY "6.Delete Cart"
              DISPLAY "0.Go Back"
              DISPLAY "Enter Choose Option:"
              ACCEPT User-Choice
              EVALUATE User-Choice
                   WHEN "1"
                       PERFORM Add-Cart
                       MOVE "1" TO EOFP
                   WHEN "2"
                       PERFORM Find-Item
                       PERFORM Add-Cart
                       MOVE "1" TO EOFP
                   WHEN "3"
                       PERFORM Find-Category-Item
                       IF Category-Count > 0
                           PERFORM Add-Cart
                       END-IF
                       MOVE "1" TO EOFP
                   WHEN "4"
                       DISPLAY "Enter Customer Name: "
                       ACCEPT Customer-Name
                       PERFORM Buy-Confirm
                       MOVE "0" TO EOFP
                   WHEN "5"
                       PERFORM Update-Cart-Quantity
                       MOVE "1" TO EOFP
                   WHEN "6"
                       PERFORM Delete-Cart
                       MOVE "1" TO EOFP
                   WHEN "0"
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
                       MOVE "1" TO EOFP
               END-EVALUATE
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM.
       Find-Id-Item.
            OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Find: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY S10 S10 S10 S10 S10 S10 S5
                   DISPLAY "Item Record"
                   DISPLAY H10 H10 H10 H10 H10 H10 H5
                   DISPLAY "Item-ID"   A5
                            "Name"     A5 A5 A7
                            "Category" A4
                            "Quantity" A6
                            "Price"

                   DISPLAY H10 H10 H10 H10 H10 H10 H5
                   DISPLAY Item-ID         A7
                           Item-Name       A1
                           Item-Category
                           Item-Qty        A9 A2
                           Item-Price
                   DISPLAY S10 S10 S10 S10 S10 S10 S5
                END-IF

            END-READ
            CLOSE ItemFile.
       Find-Item.
           OPEN I-O ItemFile

           DISPLAY "Enter Item Name: "
           ACCEPT Input-Item-Name
            DISPLAY S10 S10 S10 S10 S10 S10 S5
            DISPLAY "Item Record"
            DISPLAY H10 H10 H10 H10 H10 H10 H5
            DISPLAY "Item-ID"  A5
                    "Name"     A5 A5 A7
                    "Category" A4
                    "Quantity" A6
                    "Price"

            DISPLAY H10 H10 H10 H10 H10 H10 H5
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF FUNCTION LOWER-CASE(Item-Name) =
                      FUNCTION LOWER-CASE(Input-Item-Name)
                       DISPLAY Item-ID         A7
                               Item-Name       A1
                               Item-Category
                               Item-Qty        A9 A2
                               Item-Price
                   END-IF
            END-PERFORM
            DISPLAY S10 S10 S10 S10 S10 S10 S5
            CLOSE ItemFile.
       Find-Category-Item.
            PERFORM Category-Item
            OPEN I-O ItemFile
            MOVE "N" TO EOF
            MOVE 0 TO Category-Count
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Item-Category = Category-Index-Name(INPUT-IDX)
                       ADD 1 TO Category-Count
                   END-IF
            END-PERFORM
            CLOSE ItemFile
            OPEN I-O ItemFile
            IF Category-Count > 0
                DISPLAY S10 S10 S10 S10 S10 S10 S5
                DISPLAY "Item Record"
                DISPLAY H10 H10 H10 H10 H10 H10 H5
                DISPLAY "Item-ID"  A5
                        "Name"     A5 A5 A7
                        "Category" A4
                        "Quantity" A6
                        "Price"

                DISPLAY H10 H10 H10 H10 H10 H10 H5
                MOVE "N" TO EOF
                PERFORM UNTIL EOF = "Y"
                   READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       IF Item-Category = Category-Index-Name(INPUT-IDX)
                           DISPLAY Item-ID         A7
                                   Item-Name       A1
                                   Item-Category
                                   Item-Qty        A9 A1
                                   Item-Price      A9 A1
                       END-IF
                END-PERFORM
                DISPLAY S10 S10 S10 S10 S10 S10 S5
            ELSE
                DISPLAY S10 S10 S10
                DISPLAY ESC Blue-On
                       "This Category Not Found Item." ESC Reset-Color
                DISPLAY S10 S10 S10
            END-IF
            CLOSE ItemFile.
       Add-Cart.
            OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Add: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                    MOVE 0 TO Cart-Total-Quantity
                    OPEN I-O CartFile
                    IF File-Status = "35"
                        DISPLAY "File does not exist. Creating file..."
                        OPEN OUTPUT CartFile
                        CLOSE CartFile
                        OPEN I-O CartFile
                    END-IF
                    MOVE "N" TO EOF
                    PERFORM UNTIL EOF = "Y"
                    READ CartFile NEXT RECORD
                       AT END
                           MOVE "Y" TO EOF
                       NOT AT END
                           IF Item-ID = Cart-Item-ID
                               COMPUTE Cart-Total-Quantity =
                               Cart-Total-Quantity + Cart-Quantity
                           END-IF
                    END-PERFORM

                   OPEN I-O SaleCartFile
                    IF File-Status = "35"
                        DISPLAY "File does not exist. Creating file..."
                        OPEN OUTPUT SaleCartFile
                        CLOSE SaleCartFile
                        OPEN I-O SaleCartFile
                    END-IF
                    IF Sale-Cart-ID = SPACES
                        MOVE 1 TO Sale-Cart-ID
                    ELSE
                        MOVE "N" TO EOF
                        PERFORM UNTIL EOF = "Y"
                           READ SaleCartFile NEXT RECORD
                           AT END
                               MOVE "Y" TO EOF
                           NOT AT END
                               IF Sale-Cart-ID > MAX-ID
                                MOVE Sale-Cart-ID TO MAX-ID
                               END-IF
                        END-PERFORM

                   END-IF
                   CLOSE SaleCartFile

                   ADD 1 TO MAX-ID
                   MOVE MAX-ID TO Cart-ID

                   DISPLAY "Item Name: " Item-Name
                   MOVE Item-ID TO Cart-Item-ID
                   DISPLAY "Enter Quantity: "
                   ACCEPT Cart-Quantity
                   MOVE Item-Price TO Cart-Unit-Of-Price

                   COMPUTE Cart-Total-Quantity =
                               Cart-Total-Quantity + Cart-Quantity
                   IF Cart-Total-Quantity > Item-Qty
                       DISPLAY Item-Name"is allow: " Item-Qty
                   ELSE
                   WRITE Cart-Record INVALID KEY
                   DISPLAY "Error: Unable to write record."
                   END-WRITE
                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Added Cart successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                   END-IF

                   CLOSE CartFile
                END-IF
            END-READ
            CLOSE ItemFile.

       Buy-Cart.
            OPEN I-O CartFile
            IF File-Status = "35"
               DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT CartFile
               CLOSE CartFile
               OPEN I-O CartFile
            END-IF
            MOVE "N" TO EOF
            MOVE "N" TO Cart-Flag
            PERFORM UNTIL EOF = "Y"
               READ CartFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE "Y" TO Cart-Flag

            END-PERFORM
            CLOSE CartFile
            OPEN I-O CartFile
            IF Cart-Flag = "Y"
                DISPLAY S10 S10 S10 S10 S10 S5
                DISPLAY "Cart Record"
                DISPLAY H10 H10 H10 H10 H10 H5
                DISPLAY "Cart ID"  A5
                        "Item ID"  A5 A5
                        "Quantity" A8
                        "Price"

                DISPLAY H10 H10 H10 H10 H10 H5
                MOVE "N" TO EOF
                MOVE 0 TO Total
                PERFORM UNTIL EOF = "Y"
                   READ CartFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       DISPLAY Cart-ID             A7
                               Cart-Item-ID        A7 A5
                               Cart-Quantity       A9 A4
                               Cart-Unit-Of-Price
                       COMPUTE Total = Total +
                               (Cart-Quantity * Cart-Unit-Of-Price)
                    END-PERFORM
                DISPLAY H10 H10 H10 H10 H10 H5
                DISPLAY "Total: " Total
                DISPLAY S10 S10 S10 S10 S10 S5
            END-IF
            CLOSE CartFile.
       Update-Cart-Quantity.
            OPEN I-O CartFile
            DISPLAY "Enter Cart-ID to Update: "
            ACCEPT Cart-ID

            READ CartFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY "Enter Quantity: "
                   ACCEPT Cart-Quantity

                   REWRITE Cart-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Cart updated successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF

            END-READ
            CLOSE CartFile.
       Delete-Cart.
            OPEN I-O CartFile
            DISPLAY "Enter Cart-ID to Delete: "
            ACCEPT Cart-ID

            DELETE CartFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY ESC Blue-On
                       "Cart deleted successfully." ESC Reset-Color
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
            END-DELETE
            CLOSE CartFile.
       Buy-Confirm.
           OPEN I-O CartFile

           MOVE "N" TO EOF
           MOVE 1 TO IDX
           MOVE 0 TO Total
           PERFORM UNTIL EOF = "Y"
               READ CartFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END

      *>      Update Quantity

                OPEN I-O ItemFile
                MOVE Cart-Item-ID TO RelativeKey
                READ ItemFile INVALID KEY
                   DISPLAY "Error: Record not found."
                NOT INVALID KEY
                    IF File-Status = "00"
                       COMPUTE Update-Quantity =
                       Item-Qty - Cart-Quantity
                       MOVE Update-Quantity TO Item-Qty
                       REWRITE Item-Record INVALID KEY
                       DISPLAY "Error: Unable to rewrite record."
                       END-REWRITE

                       IF File-Status = "00"
                           DISPLAY ESC Blue-On
                           "Item Quantity updated successfully."
                           ESC Reset-Color
                       ELSE
                           DISPLAY "File Status: " File-Status
                       END-IF
                    END-IF
                END-READ
                CLOSE ItemFile

                OPEN I-O SaleCartFile
                IF File-Status = "35"
                   DISPLAY "File does not exist. Creating file..."
                   OPEN OUTPUT SaleCartFile
                   CLOSE SaleCartFile
                   OPEN I-O SaleCartFile
                END-IF

                MOVE Cart-ID TO Sale-Cart-ID
                MOVE Input-ID TO Invoice-Casher-ID
                MOVE Cart-Item-ID TO Sale-Cart-Item-ID
                MOVE Cart-Quantity TO Sale-Cart-Quantity
                MOVE Cart-Unit-Of-Price TO Sale-Cart-Unit-Of-Price
                WRITE Sale-Cart-Record
                COMPUTE Total= Total+
                   (Cart-Quantity*Cart-Unit-Of-Price)

                MOVE Cart-ID TO Temp-ID(IDX)
                ADD 1 TO IDX
                CLOSE SaleCartFile
                DELETE CartFile
            END-PERFORM

            OPEN I-O InvoiceFile
            IF File-Status = "35"
               DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT InvoiceFile
               CLOSE InvoiceFile
               OPEN I-O InvoiceFile
            END-IF
            IF Invoice-ID = SPACES
               MOVE 1 TO Invoice-ID
            ELSE
               MOVE "N" TO EOF
               PERFORM UNTIL EOF = "Y"
                   READ InvoiceFile NEXT RECORD
                       AT END
                           MOVE "Y" TO EOF
                       NOT AT END
                           IF Invoice-ID > MAX-ID
                               MOVE Invoice-ID TO MAX-ID
                           END-IF
               END-PERFORM

            END-IF

            ACCEPT Invoice-Date FROM DATE YYYYMMDD.
            ACCEPT WS-TIME-RAW FROM TIME.

            MOVE WS-TIME-RAW(1:2) TO WS-HH
            MOVE WS-TIME-RAW(3:2) TO WS-MM
            MOVE WS-TIME-RAW(5:2) TO WS-SS

            IF WS-HH >= 12
               MOVE 'PM' TO WS-AMPM
               SUBTRACT WS-HH FROM 12 GIVING WS-HH
            ELSE
               MOVE 'AM' TO WS-AMPM

            END-IF

            STRING
            WS-HH DELIMITED BY SIZE
            ':'       DELIMITED BY SIZE
            WS-MM DELIMITED BY SIZE
            ' '       DELIMITED BY SIZE
            WS-AMPM   DELIMITED BY SIZE
            INTO Invoice-Time

            OPEN I-O DiscountFile
            IF File-Status = "35"
               DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT DiscountFile
               CLOSE DiscountFile
               OPEN I-O DiscountFile
            END-IF
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ DiscountFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Invoice-Date >= Start-Date AND
                      Invoice-Date <= End-Date
                      IF  Total>= Limit-Amount
                           COMPUTE Discount-Price =
                           Total * (Percent/100)
                           EXIT PERFORM
                       END-IF
                   END-IF
            END-PERFORM
            CLOSE DiscountFile

            ADD 1 TO MAX-ID
            MOVE MAX-ID TO Invoice-ID
            MOVE Input-ID TO Invoice-Casher-ID
            MOVE Customer-Name TO Invoice-Customer-Name
            MOVE Temp-ID-List TO Item-ID-List
            MOVE Total TO Total-Amount
            MOVE Discount-Price TO Discount
            COMPUTE Final-Total-Price =
            Total-Amount - Discount
            MOVE Final-Total-Price TO Final-Amount
            MOVE "Pending" TO Invoice-Status
            WRITE Invoice-Record

            DISPLAY ESC Blue-On
                       "Invoice added successfully." ESC Reset-Color

            CLOSE InvoiceFile
            CLOSE CartFile
            PERFORM View-Invocie
            .

       View-Invocie.
            OPEN I-O InvoiceFile
            OPEN I-O SaleCartFile
            MOVE MAX-ID TO Invoice-ID
            READ InvoiceFile INVALID KEY
                DISPLAY "Error: Record not found."
            NOT INVALID KEY


            MOVE Invoice-Date(1:4) TO WS-YEAR
            MOVE Invoice-Date(5:2) TO WS-MONTH
            MOVE Invoice-Date(7:2) TO WS-DAY

            STRING
               WS-YEAR DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               WS-MONTH DELIMITED BY SIZE
               "-" DELIMITED BY SIZE
               WS-DAY DELIMITED BY SIZE
               INTO WS-DATE-OUT

           DISPLAY "Formatted date: " WS-DATE-OUT

                DISPLAY S10 S10 S10 S10 S3
                DISPLAY H10 H10 H10 H10 H3
                DISPLAY "Casher ID     : " Invoice-Casher-ID
                DISPLAY H10 H10 H5
                DISPLAY "Invoice-ID    : " Invoice-ID
                DISPLAY "Customer-Name : " Invoice-Customer-Name
                DISPLAY "Date          : " WS-DATE-OUT
                DISPLAY "Time          : " Invoice-Time
                DISPLAY "Status        : " Invoice-Status
                DISPLAY H10 H10 H5
                DISPLAY "Item-ID"  A8
                        "Quantity" A7
                        "Unit-Of-Price"
                DISPLAY H10 H10 H10 H10 H3
                PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                    IF Invoice-Sale-Cart-ID(IDX) NUMERIC
                       MOVE Invoice-Sale-Cart-ID(IDX) TO Sale-Cart-ID
                       READ SaleCartFile
                       NOT INVALID KEY
                           DISPLAY Sale-Cart-Item-ID   A9 A1
                                   Sale-Cart-Quantity  A9 A3
                                   Sale-Cart-Unit-Of-Price
                       END-READ
                    END-IF
                END-PERFORM
                DISPLAY H10 H10 H10 H10 H3
                DISPLAY "Total-Amount  : " Total-Amount
                DISPLAY "Discount      : " Discount
                DISPLAY "Final-Amount  : " Final-Amount
                DISPLAY S10 S10 S10 S10 S3
                DISPLAY S10 S10 S10 S10 S3
            END-READ

            CLOSE SaleCartFile
            CLOSE InvoiceFile.
       Pending-Invoice.
            OPEN I-O InvoiceFile
            OPEN I-O SaleCartFile
            DISPLAY S10 S10 S10 S10 S3
            DISPLAY "Pending Invoice Record"
            DISPLAY H10 H10 H10 H10 H3
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ InvoiceFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Invoice-Status = "Pending"
                      AND Invoice-Casher-ID = Input-ID
                       MOVE Invoice-Date(1:4) TO WS-YEAR
                        MOVE Invoice-Date(5:2) TO WS-MONTH
                        MOVE Invoice-Date(7:2) TO WS-DAY
                        STRING
                           WS-YEAR DELIMITED BY SIZE
                           "-" DELIMITED BY SIZE
                           WS-MONTH DELIMITED BY SIZE
                           "-" DELIMITED BY SIZE
                           WS-DAY DELIMITED BY SIZE
                           INTO WS-DATE-OUT
                       DISPLAY "Casher ID      : " Invoice-Casher-ID
                       DISPLAY H10 H10 H5
                       DISPLAY "Invoice-ID     : " Invoice-ID
                       DISPLAY "Customer-Name  : " Invoice-Customer-Name
                       DISPLAY "Date           : " WS-DATE-OUT
                       DISPLAY "Time           : " Invoice-Time
                       DISPLAY "Status         : " Invoice-Status
                       DISPLAY H10 H10 H5
                       DISPLAY "Item-ID" A8
                               "Quantity" A7
                               "Unit-Of-Price"
                       DISPLAY H10 H10 H10 H10 H3
                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                           IF Invoice-Sale-Cart-ID(IDX) NUMERIC
                               MOVE Invoice-Sale-Cart-ID(IDX) TO
                               Sale-Cart-ID
                               READ SaleCartFile
                               NOT INVALID KEY
                                   DISPLAY Sale-Cart-Item-ID   A9 A1
                                           Sale-Cart-Quantity  A9 A3
                                           Sale-Cart-Unit-Of-Price
                               END-READ
                           END-IF
                        END-PERFORM
                       DISPLAY H10 H10 H10 H10 H3
                       DISPLAY "Total-Amount   : " Total-Amount
                       DISPLAY "Discount       : " Discount
                       DISPLAY "Final-Amount   : " Final-Amount
                       DISPLAY S10 S10 S10 S10 S3
                       DISPLAY S10 S10 S10 S10 S3
                    END-IF
            END-PERFORM
            CLOSE SaleCartFile
            CLOSE InvoiceFile.
       Invoice-Process.
            MOVE "1" TO EOFP
            PERFORM UNTIL EOFP = "0"
               PERFORM Pending-Invoice
               DISPLAY "1. Invoice Confirm"
               DISPLAY "0. Go Back"
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               EVALUATE User-Choice
                   WHEN "1"
                       PERFORM Invoice-Confirm
                       MOVE "1" TO EOFP
                   WHEN "0"
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
                       MOVE "1" TO EOFP
               END-EVALUATE
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM.


       Completed-Invoice.
           OPEN I-O InvoiceFile
            OPEN I-O SaleCartFile
            DISPLAY S10 S10 S10 S10 S3
            DISPLAY "Completed Invoice Record"
            DISPLAY H10 H10 H10 H10 H3
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ InvoiceFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Invoice-Status = "Completed"
                      AND Invoice-Casher-ID = Input-ID
                        MOVE Invoice-Date(1:4) TO WS-YEAR
                        MOVE Invoice-Date(5:2) TO WS-MONTH
                        MOVE Invoice-Date(7:2) TO WS-DAY
                        STRING
                           WS-YEAR DELIMITED BY SIZE
                           "-" DELIMITED BY SIZE
                           WS-MONTH DELIMITED BY SIZE
                           "-" DELIMITED BY SIZE
                           WS-DAY DELIMITED BY SIZE
                           INTO WS-DATE-OUT
                       DISPLAY "Casher ID      : " Invoice-Casher-ID
                       DISPLAY H10 H10 H5
                       DISPLAY "Invoice-ID     : " Invoice-ID
                       DISPLAY "Customer-Name  : " Invoice-Customer-Name
                       DISPLAY "Date           : " WS-DATE-OUT
                       DISPLAY "Time           : " Invoice-Time
                       DISPLAY "Status         : " Invoice-Status
                       DISPLAY H10 H10 H5
                       DISPLAY "Item-ID" A8
                               "Quantity" A7
                               "Unit-Of-Price"
                       DISPLAY H10 H10 H10 H10 H3
                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                           IF Invoice-Sale-Cart-ID(IDX) NUMERIC
                               MOVE Invoice-Sale-Cart-ID(IDX) TO
                               Sale-Cart-ID
                               READ SaleCartFile
                               NOT INVALID KEY
                                   DISPLAY Sale-Cart-Item-ID   A9 A1
                                           Sale-Cart-Quantity  A9 A3
                                           Sale-Cart-Unit-Of-Price
                               END-READ
                           END-IF
                        END-PERFORM
                       DISPLAY H10 H10 H10 H10 H3
                       DISPLAY "Total-Amount   : " Total-Amount
                       DISPLAY "Discount       : " Discount
                       DISPLAY "Final-Amount   : " Final-Amount
                       DISPLAY S10 S10 S10 S10 S3

                       DISPLAY S10 S10 S10 S10 S3
                    END-IF
            END-PERFORM
            CLOSE SaleCartFile
            CLOSE InvoiceFile.
       Best-Sale-Item.
            OPEN INPUT ItemFile

            DISPLAY S10 S10 S10 S10 S10 S10 S3
            DISPLAY "Best Sale Item Record"
            DISPLAY H10 H10 H10 H10 H10 H10 H3
            DISPLAY "Item-ID"  A5
                    "Name"     A5 A5 A7
                    "Category"
                    "Percent"  A5
                    "Price"

            DISPLAY H10 H10 H10 H10 H10 H10 H3
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               MOVE 0 TO All-Qty
               MOVE 0 TO Each-Qty
               MOVE 0 TO Best-Sell
               READ ItemFile
               AT END MOVE "Y" TO EOF
               NOT AT END
                   OPEN INPUT SaleCartFile
                   MOVE "N" TO EOFB
                   PERFORM UNTIL EOFB = "Y"
                      READ SaleCartFile
                      AT END MOVE "Y" TO EOFB
                      NOT AT END
                          ADD Sale-Cart-Quantity TO All-Qty
                          IF Item-ID = Sale-Cart-Item-ID
                               ADD Sale-Cart-Quantity TO Each-Qty
                          END-IF
                      END-READ
                   END-PERFORM
                   CLOSE SaleCartFile
                     IF All-Qty NOT = 0
                          COMPUTE Best-Sell =
                          (Each-Qty * 100.00) / All-Qty
                     ELSE
                          MOVE 0 TO Best-Sell
                     END-IF
                     IF Best-Sell>30.0
                         DISPLAY Item-ID       A7
                                 Item-Name     A1
                                 Item-Category
                                 Best-Sell     A6
                                 Item-Price
                     END-IF
                END-READ
            END-PERFORM
            DISPLAY S10 S10 S10 S10 S10 S10 S3
            CLOSE ItemFile.
       Low-Stock-Noti.
            OPEN I-O ItemFile
            IF File-Status = "35"
               DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT ItemFile
               CLOSE ItemFile
               OPEN I-O ItemFile
            END-IF
            MOVE "N" TO EOF
            MOVE 0 TO Low-Stock-Count
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   IF Item-Qty < 10
                       ADD 1 TO Low-Stock-Count
                   END-IF
            END-PERFORM
                CLOSE ItemFile.
       Invoice-Confirm.
            OPEN I-O InvoiceFile
            DISPLAY "Enter Invoice-ID to Comfirm Invoice: "
            ACCEPT Invoice-ID

            READ InvoiceFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY "Invoice ID: " Invoice-ID
                   MOVE "Completed" TO Invoice-Status
                   REWRITE Invoice-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY ESC Blue-On
                       "Invoice Completed successfully."
                       ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF

            END-READ
            CLOSE InvoiceFile
            .
       END PROGRAM Item.

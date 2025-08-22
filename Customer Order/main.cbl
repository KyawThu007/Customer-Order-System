      *> ******************************************************************
      *> * Author:
      *> * Date:
      *> * Purpose:
      *> * Tectonics: cobc
      *> ******************************************************************
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
           05 Admin-ID                 PIC X(6).
           05 Admin-PW                 PIC X(6).
       FD CasherFile.
       01 Casher-Record.
           05 Casher-ID                PIC X(6).
           05 Casher-PW                PIC X(6).
       FD CategoryFile.
       01 Category-Record.
           05 Category-Name            PIC X(12).
       FD ItemFile.
       01 Item-Record.
           05 Item-ID                  PIC 9(5).
           05 Item-Name                PIC X(20).
           05 Item-Category            PIC X(12).
           05 Item-Qty                 PIC 9(3).
           05 Item-Price               PIC 9(5).
       FD HistoryFile.
       01 History-Record.
           05 History-ID               PIC 9(5)    VALUE ZEROES.
           05 History-Item-ID          PIC 9(5).
           05 History-Date             PIC X(10).
           05 History-Quantity         PIC 9(3).
           05 History-Unit-Of-Price    PIC 9(10).
       FD DiscountFile.
       01 Discount-Record.
           05 Discount-ID              PIC 9(5).
           05 Limit-Amount             PIC 9(6).
           05 Percent                  PIC 9(2)V99.
           05 Start-Date               PIC X(10).
           05 End-Date                 PIC X(10).
       FD CartFile.
       01 Cart-Record.
           05 Cart-ID                  PIC 9(5)    VALUE ZEROES.
           05 Cart-Item-ID             PIC 9(5).
           05 Cart-Quantity            PIC 9(3).
           05 Cart-Unit-Of-Price       PIC 9(10).
       FD SaleCartFile.
       01 Sale-Cart-Record.
           05 Sale-Cart-ID             PIC 9(5)    VALUE ZEROES.
           05 Sale-Cart-Item-ID        PIC 9(5).
           05 Sale-Cart-Quantity       PIC 9(3).
           05 Sale-Cart-Unit-Of-Price  PIC 9(10).
           05 Sale-Date                PIC 9(8).
       FD InvoiceFile.
       01 Invoice-Record.
           05 Invoice-ID               PIC 9(5).
           05 Invoice-Casher-ID        PIC X(6).
           05 Invoice-Customer-Name    PIC X(20).
           05 Item-ID-List.
              10 Invoice-Sale-Cart-ID  OCCURS      5 TIMES
                                       PIC 9(5)    VALUE ZEROES.
           05 Total-Amount             PIC 9(10).
           05 Discount                 PIC 9(8).
           05 Final-Amount             PIC 9(10).
           05 Tax                      PIC 9(3)    VALUE 500.
           05 Pay-Amount               PIC 9(10).
           05 Invoice-Date             PIC 9(8).
           05 Invoice-Time             PIC X(11).
           05 Invoice-Status           PIC A(9).
       WORKING-STORAGE SECTION.
       01 Blank-Space.
           05 A1                       PIC X(1)    VALUE SPACE.
           05 A2                       PIC X(2)    VALUE SPACE.
           05 A3                       PIC X(3)    VALUE SPACE.
           05 A4                       PIC X(4)    VALUE SPACE.
           05 A5                       PIC X(5)    VALUE SPACE.
           05 A6                       PIC X(6)    VALUE SPACE.
           05 A7                       PIC X(7)    VALUE SPACE.
           05 A8                       PIC X(8)    VALUE SPACE.
           05 A9                       PIC X(9)    VALUE SPACE.
           05 A10                      PIC X(10)   VALUE SPACE.
       01 Blank-Hipan.
           05 H1                       PIC X(1)    VALUE "-".
           05 H2                       PIC X(2)    VALUE "--".
           05 H3                       PIC X(3)    VALUE "---".
           05 H4                       PIC X(4)    VALUE "----".
           05 H5                       PIC X(5)    VALUE "-----".
           05 H6                       PIC X(6)    VALUE "------".
           05 H7                       PIC X(7)    VALUE "-------".
           05 H8                       PIC X(8)    VALUE "--------".
           05 H9                       PIC X(9)    VALUE "---------".
           05 H10                      PIC X(10)   VALUE "----------".
       01 Equal-Hipan.
           05 e5                       PIC X(5)    VALUE "=====".
           05 e10                      PIC X(10)   VALUE "==========".
       01 Date-Time-Format.
           05  WS-TIME-RAW             PIC 9(8).
           05  WS-HH                   PIC 99.
           05  WS-MM                   PIC 99.
           05  WS-SS                   PIC 99.
           05  WS-TOTAL-MINUTES        PIC 9999.
           05  WS-AMPM                 PIC X(2).
           05  WS-DISPLAY-HH           PIC 99.
           05  WS-DISPLAY-MM           PIC 99.
       01 Date-Format.
           05 WS-YEAR                  PIC 9(4).
           05 WS-MONTH                 PIC 99.
           05 WS-DAY                   PIC 99.
           01 WS-DATE-OUT              PIC X(10).
       01 Waring-Noti.
           05 ESC                      PIC X       VALUE X'1B'.
           05 Blue-On                  PIC X(5)    VALUE "[34m".
           05 Reset-Color              PIC X(4)    VALUE "[0m".
           05 Red-On                   PIC X(5)    VALUE "[31m".
           05 Cyan-On                  PIC X(5)    VALUE "[36m".
           05 Yellow-On                PIC X(5)    VALUE "[33m".
           05 Green-On                 PIC X(5)    VALUE "[32m".
           05 Pink-On                  PIC X(5)    VALUE "[35m".
           05 Bold-On                  PIC X(4)    VALUE X"1B" & "[1m".
           05 Bold-Off                 PIC X(4)    VALUE X"1B" & "[0m".
       01 Register-Check.
           05 ID-LEN                   PIC 9       VALUE 0.
           05 PW-LEN                   PIC 9       VALUE 0.
           05 Password-Length          PIC 9(2).
           05 Has-Upper                PIC X       VALUE 'N'.
           05 Has-Lower                PIC X       VALUE 'N'.
           05 Has-Number               PIC X       VALUE 'N'.
           05 Has-Special              PIC X       VALUE 'N'.
           05 I                        PIC 9(2).
           05 Curr-Char                PIC X.
           05 Done                     PIC X       VALUE 'N'.
       01 Permission.
           05 Admin-permission         PIC X       VALUE "N".
           05 Casher-permission        PIC X       VALUE "N".
       01 Category-Index-List.
           05 Category-Index-Name      OCCURS      10 TIMES
                                       PIC X(12)   VALUE SPACE.
       01 Temp-ID-List.
           05 Temp-ID                  OCCURS      5 TIMES
                                       PIC 9(5)    VALUE ZEROES.
       01 WS-RESULT                    PIC 9(4)    VALUE ZERO.
       01 File-Status                  PIC XX      VALUE "00".
       01 EOF                          PIC X       VALUE "Y".
       01 EOFP                         PIC X       VALUE "Y".
       01 User-Choice                  PIC X(2).
       01 RelativeKey                  PIC 9(5).
       01 role                         PIC X(1).
       01 EF                           PIC X(1)    VALUE "Y".
       01 End-Program                  PIC X(1).
       01 Input-ID                     PIC X(6).
       01 Input-Password               PIC X(6).
       01 Update-Quantity              PIC 9(3).
       01 Discount-Price               PIC 9(10)V99.
       01 Final-Total-Price            PIC 9(10).
       01 Final-Pay-Price              PIC 9(10).
       01 MAX-ID                       PIC 9(5)    VALUE ZERO.
       01 INPUT-IDX                    PIC 9(2).
       01 EFC                          PIC X       VALUE "N".
       01 Category-Count               PIC 9(3).
       01 Flag                         PIC X       VALUE "N".
       01 Cart-Flag                    PIC X       VALUE "N".
       01 Cart-Total-Quantity          PIC 9(5).
       01 IDX                          PIC 9.
       01 Total                        PIC 9(10).
       01 Total-Display                PIC ZZZZZZZ9.
       01 Best-Sell                    PIC 9(3)V99.
       01 Best-Sell-Qty                PIC 9(8).
       01 Best-Sell-Qty-Display        PIC ZZZZZZZ9.
       01 All-Qty                      PIC 9(8)V99.
       01 Each-Qty                     PIC 9(8)V99.
       01 EOFB                         PIC X       VALUE "N".
       01 Low-Stock-Count              PIC 9(3).
       01 Low-Stock-Count-Format       PIC ZZ9.
       01 Message-Format               PIC X(80).
       01 Customer-Name                PIC X(80).
       01 Input-Start-Date             PIC X(10).
       01 Input-End-Date               PIC X(10).
       01 a                            PIC X(12)   VALUE SPACE.
       01 b                            PIC X(5)    VALUE SPACE.
       01 option                       PIC 9.
       01 ds                           PIC X(80).
       01 alert                        PIC X(80).
       01 loginfail                    PIC X(80).
       01 Found                        PIC X       VALUE "N".
       01 Input-Item-Id                PIC 9(5).
       01 Input-Item-Name              PIC X(20).
       01 Input-Item-Category          PIC X(12).
       01 Input-Item-Price             PIC 9(5)V99.
       01 DT-STR-LEN                   PIC 9.
       01 DT-END-LEN                   PIC 9.
       01 History-Date-LEN             PIC 9.
       01 Start-Date-Numeric           PIC 9(08).
       01 Valid-Start                  PIC X       VALUE "N".
       01 Valid-End                    PIC X       VALUE "N".
       01 SS                           PIC X       VALUE "N".
       01 CC                           PIC 9.
       01 DD                           PIC 9.
       01 Start-Date-Num               PIC 9(8).
       01 End-Date-Num                 PIC 9(8).
       01 Valid-Range                  PIC X       VALUE "N".
       01 Buy-Sale                     PIC ZZZZ9.
       01 History-Num                  PIC 9(8).
       01 Delete-Permission            PIC X.
       01 Item-Record-Display.
           05 Item-ID-Display                  PIC ZZZZ9.
           05 Item-Qty-Display                 PIC ZZ9.
           05 Item-Price-Display               PIC ZZZZZZZZZ9.
       01 History-Record-Display.
           05 History-ID-Display               PIC ZZZZ9.
           05 History-Item-ID-Display          PIC ZZZZ9.
           05 History-Quantity-Display         PIC ZZ9.
           05 History-Unit-Of-Price-Display    PIC ZZZZZZZZZ9.
           05 History-Date-Display             PIC X(8).
       01 Discount-Record-Display.
           05 Discount-ID-Display              PIC ZZZZ9.
           05 Limit-Amount-Display             PIC ZZZZZ9.
       01 Cart-Record-Display.
           05 Cart-ID-Display                  PIC ZZZZ9.
           05 Cart-Item-ID-Display             PIC ZZZZ9.
           05 Cart-Quantity-Display            PIC ZZ9.
           05 Cart-Unit-Of-Price-Display       PIC ZZZZZZZZZ9.
       01 Sale-Cart-Record-Display.
           05 Sale-Cart-ID-Display             PIC ZZZZ9.
           05 Sale-Cart-Item-ID-Display        PIC ZZZZ9.
           05 Sale-Cart-Quantity-Display       PIC ZZ9.
           05 Sale-Cart-Unit-Of-Price-Display  PIC ZZZZZZZZZ9.
       01 Invoice-Record-Display.
           05 Invoice-ID-Display               PIC ZZZZ9.
           05 Total-Amount-Display             PIC ZZZZZZZZZ9.
           05 Discount-Display                 PIC ZZZZZZZ9.
           05 Final-Amount-Display             PIC ZZZZZZZZZ9.
           05 Pay-Amount-Display               PIC ZZZZZZZZZ9.
       01  String-Format                       PIC X(10).
       PROCEDURE DIVISION.
           PERFORM Dashborad
           STOP RUN.
       Dashborad.

           MOVE "1" TO End-Program
           PERFORM UNTIL End-Program = "0"

           MOVE "Y" TO EOF
           MOVE "Y" TO EOFP
           MOVE "Y" TO EF
           MOVE "N" TO Admin-permission
           MOVE "N" TO Casher-permission
           MOVE "N" TO EFC
           MOVE "N" TO Flag
           MOVE "N" TO Cart-Flag
           MOVE "N" TO EOFB
           MOVE "N" TO Found

               DISPLAY " "
               DISPLAY " "
               DISPLAY a a b Bold-On "Customer Order System "
                       Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY a a b b Bold-On "Choose Role" Bold-Off a a
               DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                       Bold-Off
               DISPLAY ""
               DISPLAY ESC Blue-On             "Admin = 1"
                       ESC Reset-Color a b A5
                       WITH NO ADVANCING
               DISPLAY ESC Green-On            "Casher = 2"
                       ESC Reset-Color a b A5
                       WITH NO ADVANCING
               DISPLAY ESC Cyan-On             "Exit = 0"
                       ESC Reset-Color
               DISPLAY " "
               String "Please choose only (1,2,0):" INTO alert
               DISPLAY ESC Yellow-On alert ESC Reset-Color
               DISPLAY  H10 H10 H10 H10 H10 H10 H10 H10 H10

               DISPLAY "Enter Choose Option:"
               ACCEPT Role

               EVALUATE Role
                   WHEN "1"
                       PERFORM Admin-Option
                   WHEN "2"
                       PERFORM Casher-Login
                   WHEN "0"
                       PERFORM End-Process
                   WHEN OTHER
                       DISPLAY ESC Red-On
                               "! Please choose only(1,2,0) !"
                               ESC Reset-Color
                       MOVE "Y" TO Flag
                       MOVE "1" TO End-Program
               END-EVALUATE
           END-PERFORM.

       Admin-Option.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY a a b
                   Bold-On "Please Choose Option:"
                   Bold-Off
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                   Bold-Off
           DISPLAY " "
           DISPLAY ESC Blue-On                 "Login = 1"
                   ESC Reset-Color a b A2
                   ESC Green-On                "Registration = 2"
                   ESC Reset-Color a b A2
                   ESC Cyan-On                 "Back = 0"
                   ESC Reset-Color
           DISPLAY " "
           DISPLAY H10 H10 H10 H10 H10 H10 H10 H10 H10
           DISPLAY "Please Choose Option:"
           ACCEPT User-Choice
           EVALUATE User-Choice
               WHEN "1"
                   PERFORM Admin-Login
               WHEN "2"
                   PERFORM Admin-Credential
               WHEN "0"
                   PERFORM Dashborad
               WHEN OTHER
                   DISPLAY "Please choose only (1 or 2):"
                   PERFORM Admin-Option
           END-EVALUATE.

       End-Process.
           IF Flag = "N"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY "1. Start Program"
               DISPLAY "0. Exit"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY ESC
                       Yellow-On "Choose only 1 or 0:" ESC Reset-Color
               ACCEPT End-Program
           END-IF
           MOVE FUNCTION UPPER-CASE(End-Program) TO End-Program.
       Admin-Credential.
           DISPLAY "Enter Admin Id:"
           ACCEPT Input-ID
           DISPLAY "Enter Password:"
           ACCEPT Input-Password
           OPEN INPUT AdminFile
           MOVE "N" TO EF
           MOVE "N" TO Admin-permission

           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
                   NOT AT END
                       IF Admin-ID = Input-ID
                          AND Admin-PW = Input-Password
                           MOVE "Y" TO EF
                           MOVE "Y" TO Admin-permission
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AdminFile
           PERFORM Register.
       Register.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           IF Admin-permission = "Y"
               DISPLAY a a A9
                       Bold-On"Choose Roles For Registration:"
                       Bold-Off
               DISPLAY e10 e10 e10 e10 e10 e10 e10 e10 e10
               DISPLAY a b "1.Admin" a b A3
               DISPLAY a b "2.Casher"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY Bold-On "0.Go Back" Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY "Enter Choose Option:"
               ACCEPT Role
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
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
               DISPLAY ESC Red-On
                       "Admin Id and Passward incorrect, Try Again!"
                       ESC Reset-Color
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               PERFORM Admin-Credential
           END-IF.
       Admin-Register.
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY a a A9 Bold-On "REGISTER PROGRAM" Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
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
           MOVE "N" TO Admin-permission
           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
                   NOT AT END
                       IF Admin-ID = Input-ID
                           MOVE "Y" TO Admin-permission
                           MOVE "Y" TO EF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE AdminFile
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           IF Admin-permission = "Y"
               DISPLAY ESC Red-On
               "Already exists. Please add a new member!"
               ESC Reset-Color
               PERFORM Admin-Register
           ELSE
               PERFORM Password-Check
               IF Done = "Y"
                   OPEN EXTEND AdminFile
                   MOVE Input-ID TO Admin-ID
                   MOVE Input-Password TO Admin-PW
                   WRITE Admin-Record
                   CLOSE AdminFile

                   DISPLAY  ESC Green-On
                   "Register successful. Welcome, " Input-ID
                   ESC Reset-Color
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
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           IF Password-Length NOT = 6
               DISPLAY ESC Red-On
                       "Password must be exactly 6 characters long."
                       ESC Reset-Color
               DISPLAY ESC Red-On "Please try again..." ESC Reset-Color
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
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
                   DISPLAY Bold-On "Password must contain at least:"
                           Bold-Off
                   IF Has-Upper NOT = 'Y'
                       DISPLAY Bold-On " - One UPPERCASE letter"
                               Bold-Off
                   END-IF
                   IF Has-Lower NOT = 'Y'
                       DISPLAY Bold-On " - One lowercase letter"
                               Bold-Off
                   END-IF
                   IF Has-Number NOT = 'Y'
                       DISPLAY Bold-On " - One digit"
                               Bold-Off
                   END-IF
                   IF Has-Special NOT = 'Y'
                       DISPLAY Bold-On "- One special character (!@#$%)"
                               Bold-Off
                   END-IF
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY ESC Red-On "Please try again..."
                           ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
               END-IF
           END-IF
           END-PERFORM.
       Casher-Register.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY a a a b Bold-On"REGISTER PROGRAM" Bold-Off
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                   Bold-Off
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
           MOVE "N" TO Casher-permission

           PERFORM UNTIL EF = "Y"
               READ CasherFile
                   AT END
                       MOVE "Y" TO EF
                   NOT AT END
                       IF Casher-ID = Input-ID
                           MOVE "Y" TO Casher-permission
                           MOVE "Y" TO EF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CasherFile
           IF Casher-permission = "Y"
               DISPLAY ESC Red-On
                       "Already exists. Please add a new member!"
                       ESC Reset-Color
               PERFORM Casher-Register
           ELSE
               PERFORM Password-Check
               IF Done = "Y"
                   OPEN EXTEND CasherFile
                   MOVE Input-ID TO Casher-ID
                   MOVE Input-Password TO Casher-PW
                   WRITE Casher-Record
                   CLOSE CasherFile
                   DISPLAY ESC Green-On
                           "Register successful. Welcome, " Input-ID
                           ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
               END-IF
           END-IF.
       Admin-Login.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY a a a  Bold-On "Login Form" Bold-Off
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
           DISPLAY "Enter Admin Id: "
           ACCEPT Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Input-Password
           OPEN INPUT AdminFile
           MOVE "N" TO Admin-permission
           MOVE "N" TO EF
           PERFORM UNTIL EF = "Y"
               READ AdminFile
                   AT END
                       MOVE "Y" TO EF
               NOT AT END
                   IF Admin-ID = Input-ID AND Admin-PW = Input-Password
                       MOVE "Y" TO EF
                       MOVE "Y" TO Admin-permission
                       EXIT PERFORM
                   END-IF
           END-PERFORM
           CLOSE AdminFile
           IF Admin-permission = "Y"
               DISPLAY " "
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY " "
               STRING  "Login successful. Welcome, " Admin-ID INTO ds
               DISPLAY ESC Green-On ds ESC Reset-Color
               DISPLAY " "
               PERFORM Admin-Process
           ELSE
               STRING "Login failed.Login try again!!!!!" INTO loginfail
               DISPLAY ESC Red-On loginfail ESC Reset-Color
               perform Admin-Login
           END-IF.

       Admin-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY Bold-On         "Back = 1"
                   a a a a a A2    "Next = 2"
                   Bold-Off
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY "Choose only (1 or 2)"
           ACCEPT option
           EVALUATE option
               WHEN "1"
                   PERFORM Dashborad
               WHEN "2"
                   PERFORM Next-Process
               WHEN OTHER
                   DISPLAY "Please choose only 1 or 2 "
                   PERFORM Admin-Process
           END-EVALUATE.
       Next-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY a a A9 Bold-On"Admins' Processes"Bold-Off
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
           DISPLAY ""
           DISPLAY ESC Blue-On A1                      "Item = 1"
                   ESC Reset-Color A10 ESC Green-On    "Stock = 2"
                   ESC Reset-Color A10 ESC Pink-On A1  "Discount = 3"
                   ESC Reset-Color A10 ESC Cyan-On     "Back = 0"
                   ESC Reset-Color
           DISPLAY ""
           String  "Please choose only (1,2,3,0) " INTO alert
           DISPLAY ESC Yellow-On alert ESC Reset-Color
           DISPLAY H10 H10 H10 H10 H10 H10 H10 H10
           DISPLAY "Enter Choose option:"
           ACCEPT option
           EVALUATE option
               WHEN "1"
                   PERFORM Item-Process
               WHEN "2"
                   PERFORM Stock-Process
               WHEN "3"
                   PERFORM Discount-Process
               WHEN "0"
                   PERFORM Admin-Process
               WHEN OTHER
                   DISPLAY "Please choose only 1 or 2 or 3 or 0"
                   PERFORM Next-Process
           END-EVALUATE.
       Item-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY a a A9 ESC Blue-On Bold-On"Item Processes"
                   Bold-Off ESC Reset-Color
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                   Bold-Off
           DISPLAY a b "1. View Items"
           DISPLAY a b "2. Add Item"
           DISPLAY a b "3. Update Item-Name"
           DISPLAY a b "4. Delete Item"
           DISPLAY a b "5. Update Item's Sale Price"
           DISPLAY a b "6. Best Sale Item"
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY Bold-On "0. Back" Bold-Off
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY "Enter Choose Option:"
           ACCEPT User-Choice
           MOVE 'N' TO EOF
           EVALUATE User-Choice
               WHEN "1"
                   PERFORM View-Item
               WHEN "2"
                   PERFORM Category-Item
                   PERFORM Add-Item
               WHEN "3"
                   PERFORM Update-Item
               WHEN "4"
                   PERFORM Delete-Item
               WHEN "5"
                   PERFORM Update-Price
               WHEN "6"
                   PERFORM Best-Sale-Item
               WHEN "0"
                   PERFORM Next-Process
               WHEN OTHER
                   DISPLAY "Please choose only (1,2,3,4,5,6,0) "
                   PERFORM Item-Process
           END-EVALUATE.

       Category-Item.
           OPEN INPUT CategoryFile
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY a a a b ESC Blue-On Bold-On "Category" Bold-Off
                   ESC Reset-Color
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
           MOVE "N" TO EOF
           MOVE 1 TO IDX
           PERFORM UNTIL EOF = "Y"
               READ CategoryFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
                   EXIT PERFORM
               NOT AT END
                   DISPLAY IDX ". " Category-Name
                   MOVE Category-Name TO Category-Index-Name(IDX)
                   ADD 1 TO IDX
           END-PERFORM
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY "Enter Choose Option:"
           ACCEPT INPUT-IDX
           MOVE "Y" TO EOF
           CLOSE CategoryFile.
       Add-Item.
           DISPLAY" "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           OPEN I-O ItemFile
           MOVE 0 TO MAX-ID
           IF Item-ID = SPACES
               MOVE 1 TO Item-ID
           ELSE
               MOVE "N" TO EOF
               PERFORM UNTIL EOF = "Y"
                   READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       IF Item-ID > MAX-ID
                           MOVE Item-ID TO MAX-ID
                       END-IF
               END-PERFORM
           END-IF
           CLOSE ItemFile
           ADD 1 TO MAX-ID
           MOVE MAX-ID TO Item-ID-Display
           MOVE FUNCTION TRIM(Item-ID-Display)TO String-Format
           DISPLAY "Item ID: "String-Format
           DISPLAY "Enter Item Name:"
           ACCEPT Input-Item-Name
           OPEN I-O ItemFile
           MOVE "N" TO EOF
           MOVE "N" TO Found
           PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       IF FUNCTION LOWER-CASE(Item-Name) =
                           FUNCTION LOWER-CASE(Input-Item-Name)
                           MOVE "Y" TO Found
                           MOVE "Y" TO EOF    *> Stop loop after finding the item
                       EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ItemFile
           IF Found = "Y"
               DISPLAY ESC Red-On
               DISPLAY "Already exist, Please enter new item name !"
               DISPLAY ESC Reset-Color
               PERFORM Add-Item
           ELSE
               OPEN I-O ItemFile
               MOVE MAX-ID TO Item-ID
               MOVE Item-ID TO RelativeKey
               MOVE Category-Index-Name(INPUT-IDX) TO Item-Category
               MOVE Input-Item-Name TO Item-Name
               DISPLAY "Enter Sale Price:"
               ACCEPT Item-Price
               MOVE 0 TO Item-Qty
               WRITE Item-Record INVALID KEY
                   DISPLAY ESC Red-On"Input data are wrong!!! "
                           ESC Reset-Color
               END-WRITE
               IF File-Status = "00"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY " "
                   DISPLAY ESC Green-On
                           "Item added successfully." ESC Reset-Color
                   DISPLAY " "
               END-IF
           MOVE "Y" TO EOF
           CLOSE ItemFile
           END-IF
           PERFORM More-Process.
       More-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY "1. Add more data"
           DISPLAY "0. Go Back"
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY "Enter Choose Option:"
           ACCEPT User-Choice
           EVALUATE User-Choice
               WHEN "1"
                   PERFORM Category-Item
                   PERFORM Add-Item
               WHEN "0"
                   PERFORM Next-Process
               WHEN OTHER
                   DISPLAY "Please Choose only (1 or 0)"
                   PERFORM Add-Item
           END-EVALUATE.
       Stock-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY a a a b ESC Green-On Bold-On "Stock Processes"
                   Bold-Off ESC Reset-Color
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
           DISPLAY a b "1. Low Stock"
           PERFORM Low-Stock-Alert
           MOVE Low-Stock-Count TO Low-Stock-Count-Format
           STRING  a b "Warning: Stock of"
                       Low-Stock-Count-Format DELIMITED BY SIZE
                       " Items  are low!!!!"
                       INTO Message-Format
           DISPLAY ESC Red-On Message-Format ESC Reset-Color
           DISPLAY  a b "2. Fill Stock"
           DISPLAY  a b "3. View History"
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY Bold-On "0. Back"Bold-Off
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY "Enter Choose Option:"
           ACCEPT User-Choice
           DISPLAY " "
           EVALUATE User-Choice
               WHEN "1"
                   PERFORM Low-Stock-Item
               WHEN "2"
                   PERFORM Add-Stock
               WHEN "3"
                   PERFORM View-History
               WHEN "0"
                   PERFORM Next-Process
               WHEN OTHER
                   DISPLAY "Please choose only (1,2,3,0) "
                   PERFORM Stock-Process
           END-EVALUATE
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off.
       Discount-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY a a A6 ESC Pink-On Bold-On"Discount Processes"
                   Bold-Off ESC Reset-Color
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
           DISPLAY a b "1. View Discount"
           DISPLAY a b "2. Add Discount"
           DISPLAY a b "3. Update Discount"
           DISPLAY a b "4. Delete Discount"
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY Bold-On"0. Back"Bold-Off
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY "Enter Choose Option:"
           ACCEPT User-Choice
           EVALUATE User-Choice
               WHEN "1"
                   PERFORM View-Discount
               WHEN "2"
                   PERFORM Add-Discount
               WHEN "3"
                   PERFORM Update-Discount
               WHEN "4"
                   PERFORM Delete-Discount
               WHEN "0"
                   PERFORM Next-Process
               WHEN OTHER
                   DISPLAY ESC Red-On
                       "Please choose only (1,2,3,4,0)" ESC Reset-Color
                   PERFORM Discount-Process
           END-EVALUATE.
       Casher-Login.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           DISPLAY a a a  Bold-On"Login Form"Bold-Off
           DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
           DISPLAY "Enter Casher Id: "
           ACCEPT Input-ID
           DISPLAY "Enter Password: "
           ACCEPT Input-Password
           DISPLAY " "
           OPEN INPUT CasherFile
           MOVE "N" TO Casher-permission
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
                       MOVE "Y" TO Casher-permission
                       EXIT PERFORM
                   END-IF

           END-PERFORM
           CLOSE CasherFile
           IF Casher-permission = "Y"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY ESC Blue-On  "Login successful. Welcome, "
                       Casher-ID ESC Reset-Color
               DISPLAY " "
               PERFORM Sale-Process
           ELSE
               DISPLAY ESC Red-On  "Login failed.Login try again!!!!!"
               ESC Reset-Color
               PERFORM Casher-Login
           END-IF.
       Sale-Process.
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           MOVE "1" TO EOFP
           PERFORM UNTIL EOFP = "0"
               PERFORM Low-Stock-Alert
               DISPLAY a a A9 Bold-On "Casher's Processes" Bold-Off
               DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                       Bold-Off
               DISPLAY a b "1. New Order"
               DISPLAY a b "2. Best Seller Item"
               DISPLAY a b "3. Low Stock Item"
               MOVE Low-Stock-Count TO Low-Stock-Count-Format
               STRING a b "<<<Warning: Stock "
                       FUNCTION TRIM(Low-Stock-Count-Format)
                       DELIMITED BY SIZE
                       ">>>"
                       INTO Message-Format
               DISPLAY ESC Yellow-On Message-Format ESC Reset-Color
               DISPLAY a b "4. Pending Invoice"
               DISPLAY a b "5. Completed Invoice"
               DISPLAY a b "6. Discount"
               DISPLAY ""
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY Bold-On "0. Back"Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               DISPLAY " "
               EVALUATE User-Choice
                   WHEN "1"
                       DISPLAY " "
                       OPEN I-O CartFile
                       IF File-Status = "35"
      *            >          DISPLAY "File does not exist. Creating file..."
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
                               DELETE CartFile
                       END-PERFORM
                       CLOSE CartFile
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
                   WHEN "5"
                       PERFORM Completed-Invoice
                   WHEN "6"
                       PERFORM View-Discount
                       MOVE "1" TO EOFP
                   WHEN "0"
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.
       Casher-Home.
           PERFORM Sale-Item
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           PERFORM Cart
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off.
       Low-Stock-Item.
            OPEN I-O ItemFile
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a a ESC Green-On Bold-On"Low Stock Item Record"
            Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"  A5
                    "Name"     A5 A5 A7
                    "Category" A4
                    "Quantity" A7
                    "Price"

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   MOVE FUNCTION TRIM(Item-ID-Display) TO String-Format
                   IF Item-Qty = 0
                       DISPLAY String-Format       A2
                               Item-Name           A1
                               Item-Category       A5
                               Item-Qty-Display    A1
                               Item-Price-Display"$"  A3
                               ESC Red-On
                               "<<Item quantity out of Stock>>"
                               ESC Reset-Color
                   ELSE IF Item-Qty < 10
                       DISPLAY String-Format       A2
                               Item-Name           A1
                               Item-Category       A5
                               Item-Qty-Display    A1
                               Item-Price-Display"$"  A3
                               ESC Yellow-On
                               "<<Item quantity is less than 10>>"
                               ESC Reset-Color
                   END-IF
            END-PERFORM
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            CLOSE ItemFile
            PERFORM Stock-Back.
       View-Item.
            OPEN I-O ItemFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT ItemFile
               CLOSE ItemFile
               OPEN I-O ItemFile
            END-IF

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a a Bold-On ESC Blue-On "Item Record"
                    Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"      A5
                    "Name"         A5 A5 A7
                    "Category"     A4
                    "Quantity"     A6
                    "Sale Price"   A5
                    "Buy Price"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
                   EXIT PERFORM *>new added
               NOT AT END
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   OPEN I-O HistoryFile
                    IF File-Status = "35"
      *        >          DISPLAY "File does not exist. Creating file..."
                       OPEN OUTPUT HistoryFile
                       CLOSE HistoryFile
                       OPEN I-O HistoryFile
                    END-IF

                    MOVE "N" TO EFC
                    MOVE 0 TO Buy-Sale
                    PERFORM UNTIL EFC = "Y"
                       READ HistoryFile NEXT RECORD
                       AT END

                               MOVE "Y" TO EFC
                           EXIT PERFORM *>new added
                       NOT AT END
                           IF Item-ID = History-Item-ID
                              MOVE History-Unit-Of-Price TO Buy-Sale
                           END-IF
                    END-PERFORM
                    CLOSE HistoryFile
                    MOVE FUNCTION TRIM(Item-ID-Display) TO String-Format
                IF Item-Qty = 0
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                   ESC Red-On
                                   "<<Item quantity out of Stock>>"
                                   ESC Reset-Color
                       ELSE IF Item-Qty < 10
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Yellow-On
                                    "<<Item quantity is less than 10>>"
                                    ESC Reset-Color
                       ELSE
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                       END-IF
            END-PERFORM
            CLOSE ItemFile
            DISPLAY " "
            PERFORM Search-Process.
       Sale-Item.
            OPEN I-O ItemFile
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a A5  Bold-On ESC Blue-On "Sale Item Record"
                    Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"      A5
                    "Name"         A5 A5 A7
                    "Category"     A9 A10
                    "Quantity"     A10 A1
                    "Price"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
                   EXIT PERFORM *>new added
               NOT AT END
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   MOVE FUNCTION TRIM(Item-ID-Display) TO String-Format
                   IF Item-Qty = 0
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A10 A10
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A3
                                    ESC Red-On
                                    "<<Item quantity out of Stock>>"
                                    ESC Reset-Color
                       ELSE IF Item-Qty < 10
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A10 A10
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A3
                                    ESC Yellow-On
                                    "<<Item quantity is less than 10>>"
                                    ESC Reset-Color
                       ELSE
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A10 A10
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"
                       END-IF
            END-PERFORM
            CLOSE ItemFile.
       Search-Process.
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "1. Find With Item ID"
            DISPLAY "2. Find With Item Name"
            DISPLAY "3. Find With Category"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY  a a a a a a Bold-On "0. Back"Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT User-Choice
            EVALUATE User-Choice
               WHEN "1"
                   IF Admin-permission = "Y"
                       PERFORM Admin-Find-Id-Item
                   END-IF
                   IF Casher-permission = "Y"
                       PERFORM Find-Id-Item
                   END-IF
               WHEN "2"
                   IF Admin-permission = "Y"
                       PERFORM Admin-Find-Item
                   END-IF
                   IF Casher-permission = "Y"
                       PERFORM Find-Item
                   END-IF
               WHEN "3"
                   IF Admin-permission = "Y"
                       PERFORM Admin-Find-Category-Item
                   END-IF
                   IF Casher-permission = "Y"
                       PERFORM Find-Category-Item
                   END-IF
               WHEN "0"
                   IF Admin-permission = "Y"
                       PERFORM Item-Process
                   END-IF
               WHEN OTHER
                   DISPLAY ESC Red-On
                           "Invalid choice. Please choose only(1,2,3)"
                           ESC Reset-Color
               PERFORM Search-Process
            END-EVALUATE.
       Update-Item.
            OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Update:"
            ACCEPT RelativeKey
            READ ItemFile
            INVALID KEY
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
               DISPLAY ESC Red-On "Error: Record not found."
               ESC Reset-Color
               PERFORM Update-Item
            NOT INVALID KEY
                IF File-Status = "00"
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY a a a  Bold-On ESC Blue-On "Item Record"
                           Bold-Off ESC Reset-Color
                   DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                           Bold-Off
                   DISPLAY "Item-ID"               A5
                            "Name"                 A5 A5 A7
                            "Category"             A4 A10
                            "Quantity"             A10 A1
                            "Price"
                   MOVE FUNCTION TRIM(Item-ID-Display) TO String-Format
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY String-Format           A2
                           Item-Name               A1
                           Item-Category           A5 A10
                           Item-Qty-Display        A5
                           Item-Price-Display"$"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   Display " "
                   DISPLAY "Enter New Item Name: "
                   ACCEPT Item-Name
                   REWRITE Item-Record INVALID KEY
                       DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE
                   IF File-Status = "00"
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
                       DISPLAY ESC Green-On
                       "Item updated successfully." ESC Reset-Color
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF
            END-READ
            CLOSE ItemFile
            PERFORM Search-Next-Process.
       Delete-Item.
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Delete: "
            ACCEPT RelativeKey
            PERFORM Preveiw-Item
            DISPLAY "Are you sure want to delete?"
            DISPLAY "1. Confirm"
            DISPLAY "0. Cancel"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT User-Choice
            MOVE "N" TO Delete-Permission
            EVALUATE User-Choice
            WHEN "1"
               MOVE "Y" TO Delete-Permission
            WHEN "0"
               MOVE "N" TO Delete-Permission
            WHEN OTHER
               MOVE "N" TO Delete-Permission
            END-EVALUATE
            IF Delete-Permission = "Y"
            DELETE ItemFile INVALID KEY
               DISPLAY ESC Red-On "Error: Record not found." ESC
               Reset-Color
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY " "
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
                   DISPLAY ESC Green-On
                       "This item's info deleted successfully."
                       ESC Reset-Color
                       DISPLAY " "
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off

               ELSE
                   DISPLAY ESC Red-On "This item's info cannot delete"
                   ESC Reset-Color
               END-IF
            END-DELETE
            CLOSE ItemFile
            PERFORM Search-Next-Process
            ELSE
                PERFORM Item-Process
            END-IF.
       Preveiw-Item.
            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
               PERFORM Delete-Item
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
            DISPLAY a a a  Bold-On ESC Blue-On "Item Record"
            Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"      A5
                    "Name"         A5 A5 A7
                    "Category"     A4
                    "Quantity"     A6
                    "Sale Price"   A5
                    "Buy Price"

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                MOVE Item-ID TO Item-ID-Display
                MOVE Item-Qty TO Item-Qty-Display
                MOVE Item-Price TO Item-Price-Display

                OPEN I-O HistoryFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT HistoryFile
               CLOSE HistoryFile
               OPEN I-O HistoryFile
            END-IF

                MOVE "N" TO EFC
                MOVE 0 TO Buy-Sale
                PERFORM UNTIL EFC = "Y"
                   READ HistoryFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EFC
                       EXIT PERFORM *>new added
                   NOT AT END
                       IF Item-ID = History-Item-ID
                          MOVE History-Unit-Of-Price TO Buy-Sale
                       END-IF
                END-PERFORM
                CLOSE HistoryFile
                MOVE FUNCTION TRIM(Item-ID-Display)
                       TO String-Format
                IF Item-Qty = 0
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Red-On
                                    "<<Item quantity out of Stock>>"
                                    ESC Reset-Color
                       ELSE IF Item-Qty < 10
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Yellow-On
                                    "<<Item quantity is less than 10>>"
                                    ESC Reset-Color
                       ELSE
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                       END-IF
                END-IF
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            .
       Update-Price.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10  Bold-Off
           DISPLAY "1. To Update, search with Item Id"
           DISPLAY "2. To Update, search with Item Name"
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           DISPLAY "Enter option (1 or 2)"
           ACCEPT User-Choice
           EVALUATE User-Choice
               WHEN "1"
               PERFORM UpdateId
               WHEN "2"
               PERFORM UpdateName
               WHEN OTHER
               DISPLAY ESC Red-On "Please choose(1 or 2) to search item"
               ESC Reset-Color
               PERFORM Update-Price
           END-EVALUATE.


       UpdateId.
           OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Update: "
            ACCEPT RelativeKey

            READ ItemFile INVALID KEY
            DISPLAY ESC Red-On"Error: Record not found."
            ESC Reset-Color
            NOT INVALID KEY
                IF File-Status = "00"
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display

                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
            DISPLAY a a a  Bold-On ESC Blue-On "Item Record"
            Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"  A5
                        "Name"     A5 A5 A7
                        "Category" A4
                        "Quantity" A6
                        "Sale Price" A5
                        "Buy Price"

                OPEN I-O HistoryFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT HistoryFile
               CLOSE HistoryFile
               OPEN I-O HistoryFile
            END-IF

                MOVE "N" TO EFC
                MOVE 0 TO Buy-Sale
                PERFORM UNTIL EFC = "Y"
                   READ HistoryFile NEXT RECORD
                   AT END

                       MOVE "Y" TO EFC
                       EXIT PERFORM *>new added
                   NOT AT END
                       IF Item-ID = History-Item-ID
                          MOVE History-Unit-Of-Price TO Buy-Sale
                       END-IF
                END-PERFORM
                CLOSE HistoryFile
                MOVE FUNCTION TRIM(Item-ID-Display)
                       TO String-Format
                IF Item-Qty = 0
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Red-On
                                    "<<Item quantity out of Stock>>"
                                    ESC Reset-Color
                       ELSE IF Item-Qty < 10
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Yellow-On
                                    "<<Item quantity is less than 10>>"
                                    ESC Reset-Color
                       ELSE
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                       END-IF
             DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
                   DISPLAY "Update Price: "
                   ACCEPT Item-Price
                   REWRITE Item-Record INVALID KEY
                   DISPLAY ESC Red-On "Error: Unable to rewrite record."
                   ESC Reset-Color
                   END-REWRITE

                   IF File-Status = "00"
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                       DISPLAY " "
                       DISPLAY ESC Green-On
                       "Item updated successfully." ESC Reset-Color
                       DISPLAY " "
                       DISPLAY
                       Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF
            END-READ
           CLOSE ItemFile
           PERFORM Search-Next-Process.
       UpdateName.
           OPEN INPUT ItemFile
           MOVE "N" TO EOF
           MOVE "N" TO Found
           OPEN INPUT ItemFile
           DISPLAY "Enter Item Name: "
           ACCEPT Input-Item-Name
           PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                       EXIT PERFORM
                   NOT AT END
                       IF FUNCTION LOWER-CASE(Item-Name) =
                           FUNCTION LOWER-CASE(Input-Item-Name)
                           MOVE Item-ID TO Item-ID-Display
                           MOVE Item-Qty TO Item-Qty-Display
                           MOVE Item-Price TO Item-Price-Display
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY a a a  Bold-On ESC Blue-On "Item Record"
                           Bold-Off ESC Reset-Color
                   DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                           Bold-Off
                           DISPLAY "Item-ID"       A5
                                   "Name"          A5 A5 A7
                                   "Category"      A4
                                   "Quantity"      A6
                                   "Sale Price"    A5
                                    "Buy Price"

                           OPEN I-O HistoryFile
                            IF File-Status = "35"
      *                >          DISPLAY "File does not exist. Creating file..."
                               OPEN OUTPUT HistoryFile
                               CLOSE HistoryFile
                               OPEN I-O HistoryFile
                            END-IF
                MOVE "N" TO EFC
                MOVE 0 TO Buy-Sale
                PERFORM UNTIL EFC = "Y"
                   READ HistoryFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EFC
                       EXIT PERFORM *>new added
                   NOT AT END
                       IF Item-ID = History-Item-ID
                          MOVE History-Unit-Of-Price TO Buy-Sale
                       END-IF
                END-PERFORM
                CLOSE HistoryFile
                MOVE FUNCTION TRIM(Item-ID-Display)
                       TO String-Format
                IF Item-Qty = 0
                       DISPLAY String-Format           A2
                               Item-Name               A1
                               Item-Category           A5
                               Item-Qty-Display        A5
                               Item-Price-Display"$"   A8
                               Buy-Sale"$"
                               ESC Red-On
                               "<<Item quantity out of Stock>>"
                               ESC Reset-Color
                 ELSE IF Item-Qty < 10
                       DISPLAY String-Format           A2
                               Item-Name               A1
                               Item-Category           A5
                               Item-Qty-Display        A5
                               Item-Price-Display"$"   A8
                               Buy-Sale"$"
                               ESC Yellow-On
                               "<<Item quantity is less than 10>>"
                               ESC Reset-Color
                 ELSE
                       DISPLAY String-Format           A2
                               Item-Name               A1
                               Item-Category           A5
                               Item-Qty-Display        A5
                               Item-Price-Display"$"   A8
                               Buy-Sale"$"
                 END-IF
                       MOVE "Y" TO Found
                       MOVE "Y" TO EOF    *> Stop loop after finding the item
                 END-IF
           END-PERFORM
           CLOSE ItemFile
           IF Found NOT = "Y"
           DISPLAY ESC Red-On "! Please enter only item name !"
               ESC Reset-Color
           PERFORM Find-Item
           END-IF
           OPEN I-O ItemFile
           IF Admin-permission = 'Y'
                DISPLAY " "
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
               DISPLAY "Update Price: "
               ACCEPT Item-Price
               REWRITE Item-Record INVALID KEY
               DISPLAY ESC Red-On "Error: Unable to rewrite record."
               ESC Reset-Color
               END-REWRITE
               IF File-Status = "00"
                   DISPLAY " "
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY ESC Green-On
                           "Item updated successfully." ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
               CLOSE ItemFile
               PERFORM Search-Next-Process
           END-IF.
       Add-Stock.
           DISPLAY " "
           DISPLAY Bold-On"Please enter info to fill stock"Bold-Off
           ACCEPT Invoice-Date FROM DATE YYYYMMDD
           MOVE "N" TO EOF
           DISPLAY H10 H10 H10 H10 H10 H10 H10 H10 H10
           PERFORM UNTIL EOF = "Y"
               DISPLAY "Enter Item-ID to Add: "
               ACCEPT RelativeKey
               OPEN I-O ItemFile
               READ ItemFile
               INVALID KEY
                   DISPLAY ESC Red-On "Error: Record not found."
                   ESC Reset-Color
               NOT INVALID KEY
                   IF File-Status = "00"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a a  Bold-On ESC Blue-On "Item Record"
            Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"  A5
                        "Name"     A5 A5 A7
                        "Category" A4
                        "Quantity" A6
                        "Sale Price" A5
                        "Buy Price"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   MOVE FUNCTION TRIM(Item-ID-Display)
                       TO String-Format
                OPEN I-O HistoryFile
                MOVE "N" TO EFC
                MOVE 0 TO Buy-Sale
                PERFORM UNTIL EFC = "Y"
                   READ HistoryFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EFC
                       EXIT PERFORM *>new added
                   NOT AT END
                       IF RelativeKey = History-Item-ID
                          MOVE History-Unit-Of-Price TO Buy-Sale
                          EXIT PERFORM
                       END-IF
                END-PERFORM
                CLOSE HistoryFile
                DISPLAY String-Format           A2
                        Item-Name               A1
                        Item-Category           A5
                        Item-Qty-Display        A5
                        Item-Price-Display"$"   A8
                        Buy-Sale"$"
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
               CLOSE ItemFile
               OPEN I-O HistoryFile
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
           CLOSE HistoryFile
           MOVE "N" TO SS
               PERFORM UNTIL SS = "Y"
               DISPLAY "Enter Date (YYYYMMDD): "
               ACCEPT History-Date
               MOVE FUNCTION LENGTH(FUNCTION TRIM(History-Date)) TO CC
               IF CC = 8
                   MOVE History-Date TO Start-Date-Num
                   IF Start-Date-Num > 1
                       IF Start-Date-Num >= Invoice-Date
                           MOVE 'Y' TO SS
                       ELSE
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                         DISPLAY ESC Red-On
                         "Today-date equal and greater than use:"
                         Invoice-Date
                           ESC Reset-Color
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                       END-IF
                   ELSE
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                       DISPLAY b ESC Red-On"Start Date must be numeric."
                       ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   END-IF
               ELSE
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   DISPLAY b ESC Red-On
                       "Start Date must have 8 digits"
                       ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
               END-IF
               END-PERFORM

           IF SS = "Y"
           OPEN I-O HistoryFile
           MOVE History-Date TO History-Num
           DISPLAY "Enter Quantity: "
           ACCEPT History-Quantity
           DISPLAY "Enter Unit of Price: "
           ACCEPT History-Unit-Of-Price
           WRITE History-Record
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
           DISPLAY ESC Green-On"Stock fill successfully."
           ESC Reset-Color
           DISPLAY " "
           CLOSE HistoryFile
           OPEN I-O ItemFile
           ADD History-Quantity TO Item-Qty
           GIVING Update-Quantity
           MOVE Update-Quantity TO Item-Qty
           REWRITE Item-Record
           END-REWRITE
           CLOSE ItemFile
           END-IF
           MOVE "Y" TO EOF
           END-PERFORM

           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
            DISPLAY Bold-On "1.Add more fill Stock" Bold-Off
            a a b Bold-On "0.Go Back" Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
            DISPLAY "Please choose option"
            ACCEPT User-Choice
            EVALUATE User-Choice
               WHEN '1'
                   PERFORM Add-Stock
               WHEN '0'
                   PERFORM Stock-Process
                   MOVE 'Y' TO EOF
            END-EVALUATE.
       View-History.
            OPEN I-O HistoryFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT HistoryFile
               CLOSE HistoryFile
               OPEN I-O HistoryFile
            END-IF

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
            Bold-Off
            DISPLAY a a A4 ESC Green-On Bold-On
            "Stock filled History Record" Bold-Off ESC Reset-Color
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "History-ID"   A2
                    "Item-Name"    A2 A10
                    "Date"         A5 A5
                    "Quantity"     A9
                    "Unit of Price"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ HistoryFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   OPEN I-O ItemFile
                   MOVE History-Item-ID TO RelativeKey
                   READ ItemFile INVALID KEY
                       DISPLAY "Error: Record not found."
                   NOT INVALID KEY
                       IF File-Status = "00"
                           MOVE Item-Name TO Input-Item-Name
                       END-IF

                   END-READ
                   CLOSE ItemFile

                   MOVE History-ID TO History-ID-Display
                   MOVE History-Item-ID TO History-Item-ID-Display
                   MOVE History-Quantity TO History-Quantity-Display
                   MOVE History-Date TO History-Date-Display
                   MOVE History-Unit-Of-Price TO
                   History-Unit-Of-Price-Display
                   MOVE FUNCTION TRIM(History-ID-Display)
                       TO String-Format
                   DISPLAY String-Format           A2
                           Input-Item-Name         A1
                           History-Date-Display    A10 A1
                           History-Quantity-Display        A8 A3
                           History-Unit-Of-Price-Display"$"
            END-PERFORM
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            CLOSE HistoryFile
            PERFORM Stock-Back.
       Stock-Back.
           DISPLAY a a a a a a Bold-On "0. Back"Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
               DISPLAY "Enter Choose Option:"
               ACCEPT option
               EVALUATE option
                   WHEN "0"
                   IF Admin-permission = "Y"
                       PERFORM Stock-Process
                   END-IF
                   WHEN OTHER
                       DISPLAY ESC Red-On "Please choose only 0"
                       ESC Reset-Color
                       PERFORM Search-Next-Process
               END-EVALUATE.
       View-Discount.
            OPEN I-O DiscountFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT DiscountFile
               CLOSE DiscountFile
               OPEN I-O DiscountFile
            END-IF

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a A8 Bold-On ESC Pink-On "Discount Record"
            ESC Reset-Color Bold-Off
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Discount ID"      A5
                    "Limit Amount"     A5
                    "Percent"          A5
                    "Start Date"       A8
                    "End Date"

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               READ DiscountFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE Discount-ID TO Discount-ID-Display
                   MOVE Limit-Amount TO Limit-Amount-Display
                   MOVE FUNCTION TRIM(Discount-ID-Display)
                       TO String-Format
                   DISPLAY String-Format           A7 A4
                           Limit-Amount-Display"$"    A7
                           Percent                 A7
                           Start-Date              A6
                           End-Date

            END-PERFORM
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            CLOSE DiscountFile
            MOVE 'Y' TO EOF
            PERFORM Discount-Back.
       Discount-Back.
               DISPLAY a a a a a a Bold-On "0. Back"Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
               DISPLAY "Enter Choose Option:"
               ACCEPT option
               EVALUATE option
                   WHEN "0"
                   IF Admin-permission ="Y"
                       PERFORM Discount-Process
                   END-IF
                   WHEN OTHER
                       DISPLAY ESC Red-On "Please choose only 0"
                       ESC Reset-Color
                       PERFORM Discount-Back
               END-EVALUATE.

       Add-Discount.
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
            MOVE "1" TO EOF
            ACCEPT Invoice-Date FROM DATE YYYYMMDD
            PERFORM UNTIL EOF = "0"
               OPEN I-O DiscountFile
                    IF File-Status = "35"
      *>                   DISPLAY "File does not exist. Creating file..."
                        OPEN OUTPUT DiscountFile
                        CLOSE DiscountFile
                        OPEN I-O DiscountFile
                    END-IF
                   MOVE 0 TO MAX-ID
                   IF Discount-ID = SPACES
                     MOVE 1 TO Discount-ID
                   ELSE
                        MOVE "N" TO EOF
                        PERFORM UNTIL EOF = "Y"
                           READ DiscountFile NEXT RECORD
                           AT END
                               MOVE "Y" TO EOF
                           NOT AT END
                               IF Discount-ID > MAX-ID
                                MOVE Discount-ID TO MAX-ID
                               END-IF
                        END-PERFORM
                   END-IF
                  ADD 1 TO MAX-ID
               MOVE MAX-ID TO Discount-ID
               MOVE Discount-ID TO Discount-ID-Display
               MOVE FUNCTION TRIM(Discount-ID-Display)
                       TO String-Format
               DISPLAY "Discount ID: " String-Format
               MOVE Discount-ID TO RelativeKey
               DISPLAY "Enter Limit Amount: "
               ACCEPT Limit-Amount
               DISPLAY "Enter Percent: "
               ACCEPT Percent

               MOVE "N" TO SS
               PERFORM UNTIL SS = "Y"
               DISPLAY "Enter Start Date (YYYYMMDD): "
               ACCEPT Start-Date
               MOVE Start-Date TO Start-Date-Num
               MOVE FUNCTION LENGTH(FUNCTION TRIM(Start-Date)) TO CC
               IF CC = 8
                   MOVE Start-Date TO Start-Date-Num
                   IF Start-Date-Num > 1
                       IF Start-Date-Num >= Invoice-Date
                           MOVE 'Y' TO SS
                       ELSE
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                         DISPLAY ESC Red-On
                         "Today-date equal and greater than use:"
                         Invoice-Date
                           ESC Reset-Color
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                       END-IF
                   ELSE
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                       DISPLAY b ESC Red-On"Start Date must be numeric."
                       ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   END-IF
               ELSE
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   DISPLAY b ESC Red-On
                       "Start Date must have 8 digits"
                       ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
               END-IF
               END-PERFORM


               MOVE "N" TO SS
               PERFORM UNTIL SS = "Y"
               DISPLAY "Enter End Date (YYYYMMDD): "
               ACCEPT End-Date
               MOVE End-Date TO End-Date-Num
               MOVE FUNCTION LENGTH(FUNCTION TRIM(End-Date)) TO DD

               IF DD = 8
                   MOVE End-Date TO End-Date-Num
                   IF End-Date-Num > 1
                       IF End-Date-Num >= Invoice-Date
                           MOVE 'Y' TO SS
                       ELSE
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                         DISPLAY ESC Red-On
                         "Today-date equal and greater than use:"
                         Invoice-Date
                           ESC Reset-Color
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                       END-IF

                   ELSE
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                       DISPLAY b ESC Red-On"End Date must be numeric."
                       ESC Reset-Color
                   DISPLAY Bold-On
                   H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   END-IF
               ELSE
                   DISPLAY Bold-On
                   H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   DISPLAY b ESC Red-On
                       "End Date must have 8 digits"
                       ESC Reset-Color
                   DISPLAY Bold-On
                   H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
               END-IF
               END-PERFORM

           IF Start-Date-Num > End-Date-Num
           DISPLAY " Error: Start Date cannot be after End Date. "
           CLOSE DiscountFile
           MOVE "N" TO Valid-Range
           PERFORM Add-Discount
           ELSE
               MOVE "Y" TO Valid-Range

           END-IF

           IF Valid-Range = "Y"

               WRITE Discount-Record
               INVALID KEY
                       DISPLAY "Error: Unable to write record."
               END-WRITE

               IF File-Status = "00"
                   DISPLAY " "
                   DISPLAY Bold-On
                   H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
                   DISPLAY ESC Green-On
                   "Discount added successfully."
                   ESC Reset-Color
                   DISPLAY " "
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
           END-IF

               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
               DISPLAY "1.Add more data"
               DISPLAY "0.Go Back"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
               DISPLAY "Please choose 1 or 0"
               ACCEPT EOF

               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off

               MOVE FUNCTION UPPER-CASE(EOF) TO EOF
               CLOSE DiscountFile
            END-PERFORM
            MOVE 'Y' TO EOF
           PERFORM Discount-Process.

       Update-Discount.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
           ACCEPT Invoice-Date FROM DATE YYYYMMDD
           OPEN I-O DiscountFile
            DISPLAY "Enter Discount-ID to Update: "
            ACCEPT RelativeKey

            READ DiscountFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                    DISPLAY Bold-On
                    H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
                    DISPLAY a a A8 Bold-On ESC Pink-On "Discount Record"
            ESC Reset-Color Bold-Off
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Discount ID"      A5
                    "Limit Amount"     A5
                    "Percent"          A5
                    "Start Date"       A8
                    "End Date"

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
            MOVE Discount-ID TO Discount-ID-Display
            MOVE Limit-Amount TO Limit-Amount-Display
            MOVE FUNCTION TRIM(Discount-ID-Display) TO String-Format
            DISPLAY String-Format                  A7 A4
                    Limit-Amount-Display"$"        A7
                    Percent                        A7
                    Start-Date                     A6
                    End-Date
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
            DISPLAY "Enter Limit Amount:"
            ACCEPT Limit-Amount
            DISPLAY "Enter Percent:"
            ACCEPT Percent
            MOVE "N" TO SS
            PERFORM UNTIL SS = "Y"
               DISPLAY "Enter Start Date (YYYYMMDD): "
               ACCEPT Start-Date
               MOVE Start-Date TO Start-Date-Num
               MOVE FUNCTION LENGTH(FUNCTION TRIM(Start-Date)) TO CC
               IF CC = 8
                   MOVE Start-Date TO Start-Date-Num
                   IF Start-Date-Num > 1
                       IF Start-Date-Num >= Invoice-Date
                           MOVE 'Y' TO SS
                       ELSE
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                         DISPLAY ESC Red-On
                         "Today-date equal and greater than use:"
                         Invoice-Date
                         ESC Reset-Color
                         DISPLAY " "
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                       END-IF
                   ELSE
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                       DISPLAY b ESC Red-On"Start Date must be numeric."
                               ESC Reset-Color
                        DISPLAY " "
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
                  END-IF
               ELSE
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   DISPLAY b ESC Red-On
                       "Start Date must have 8 digits"
                       ESC Reset-Color
                       DISPLAY " "
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
               END-IF
           END-PERFORM
           MOVE "N" TO SS
           PERFORM UNTIL SS = "Y"
               DISPLAY "Enter End Date (YYYYMMDD): "
               ACCEPT End-Date
               MOVE End-Date TO End-Date-Num
               MOVE FUNCTION LENGTH(FUNCTION TRIM(End-Date)) TO DD
               IF DD = 8
                   MOVE End-Date TO End-Date-Num
                   IF End-Date-Num > 1
                       IF End-Date-Num >= Invoice-Date
                           MOVE 'Y' TO SS
                       ELSE
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
                         DISPLAY ESC Red-On
                         "Today-date equal and greater than use:"
                         Invoice-Date
                           ESC Reset-Color
                           DISPLAY " "
                         DISPLAY Bold-On
                         H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                       END-IF
                   ELSE
                      DISPLAY Bold-On
                      H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                       DISPLAY b ESC Red-On"End Date must be numeric."
                       ESC Reset-Color
                       DISPLAY " "
                  DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   END-IF
               ELSE
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                   DISPLAY b ESC Red-On
                       "End Date must have 8 digits"
                       ESC Reset-Color
                       DISPLAY " "
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
               END-IF
           END-PERFORM
           IF Start-Date-Num > End-Date-Num
               DISPLAY ESC Red-On
               " Error: Start Date cannot be after End Date. "
               ESC Reset-Color
               CLOSE DiscountFile
               MOVE "N" TO Valid-Range
               PERFORM Update-Discount
           ELSE
               MOVE "Y" TO Valid-Range
           END-IF

           IF Valid-Range = "Y"
               REWRITE Discount-Record INVALID KEY
                   DISPLAY ESC Red-On
                   "Error: Unable to rewrite record." ESC Reset-Color
               END-REWRITE

               IF File-Status = "00"
                   DISPLAY " "
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
                   DISPLAY ESC Green-On
                   "Discount updated successfully." ESC Reset-Color
                   DISPLAY " "
               ELSE
                   DISPLAY "File Status: " File-Status
               END-IF
           END-IF
            END-READ
            CLOSE DiscountFile
            MOVE 'Y' TO EOF
           PERFORM Discount-Process.

       Delete-Discount.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
           OPEN I-O DiscountFile
            DISPLAY "Enter Discount-ID to Delete:"
            ACCEPT RelativeKey
            PERFORM Preview-Discount
            DISPLAY "Are you sure want to delete?"
            DISPLAY "1. Confirm"
            DISPLAY "0. Cancel"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT User-Choice
            MOVE "N" TO Delete-Permission
            EVALUATE User-Choice
            WHEN "1"
               MOVE "Y" TO Delete-Permission
            WHEN "0"
               MOVE "N" TO Delete-Permission
            WHEN OTHER
               MOVE "N" TO Delete-Permission
            END-EVALUATE
            IF Delete-Permission = "Y"
               DELETE DiscountFile INVALID KEY
                   DISPLAY ESC Red-On "Error: Record not found."
                   ESC Reset-Color
                NOT INVALID KEY
                   IF File-Status = "00"
                       DISPLAY " "
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                       DISPLAY ESC Green-On
                               "Discount deleted successfully."
                               ESC Reset-Color
                       DISPLAY " "
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
               END-DELETE
               CLOSE DiscountFile
               PERFORM Discount-Back
            ELSE
               PERFORM Discount-Process
            END-IF.
       Preview-Discount.
            READ DiscountFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                    DISPLAY Bold-On
                    H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
                    DISPLAY a a A8 Bold-On ESC Pink-On "Discount Record"
            ESC Reset-Color Bold-Off
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Discount ID"      A5
                    "Limit Amount"     A5
                    "Percent"          A5
                    "Start Date"       A8
                    "End Date"

            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
                    MOVE Discount-ID TO Discount-ID-Display
                    MOVE Limit-Amount TO Limit-Amount-Display
                    MOVE FUNCTION TRIM(Discount-ID-Display)
                       TO String-Format
                    DISPLAY String-Format              A7 A4
                           Limit-Amount-Display"$"     A7
                           Percent                     A7
                           Start-Date                  A6
                           End-Date
                    DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                    Bold-Off
            END-READ.
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
              DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
              Bold-Off
              DISPLAY a a a a a a Bold-On "0. Back"Bold-Off
              DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
              Bold-Off
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
                       OPEN I-O CartFile
                       MOVE "N" TO EOF
                       MOVE "N" TO Cart-Flag
                       PERFORM UNTIL EOF = "Y"
                           READ CartFile NEXT RECORD
                           AT END
                               MOVE "Y" TO EOF
                           NOT AT END
                               IF Cart-Record NOT = ""
                                   MOVE "Y" TO Cart-Flag
                               END-IF
                           END-READ
                       END-PERFORM
                       CLOSE CartFile
                       IF Cart-Flag = "Y"
                       DISPLAY "Enter Customer Name:"
                       ACCEPT Customer-Name
                       PERFORM Buy-Confirm
                       MOVE "0" TO EOFP
                       ELSE
                           DISPLAY Bold-On H10 H10 H10 H10 H10
                           H10 H10 H10 H10
                           Bold-Off
                           DISPLAY Bold-On ESC Red-On
                           "Please add to cart, Try again!"
                           ESC Reset-Color
                           Bold-Off
                       END-IF
                   WHEN "5"
                       PERFORM Update-Cart-Quantity
                       MOVE "1" TO EOFP
                   WHEN "6"
                       PERFORM Delete-Cart
                       MOVE "1" TO EOFP
                   WHEN "0"
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY ESC Red-On "Invalid choice. Try again."
                       ESC Reset-Color
                       MOVE "1" TO EOFP
               END-EVALUATE
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM.
       Find-Id-Item.
            OPEN I-O ItemFile
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Item-ID to Find:"
            ACCEPT RelativeKey
            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
                IF File-Status = "00"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY a a a Bold-On ESC Blue-On "Item Record"
                           Bold-Off ESC Reset-Color
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                           Bold-Off
                   DISPLAY "Item-ID"               A5
                            "Name"                 A5  A5 A7
                            "Category"             A4
                            "Quantity"             A10 A1
                            "Price"
                   MOVE FUNCTION TRIM(Item-ID-Display) TO String-Format
                   DISPLAY H10 H10 H10 H10 H10 H10 H10 H10 H10
                   DISPLAY String-Format           A2
                           Item-Name               A1
                           Item-Category           A5
                           Item-Qty-Display        A5
                           Item-Price-Display"$"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                END-IF
            END-READ
            CLOSE ItemFile
            PERFORM Search-Next-Process.
       Admin-Find-Id-Item.
            OPEN I-O ItemFile
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Item-ID to Find:"
            ACCEPT RelativeKey
            READ ItemFile INVALID KEY
               DISPLAY "Error: Record not found."
               PERFORM Admin-Find-Id-Item
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY a a a Bold-On ESC Blue-On "Item Record"
                           Bold-Off ESC Reset-Color
                   DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                           Bold-Off
                   DISPLAY "Item-ID"      A5
                           "Name"         A5 A5 A7
                           "Category"     A4
                           "Quantity"     A6
                           "Sale Price"   A5
                           "Buy Price"
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   MOVE Item-ID TO Item-ID-Display
                   MOVE Item-Qty TO Item-Qty-Display
                   MOVE Item-Price TO Item-Price-Display
                   OPEN I-O HistoryFile
                   IF File-Status = "35"
      *    >          DISPLAY "File does not exist. Creating file..."
                       OPEN OUTPUT HistoryFile
                       CLOSE HistoryFile
                       OPEN I-O HistoryFile
                   END-IF
                   MOVE "N" TO EFC
                   MOVE 0 TO Buy-Sale
                    PERFORM UNTIL EFC = "Y"
                       READ HistoryFile NEXT RECORD
                       AT END
                           MOVE "Y" TO EFC
                           EXIT PERFORM *>new added
                       NOT AT END
                           IF Item-ID = History-Item-ID
                              MOVE History-Unit-Of-Price TO Buy-Sale
                           END-IF
                    END-PERFORM
                    CLOSE HistoryFile
                    MOVE FUNCTION TRIM(Item-ID-Display) TO String-Format
                    IF Item-Qty = 0
                       DISPLAY String-Format           A2
                               Item-Name               A1
                               Item-Category           A5
                               Item-Qty-Display        A5
                               Item-Price-Display"$"   A8
                               Buy-Sale"$"
                               ESC Red-On
                               "<<Item quantity out of Stock>>"
                               ESC Reset-Color
                    ELSE IF Item-Qty < 10
                       DISPLAY String-Format           A2
                               Item-Name               A1
                               Item-Category           A5
                               Item-Qty-Display        A5
                               Item-Price-Display"$"   A8
                               Buy-Sale"$"
                               ESC Yellow-On
                               "<<Item quantity is less than 10>>"
                               ESC Reset-Color
                    ELSE
                       DISPLAY String-Format           A2
                               Item-Name               A1
                               Item-Category           A5
                               Item-Qty-Display        A5
                               Item-Price-Display"$"   A8
                               Buy-Sale"$"
                    END-IF
                END-IF
            END-READ
            CLOSE ItemFile
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            PERFORM Search-Next-Process.
       Find-Item.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                   Bold-Off
           MOVE "N" TO EOF
           MOVE "N" TO Found
           OPEN INPUT ItemFile
           DISPLAY "Enter Item Name:"
           ACCEPT Input-Item-Name
           PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                       EXIT PERFORM
                   NOT AT END
                       IF FUNCTION LOWER-CASE(Item-Name) =
                          FUNCTION LOWER-CASE(Input-Item-Name)
                          DISPLAY Bold-On
                                  H10 H10 H10 H10 H10 H10 H10 H10 H10
                                  Bold-Off
                          DISPLAY a a a Bold-On ESC Blue-On
                                  "Item Record"
                                  Bold-Off ESC Reset-Color
                          MOVE Item-ID TO Item-ID-Display
                          MOVE Item-Qty TO Item-Qty-Display
                          MOVE Item-Price TO Item-Price-Display
                          DISPLAY Bold-On
                                  e10 e10 e10 e10 e10 e10 e10 e10 e10
                                  Bold-Off
                          DISPLAY "Item-ID"               A5
                                  "Name"                  A5  A5 A7
                                  "Category"              A4  A10
                                  "Quantity"              A10 A1
                                  "Price"
                          MOVE FUNCTION TRIM(Item-ID-Display)
                          TO String-Format
                          DISPLAY Bold-On
                                  H10 H10 H10 H10 H10 H10 H10 H10 H10
                                  Bold-Off
                          DISPLAY String-Format           A2
                                  Item-Name               A1
                                  Item-Category           A5 A10
                                  Item-Qty-Display        A5
                                  Item-Price-Display"$"
                          MOVE "Y" TO Found
                          MOVE "Y" TO EOF    *> Stop loop after finding the item
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ItemFile
           IF Found NOT = "Y"
           DISPLAY ESC Red-On
           DISPLAY "! Please enter only item name !"
           DISPLAY ESC Reset-Color
           PERFORM Find-Item
           END-IF.
       Admin-Find-Item.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           MOVE "N" TO EOF
           MOVE "N" TO Found
           OPEN INPUT ItemFile
           DISPLAY "Enter Item Name:"
           ACCEPT Input-Item-Name
           PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                       EXIT PERFORM
                   NOT AT END
                       IF FUNCTION LOWER-CASE(Item-Name) =
                           FUNCTION LOWER-CASE(Input-Item-Name)

                       DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10
                               H10 H10 Bold-Off
                       DISPLAY a a a Bold-On ESC Blue-On "Item Record"
                               Bold-Off ESC Reset-Color
                       DISPLAY Bold-On
                               e10 e10 e10 e10 e10 e10 e10 e10 e10
                               Bold-Off
                       DISPLAY "Item-ID"       A5
                               "Name"          A5 A5 A7
                               "Category"      A4
                               "Quantity"      A6
                               "Sale Price"    A5
                               "Buy Price"
                       DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10
                               H10 H10 Bold-Off
                       MOVE Item-ID TO Item-ID-Display
                       MOVE Item-Qty TO Item-Qty-Display
                       MOVE Item-Price TO Item-Price-Display
                       OPEN I-O HistoryFile
                       IF File-Status = "35"
      *            >          DISPLAY "File does not exist. Creating file..."
                           OPEN OUTPUT HistoryFile
                           CLOSE HistoryFile
                           OPEN I-O HistoryFile
                       END-IF
                       MOVE "N" TO EFC
                       MOVE 0 TO Buy-Sale
                        PERFORM UNTIL EFC = "Y"
                           READ HistoryFile NEXT RECORD
                           AT END

                                       MOVE "Y" TO EFC
                               EXIT PERFORM *>new added
                           NOT AT END
                               IF Item-ID = History-Item-ID
                                  MOVE History-Unit-Of-Price TO Buy-Sale
                               END-IF
                        END-PERFORM
                       CLOSE HistoryFile
                       MOVE FUNCTION TRIM(Item-ID-Display)
                       TO String-Format
                       IF Item-Qty = 0
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Red-On
                                    "<<Item quantity out of Stock>>"
                                    ESC Reset-Color
                       ELSE IF Item-Qty < 10
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                                    ESC Yellow-On
                                    "<<Item quantity is less than 10>>"
                                    ESC Reset-Color
                       ELSE
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"   A8
                                   Buy-Sale"$"
                       END-IF
                           MOVE "Y" TO Found
                           MOVE "Y" TO EOF    *> Stop loop after finding the item
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ItemFile
           IF Found NOT = "Y"
           DISPLAY ESC Red-On
           DISPLAY "! Please enter only item name !"
           DISPLAY ESC Reset-Color
           PERFORM Admin-Find-Item
           END-IF
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           PERFORM Search-Next-Process.
       Find-Category-Item.
            PERFORM Category-Item
            OPEN input ItemFile
            MOVE "N" TO EOF
            MOVE 0 TO Category-Count
            PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
                   exit perform
               NOT AT END
                   IF Item-Category = Category-Index-Name(INPUT-IDX)
                       ADD 1 TO Category-Count
                   END-IF
               END-READ
            END-PERFORM
            CLOSE ItemFile
            OPEN input ItemFile
            IF Category-Count > 0
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY a a a Bold-On ESC Blue-On "Item Record"
                       Bold-Off ESC Reset-Color
               DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                       Bold-Off
               DISPLAY "Item-ID"       A5
                       "Name"          A5 A5 A7
                       "Category"      A4 A10
                       "Quantity"      A10 A1
                       "Price"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               MOVE "N" TO EOF
               CLOSE ItemFile
               OPEN INPUT ItemFile
               PERFORM UNTIL EOF = "Y"
                  READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                       EXIT PERFORM
                   NOT AT END
                       IF Item-Category = Category-Index-Name(INPUT-IDX)
                           MOVE Item-ID TO Item-ID-Display
                           MOVE Item-Qty TO Item-Qty-Display
                           MOVE Item-Price TO Item-Price-Display
                           MOVE FUNCTION TRIM(Item-ID-Display)
                           TO String-Format
                           DISPLAY String-Format           A2
                                   Item-Name               A1
                                   Item-Category           A5 A10
                                   Item-Qty-Display        A5
                                   Item-Price-Display"$"
                       END-IF
                  END-READ
               END-PERFORM
               CLOSE ItemFile
               MOVE "Y" TO EOF
               IF Admin-permission ='Y'
                    DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                    PERFORM Search-Next-Process
               END-IF
            ELSE
               DISPLAY ESC Red-On
                       "This Category Not Found Item." ESC Reset-Color
                       DISPLAY Bold-On
                       H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               PERFORM Try-Process
            END-IF
            MOVE 'Y' TO EOF
            CLOSE ItemFile.
       Admin-Find-Category-Item.
           PERFORM Category-Item
            OPEN input ItemFile
            MOVE "N" TO EOF
            MOVE 0 TO Category-Count
            PERFORM UNTIL EOF = "Y"
              READ ItemFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
                   exit perform
               NOT AT END
                   IF Item-Category = Category-Index-Name(INPUT-IDX)
                       ADD 1 TO Category-Count
                   END-IF
              END-READ
            END-PERFORM
            CLOSE ItemFile
            OPEN INPUT ItemFile
            IF Category-Count > 0
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY a a a Bold-On ESC Blue-On "Item Record"
                       Bold-Off ESC Reset-Color
               DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                       Bold-Off
               DISPLAY "Item-ID"           A5
                       "Name"              A5 A5 A7
                       "Category"          A4
                       "Quantity"          A6
                       "Sale Price"        A5
                       "Buy Price"
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               MOVE "N" TO EOF
               CLOSE ItemFile
               OPEN INPUT ItemFile
               PERFORM UNTIL EOF = "Y"
                  READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                       exit perform
                   NOT AT END
                       IF Item-Category = Category-Index-Name(INPUT-IDX)
                           MOVE Item-ID TO Item-ID-Display
                           MOVE Item-Qty TO Item-Qty-Display
                           MOVE Item-Price TO Item-Price-Display

                           OPEN I-O HistoryFile
                           IF File-Status = "35"
      *                >          DISPLAY "File does not exist. Creating file..."
                               OPEN OUTPUT HistoryFile
                               CLOSE HistoryFile
                               OPEN I-O HistoryFile
                           END-IF
                           MOVE "N" TO EFC
                           MOVE 0 TO Buy-Sale
                           PERFORM UNTIL EFC = "Y"
                              READ HistoryFile NEXT RECORD
                               AT END
                                       MOVE "Y" TO EFC
                                   EXIT PERFORM *>new added
                               NOT AT END
                                   IF Item-ID = History-Item-ID
                                  MOVE History-Unit-Of-Price TO Buy-Sale
                                   END-IF
                              END-READ
                           END-PERFORM
                           CLOSE HistoryFile
                           MOVE FUNCTION TRIM(Item-ID-Display)
                           TO String-Format
                           IF Item-Qty = 0
                               DISPLAY String-Format           A2
                                       Item-Name               A1
                                       Item-Category           A5
                                       Item-Qty-Display        A5
                                       Item-Price-Display"$"   A8
                                       Buy-Sale"$"
                                       ESC Red-On
                                       "<<Item quantity out of Stock>>"
                                       ESC Reset-Color
                           ELSE IF Item-Qty < 10
                               DISPLAY String-Format           A2
                                       Item-Name               A1
                                       Item-Category           A5
                                       Item-Qty-Display        A5
                                       Item-Price-Display"$"   A8
                                       Buy-Sale"$"
                                       ESC Yellow-On
                                    "<<Item quantity is less than 10>>"
                                       ESC Reset-Color
                           ELSE
                               DISPLAY String-Format           A2
                                       Item-Name               A1
                                       Item-Category           A5
                                       Item-Qty-Display        A5
                                       Item-Price-Display"$"   A8
                                       Buy-Sale"$"
                           END-IF
                       END-IF
                  END-READ
                END-PERFORM
                CLOSE ItemFile
                MOVE "Y" TO EOF
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                PERFORM Search-Next-Process
            ELSE
                DISPLAY ESC Red-On
                        "This Category Not Found Item."
                        ESC Reset-Color
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                PERFORM Try-Process
            END-IF
            MOVE 'Y' TO EOF
            CLOSE ItemFile.
       Try-Process.
            DISPLAY "1. Try Again "
            DISPLAY "0. Back "
            DISPLAY "Enter Choose option only(1 or 0)"
            ACCEPT User-Choice
            EVALUATE User-Choice
                WHEN "1"
                   PERFORM Find-Category-Item
                WHEN "0"
                   PERFORM Item-Process
                WHEN OTHER
                   DISPLAY ESC Red-On" !Please choose only(1 or 2) !"
                   ESC Reset-Color
                   PERFORM Try-Process
            END-EVALUATE.
       Search-Next-Process.
            DISPLAY a a a a a a Bold-On "0. Back"Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT option
            EVALUATE option
               WHEN "0"
                   PERFORM Item-Process
               WHEN OTHER
                   DISPLAY ESC Red-On "Please choose only 0"
                           ESC Reset-Color
                   PERFORM Search-Next-Process
            END-EVALUATE.
       Add-Cart.
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            OPEN I-O ItemFile
            DISPLAY "Enter Item-ID to Add:"
            ACCEPT RelativeKey
            READ ItemFile INVALID KEY
               DISPLAY ESC Red-On "Error: Record not found."
               ESC Reset-Color
            NOT INVALID KEY
                IF File-Status = "00"
                    MOVE 0 TO Cart-Total-Quantity
                     OPEN I-O SaleCartFile
                    IF File-Status = "35"
      *>                   DISPLAY "File does not exist. Creating file..."
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

                    OPEN I-O CartFile
                    IF File-Status = "35"
      *>                   DISPLAY "File does not exist. Creating file..."
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

                   ADD 1 TO MAX-ID
                   MOVE MAX-ID TO Cart-ID
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
               Bold-Off
                   DISPLAY "Item Name: " Item-Name
                   MOVE Item-ID TO Cart-Item-ID
                   DISPLAY "Enter Quantity: "
                   ACCEPT Cart-Quantity
                   MOVE Item-Price TO Cart-Unit-Of-Price
                   COMPUTE Cart-Total-Quantity =
                           Cart-Total-Quantity + Cart-Quantity
                   IF Cart-Total-Quantity > Item-Qty
                       IF Item-Qty = 0
                           DISPLAY ESC Red-On
                           "This item is out of stock!!!!"
                           ESC Reset-Color
                       ELSE
                           MOVE Item-Qty To Item-Qty-Display
                           MOVE FUNCTION TRIM(Item-Qty-Display)
                           TO String-Format
                           DISPLAY ESC Red-On
                           "This item only have " String-Format
                           ESC Reset-Color
                           DISPLAY Bold-On
                           H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
                       END-IF
                   ELSE
                   WRITE Cart-Record INVALID KEY
                   DISPLAY ESC Red-On "Error: Unable to write record."
                   ESC Reset-Color
                   END-WRITE
                   IF File-Status = "00"
                       DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                       DISPLAY ESC Green-On
                               "Added Cart successfully."
                               ESC Reset-Color
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
      *>          DISPLAY "File does not exist. Creating file..."
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
            DISPLAY H10 H10 H10 H10 H10 H10 H10 H10 H10
            IF Cart-Flag = "Y"
                DISPLAY a a a ESC Green-On Bold-On "Cart Record"
                        Bold-Off ESC Reset-Color
                DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10
                        Bold-Off
                DISPLAY "Cart ID"              A5
                        "Item Name"            A5  A5
                        "Quantity"             A10 A1
                        "Price"
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                Bold-Off
                MOVE "N" TO EOF
                MOVE 0 TO Total
                PERFORM UNTIL EOF = "Y"
                  READ CartFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                        OPEN I-O ItemFile
                        MOVE Cart-Item-ID TO RelativeKey
                        READ ItemFile INVALID KEY
                           DISPLAY ESC Red-On "Error: Record not found."
                           ESC Reset-Color
                        NOT INVALID KEY
                            IF File-Status = "00"
                               MOVE Item-Name TO Input-Item-Name
                            END-IF
                        END-READ
                        CLOSE ItemFile
                       MOVE Cart-ID TO Cart-ID-Display
                       MOVE Cart-Item-ID TO Cart-Item-ID-Display
                       MOVE Cart-Quantity TO Cart-Quantity-Display
                       MOVE Cart-Unit-Of-Price TO
                       Cart-Unit-Of-Price-Display
                       MOVE FUNCTION TRIM(Cart-ID-Display)
                       TO String-Format
                       DISPLAY String-Format               A2
                               Input-Item-Name             A4
                               Cart-Quantity-Display       A5
                               Cart-Unit-Of-Price-Display"$"
                       COMPUTE Total = Total +
                               (Cart-Quantity * Cart-Unit-Of-Price)
                  END-READ
                END-PERFORM
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                MOVE Total TO Total-Display
                MOVE FUNCTION TRIM(Total-Display) TO String-Format
                DISPLAY "Total: $" String-Format
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
            END-IF
            CLOSE CartFile.
       Update-Cart-Quantity.
            OPEN I-O CartFile
            DISPLAY "Enter Cart-ID to Update:"
            ACCEPT Cart-ID
            READ CartFile INVALID KEY
               DISPLAY ESC Red-On"Error: Record not found." ESC
               Reset-Color
            NOT INVALID KEY
               IF File-Status = "00"
                   DISPLAY "Enter Quantity:"
                   ACCEPT Cart-Quantity
                   REWRITE Cart-Record INVALID KEY
                   DISPLAY ESC Red-On"Error: Unable to rewrite record."
                   ESC Reset-Color
                   END-REWRITE
                   IF File-Status = "00"
                       DISPLAY ESC Green-On
                               "Cart updated successfully."
                               ESC Reset-Color
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
                   DISPLAY ESC Green-On
                           "Cart deleted successfully."
                           ESC Reset-Color
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
           MOVE "N" TO Cart-Flag
           ACCEPT Invoice-Date FROM DATE YYYYMMDD
           ACCEPT WS-TIME-RAW FROM TIME.
           PERFORM UNTIL EOF = "Y"
               READ CartFile NEXT RECORD
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
      *>      Update Quantity

                OPEN I-O ItemFile
                MOVE Cart-Item-ID TO RelativeKey
                READ ItemFile INVALID KEY
                   DISPLAY ESC Red-On "Error: Record not found."
                   ESC Reset-Color
                NOT INVALID KEY
                    IF File-Status = "00"
                           COMPUTE Update-Quantity =
                                   Item-Qty - Cart-Quantity
                           MOVE Update-Quantity TO Item-Qty
                           REWRITE Item-Record INVALID KEY
                               DISPLAY ESC Red-On
                                      "Error: Unable to rewrite record."
                                       ESC Reset-Color
                           END-REWRITE
                    END-IF
                END-READ
                CLOSE ItemFile
                OPEN I-O SaleCartFile
                IF File-Status = "35"
      *>              DISPLAY "File does not exist. Creating file..."
                   OPEN OUTPUT SaleCartFile
                   CLOSE SaleCartFile
                   OPEN I-O SaleCartFile
                END-IF
                MOVE Cart-ID TO Sale-Cart-ID
                MOVE Input-ID TO Invoice-Casher-ID
                MOVE Cart-Item-ID TO Sale-Cart-Item-ID
                MOVE Cart-Quantity TO Sale-Cart-Quantity
                MOVE Invoice-Date TO Sale-Date
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
      *>          DISPLAY "File does not exist. Creating file..."
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
            MOVE WS-TIME-RAW(1:2) TO WS-HH
            MOVE WS-TIME-RAW(3:2) TO WS-MM
            MOVE WS-TIME-RAW(5:2) TO WS-SS
            IF WS-HH >= 12
               MOVE 'PM' TO WS-AMPM
               SUBTRACT WS-HH FROM 12 GIVING WS-HH
            ELSE
               MOVE 'AM' TO WS-AMPM
            END-IF
            STRING WS-HH       DELIMITED BY SIZE
                   ':'         DELIMITED BY SIZE
                   WS-MM       DELIMITED BY SIZE
                   ' '         DELIMITED BY SIZE
                   WS-AMPM     DELIMITED BY SIZE INTO Invoice-Time
            OPEN I-O DiscountFile
            MOVE 0 TO Discount-Price
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
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
                      IF Total>= Limit-Amount
                          COMPUTE Discount-Price = Total * (Percent/100)
                          EXIT PERFORM
                      END-IF
                   END-IF
               END-READ
            END-PERFORM
            CLOSE DiscountFile
            ADD 1 TO MAX-ID
            MOVE MAX-ID TO Invoice-ID
            MOVE Input-ID TO Invoice-Casher-ID
            MOVE Customer-Name TO Invoice-Customer-Name
            MOVE Temp-ID-List TO Item-ID-List
            MOVE Total TO Total-Amount
            MOVE Discount-Price TO Discount
            COMPUTE Final-Total-Price = Total-Amount - Discount
            MOVE Final-Total-Price TO Final-Amount
            MOVE 500 TO Tax
            COMPUTE Final-Pay-Price = Final-Amount + Tax
            MOVE Final-Pay-Price TO Pay-Amount
            MOVE "Pending" TO Invoice-Status
            WRITE Invoice-Record
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY ESC Blue-On
                   "Invoice added successfully." ESC Reset-Color
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            CLOSE InvoiceFile
            CLOSE CartFile
            PERFORM View-Invocie
            DISPLAY  a a a a a a Bold-On "0. Back"Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT User-Choice
            EVALUATE User-Choice
               WHEN "0"
                   MOVE "0" TO EOFP
            END-EVALUATE.
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
                STRING WS-YEAR     DELIMITED BY SIZE
                       "-"         DELIMITED BY SIZE
                       WS-MONTH    DELIMITED BY SIZE
                       "-"         DELIMITED BY SIZE
                       WS-DAY      DELIMITED BY SIZE INTO WS-DATE-OUT
                MOVE Invoice-ID TO Invoice-ID-Display
                DISPLAY "Casher ID      : " Invoice-Casher-ID
                DISPLAY H10 H10 H5
                MOVE FUNCTION TRIM(Invoice-ID-Display)
                       TO String-Format
                DISPLAY "Invoice-ID     : " String-Format
                DISPLAY "Customer-Name  : " Invoice-Customer-Name
                DISPLAY "Date           : " WS-DATE-OUT
                DISPLAY "Time           : " Invoice-Time
                DISPLAY "Status         : " ESC Red-On
                Invoice-Status ESC Reset-Color
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                Bold-Off
                DISPLAY "No"            A10 A3
                        "Item Name"     A5 A5
                        "Quantity"      A7
                        "Unit-Of-Price"
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                Bold-Off
                PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                    IF Invoice-Sale-Cart-ID(IDX) NUMERIC
                       MOVE Invoice-Sale-Cart-ID(IDX) TO Sale-Cart-ID
                       READ SaleCartFile
                       NOT INVALID KEY
                           OPEN I-O ItemFile
                           MOVE Sale-Cart-Item-ID TO RelativeKey
                           READ ItemFile INVALID KEY
                           DISPLAY ESC Red-On "Error: Record not found."
                           ESC Reset-Color
                           NOT INVALID KEY
                               IF File-Status = "00"
                                   MOVE Item-Name TO Input-Item-Name
                               END-IF
                           END-READ
                           CLOSE ItemFile
                           MOVE Sale-Cart-Item-ID TO
                           Sale-Cart-Item-ID-Display
                           MOVE Sale-Cart-Quantity TO
                           Sale-Cart-Quantity-Display
                           MOVE Sale-Cart-Unit-Of-Price TO
                           Sale-Cart-Unit-Of-Price-Display
                           MOVE FUNCTION TRIM(IDX) TO String-Format
                           DISPLAY String-Format               A5
                                   Input-Item-Name             A4
                                   Sale-Cart-Quantity-Display  A9
                                   Sale-Cart-Unit-Of-Price-Display"$"
                       END-READ
                    END-IF
                END-PERFORM
                MOVE Total-Amount TO Total-Amount-Display
                MOVE Discount TO Discount-Display
                MOVE Final-Amount TO Final-Amount-Display
                MOVE Pay-Amount TO Pay-Amount-Display
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                MOVE FUNCTION TRIM(Total-Amount-Display)
                       TO String-Format
                DISPLAY "Total-Amount   : $" String-Format
                MOVE FUNCTION TRIM(Discount-Display)
                       TO String-Format
                DISPLAY "Discount       : $" String-Format
                MOVE FUNCTION TRIM(Final-Amount-Display)
                       TO String-Format
                DISPLAY "Final-Amount   : $" String-Format
                MOVE FUNCTION TRIM(Tax)
                       TO String-Format
                DISPLAY "Tax            : $" String-Format
                MOVE FUNCTION TRIM(Pay-Amount-Display)
                       TO String-Format
                DISPLAY "Pay-Amount     : $" String-Format
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
            END-READ
            CLOSE SaleCartFile
            CLOSE InvoiceFile.
       Pending-Invoice.
             OPEN I-O InvoiceFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT InvoiceFile
               CLOSE InvoiceFile
               OPEN I-O InvoiceFile
            END-IF
            OPEN I-O SaleCartFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT SaleCartFile
               CLOSE SaleCartFile
               OPEN I-O SaleCartFile
            END-IF
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a A9 Bold-On
                    ESC Green-On "Pending Invoice Record"
                    ESc Reset-Color Bold-Off
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
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
                       MOVE Invoice-ID TO Invoice-ID-Display
                       MOVE FUNCTION TRIM(Invoice-ID-Display)
                       TO String-Format
                       DISPLAY "Casher ID      : " Invoice-Casher-ID
                       DISPLAY H10 H10 H5
                       DISPLAY "Invoice-ID     : " String-Format
                       DISPLAY "Customer-Name  : " Invoice-Customer-Name
                       DISPLAY "Date           : " WS-DATE-OUT
                       DISPLAY "Time           : " Invoice-Time
                       DISPLAY "Status         : " ESC Red-On
                       Invoice-Status ESC Reset-Color
                       DISPLAY Bold-On H10 H10 H5 Bold-Off
                       DISPLAY "No"            A10 A3
                               "Item Name"     A5 A5
                               "Quantity"      A7
                               "Unit-Of-Price"
                       DISPLAY H10 H10 H10 H10 H10 H10 H10 H10
                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                           IF Invoice-Sale-Cart-ID(IDX) NUMERIC
                               MOVE Invoice-Sale-Cart-ID(IDX) TO
                               Sale-Cart-ID
                               READ SaleCartFile
                               NOT INVALID KEY
                                   MOVE Sale-Cart-Item-ID TO
                               Sale-Cart-Item-ID-Display
                               MOVE Sale-Cart-Quantity TO
                               Sale-Cart-Quantity-Display
                               MOVE Sale-Cart-Unit-Of-Price TO
                               Sale-Cart-Unit-Of-Price-Display

                               OPEN I-O ItemFile
                           MOVE Sale-Cart-Item-ID TO RelativeKey
                           READ ItemFile INVALID KEY
                           DISPLAY ESC Red-On "Error: Record not found."
                           ESC Reset-Color
                           NOT INVALID KEY
                               IF File-Status = "00"
                                   MOVE Item-Name TO Input-Item-Name
                               END-IF

                           END-READ
                           CLOSE ItemFile
                           MOVE FUNCTION TRIM(IDX) TO String-Format
                           DISPLAY String-Format               A5
                                   Input-Item-Name             A4
                                   Sale-Cart-Quantity-Display  A9
                                   Sale-Cart-Unit-Of-Price-Display"$"
                               END-READ
                           END-IF
                       END-PERFORM
                    MOVE Total-Amount TO Total-Amount-Display
                    MOVE Discount TO Discount-Display
                    MOVE Final-Amount TO Final-Amount-Display
                    MOVE Pay-Amount TO Pay-Amount-Display
                    DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                            Bold-Off
                    MOVE FUNCTION TRIM(Total-Amount-Display)
                    TO String-Format
                    DISPLAY "Total-Amount   : $" String-Format
                    MOVE FUNCTION TRIM(Discount-Display)
                    TO String-Format
                    DISPLAY "Discount       : $" String-Format
                    MOVE FUNCTION TRIM(Final-Amount-Display)
                    TO String-Format
                    DISPLAY "Final-Amount   : $" String-Format
                    MOVE FUNCTION TRIM(Tax)
                    TO String-Format
                    DISPLAY "Tax            : $" Tax
                    MOVE FUNCTION TRIM(Pay-Amount-Display)
                    TO String-Format
                    DISPLAY "Pay-Amount     : $" String-Format

                        DISPLAY Bold-On ESC Blue-On
                            H10 H10 H10 H10 H10 H10 H10 H10 H10
                            ESC Reset-Color Bold-Off
                    DISPLAY " "
                    DISPLAY " "
                   END-IF
               END-READ
            END-PERFORM
            CLOSE SaleCartFile
            CLOSE InvoiceFile.
       Invoice-Process.
            MOVE "1" TO EOFP
            PERFORM UNTIL EOFP = "0"
               PERFORM Pending-Invoice
               DISPLAY Bold-On             "1. Invoice Confirm"
                       Bold-Off
                       a a a a a Bold-On   "0. Back"
                       Bold-Off
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               DISPLAY "Enter Choose Option:"
               ACCEPT User-Choice
               DISPLAY " "
               DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                       Bold-Off
               EVALUATE User-Choice
                   WHEN "1"
                       PERFORM Invoice-Confirm
                   WHEN "0"
                       EXIT PERFORM
                       MOVE "0" TO EOFP
                   WHEN OTHER
                       DISPLAY ESC Red-On"Invalid choice. Try again."
                       ESC Reset-Color
                       MOVE "1" TO EOFP
               END-EVALUATE
               MOVE FUNCTION UPPER-CASE(EOFP) TO EOFP
            END-PERFORM
            MOVE "1" TO EOFP.
       Invoice-Confirm.
            OPEN I-O InvoiceFile
            DISPLAY "Enter Invoice-ID to Comfirm Invoice:"
            ACCEPT Invoice-ID
            READ InvoiceFile INVALID KEY
               DISPLAY "Error: Record not found."
            NOT INVALID KEY
               IF File-Status = "00"
                   MOVE "Completed" TO Invoice-Status
                   REWRITE Invoice-Record INVALID KEY
                   DISPLAY "Error: Unable to rewrite record."
                   END-REWRITE
                   IF File-Status = "00"
                       DISPLAY " "
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                       DISPLAY ESC Blue-On
                               "Invoice Completed successfully."
                               ESC Reset-Color
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                   ELSE
                       DISPLAY "File Status: " File-Status
                   END-IF
                END-IF
            END-READ
            CLOSE InvoiceFile
            DISPLAY Bold-On            "1. Next Confirm"
                    Bold-Off
                    a a a a a Bold-On  "0. Back"
                    Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT User-Choice
            EVALUATE User-Choice
               WHEN "0"
                   MOVE "0" TO EOFP
               WHEN "1"
                   MOVE "1" TO EOFP
            END-EVALUATE.
       Completed-Invoice.
            OPEN I-O InvoiceFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT InvoiceFile
               CLOSE InvoiceFile
               OPEN I-O InvoiceFile
            END-IF
            OPEN I-O SaleCartFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT SaleCartFile
               CLOSE SaleCartFile
               OPEN I-O SaleCartFile
            END-IF
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a A6 Bold-On
                    ESC Green-On "Completed Invoice Record"
                    ESC Reset-Color Bold-Off
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
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
                        STRING WS-YEAR     DELIMITED BY SIZE
                               "-"         DELIMITED BY SIZE
                               WS-MONTH    DELIMITED BY SIZE
                               "-"         DELIMITED BY SIZE
                               WS-DAY      DELIMITED BY SIZE
                               INTO WS-DATE-OUT
                       MOVE Invoice-ID TO Invoice-ID-Display
                       MOVE FUNCTION TRIM(Invoice-ID-Display)
                       TO String-Format
                       DISPLAY "Casher ID      : " Invoice-Casher-ID
                       DISPLAY H10 H10 H5
                       DISPLAY "Invoice-ID     : " String-Format
                       DISPLAY "Customer-Name  : " Invoice-Customer-Name
                       DISPLAY "Date           : " WS-DATE-OUT
                       DISPLAY "Time           : " Invoice-Time
                       DISPLAY "Status         : " ESC Green-On
                                                   Invoice-Status
                                                   ESC Reset-Color
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                       DISPLAY "No"            A10 A3
                               "Item Name"     A5 A5
                               "Quantity"      A7
                               "Unit-Of-Price"
                       DISPLAY H10 H10 H10 H10 H10 H10 H10 H10
                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                           IF Invoice-Sale-Cart-ID(IDX) NUMERIC
                               MOVE Invoice-Sale-Cart-ID(IDX) TO
                               Sale-Cart-ID
                               READ SaleCartFile
                               NOT INVALID KEY
                                   MOVE Sale-Cart-Item-ID TO
                                   Sale-Cart-Item-ID-Display
                                   MOVE Sale-Cart-Quantity TO
                                   Sale-Cart-Quantity-Display
                                   MOVE Sale-Cart-Unit-Of-Price TO
                                   Sale-Cart-Unit-Of-Price-Display
                           OPEN I-O ItemFile
                           MOVE Sale-Cart-Item-ID TO RelativeKey
                           READ ItemFile INVALID KEY
                           DISPLAY ESC Red-On "Error: Record not found."
                           ESC Reset-Color
                           NOT INVALID KEY
                               IF File-Status = "00"
                                   MOVE Item-Name TO Input-Item-Name
                               END-IF
                           END-READ
                           CLOSE ItemFile

                           MOVE FUNCTION TRIM(IDX) TO String-Format
                           DISPLAY String-Format               A5
                                   Input-Item-Name             A4
                                   Sale-Cart-Quantity-Display  A9
                                   Sale-Cart-Unit-Of-Price-Display"$"
                           END-READ
                           END-IF
                       END-PERFORM
                MOVE Total-Amount TO Total-Amount-Display
                MOVE Discount TO Discount-Display
                MOVE Final-Amount TO Final-Amount-Display
                MOVE Pay-Amount TO Pay-Amount-Display
                DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                        Bold-Off
                MOVE FUNCTION TRIM(Total-Amount-Display)
                TO String-Format
                DISPLAY "Total-Amount   : $" String-Format
                MOVE FUNCTION TRIM(Discount-Display)
                TO String-Format
                DISPLAY "Discount       : $" String-Format
                MOVE FUNCTION TRIM(Final-Amount-Display)
                TO String-Format
                DISPLAY "Final-Amount   : $" String-Format
                MOVE FUNCTION TRIM(Tax)
                TO String-Format
                DISPLAY "Tax            : $" String-Format
                MOVE FUNCTION TRIM(Pay-Amount-Display)
                TO String-Format
                DISPLAY "Pay-Amount     : $" String-Format
                DISPLAY Bold-On ESC Blue-On
                        H10 H10 H10 H10 H10 H10 H10 H10 H10
                        ESC Reset-Color Bold-Off
                DISPLAY " "
               END-IF
            END-PERFORM
            CLOSE SaleCartFile
            CLOSE InvoiceFile
            DISPLAY a a a a a a Bold-On "0. Back"Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Enter Choose Option:"
            ACCEPT User-Choice
            EVALUATE User-Choice
               WHEN "0"
                   MOVE "1" TO EOFP
            END-EVALUATE.
       Best-Sale-Item.
           DISPLAY " "
           DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
           MOVE "N" TO SS
               PERFORM UNTIL SS = "Y"
               DISPLAY "Enter Start Date (YYYYMMDD):"
               ACCEPT Input-Start-Date
               MOVE Input-Start-Date TO Start-Date-Num
               MOVE FUNCTION LENGTH(FUNCTION TRIM(Input-Start-Date))
               TO CC
               IF CC = 8
                   IF Start-Date-Num > 1
                       MOVE 'Y' TO SS
                   ELSE
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                       DISPLAY b ESC Red-On"Start Date must be numeric."
                               ESC Reset-Color
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                   END-IF
               ELSE
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY b ESC Red-On
                           "Start Date must have 8 digits"
                           ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
               END-IF
               END-PERFORM
               MOVE "N" TO SS
               PERFORM UNTIL SS = "Y"
               DISPLAY "Enter End Date (YYYYMMDD): "
               ACCEPT Input-End-Date
               MOVE Input-End-Date TO End-Date-Num
               MOVE FUNCTION LENGTH(FUNCTION TRIM(Input-End-Date)) TO DD
               IF DD = 8
                   IF End-Date-Num > 1
                       MOVE 'Y' TO SS
                   ELSE
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                       DISPLAY b ESC Red-On"End Date must be numeric."
                               ESC Reset-Color
                       DISPLAY Bold-On
                               H10 H10 H10 H10 H10 H10 H10 H10 H10
                               Bold-Off
                   END-IF
               ELSE
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
                   DISPLAY b ESC Red-On
                           "End Date must have 8 digits"
                           ESC Reset-Color
                   DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10
                           Bold-Off
               END-IF
               END-PERFORM
           IF Start-Date-Num > End-Date-Num
           DISPLAY ESC Red-On
           " Error: Start Date cannot be after End Date. "
           ESC Reset-Color
           MOVE "N" TO Valid-Range
           ELSE
               MOVE "Y" TO Valid-Range
           END-IF
           IF Valid-Range = "Y"
               PERFORM Best-Seller-Process
           END-IF.
       Best-Seller-Process.
            OPEN INPUT ItemFile
            OPEN I-O ItemFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
               OPEN OUTPUT ItemFile
               CLOSE ItemFile
               OPEN I-O ItemFile
            END-IF
            DISPLAY " "
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a A2 Bold-On ESC Blue-On "Best Sale Item Record"
                    ESC Reset-Color
                    "<<< From " Input-Start-Date
                    " To " Input-End-Date
                    Bold-Off
            DISPLAY Bold-On e10 e10 e10 e10 e10 e10 e10 e10 e10 Bold-Off
            DISPLAY "Item-ID"          A5
                    "Name"             A5 A5 A7
                    "Category"         A8
                    "Percent"          A5
                    "Quantity"         A6
                    "Price"
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            MOVE "N" TO EOF
            PERFORM UNTIL EOF = "Y"
               MOVE 0 TO All-Qty
               MOVE 0 TO Each-Qty
               MOVE 0 TO Best-Sell
               READ ItemFile
               AT END MOVE "Y" TO EOF
               NOT AT END
                   OPEN I-O SaleCartFile
                    IF File-Status = "35"
      *>                  DISPLAY "File does not exist. Creating file..."
                       OPEN OUTPUT SaleCartFile
                       CLOSE SaleCartFile
                       OPEN I-O SaleCartFile
                    END-IF
                   MOVE "N" TO EOFB
                   PERFORM UNTIL EOFB = "Y"
                      READ SaleCartFile
                      AT END MOVE "Y" TO EOFB
                      NOT AT END
                           IF Sale-Date >= Input-Start-Date
                           AND Sale-Date <= Input-End-Date
                              ADD Sale-Cart-Quantity TO All-Qty
                              IF Item-ID = Sale-Cart-Item-ID
                                   ADD Sale-Cart-Quantity TO Each-Qty
                              END-IF
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
                         MOVE Item-ID TO Item-ID-Display
                         MOVE Each-Qty TO Best-Sell-Qty-Display
                         MOVE Item-Price TO Item-Price-Display
                         MOVE FUNCTION TRIM(Item-ID-Display)
                         TO String-Format
                         DISPLAY String-Format         A2
                                 Item-Name             A1
                                 Item-Category         A5
                                 Best-Sell             A5
                                 Best-Sell-Qty-Display
                                 Item-Price-Display"$"
                     END-IF
                END-READ
            END-PERFORM
            CLOSE ItemFile
            DISPLAY " "
            PERFORM Seller-Process.
       Seller-Process.
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY a a a a a a Bold-On "0. Back"Bold-Off
            DISPLAY Bold-On H10 H10 H10 H10 H10 H10 H10 H10 H10 Bold-Off
            DISPLAY "Please Choose Option:"
            ACCEPT option
            EVALUATE option
                WHEN "0"
                   IF Admin-permission = 'Y'
                       PERFORM Item-Process
                   END-IF
                WHEN OTHER
                   DISPLAY ESC Red-On "Please choose only 0 ."
                           ESC Reset-Color
                   DISPLAY ESC Red-On "Other character is not allowed"
                           ESC Reset-Color
                   PERFORM Seller-Process
            END-EVALUATE.

       Low-Stock-Alert.
            OPEN I-O ItemFile
            IF File-Status = "35"
      *>          DISPLAY "File does not exist. Creating file..."
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
       END PROGRAM Item.

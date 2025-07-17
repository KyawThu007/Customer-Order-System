       IDENTIFICATION DIVISION.
       PROGRAM-ID. BESTSELLITEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ItemFile      ASSIGN TO 'item.dat'
               ORGANIZATION IS SEQUENTIAL.
           SELECT SaleCartFile  ASSIGN TO 'salecart.dat'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ItemFile.
       01 Item-Record.
           05 Item-ID        PIC X(5).
           05 Item-Name      PIC X(20).
           05 Item-Price     PIC 9(5)V99.

       FD SaleCartFile.
       01 SaleCart-Record.
           05 Sale-Cart-Item-ID  PIC X(5).
           05 Sale-Cart-Quantity PIC 9(5).

       WORKING-STORAGE SECTION.
       01 EOF             PIC X VALUE "N".
       01 EOFB            PIC X VALUE "N".
       01 Total-Qty       PIC 9(6) VALUE ZERO.
       01 Item-Total      PIC 9(6) VALUE ZERO.
       01 Best-Sell       PIC 9(5)V99 VALUE ZERO.

       01 HEAD-SEPARATOR     PIC X(50) VALUE ALL "-".
       01 SPACES10           PIC X(10) VALUE SPACES.

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT ItemFile
           OPEN INPUT SaleCartFile

           DISPLAY SPACES10 SPACES10 SPACES10 SPACES10 SPACES10
           DISPLAY "Best Sell Record"
           DISPLAY HEAD-SEPARATOR
           DISPLAY "Item-ID   Item-Name           Percent   Price"
           DISPLAY HEAD-SEPARATOR


           MOVE 0 TO Total-Qty
           MOVE "N" TO EOFB
           PERFORM UNTIL EOFB = "Y"
               READ SaleCartFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOFB
                   NOT AT END
                       ADD Sale-Cart-Quantity TO Total-Qty
               END-READ
           END-PERFORM


           CLOSE SaleCartFile
           OPEN INPUT SaleCartFile


           MOVE "N" TO EOF
           PERFORM UNTIL EOF = "Y"
               READ ItemFile NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       MOVE 0 TO Item-Total
                       MOVE "N" TO EOFB


                       PERFORM UNTIL EOFB = "Y"
                           READ SaleCartFile NEXT RECORD
                               AT END
                                   MOVE "Y" TO EOFB
                               NOT AT END
                                   IF Item-ID = Sale-Cart-Item-ID
                                       ADD Sale-Cart-Quantity
                                       TO Item-Total
                                   END-IF
                           END-READ
                       END-PERFORM


                       CLOSE SaleCartFile
                       OPEN INPUT SaleCartFile


                       IF Total-Qty NOT = 0
                           COMPUTE Best-Sell =
                           (Item-Total * 100.00) / Total-Qty
                       ELSE
                           MOVE 0 TO Best-Sell
                       END-IF


                       DISPLAY Item-ID SPACE
                               Item-Name SPACE
                               Best-Sell SPACE
                               Item-Price
               END-READ
           END-PERFORM

           DISPLAY HEAD-SEPARATOR
           CLOSE SaleCartFile
           CLOSE ItemFile
           STOP RUN.

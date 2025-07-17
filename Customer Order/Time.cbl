  ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ESC             PIC X       VALUE X'1B'.        *> Escape character (ASCII 27)
       01 Red-On          PIC X(5)    VALUE "[34m".       *> Red text
       01 Reset-Color     PIC X(4)    VALUE "[0m".        *> Reset color

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY ESC Red-On "Low-Stock-Message" ESC Reset-Color
            DISPLAY "HEllo"
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.

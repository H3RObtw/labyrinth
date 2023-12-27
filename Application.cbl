      ******************************************************************
      * Author: Andreas Bauerfeind
      * Purpose: Einlesen aller Testdatein und diese an LABYRINT 
      *          weitergeben
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. LABYRINT-APPLICATION.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "resources/Filenames.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.  
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD INPUT-FILE.
            01 LINE-OF-FILE PIC X(50).

       LOCAL-STORAGE SECTION.
            01 WS-EOF       PIC X(1)    VALUE "X". 
            01 FILENAME     PIC X(50).  
    
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL WS-EOF = "Y"
                READ INPUT-FILE INTO FILENAME
                    AT END MOVE "Y" TO WS-EOF
                END-READ
                DISPLAY "DATEI: " FILENAME
                CALL "LABYRINT" USING FILENAME
           END-PERFORM.        
           CLOSE INPUT-FILE.
       STOP RUN.

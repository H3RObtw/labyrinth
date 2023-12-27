      ******************************************************************
      * Author: Andreas Bauerfeind, Jona Gölthenboth
      * Purpose:Einlesen der Eingabedatei, Aufrufen der Unterprogramme,
      *         Schreiben der Ausgabedatei
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. LABYRINT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
       FILE-CONTROL.
           SELECT LABEIN   ASSIGN TO LABY
               ORGANIZATION IS LINE SEQUENTIAL.  
           SELECT LABOUT   ASSIGN TO AUSGEBEN
               ORGANIZATION IS LINE SEQUENTIAL.  
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD LABEIN
            BLOCK CONTAINS 0
            RECORDING F
            RECORD    050
            LABEL RECORD IS STANDARD.
       01 LABY PIC X(500).
      
       FD LABOUT
            BLOCK CONTAINS 0
            RECORDING F
            RECORD    050
            LABEL RECORD IS STANDARD.
       01 AUSGEBEN PIC X(500).
      
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WS-EOF         PIC X(1)        VALUE "X".
       01  WS-COUNT       PIC 99.
       01  LABYZEILE      PIC X(50)       VALUE ' '.
       01  ZAEHLER        PIC 99          VALUE 1.
       01  TEMP           PIC 99.
       01  LABYRINTH.
           05  LABZEILE OCCURS 50 PIC X(50).
           05  LETZTEZEILE        PIC 99.
           05  ENDEZEILE          PIC 99.
       01  POSBESUCH.
           05  BESUCHER OCCURS 5.
               10 ZEILE           PIC 99.
               10 STELLE          PIC 99.
           05  WEGE     OCCURS 5.
               10 PFAD            PIC X(2500).
               10 PFAD-LAENGE     PIC 9999.
       01  FEHLERMELDUNG  PIC X(50)       VALUE '#'.

      / 
      ******************************************************************
       LINKAGE SECTION.
      ****************************************************************** 
       01 FILENAME PIC X(50).
      /
      ******************************************************************
       PROCEDURE DIVISION USING FILENAME.
      ******************************************************************
      /
       STEUERUNG SECTION.
           PERFORM INITIALISIEREN.
           OPEN INPUT  LABEIN.
           OPEN OUTPUT LABOUT.
           PERFORM UNTIL WS-EOF = "Y"
               IF ZAEHLER > 50
                  MOVE 'ES GIBT ZU VIELE ZEILEN IN DER EINGABEDATEI'
                  TO FEHLERMELDUNG
                  MOVE "Y" TO WS-EOF
               ELSE      
                  READ LABEIN INTO LABY
                     AT END MOVE "Y" TO WS-EOF
                  END-READ
                  INSPECT LABY(51:99) TALLYING WS-COUNT FOR ALL "X"
                  IF WS-COUNT = 0
                     STRING LABY(1:50) DELIMITED BY SIZE INTO LABYZEILE
                     MOVE LABYZEILE TO LABZEILE(ZAEHLER)
                     DISPLAY LABZEILE(ZAEHLER) " : " ZAEHLER
                     ADD 1 TO ZAEHLER
                  ELSE          
                     MOVE 'ES GIBT ZU VIELE SPALTEN IN DER EINGABEDATEI'
                     TO FEHLERMELDUNG
                     MOVE "Y" TO WS-EOF
                  END-IF
               END-IF
           END-PERFORM.
           
           IF FEHLERMELDUNG = '#'
               CALL 'LABPRUEF' USING LABYRINTH, POSBESUCH, FEHLERMELDUNG.
               DISPLAY "FEHLERMELDUNG:" FEHLERMELDUNG
               DISPLAY "LABYRINTH:" 
               DISPLAY LABYRINTH
               DISPLAY "POSBESUCH:" POSBESUCH.

           IF FEHLERMELDUNG = '#'
              MOVE 1 TO ZAEHLER
              CALL 'LABWEG' USING LABYRINTH, POSBESUCH
              MOVE "BESUCHER:" TO AUSGEBEN
              PERFORM UNTIL ZAEHLER > 5
                  MOVE ZEILE(ZAEHLER)           TO TEMP
                  IF NOT TEMP = 0
                     MOVE LABZEILE(TEMP)           TO LABYZEILE
                     MOVE STELLE(ZAEHLER)          TO TEMP
                     STRING "BESUCHER: ZEILE: " ZEILE(ZAEHLER)
                     ", STELLE: " STELLE(ZAEHLER)
                     ", ZEICHEN: " LABYZEILE(TEMP:1)
                     DELIMITED BY SIZE INTO AUSGEBEN
                     display AUSGEBEN
                     WRITE AUSGEBEN
                     MOVE SPACES TO AUSGEBEN
                     STRING "WEG: " PFAD(ZAEHLER)
                     DELIMITED BY SIZE INTO AUSGEBEN
                     WRITE AUSGEBEN  
                     MOVE SPACES TO AUSGEBEN
                  END-IF
                  ADD 1 TO ZAEHLER
              END-PERFORM
           ELSE
              MOVE FEHLERMELDUNG TO AUSGEBEN
              WRITE AUSGEBEN
           END-IF.
      
           CLOSE LABEIN.
           CLOSE LABOUT.
           EXIT PROGRAM.
       STEUERUNG-EXIT. EXIT.

       INITIALISIEREN SECTION.
           INITIALIZE TEMP.
           INITIALIZE LABYRINTH.
           INITIALIZE LABYZEILE.
           INITIALIZE ZAEHLER.
           INITIALIZE POSBESUCH.
           INITIALIZE WS-EOF.
           INITIALIZE WS-COUNT.
           INITIALIZE FEHLERMELDUNG.
           MOVE '#' TO FEHLERMELDUNG.
           MOVE 1 TO ZAEHLER.
           STRING "resources/in/" FILENAME INTO LABY END-STRING.
           STRING "resources/out/" FILENAME INTO AUSGEBEN END-STRING.
       INITIALISIEREN-EXIT. EXIT.
       END PROGRAM LABYRINT.

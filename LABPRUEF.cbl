      ******************************************************************
      * Author: Jona Gölthenboth
      * Purpose:Eingabevalidierung und Analyse
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. LABPRUEF.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      /
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
      
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      
      /
      *****************************************************************
       WORKING-STORAGE SECTION.
      *****************************************************************
      * ZAEHLER
       01 ZAEHLERZEILE               PIC 9(2)  VALUE 1.
       01 ZAEHLERLABYRINTH           PIC 9(2)  VALUE 1.
       01 LETZTESZEICHEN             PIC 9(2)  VALUE 1.
       01 ZAEHLERISTBUCHSTABE        PIC 9(2)  VALUE 1.
      * MAX 100 BESONDERE PUNKTE
       01 ANZBESPUNKTE               PIC 9(3)  VALUE 0.
      * MAX 5 BESUCHER
       01 ANZBESUCHER                PIC 9     VALUE 1.
       01 ANZRICHTUNGEN              PIC 9     VALUE 0.
      * BOOLSCHE VARIABLEN
       01 KEINDOPPELFELD             PIC 9     VALUE 0.
       01 BUCHSTABE                  PIC 9     VALUE 0.
       01 ENDEGEFUNDEN               PIC 9     VALUE 0.
      * SONSTIGE VARIABLEN
       01 AKTUELLEZEILE              PIC X(50).
       01 ZEICHEN                    PIC X.
       01 DIFFERENZ                  PIC 9(2)  VALUE 0.
      /
      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
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
       01  FEHLERMELDUNG          PIC X(50)       VALUE '#'.
      /
      ******************************************************************
       PROCEDURE DIVISION USING LABYRINTH, POSBESUCH, FEHLERMELDUNG.
      ******************************************************************
       STEUERUNG SECTION.
           PERFORM INITIALISIEREN.
      * DIMENSION FESTLEGEN UND ÜBERPRÜFEN
           PERFORM UNTIL ZAEHLERLABYRINTH > 50
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE
              MOVE 50 TO ZAEHLERZEILE
              INITIALIZE ENDEGEFUNDEN
              IF AKTUELLEZEILE = SPACE THEN
                 MOVE 0 TO LETZTESZEICHEN
                 MOVE 1 TO ENDEGEFUNDEN
              ELSE
                 PERFORM UNTIL ZAEHLERZEILE < 1 OR ENDEGEFUNDEN = 1
                    IF AKTUELLEZEILE(ZAEHLERZEILE:1) NOT = ' ' THEN
                       MOVE ZAEHLERZEILE TO LETZTESZEICHEN
                       MOVE 1 TO ENDEGEFUNDEN
                    END-IF
                    SUBTRACT 1 FROM ZAEHLERZEILE
                  END-PERFORM
              END-IF
              IF ENDEZEILE < LETZTESZEICHEN AND LETZTESZEICHEN NOT = 0
                 MOVE LETZTESZEICHEN TO ENDEZEILE
              END-IF
              IF LETZTESZEICHEN > 0
                 MOVE ZAEHLERLABYRINTH TO LETZTEZEILE
              END-IF
      
              ADD 1 TO ZAEHLERLABYRINTH
           END-PERFORM.
           SUBTRACT 1 FROM LETZTEZEILE.
      
           MOVE 1 TO ZAEHLERZEILE.
           MOVE 1 TO ZAEHLERLABYRINTH.
           PERFORM UNTIL ZAEHLERLABYRINTH > LETZTEZEILE
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE
      * IST ES EIN 2x2 FELD?
              MOVE 1 TO ZAEHLERZEILE
              PERFORM UNTIL ZAEHLERZEILE >=  ENDEZEILE
                 IF ZAEHLERLABYRINTH NOT = LETZTEZEILE AND
                  ZAEHLERZEILE NOT = ENDEZEILE AND
                  AKTUELLEZEILE(ZAEHLERZEILE:1)= SPACE
                   PERFORM ZWEIERFELD
                 END-IF
                 ADD 1 TO ZAEHLERZEILE
              END-PERFORM
              ADD 1 TO ZAEHLERLABYRINTH
           END-PERFORM.
      
      * AUF BESONDERE FELDER ÜBERPRÜFEN
           MOVE 1 TO ZAEHLERZEILE.
           MOVE 1 TO ZAEHLERLABYRINTH.
           PERFORM UNTIL ZAEHLERLABYRINTH > LETZTEZEILE
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE
              MOVE 1 TO ZAEHLERZEILE
              PERFORM UNTIL ZAEHLERZEILE > ENDEZEILE
                 PERFORM PRUEFPUNKT
                 ADD 1 TO ZAEHLERZEILE
              END-PERFORM
              ADD 1 TO ZAEHLERLABYRINTH
           END-PERFORM.

           IF ANZBESUCHER = 1
              MOVE "KEINE BESUCHER VORHANDEN" TO FEHLERMELDUNG
           END-IF.
           
           EXIT PROGRAM.
       STEUERUNG-EXIT. EXIT.
      
       PRUEFPUNKT SECTION.
           MOVE AKTUELLEZEILE(ZAEHLERZEILE:1) TO ZEICHEN.
      * IST ES EIN ZUGELASSENES ZEICHEN?
           IF ZEICHEN IS NOT ALPHABETIC
              MOVE "EIN NICHTZUGELASSENES ZEICHEN ENTHALTEN"
               TO FEHLERMELDUNG
              EXIT PROGRAM
           END-IF.
      * IST ES EIN RANDZEICHEN > AUSGANG?
           IF ZAEHLERZEILE   = ENDEZEILE   OR
            ZAEHLERLABYRINTH = LETZTEZEILE OR
            ZAEHLERZEILE     = 1           OR
            ZAEHLERLABYRINTH = 1
              IF ZEICHEN = ' '
                 ADD 1 TO ANZBESPUNKTE
              END-IF
           END-IF.
      * IST ES EIN BESUCHER?
           IF ZEICHEN IS ALPHABETIC AND ZEICHEN NOT = 'X'
            AND ZEICHEN NOT = SPACE
              IF ANZBESUCHER < 6
                 MOVE ZAEHLERLABYRINTH TO ZEILE(ANZBESUCHER)
                 MOVE ZAEHLERZEILE     TO STELLE(ANZBESUCHER)
                 ADD 1 TO ANZBESUCHER
              ELSE
                 MOVE "ZU VIELE BESUCHER" TO FEHLERMELDUNG
                 EXIT PROGRAM
              END-IF
           END-IF.
      * KREUZUNG ODER SACKGASSE?
           IF ZAEHLERLABYRINTH NOT = ENDEZEILE OR
            ZAEHLERZEILE NOT = ENDEZEILE OR
            ZAEHLERLABYRINTH = 1 OR
            ZAEHLERZEILE = 1
              IF ZEICHEN = ' '
                 PERFORM MENGERICHTUNGEN
                 IF ANZRICHTUNGEN = 1 OR ANZRICHTUNGEN > 3
                    ADD 1 TO ANZBESPUNKTE
                 END-IF
              END-IF
           END-IF.
           IF ANZBESPUNKTE > 100
              MOVE "ZU VIELE AUSGÄNGE, SACKGASSEN UND KREUZUNGSPUNKTE"
                 TO FEHLERMELDUNG
              EXIT PROGRAM
           END-IF.
       PRUEFPUNKT-EXIT. EXIT.
      
       MENGERICHTUNGEN SECTION.
              MOVE 0 TO ANZRICHTUNGEN.
              SUBTRACT 1 FROM ZAEHLERLABYRINTH.
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE.
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = ' '
                 ADD 1 TO ANZRICHTUNGEN
              END-IF.
              ADD 2 TO ZAEHLERLABYRINTH.
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE.
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = ' '
                 ADD 1 TO ANZRICHTUNGEN
              END-IF.
              SUBTRACT 1 FROM ZAEHLERLABYRINTH.
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE.
              SUBTRACT 1 FROM ZAEHLERZEILE.
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = ' '
                 ADD 1 TO ANZRICHTUNGEN
              END-IF.
              ADD 2 TO ZAEHLERZEILE.
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = ' '
                 ADD 1 TO ANZRICHTUNGEN
              END-IF.
              SUBTRACT 1 FROM ZAEHLERZEILE.
       MENGERICHTUNGEN-EXIT. EXIT.
      
       ZWEIERFELD SECTION.
              INITIALIZE KEINDOPPELFELD.
              ADD 1 TO ZAEHLERZEILE.
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = 'X'
                 MOVE 1 TO KEINDOPPELFELD
              END-IF.
              ADD 1 TO ZAEHLERLABYRINTH.
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = 'X'
                 MOVE 1 TO KEINDOPPELFELD
              END-IF.
              SUBTRACT 1 FROM ZAEHLERZEILE.
              IF AKTUELLEZEILE(ZAEHLERZEILE:1) = 'X'
                 MOVE 1 TO KEINDOPPELFELD
              END-IF.
              SUBTRACT 1 FROM ZAEHLERLABYRINTH.
              MOVE LABZEILE(ZAEHLERLABYRINTH) TO AKTUELLEZEILE.
              IF KEINDOPPELFELD = 0
                  STRING '2x2 FELD BEI ZEILE: ' ZAEHLERLABYRINTH
                         ', ZEICHEN: ' ZAEHLERZEILE
                  DELIMITED BY SIZE INTO FEHLERMELDUNG
                  DISPLAY '2x2 FELD BEI ZEILE: ' ZAEHLERLABYRINTH
                         ', ZEICHEN: ' ZAEHLERZEILE
                  DISPLAY 'LETZTEZEILE:' LETZTEZEILE
                  EXIT PROGRAM
              END-IF.
       ZWEIERFELD-EXIT. EXIT.
      
       INITIALISIEREN SECTION.
              INITIALIZE ENDEGEFUNDEN.
              INITIALIZE BUCHSTABE.
              INITIALIZE ANZBESPUNKTE.
              INITIALIZE KEINDOPPELFELD.
              INITIALIZE AKTUELLEZEILE.
              INITIALIZE LETZTESZEICHEN.
              INITIALIZE LETZTEZEILE.
              INITIALIZE ZAEHLERZEILE.
              INITIALIZE ZAEHLERLABYRINTH.
              INITIALIZE ENDEZEILE.
              INITIALIZE ZEICHEN.
              INITIALIZE ZAEHLERISTBUCHSTABE.
              INITIALIZE ANZBESUCHER.
              INITIALIZE ANZRICHTUNGEN.
              INITIALIZE DIFFERENZ.
              MOVE 0                            TO ANZBESPUNKTE.
              MOVE 1                            TO LETZTESZEICHEN.
              MOVE 1                            TO LETZTEZEILE.
              MOVE 1                            TO ZAEHLERZEILE.
              MOVE 1                            TO ZAEHLERLABYRINTH.
              MOVE 1                            TO ENDEZEILE.
              MOVE 1                            TO ANZBESUCHER.
       INITIALISIEREN-EXIT. EXIT.
       END PROGRAM LABPRUEF.

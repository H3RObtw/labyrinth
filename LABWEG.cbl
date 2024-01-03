      ******************************************************************
      * Author: Andreas Bauerfeind
      * Purpose:Sucht den kürzesten Weg zum Ausgang für jeden Besucher
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. LABWEG.
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
       01 ZAEHLER.
          05 ZAEHLERZEILE       PIC 99.
          05 ZAEHLERSTELLE      PIC 99.
          05 ZAEHLERBESUCHER    PIC 9.
          05 ZAEHLERZUSTAND     PIC 9.       
       01 WEITEREVARIABLEN.
          05 ZUSTAND            PIC 9.
          05 AKTZEILE           PIC X(50).
          05 AKTZEICHEN         PIC X.
          05 BESUCHERZEICHEN    PIC X.
          05 ALLEGEFUNDEN       PIC 9.
          05 AKTPFAD            PIC X(2500).
          05 AKTPFAD-LAENGE     PIC 9999.
      *****************************************************************
      /
      ******************************************************************
       LINKAGE SECTION.
       01  LABYRINTH.
           05  LABZEILE OCCURS 50 PIC X(50).
           05  LETZTEZEILE        PIC 99.
           05  ENDEZEILE          PIC 99.
       01  POSBESUCH.
           05  BESUCHER OCCURS 5.
               10 ZEILE           PIC 99.
               10 STELLE          PIC 99.
               10 BESUCHERZEICHEN PIC X(1).
           05  WEGE     OCCURS 5.
               10 PFAD            PIC X(2500).
               10 PFAD-LAENGE     PIC 9999.
      ******************************************************************
      /
      ******************************************************************
       PROCEDURE DIVISION USING LABYRINTH, POSBESUCH.
      ******************************************************************
       STEUERUNG SECTION.
           PERFORM INITIALISIEREN.       
      *    ALLE BESUCHER DURCHGEHEN
           PERFORM UNTIL ZAEHLERBESUCHER > 5
              INITIALIZE WEGE(ZAEHLERBESUCHER)       
      *       MUSS EIN WEG GESUCHT WERDEN?
              EVALUATE TRUE
      *        KEINE GÜLTIGEN DATEN EINGETRAGEN -> KEIN BESUCHER
               WHEN STELLE(ZAEHLERBESUCHER) = 0
               AND  ZEILE(ZAEHLERBESUCHER) = 0
                 MOVE 'KEIN BESUCHER'    TO PFAD(ZAEHLERBESUCHER)
                 MOVE 0               TO PFAD-LAENGE(ZAEHLERBESUCHER)
      *        IST DER BESUCHER AM RAND? -> KEINEN PFAD SUCHEN
               WHEN STELLE(ZAEHLERBESUCHER) = 1
               OR   STELLE(ZAEHLERBESUCHER) = ENDEZEILE
               OR   ZEILE(ZAEHLERBESUCHER) = LETZTEZEILE
               OR   ZEILE(ZAEHLERBESUCHER) = 1
                 MOVE 'AM AUSGANG'       TO PFAD(ZAEHLERBESUCHER)
                 MOVE 0                  TO PFAD-LAENGE(ZAEHLERBESUCHER)
      *        GüLTIGER BESUCHER -> VARIABLEN INITIALISEREN, WEG SUCHEN
               WHEN OTHER
                 INITIALIZE WEITEREVARIABLEN
                 MOVE 0                       TO ZAEHLERZUSTAND
                 MOVE ZEILE(ZAEHLERBESUCHER)  TO ZAEHLERZEILE
                 MOVE STELLE(ZAEHLERBESUCHER) TO ZAEHLERSTELLE
                 MOVE 0    TO AKTPFAD-LAENGE 
                 MOVE 9999 TO PFAD-LAENGE(ZAEHLERBESUCHER)
                 PERFORM WEG-SUCHEN UNTIL ALLEGEFUNDEN = 1
                 IF PFAD-LAENGE(ZAEHLERBESUCHER) = 9999
                    MOVE 'BESUCHER HAT KEINEN AUSGANG' 
                        TO PFAD(ZAEHLERBESUCHER) 
                 END-IF
              END-EVALUATE 
              ADD 1 TO ZAEHLERBESUCHER
           END-PERFORM.
           EXIT PROGRAM.
       STEUERUNG-EXIT. EXIT.      

       WEG-SUCHEN SECTION.
      *    IN WELCHE RICHTUNG?
           IF (AKTPFAD(AKTPFAD-LAENGE:1) = 'U' AND ZUSTAND = 0)
           OR (AKTPFAD(AKTPFAD-LAENGE:1) = 'L' AND ZUSTAND = 1)
           OR (AKTPFAD(AKTPFAD-LAENGE:1) = 'H' AND ZUSTAND = 2)
           OR (AKTPFAD(AKTPFAD-LAENGE:1) = 'R' AND ZUSTAND = 3)
              ADD 1 TO ZUSTAND
           END-IF       
           EVALUATE TRUE
           WHEN ZUSTAND = 0
      *    BREADCRUMB SETZTEN     
              MOVE '.' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
              SUBTRACT 1 FROM ZAEHLERZEILE
              ADD      1 TO AKTPFAD-LAENGE
              MOVE 'H'   TO AKTPFAD(AKTPFAD-LAENGE:1)
           WHEN ZUSTAND = 1
              MOVE '.' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
              ADD      1 TO ZAEHLERSTELLE
              ADD      1 TO AKTPFAD-LAENGE
              MOVE 'R'   TO AKTPFAD(AKTPFAD-LAENGE:1)
           WHEN ZUSTAND = 2
              MOVE '.' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
              ADD      1 TO ZAEHLERZEILE
              ADD      1 TO AKTPFAD-LAENGE
              MOVE 'U'   TO AKTPFAD(AKTPFAD-LAENGE:1)
           WHEN ZUSTAND = 3
              MOVE '.' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
              SUBTRACT 1 FROM ZAEHLERSTELLE
              ADD      1 TO AKTPFAD-LAENGE
              MOVE 'L'   TO AKTPFAD(AKTPFAD-LAENGE:1)
           END-EVALUATE       
      *    AKTUELLES ZEICHEN UND ZEILE SPEICHERN, ZUSTAND AKTUALISIEREN
           MOVE LABZEILE(ZAEHLERZEILE)    TO AKTZEILE
           MOVE AKTZEILE(ZAEHLERSTELLE:1) TO AKTZEICHEN
           MOVE 0                         TO ZUSTAND       
      *    ÜBERPRÜFEN OB ZURÜCK GEGANGEN WERDEN MUSS
           IF   ZAEHLERSTELLE = 1
           OR   ZAEHLERZEILE  = 1
           OR   ZAEHLERSTELLE = ENDEZEILE
           OR   ZAEHLERZEILE  = LETZTEZEILE
           OR   AKTPFAD-LAENGE > PFAD-LAENGE(ZAEHLERBESUCHER)
              EVALUATE TRUE

               WHEN  AKTZEICHEN = 'X'  OR  AKTZEICHEN = '.'
                  MOVE    4 TO ZUSTAND
                  PERFORM ZUSTAND-SETZEN UNTIL ZUSTAND NOT = 4
                  
              WHEN  AKTPFAD-LAENGE > PFAD-LAENGE(ZAEHLERBESUCHER)
                  MOVE    4 TO ZUSTAND
                  PERFORM ZUSTAND-SETZEN UNTIL ZUSTAND NOT = 4

               WHEN  AKTPFAD-LAENGE > 2499
                  MOVE    4 TO ZUSTAND
                  PERFORM ZUSTAND-SETZEN UNTIL ZUSTAND NOT = 4
              
              WHEN OTHER
      *          WURDE EIN PFAD GEFUNDEN? WENN JA IST ER KÜRZER?
                 IF  AKTPFAD-LAENGE < PFAD-LAENGE(ZAEHLERBESUCHER)
                    MOVE AKTPFAD-LAENGE TO PFAD-LAENGE(ZAEHLERBESUCHER)
                    MOVE AKTPFAD        TO PFAD(ZAEHLERBESUCHER)
                 END-IF
                 MOVE    4 TO ZUSTAND
                 PERFORM ZUSTAND-SETZEN UNTIL ZUSTAND NOT = 4
              END-EVALUATE
           ELSE
              IF  AKTZEICHEN = 'X'  OR  AKTZEICHEN = '.'
                  MOVE    4 TO ZUSTAND
                  PERFORM ZUSTAND-SETZEN UNTIL ZUSTAND NOT = 4
              END-IF
           END-IF.
       WEG-SUCHEN-EXIT. EXIT.  

       ZUSTAND-SETZEN SECTION.
      *    WAS WAR DER VORHERIGE SCHRITT?
           EVALUATE TRUE
           WHEN AKTPFAD(AKTPFAD-LAENGE:1) = 'H'
              MOVE SPACE TO AKTPFAD(AKTPFAD-LAENGE:1)
              SUBTRACT 1 FROM AKTPFAD-LAENGE
              ADD      1 TO ZAEHLERZEILE
              MOVE ' ' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
      *       WURDE MAN DEN VORHERIGEN WEG UNABSICHTLICH ZURÜCK GEHEN?
              IF AKTPFAD(AKTPFAD-LAENGE:1) = 'L'
                 MOVE 2 TO ZUSTAND
              ELSE
                 MOVE 1 TO ZUSTAND
              END-IF
           WHEN AKTPFAD(AKTPFAD-LAENGE:1) = 'R'
              MOVE SPACE TO AKTPFAD(AKTPFAD-LAENGE:1)
              SUBTRACT 1 FROM AKTPFAD-LAENGE
              SUBTRACT 1 FROM ZAEHLERSTELLE
              MOVE ' ' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
              IF AKTPFAD(AKTPFAD-LAENGE:1) = 'H'
                 MOVE 3 TO ZUSTAND
              ELSE
                 MOVE 2 TO ZUSTAND
              END-IF
           WHEN AKTPFAD(AKTPFAD-LAENGE:1) = 'U'
              MOVE SPACE TO AKTPFAD(AKTPFAD-LAENGE:1)
              SUBTRACT 1 FROM AKTPFAD-LAENGE
              SUBTRACT 1 FROM ZAEHLERZEILE
              MOVE ' ' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
              IF AKTPFAD(AKTPFAD-LAENGE:1) = 'R'
                 MOVE 4 TO ZUSTAND
              ELSE
                 MOVE 3 TO ZUSTAND
              END-IF
           WHEN AKTPFAD(AKTPFAD-LAENGE:1) = 'L'
              MOVE SPACE TO AKTPFAD(AKTPFAD-LAENGE:1)
              SUBTRACT 1 FROM AKTPFAD-LAENGE
              ADD      1 TO   ZAEHLERSTELLE
              MOVE ' ' TO LABZEILE(ZAEHLERZEILE)(ZAEHLERSTELLE:1)
           END-EVALUATE.       
      *    IST MAN WIEDER BEIM BESUCHER ANGEKOMMEN?
           IF AKTPFAD-LAENGE < 1
              ADD  1              TO ZAEHLERZUSTAND
              MOVE ZAEHLERZUSTAND TO ZUSTAND
      *    WURDE JEDER MÖGLICHE PFAD GEFUNDEN?
              IF ZAEHLERZUSTAND > 3
                 MOVE    1  TO ALLEGEFUNDEN              
              END-IF
           END-IF.
       ZUSTAND-SETZEN-EXIT. EXIT. 
             
       INITIALISIEREN SECTION.
           INITIALIZE ZAEHLER.
           INITIALIZE WEITEREVARIABLEN.
           MOVE 1 TO ZAEHLERZEILE.
           MOVE 1 TO ZAEHLERSTELLE.
           MOVE 1 TO ZAEHLERBESUCHER.
           MOVE 0 TO ZAEHLERZUSTAND.
       INITIALISIEREN-EXIT. EXIT.
       END PROGRAM LABWEG.
       
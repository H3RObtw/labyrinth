# Labyrinth

## TODOs

testfälle schreiben

fehler fixen
- was wenn es kein ausgang gibt, kein ausgang gefunden wird
- was wenn es mehr als 50 zeichen/ zeilen sind (einlesen schlägt fehl)


## Benutzte Programme & Extensions
- Visual Studio Code, Version 1.63.2
- Gnucobol, Version 3.2.0
- Bitlang "Cobol" Extension, Version 8.1.31
- COBOLworx "GDB Debugging support for GnuCOBOL" Extension, Version 4.2.3


## Aufgabenstellung

### Problemstellung

Die Abteilung Irrgarten der Redaktion einer Rätselzeitschrift wünscht ein Programm,
welches zu einem vorgegebenen Labyrinth für verschiedene im Irrgarten befindliche
Besucher einen möglichst kurzenWeg ins Freie ermittelt. Die Maximalzahl der Besucher, die sich im Labyrinth aufhalten, sei fünf. Das Labyrinthmit den Positionen der Besucher soll aus einer Datei eingelesen werden.

Für jeden vorhandenen Besucher soll ein Plan ausgegeben werden, der ihm dern Weg aus dem Labyrinth anzeigt. Dazu soll ermittelt werden, wie viele Schritte er bis zum Verlassen des Labyrinths benötigt. Dabei entspricht ein Schritt dem Aufsuchen einer Nachbarpositition. Das Labyrinth ist verlassen, wenn der Ausgang erreicht ist.

### Programmsystem

Neben selbstgewählten Testbeispielen soll das unten angegebenene Labyrinth gerechnet
werden.
Kriterien für das Labyrinth:
- Begehbare Positionen sind durch Blank gekennzeichnet, nicht begehbare durch X.
Die Positionen der Besucher sind mit den Buchstaben A bis E gekennzeichnet und
sind alle verschieden.
- Wege verlaufen nur waagerechnet und senkrecht und haben stets die Breite 1.
- Kreuzungspunkte sind Positionen, von denen aus man in wenigstens drei
verschiedenen Richtungen weitergehen kann. Als Sackgasse werden Positionen
bezeichnet, von denen aus man in höchstens eine Richtung gehen kann. Die
Gesamtzahl der Kreuzungspunkte, Sackgassenpunkte und Ausgänge sei durch
100 beschränkt.
- Das Labyrinth ist rechteckig mit höchstens 50 x 50 Positionen.
- Der Rand des Labyrinths muss mit eingegeben werden.
Eine Lücke im Rand (mit Breite 1) wird als Ausgang bezeichnet
- Flächen, die begehbare Felder der Größe 2x2 enthalten, sind nicht erlaubt

Modularisieren Sie Ihr Programmsystem nach dem EVA-Prinzip sowie nach sinnvollen
Funktionseinheiten.

### Entwicklerdokumentation

Die ganzheitliche Entwicklerdokumentation sollte ein Inhaltsverzeichnis aufweisen und zu
jedem Kapitel eine Autorenangabe erhalten.
Dokumentieren Sie jeweils zu Ihrem Teil (s. u.), Beschreibung der Algorithmen anhand
eines selbstgewählten Beispiels sowie als Struktogramm, Programmeinheiten und
mathematische Hintergründe.
Begründen Sie die Auswahl Ihrer Testfälle und diskutieren Sie sie.

Teil A
- Eingaben (inklusive formaler Fehlerprüfung), Ausgabe der Ergebnisse jeweils über
Dateien
- Prüfung, ob eingelesenes Labyrinth den geforderten Eigenschaften entspricht
(inklusive korrekter Besucherzahl)
- Wahl und Begründung geeigneter Datenstrukturen

Teil B
- Ermittlung des Wegs jedes Besuchers
- Berechnung, wie viele Schritte jeder Besucher bis zum Ausgang benötigt

### Abzugeben sind:

Aufgabenstellung, Programmcodes als Listing und ausführbares Programmsystem, E-/A-
Dateien der Tests, die oben beschriebene ganzheitliche Entwicklerdokumentation
entweder in schriftlicher Form (Word-, oder pdf-Datei) oder als Präsentation (ppt-Datei,dann auch Darbietung der Präsentation) Unterschriebene Eigenständigkeitserkärung mit Nennung der eigenen erstellten
Programmteile.

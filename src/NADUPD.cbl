      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NADUPD.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - SERVIS VIDEO NADZORA.
       DATE-WRITTEN.  2000-02-21.
      *================================================================*
      * SISTEM  : NADZOR - UGOVORI O ODRŽAVANJU I NAPLATA
      * PROGRAM : NADUPD - KORAK 010 MJESEČNE OBRADE
      * OPIS    : AŽURIRANJE MATIČNE DATOTEKE UGOVORA TRANSAKCIJAMA
      *           MJESECA, KLASIČNIM ALGORITMOM "BALANCED LINE":
      *           STARI MATIČNI + TRANSAKCIJE -> NOVI MATIČNI.
      *           TRANSAKCIJE SE PRVO SORTIRAJU (UGOVOR, DATUM, RED.BR.)
      *           SVAKA TRANSAKCIJA SE ŠTAMPA U DNEVNIK PROMJENA
      *           (AUDIT TRAIL), SA RAZLOGOM AKO JE ODBIJENA.
      * ULAZ    : TRANIN   - TRANSAKCIJE (TRANREC), NESORTIRANO
      *           CUSTOLD  - STARI MATIČNI UGOVORI (CUSTREC)
      * IZLAZ   : TRANSRT  - SORTIRANE TRANSAKCIJE (RADNA DATOTEKA)
      *           CUSTNEW  - NOVI MATIČNI UGOVORI
      *           UPDRPT   - DNEVNIK PROMJENA, 132 KOLONE
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - SVE TRANSAKCIJE PRIMIJENJENE
      *            4 - NEKE TRANSAKCIJE ODBIJENE
      *           12 - FATALNO: DATOTEKE, SORT ILI NESORTIRAN MATIČNI
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-08-03 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-IN     ASSIGN TO TRANIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-TRIN.
           SELECT SORT-WORK    ASSIGN TO SORTWK.
           SELECT TRANS-SRT    ASSIGN TO TRANSRT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-TRSRT.
           SELECT CUST-OLD     ASSIGN TO CUSTOLD
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-OLD.
           SELECT CUST-NEW     ASSIGN TO CUSTNEW
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-NEW.
           SELECT UPD-REPORT   ASSIGN TO UPDRPT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-RPT.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-IN.
       01  TRANS-IN-REC                PIC X(150).
      *    SORT SLOG - SAMO KLJUČEVI SU IMENOVANI
       SD  SORT-WORK.
       01  SW-RECORD.
           05  SW-CONTRACT-NO          PIC X(08).
           05  SW-TYPE                 PIC X(01).
           05  SW-DATE                 PIC 9(08).
           05  SW-SEQ                  PIC 9(04).
           05  FILLER                  PIC X(129).
       FD  TRANS-SRT.
           COPY TRANREC.
       FD  CUST-OLD.
           COPY CUSTREC.
       FD  CUST-NEW.
       01  CUST-NEW-REC                PIC X(200).
       FD  UPD-REPORT.
       01  RPT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-TRIN              PIC X(02).
           05  WS-FS-TRSRT             PIC X(02).
               88  FS-TRSRT-OK         VALUE '00'.
           05  WS-FS-OLD               PIC X(02).
               88  FS-OLD-OK           VALUE '00'.
           05  WS-FS-NEW               PIC X(02).
               88  FS-NEW-OK           VALUE '00'.
           05  WS-FS-RPT               PIC X(02).
               88  FS-RPT-OK           VALUE '00'.

      *    NOVI MATIČNI SLOG SE GRADI U RADNOJ MEMORIJI (PREFIKS NM-)
           COPY CUSTREC REPLACING ==CUST-RECORD== BY ==NM-RECORD==
                                  LEADING ==CM-== BY ==NM-==.

           COPY NADCONST.
           COPY RUNDATE.

      *    KLJUČEVI ZA BALANCED LINE - HIGH-VALUES ZNAČI KRAJ DATOTEKE
       01  WS-KEYS.
           05  WS-OLD-KEY              PIC X(08).
           05  WS-TR-KEY               PIC X(08).
           05  WS-ACTIVE-KEY           PIC X(08).
           05  WS-PREV-OLD-KEY         PIC X(08)   VALUE LOW-VALUES.

       01  WS-SWITCHES.
           05  WS-MASTER-SW            PIC X(01).
               88  MASTER-EXISTS       VALUE 'Y'.
               88  MASTER-MISSING      VALUE 'N'.
           05  WS-RESULT-SW            PIC X(01).
               88  TR-APPLIED          VALUE 'Y'.
               88  TR-REJECTED         VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-ACTION               PIC X(25).
           05  WS-NOTE                 PIC X(40).
           05  WS-AMOUNT               PIC 9(07)V99.
           05  WS-OLD-FEE-ED           PIC ZZ,ZZ9.99.
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-CNT-OLD              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-NEW              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-TR               PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-OK               PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-REJ              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-ADD              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-CANCEL           PIC 9(05)   COMP-3 VALUE ZERO.

       01  WS-PAGE-CONTROL.
           05  WS-PAGE-NO              PIC 9(04)   VALUE ZERO.
           05  WS-LINE-CNT             PIC 9(03)   VALUE 99.
           05  WS-MAX-LINES            PIC 9(03)   VALUE 50.

       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

      *----------------------------------------------------------------*
      * LINIJE DNEVNIKA PROMJENA (132 KOLONE)
      *----------------------------------------------------------------*
       01  WS-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  HDR1-COMPANY            PIC X(40).
           05  FILLER                  PIC X(76)
                   VALUE 'NADZOR - DNEVNIK PROMJENA UGOVORA'.
           05  FILLER                  PIC X(07)   VALUE 'STRANA '.
           05  HDR1-PAGE               PIC ZZZ9.
           05  FILLER                  PIC X(04)   VALUE SPACES.
       01  WS-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)   VALUE
                   'PROGRAM: NADUPD'.
           05  FILLER                  PIC X(14)   VALUE
                   'DATUM OBRADE: '.
           05  HDR2-DATE               PIC X(10).
           05  FILLER                  PIC X(67)   VALUE SPACES.
       01  WS-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'UGOVOR'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(01)  VALUE 'T'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'DATUM'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(25)  VALUE 'PROMJENA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(09)  VALUE '    IZNOS'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'STATUS'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(40)  VALUE 'NAPOMENA'.
           05  FILLER  PIC X(24)  VALUE SPACES.
       01  WS-HDR4.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(107)  VALUE ALL '-'.
           05  FILLER                  PIC X(24)   VALUE SPACES.
       01  WS-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-CONTRACT             PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-TYPE                 PIC X(01).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-DATE                 PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-ACTION               PIC X(25).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-AMOUNT               PIC ZZ,ZZ9.99 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-STATUS               PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-NOTE                 PIC X(40).
           05  FILLER                  PIC X(24)   VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  TL-LABEL                PIC X(40).
           05  TL-COUNT                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(85)   VALUE SPACES.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA - BALANCED LINE: OBRAĐUJE SE UVIJEK MANJI KLJUČ
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-KEY
               UNTIL WS-OLD-KEY = HIGH-VALUES
                 AND WS-TR-KEY  = HIGH-VALUES
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * SORT TRANSAKCIJA, OTVARANJE DATOTEKA, PRVO ČITANJE
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'NADUPD   - START AZURIRANJA UGOVORA'
           PERFORM 8900-GET-RUN-DATE
           MOVE WS-RUN-DATE-TXT TO HDR2-DATE
           MOVE NC-COMPANY-1    TO HDR1-COMPANY
           SORT SORT-WORK
               ON ASCENDING KEY SW-CONTRACT-NO
               ON ASCENDING KEY SW-DATE
               ON ASCENDING KEY SW-SEQ
               USING  TRANS-IN
               GIVING TRANS-SRT
           IF SORT-RETURN NOT = ZERO
               DISPLAY 'NADUPD   - GRESKA U SORTU TRANSAKCIJA, RC '
                       SORT-RETURN
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT  TRANS-SRT
                       CUST-OLD
                OUTPUT CUST-NEW
                       UPD-REPORT
           IF NOT FS-TRSRT-OK OR NOT FS-OLD-OK
              OR NOT FS-NEW-OK OR NOT FS-RPT-OK
               DISPLAY 'NADUPD   - GRESKA OTVARANJA DATOTEKA: '
                       WS-FS-TRSRT ' ' WS-FS-OLD ' '
                       WS-FS-NEW ' ' WS-FS-RPT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8100-READ-OLD
           PERFORM 8200-READ-TRANS.

      *----------------------------------------------------------------*
      * JEDAN KLJUČ: PREUZMI STARI SLOG (AKO POSTOJI), PRIMIJENI SVE
      * TRANSAKCIJE ZA TAJ KLJUČ I UPIŠI NOVI SLOG
      *----------------------------------------------------------------*
       2000-PROCESS-KEY.
           IF WS-OLD-KEY < WS-TR-KEY
               MOVE WS-OLD-KEY TO WS-ACTIVE-KEY
           ELSE
               MOVE WS-TR-KEY  TO WS-ACTIVE-KEY
           END-IF
           IF WS-OLD-KEY = WS-ACTIVE-KEY
               MOVE CUST-RECORD TO NM-RECORD
               SET MASTER-EXISTS TO TRUE
               PERFORM 8100-READ-OLD
           ELSE
               SET MASTER-MISSING TO TRUE
           END-IF
           PERFORM 3000-APPLY-TRANSACTION
               UNTIL WS-TR-KEY NOT = WS-ACTIVE-KEY
           IF MASTER-EXISTS
               WRITE CUST-NEW-REC FROM NM-RECORD
               ADD 1 TO WS-CNT-NEW
           END-IF.

      *----------------------------------------------------------------*
      * PROVJERA I PRIMJENA JEDNE TRANSAKCIJE
      *----------------------------------------------------------------*
       3000-APPLY-TRANSACTION.
           ADD 1 TO WS-CNT-TR
           MOVE SPACES TO WS-ACTION WS-NOTE
           MOVE ZERO   TO WS-AMOUNT
           SET TR-APPLIED TO TRUE
           EVALUATE TRUE
               WHEN TR-ADD
                   MOVE 'NOVI UGOVOR' TO WS-ACTION
                   IF MASTER-EXISTS
                       MOVE 'UGOVOR VEC POSTOJI' TO WS-NOTE
                       SET TR-REJECTED TO TRUE
                   ELSE
                       PERFORM 3100-ADD-CONTRACT
                   END-IF
               WHEN TR-PRICE OR TR-CANCEL OR TR-SERVICE
                   EVALUATE TRUE
                       WHEN TR-PRICE
                           MOVE 'PROMJENA CIJENE'   TO WS-ACTION
                       WHEN TR-CANCEL
                           MOVE 'OTKAZ UGOVORA'     TO WS-ACTION
                       WHEN OTHER
                           MOVE 'SERVISNA INTERVENCIJA'
                                                    TO WS-ACTION
                   END-EVALUATE
                   EVALUATE TRUE
                       WHEN MASTER-MISSING
                           MOVE 'UGOVOR NE POSTOJI' TO WS-NOTE
                           SET TR-REJECTED TO TRUE
                       WHEN NM-CANCELLED
                           MOVE 'UGOVOR JE VEC OTKAZAN' TO WS-NOTE
                           SET TR-REJECTED TO TRUE
                       WHEN TR-PRICE
                           PERFORM 3200-CHANGE-PRICE
                       WHEN TR-CANCEL
                           PERFORM 3300-CANCEL-CONTRACT
                       WHEN OTHER
                           PERFORM 3400-SERVICE-CALL
                   END-EVALUATE
               WHEN OTHER
                   MOVE 'NEPOZNATA TRANSAKCIJA' TO WS-ACTION
                   STRING 'NEPOZNAT TIP TRANSAKCIJE: ' TR-TYPE
                          DELIMITED BY SIZE INTO WS-NOTE
                   END-STRING
                   SET TR-REJECTED TO TRUE
           END-EVALUATE
           IF TR-APPLIED
               ADD 1 TO WS-CNT-OK
           ELSE
               ADD 1 TO WS-CNT-REJ
               MOVE 4 TO WS-RC
           END-IF
           PERFORM 4000-PRINT-AUDIT
           PERFORM 8200-READ-TRANS.

      *    A - NOVI UGOVOR: SLOG SE GRADI OD NULE
       3100-ADD-CONTRACT.
           EVALUATE TRUE
               WHEN TA-CUST-NAME = SPACES
                   MOVE 'NAZIV KUPCA JE PRAZAN' TO WS-NOTE
                   SET TR-REJECTED TO TRUE
               WHEN TA-MONTHLY-FEE NOT NUMERIC
               WHEN TA-MONTHLY-FEE = ZERO
                   MOVE 'NEISPRAVNA MJESECNA NAKNADA' TO WS-NOTE
                   SET TR-REJECTED TO TRUE
               WHEN TA-CYCLE NOT = 'M' AND NOT = 'Q' AND NOT = 'Y'
                   MOVE 'NEISPRAVAN CIKLUS NAPLATE (M/Q/Y)'
                     TO WS-NOTE
                   SET TR-REJECTED TO TRUE
               WHEN OTHER
                   MOVE SPACES          TO NM-RECORD
                   MOVE TR-CONTRACT-NO  TO NM-CONTRACT-NO
                   MOVE TA-CUST-NAME    TO NM-CUST-NAME
                   MOVE TA-STREET       TO NM-STREET
                   MOVE TA-CITY         TO NM-CITY
                   MOVE TA-SITE-DESC    TO NM-SITE-DESC
                   MOVE TA-CAMERAS      TO NM-CAMERAS
                   MOVE TA-MONTHLY-FEE  TO NM-MONTHLY-FEE
                   MOVE TA-CYCLE        TO NM-CYCLE
                   MOVE TR-DATE         TO NM-START-DATE
                   MOVE ZERO            TO NM-END-DATE
                                           NM-PENDING-EXTRA
                                           NM-PENDING-CNT
                                           NM-LAST-BILLED
                   SET NM-ACTIVE        TO TRUE
                   SET MASTER-EXISTS    TO TRUE
                   MOVE TA-MONTHLY-FEE  TO WS-AMOUNT
                   MOVE TA-CUST-NAME    TO WS-NOTE
                   ADD 1 TO WS-CNT-ADD
           END-EVALUATE.

      *    P - NOVA MJESEČNA NAKNADA OD SLJEDEĆEG OBRAČUNA
       3200-CHANGE-PRICE.
           IF TP-NEW-FEE NOT NUMERIC OR TP-NEW-FEE = ZERO
               MOVE 'NEISPRAVNA NOVA NAKNADA' TO WS-NOTE
               SET TR-REJECTED TO TRUE
           ELSE
               MOVE NM-MONTHLY-FEE TO WS-OLD-FEE-ED
               STRING 'STARA NAKNADA ' WS-OLD-FEE-ED
                      DELIMITED BY SIZE INTO WS-NOTE
               END-STRING
               MOVE TP-NEW-FEE TO NM-MONTHLY-FEE
               MOVE TP-NEW-FEE TO WS-AMOUNT
           END-IF.

      *    X - OTKAZ: UGOVOR OSTAJE U DATOTECI (ISTORIJA), NE NAPLAĆUJE
      *        SE VIŠE NAKNADA, ALI SE NAPLAĆUJU NEFAKTURISANE USLUGE
       3300-CANCEL-CONTRACT.
           SET NM-CANCELLED TO TRUE
           MOVE TR-DATE     TO NM-END-DATE
           MOVE TX-REASON   TO WS-NOTE
           ADD 1 TO WS-CNT-CANCEL.

      *    S - INTERVENCIJA: SATI x SATNICA + MATERIJAL, ČEKA FAKTURU
       3400-SERVICE-CALL.
           IF TS-HOURS NOT NUMERIC OR TS-MATERIAL NOT NUMERIC
               MOVE 'NEISPRAVNI SATI ILI MATERIJAL' TO WS-NOTE
               SET TR-REJECTED TO TRUE
           ELSE
               COMPUTE WS-AMOUNT ROUNDED =
                       TS-HOURS * NC-HOURLY-RATE + TS-MATERIAL
               ADD WS-AMOUNT TO NM-PENDING-EXTRA
               ADD 1         TO NM-PENDING-CNT
               MOVE TS-DESC  TO WS-NOTE
           END-IF.

      *----------------------------------------------------------------*
      * ŠTAMPA STAVKE DNEVNIKA PROMJENA
      *----------------------------------------------------------------*
       4000-PRINT-AUDIT.
           IF WS-LINE-CNT >= WS-MAX-LINES
               PERFORM 4100-PRINT-HEADERS
           END-IF
           MOVE TR-CONTRACT-NO TO DL-CONTRACT
           MOVE TR-TYPE        TO DL-TYPE
           MOVE TR-DATE        TO WS-FMT-IN
           PERFORM 8910-FORMAT-DATE
           MOVE WS-FMT-OUT     TO DL-DATE
           MOVE WS-ACTION      TO DL-ACTION
           MOVE WS-AMOUNT      TO DL-AMOUNT
           IF TR-APPLIED
               MOVE 'OK'       TO DL-STATUS
           ELSE
               MOVE 'ODBIJENO' TO DL-STATUS
           END-IF
           MOVE WS-NOTE        TO DL-NOTE
           WRITE RPT-LINE FROM WS-DETAIL AFTER ADVANCING 1 LINE
           ADD 1 TO WS-LINE-CNT.

       4100-PRINT-HEADERS.
           ADD 1 TO WS-PAGE-NO
           MOVE WS-PAGE-NO TO HDR1-PAGE
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-HDR2 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR3 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE 5 TO WS-LINE-CNT.

      *----------------------------------------------------------------*
      * ČITANJE - NA KRAJU DATOTEKE KLJUČ DOBIJA HIGH-VALUES
      *----------------------------------------------------------------*
       8100-READ-OLD.
           READ CUST-OLD
               AT END
                   MOVE HIGH-VALUES TO WS-OLD-KEY
               NOT AT END
                   ADD 1 TO WS-CNT-OLD
                   MOVE CM-CONTRACT-NO TO WS-OLD-KEY
      *            MATIČNI MORA BITI STROGO RASTUĆE SORTIRAN
                   IF WS-OLD-KEY NOT > WS-PREV-OLD-KEY
                       DISPLAY 'NADUPD   - *** MATICNA DATOTEKA NIJE '
                               'SORTIRANA KOD UGOVORA ' WS-OLD-KEY
                       DISPLAY 'NADUPD   - *** OBRADA PREKINUTA'
                       CLOSE TRANS-SRT CUST-OLD CUST-NEW UPD-REPORT
                       MOVE 12 TO RETURN-CODE
                       STOP RUN
                   END-IF
                   MOVE WS-OLD-KEY TO WS-PREV-OLD-KEY
           END-READ.

       8200-READ-TRANS.
           READ TRANS-SRT
               AT END
                   MOVE HIGH-VALUES TO WS-TR-KEY
               NOT AT END
                   MOVE TR-CONTRACT-NO TO WS-TR-KEY
           END-READ.

           COPY GETDATE.

      *----------------------------------------------------------------*
      * KRAJ: ZBIRNI PODACI U DNEVNIK I NA KONZOLU
      *----------------------------------------------------------------*
       9000-TERMINATE.
           IF WS-PAGE-NO = ZERO
               PERFORM 4100-PRINT-HEADERS
           END-IF
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE 'UGOVORA U STAROJ MATICNOJ DATOTECI ....:'
             TO TL-LABEL
           MOVE WS-CNT-OLD TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'TRANSAKCIJA UKUPNO ....................:'
             TO TL-LABEL
           MOVE WS-CNT-TR TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  OD TOGA PRIMIJENJENO ................:'
             TO TL-LABEL
           MOVE WS-CNT-OK TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  OD TOGA ODBIJENO ....................:'
             TO TL-LABEL
           MOVE WS-CNT-REJ TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'NOVIH UGOVORA .........................:'
             TO TL-LABEL
           MOVE WS-CNT-ADD TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'OTKAZANIH UGOVORA .....................:'
             TO TL-LABEL
           MOVE WS-CNT-CANCEL TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'UGOVORA U NOVOJ MATICNOJ DATOTECI .....:'
             TO TL-LABEL
           MOVE WS-CNT-NEW TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO RPT-LINE
           WRITE RPT-LINE AFTER ADVANCING 2 LINES
           CLOSE TRANS-SRT
                 CUST-OLD
                 CUST-NEW
                 UPD-REPORT
           MOVE WS-CNT-TR  TO WS-DISP-CNT
           DISPLAY 'NADUPD   - TRANSAKCIJA OBRADJENO : ' WS-DISP-CNT
           MOVE WS-CNT-REJ TO WS-DISP-CNT
           DISPLAY 'NADUPD   - TRANSAKCIJA ODBIJENO  : ' WS-DISP-CNT
           MOVE WS-CNT-NEW TO WS-DISP-CNT
           DISPLAY 'NADUPD   - UGOVORA U NOVOM MAT.  : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'NADUPD   - KRAJ, RC=' WS-DISP-RC.

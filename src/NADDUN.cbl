      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NADDUN.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - SERVIS VIDEO NADZORA.
       DATE-WRITTEN.  2000-02-18.
      *================================================================*
      * SISTEM  : NADZOR - UGOVORI O ODRŽAVANJU I NAPLATA
      * PROGRAM : NADDUN - KORAK 040 MJESEČNE OBRADE
      * OPIS    : STAROSNA STRUKTURA POTRAŽIVANJA I OPOMENE.
      *           1. SORT OTVORENIH STAVKI PO UGOVORU; U IZLAZNOJ
      *              PROCEDURI KONTROLNI PREKID PO UGOVORU:
      *                - RASPORED DUGA PO DANIMA KAŠNJENJA
      *                  (TEKUĆE, 1-30, 31-60, 61-90, PREKO 90)
      *                - NIVO OPOMENE PO STAVCI (1/2/3) PREMA DANIMA
      *                  KAŠNJENJA; AKO JE NIVO PORASTAO, KUPAC DOBIJA
      *                  PISMO SA SVIM DOSPJELIM STAVKAMA
      *              ADRESA KUPCA SE UZIMA IZ MATIČNE DATOTEKE
      *              UGOVORA SEKVENCIJALNIM UPAROŽIVANJEM (MATCH).
      *           2. SORT AŽURIRANIH STAVKI NAZAD PO BROJU FAKTURE -
      *              TO JE ULAZ ZA NADPAY SLJEDEĆEG MJESECA.
      * ULAZ    : OPENIN   - OTVORENE STAVKE (IZ NADPAY)
      *           CUSTMST  - MATIČNI UGOVORI (IZ NADBILL)
      * IZLAZ   : OPENTMP  - STAVKE PO UGOVORU (RADNA DATOTEKA)
      *           OPENOUT  - OTVORENE STAVKE ZA SLJEDEĆI MJESEC
      *           AGERPT   - STAROSNA STRUKTURA, 132 KOLONE
      *           DUNLTR   - OPOMENE ZA ŠTAMPU, 80 KOLONA
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-06-11 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPEN-IN      ASSIGN TO OPENIN
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-CUST    ASSIGN TO SORTWK1.
           SELECT OPEN-TMP     ASSIGN TO OPENTMP
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-TMP.
           SELECT SORT-INV     ASSIGN TO SORTWK2.
           SELECT OPEN-OUT     ASSIGN TO OPENOUT
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUST-MAST    ASSIGN TO CUSTMST
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-CUST.
           SELECT AGE-RPT      ASSIGN TO AGERPT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-AGE.
           SELECT DUN-LTR      ASSIGN TO DUNLTR
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-LTR.

       DATA DIVISION.
       FILE SECTION.
       FD  OPEN-IN.
       01  OPEN-IN-REC                 PIC X(90).
      *    SORT 1: PO UGOVORU, PA PO FAKTURI
       SD  SORT-CUST.
       01  SC-RECORD.
           05  SC-INV-NO               PIC 9(08).
           05  SC-CONTRACT-NO          PIC X(08).
           05  FILLER                  PIC X(74).
       FD  OPEN-TMP.
       01  OPEN-TMP-REC                PIC X(90).
      *    SORT 2: NAZAD PO BROJU FAKTURE
       SD  SORT-INV.
       01  SI-RECORD.
           05  SI-INV-NO               PIC 9(08).
           05  FILLER                  PIC X(82).
       FD  OPEN-OUT.
       01  OPEN-OUT-REC                PIC X(90).
       FD  CUST-MAST.
           COPY CUSTREC.
       FD  AGE-RPT.
       01  AGE-LINE                    PIC X(132).
       FD  DUN-LTR.
       01  LTR-LINE                    PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-TMP               PIC X(02).
               88  FS-TMP-OK           VALUE '00'.
           05  WS-FS-CUST              PIC X(02).
               88  FS-CUST-OK          VALUE '00'.
           05  WS-FS-AGE               PIC X(02).
               88  FS-AGE-OK           VALUE '00'.
           05  WS-FS-LTR               PIC X(02).
               88  FS-LTR-OK           VALUE '00'.

           COPY NADCONST.
           COPY RUNDATE.
      *    TRENUTNA STAVKA IZ SORTA
           COPY OPENREC.

       01  WS-SWITCHES.
           05  WS-SORT-EOF-SW          PIC X(01)   VALUE 'N'.
               88  SORT-EOF            VALUE 'Y'.
           05  WS-CUST-EOF-SW          PIC X(01)   VALUE 'N'.
               88  CUST-EOF            VALUE 'Y'.
           05  WS-ESCALATE-SW          PIC X(01)   VALUE 'N'.
               88  CUST-ESCALATED      VALUE 'Y'.

       01  WS-BREAK-FIELDS.
           05  WS-BRK-CONTRACT         PIC X(08).
           05  WS-BRK-NAME             PIC X(30).
           05  WS-OPEN-AMT             PIC 9(07)V99.
           05  WS-DAYS-LATE            PIC S9(05).
           05  WS-TARGET-LEVEL         PIC 9(01).
           05  WS-LETTER-LEVEL         PIC 9(01).

      *    STAROSNI RAZREDI: 1=TEKUĆE 2=1-30 3=31-60 4=61-90 5=90+ 6=UK.
       01  WS-AGE-CUST.
           05  WS-AGE-C                PIC 9(07)V99 OCCURS 6 TIMES.
       01  WS-AGE-GRAND.
           05  WS-AGE-G                PIC 9(09)V99 OCCURS 6 TIMES.
       01  WS-BUCKET                   PIC 9(01).
       01  WS-BX                       PIC 9(01).

      *    DOSPJELE STAVKE KUPCA ZA PISMO
       01  WS-LTR-TABLE.
           05  WS-LTR-COUNT            PIC 9(02)   VALUE ZERO.
           05  WS-LTR-ITEM             OCCURS 20 TIMES.
               10  LT-INV-NO           PIC 9(08).
               10  LT-INV-DATE         PIC 9(08).
               10  LT-DUE-DATE         PIC 9(08).
               10  LT-DAYS             PIC 9(05).
               10  LT-OPEN             PIC 9(07)V99.
       01  WS-LX                       PIC 9(02).
       01  WS-LTR-TOTAL                PIC 9(07)V99.

      *    TEKSTOVI OPOMENA PO NIVOU - TABELA PREKO REDEFINES
       01  WS-DUN-TEXTS.
           05  FILLER  PIC X(30)  VALUE 'PODSJETNIK NA PLACANJE'.
           05  FILLER  PIC X(70)  VALUE
               'PREMA NASOJ EVIDENCIJI SLJEDECE FAKTURE NISU PLACENE U R
      -        'OKU.'.
           05  FILLER  PIC X(70)  VALUE
               'MOLIMO VAS DA UPLATU IZVRSITE U NAREDNIH 8 DANA.'.
           05  FILLER  PIC X(30)  VALUE 'OPOMENA'.
           05  FILLER  PIC X(70)  VALUE
               'UPRKOS PODSJETNIKU, SLJEDECE FAKTURE JOS UVIJEK NISU PLA
      -        'CENE.'.
           05  FILLER  PIC X(70)  VALUE
               'MOLIMO HITNU UPLATU, NAJKASNIJE U ROKU OD 8 DANA.'.
           05  FILLER  PIC X(30)  VALUE 'OPOMENA PRED UTUZENJE'.
           05  FILLER  PIC X(70)  VALUE
               'OVO JE POSLJEDNJA OPOMENA PRIJE POKRETANJA SUDSKOG POSTU
      -        'PKA.'.
           05  FILLER  PIC X(70)  VALUE
               'BEZ UPLATE U ROKU OD 8 DANA SERVIS ODRZAVANJA SE OBUSTAV
      -        'LJA.'.
       01  WS-DUN-TABLE REDEFINES WS-DUN-TEXTS.
           05  WS-DUN-ENTRY            OCCURS 3 TIMES.
               10  DT-TITLE            PIC X(30).
               10  DT-LINE1            PIC X(70).
               10  DT-LINE2            PIC X(70).

       01  WS-COUNTERS.
           05  WS-CNT-ITEMS            PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-CUST             PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-LETTERS          PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-LVL              PIC 9(05)   COMP-3
                                       VALUE ZERO OCCURS 3 TIMES.

       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-LVL-ED                   PIC 9.

      *----------------------------------------------------------------*
      * STAROSNA STRUKTURA - 132 KOLONE
      *----------------------------------------------------------------*
       01  WS-AGE-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  AH1-COMPANY             PIC X(40).
           05  FILLER                  PIC X(91)   VALUE
                   'NADZOR - STAROSNA STRUKTURA POTRAZIVANJA'.
       01  WS-AGE-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)   VALUE
                   'PROGRAM: NADDUN'.
           05  FILLER                  PIC X(08)   VALUE
                   'NA DAN: '.
           05  AH2-DATE                PIC X(10).
           05  FILLER                  PIC X(73)   VALUE SPACES.
       01  WS-AGE-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'UGOVOR'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(30)  VALUE 'KUPAC'.
           05  FILLER  PIC X(11)  VALUE '     TEKUCE'.
           05  FILLER  PIC X(11)  VALUE '  1-30 DANA'.
           05  FILLER  PIC X(11)  VALUE ' 31-60 DANA'.
           05  FILLER  PIC X(11)  VALUE ' 61-90 DANA'.
           05  FILLER  PIC X(11)  VALUE '   PREKO 90'.
           05  FILLER  PIC X(11)  VALUE '     UKUPNO'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(07)  VALUE 'OPOMENA'.
           05  FILLER  PIC X(18)  VALUE SPACES.
       01  WS-AGE-DASH.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(113)  VALUE ALL '-'.
           05  FILLER                  PIC X(18)   VALUE SPACES.
       01  WS-AGE-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  AD-CONTRACT             PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  AD-NAME                 PIC X(30).
           05  AD-BUCKET               OCCURS 6 TIMES.
               10  FILLER              PIC X(01).
               10  AD-AMT              PIC ZZZ,ZZ9.99 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  AD-LEVEL                PIC X(07).
           05  FILLER                  PIC X(18)   VALUE SPACES.
       01  WS-AGE-COUNT.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  AC-LABEL                PIC X(40).
           05  AC-COUNT                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(85)   VALUE SPACES.

      *----------------------------------------------------------------*
      * OPOMENA - 80 KOLONA
      *----------------------------------------------------------------*
       01  WS-LTR-TEXT.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  LX-TEXT                 PIC X(79).
       01  WS-LTR-RIGHT.
           05  FILLER                  PIC X(48)   VALUE SPACES.
           05  LR-TEXT                 PIC X(32).
       01  WS-LTR-COLHDR.
           05  FILLER  PIC X(05)  VALUE SPACES.
           05  FILLER  PIC X(10)  VALUE 'FAKTURA'.
           05  FILLER  PIC X(12)  VALUE 'DATUM'.
           05  FILLER  PIC X(12)  VALUE 'VALUTA'.
           05  FILLER  PIC X(11)  VALUE 'DANA KASNI'.
           05  FILLER  PIC X(15)  VALUE '      ZA UPLATU'.
           05  FILLER  PIC X(15)  VALUE SPACES.
       01  WS-LTR-DASH.
           05  FILLER                  PIC X(05)   VALUE SPACES.
           05  FILLER                  PIC X(59)   VALUE ALL '-'.
           05  FILLER                  PIC X(16)   VALUE SPACES.
       01  WS-LTR-ITEM-LINE.
           05  FILLER                  PIC X(05)   VALUE SPACES.
           05  LI-INV-NO               PIC 9(08).
           05  FILLER                  PIC X(02)   VALUE SPACES.
           05  LI-INV-DATE             PIC X(10).
           05  FILLER                  PIC X(02)   VALUE SPACES.
           05  LI-DUE-DATE             PIC X(10).
           05  FILLER                  PIC X(04)   VALUE SPACES.
           05  LI-DAYS                 PIC ZZZZ9.
           05  FILLER                  PIC X(08)   VALUE SPACES.
           05  LI-OPEN                 PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(16)   VALUE SPACES.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA: DVA SORTA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           SORT SORT-CUST
               ON ASCENDING KEY SC-CONTRACT-NO
               ON ASCENDING KEY SC-INV-NO
               USING OPEN-IN
               OUTPUT PROCEDURE IS 3000-AGING-AND-DUNNING
                                THRU 3000-EXIT
           IF SORT-RETURN NOT = ZERO
               DISPLAY 'NADDUN   - GRESKA U SORTU PO UGOVORU'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           CLOSE OPEN-TMP
      *    STAVKE NAZAD U REDOSLIJED FAKTURA ZA SLJEDEĆI MJESEC
           SORT SORT-INV
               ON ASCENDING KEY SI-INV-NO
               USING  OPEN-TMP
               GIVING OPEN-OUT
           IF SORT-RETURN NOT = ZERO
               DISPLAY 'NADDUN   - GRESKA U SORTU PO FAKTURI'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           DISPLAY 'NADDUN   - START STAROSNE STRUKTURE I OPOMENA'
           PERFORM 8900-GET-RUN-DATE
           MOVE NC-COMPANY-1    TO AH1-COMPANY
           MOVE WS-RUN-DATE-TXT TO AH2-DATE
           INITIALIZE WS-AGE-GRAND
           OPEN INPUT  CUST-MAST
                OUTPUT OPEN-TMP AGE-RPT DUN-LTR
           IF NOT FS-CUST-OK OR NOT FS-TMP-OK
              OR NOT FS-AGE-OK OR NOT FS-LTR-OK
               DISPLAY 'NADDUN   - GRESKA OTVARANJA DATOTEKA'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           WRITE AGE-LINE FROM WS-AGE-HDR1 AFTER ADVANCING PAGE
           WRITE AGE-LINE FROM WS-AGE-HDR2 AFTER ADVANCING 1 LINE
           WRITE AGE-LINE FROM WS-AGE-DASH AFTER ADVANCING 1 LINE
           WRITE AGE-LINE FROM WS-AGE-HDR3 AFTER ADVANCING 1 LINE
           WRITE AGE-LINE FROM WS-AGE-DASH AFTER ADVANCING 1 LINE
           PERFORM 8100-READ-CUST.

      *----------------------------------------------------------------*
      * IZLAZNA PROCEDURA SORTA 1 - KONTROLNI PREKID PO UGOVORU
      *----------------------------------------------------------------*
       3000-AGING-AND-DUNNING.
           PERFORM 3100-RETURN-ITEM
           PERFORM UNTIL SORT-EOF
               MOVE OI-CONTRACT-NO TO WS-BRK-CONTRACT
               MOVE OI-CUST-NAME   TO WS-BRK-NAME
               INITIALIZE WS-AGE-CUST
               MOVE ZERO TO WS-LTR-COUNT WS-LETTER-LEVEL
               MOVE 'N'  TO WS-ESCALATE-SW
               PERFORM UNTIL SORT-EOF
                          OR OI-CONTRACT-NO NOT = WS-BRK-CONTRACT
                   PERFORM 3200-PROCESS-ITEM
                   PERFORM 3100-RETURN-ITEM
               END-PERFORM
               PERFORM 3500-CONTRACT-BREAK
           END-PERFORM
           PERFORM 3900-PRINT-GRAND-TOTALS.
       3000-EXIT.
           EXIT.

       3100-RETURN-ITEM.
           RETURN SORT-CUST INTO OPEN-RECORD
               AT END SET SORT-EOF TO TRUE
           END-RETURN.

      *----------------------------------------------------------------*
      * JEDNA STAVKA: RAZRED KAŠNJENJA I NIVO OPOMENE
      *----------------------------------------------------------------*
       3200-PROCESS-ITEM.
           ADD 1 TO WS-CNT-ITEMS
           COMPUTE WS-OPEN-AMT = OI-AMOUNT - OI-PAID
           COMPUTE WS-DAYS-LATE = WS-RUN-INT
                   - FUNCTION INTEGER-OF-DATE (OI-DUE-DATE)
           EVALUATE TRUE
               WHEN WS-DAYS-LATE <= 0   MOVE 1 TO WS-BUCKET
               WHEN WS-DAYS-LATE <= 30  MOVE 2 TO WS-BUCKET
               WHEN WS-DAYS-LATE <= 60  MOVE 3 TO WS-BUCKET
               WHEN WS-DAYS-LATE <= 90  MOVE 4 TO WS-BUCKET
               WHEN OTHER               MOVE 5 TO WS-BUCKET
           END-EVALUATE
           ADD WS-OPEN-AMT TO WS-AGE-C (WS-BUCKET) WS-AGE-C (6)
      *    OPOMENA TEK POSLIJE DANA TOLERANCIJE
           IF WS-DAYS-LATE > NC-GRACE-DAYS
               EVALUATE TRUE
                   WHEN WS-DAYS-LATE > NC-DUN3-DAYS
                       MOVE 3 TO WS-TARGET-LEVEL
                   WHEN WS-DAYS-LATE > NC-DUN2-DAYS
                       MOVE 2 TO WS-TARGET-LEVEL
                   WHEN OTHER
                       MOVE 1 TO WS-TARGET-LEVEL
               END-EVALUATE
      *        NIVO SE SAMO POVEĆAVA - ISTA OPOMENA SE NE ŠALJE DVAPUT
               IF WS-TARGET-LEVEL > OI-DUN-LEVEL
                   MOVE WS-TARGET-LEVEL TO OI-DUN-LEVEL
                   MOVE WS-RUN-DATE     TO OI-LAST-DUN-DATE
                   SET CUST-ESCALATED   TO TRUE
               END-IF
               IF OI-DUN-LEVEL > WS-LETTER-LEVEL
                   MOVE OI-DUN-LEVEL TO WS-LETTER-LEVEL
               END-IF
               IF WS-LTR-COUNT < 20
                   ADD 1 TO WS-LTR-COUNT
                   MOVE OI-INV-NO    TO LT-INV-NO   (WS-LTR-COUNT)
                   MOVE OI-INV-DATE  TO LT-INV-DATE (WS-LTR-COUNT)
                   MOVE OI-DUE-DATE  TO LT-DUE-DATE (WS-LTR-COUNT)
                   MOVE WS-DAYS-LATE TO LT-DAYS     (WS-LTR-COUNT)
                   MOVE WS-OPEN-AMT  TO LT-OPEN     (WS-LTR-COUNT)
               END-IF
           END-IF
           WRITE OPEN-TMP-REC FROM OPEN-RECORD.

      *----------------------------------------------------------------*
      * KRAJ UGOVORA: RED STAROSNE STRUKTURE I EVENTUALNO PISMO
      *----------------------------------------------------------------*
       3500-CONTRACT-BREAK.
           ADD 1 TO WS-CNT-CUST
           MOVE WS-BRK-CONTRACT TO AD-CONTRACT
           MOVE WS-BRK-NAME     TO AD-NAME
           PERFORM VARYING WS-BX FROM 1 BY 1 UNTIL WS-BX > 6
               MOVE WS-AGE-C (WS-BX) TO AD-AMT (WS-BX)
               ADD  WS-AGE-C (WS-BX) TO WS-AGE-G (WS-BX)
           END-PERFORM
           MOVE SPACES TO AD-LEVEL
           IF CUST-ESCALATED
               MOVE WS-LETTER-LEVEL TO WS-LVL-ED
               STRING 'NIVO ' WS-LVL-ED
                      DELIMITED BY SIZE INTO AD-LEVEL
               END-STRING
               PERFORM 4000-PRINT-LETTER
           END-IF
           WRITE AGE-LINE FROM WS-AGE-DETAIL AFTER ADVANCING 1 LINE.

       3900-PRINT-GRAND-TOTALS.
           WRITE AGE-LINE FROM WS-AGE-DASH AFTER ADVANCING 1 LINE
           MOVE SPACES   TO AD-CONTRACT AD-LEVEL
           MOVE 'UKUPNO' TO AD-NAME
           PERFORM VARYING WS-BX FROM 1 BY 1 UNTIL WS-BX > 6
               MOVE WS-AGE-G (WS-BX) TO AD-AMT (WS-BX)
           END-PERFORM
           WRITE AGE-LINE FROM WS-AGE-DETAIL AFTER ADVANCING 1 LINE
           WRITE AGE-LINE FROM WS-AGE-DASH AFTER ADVANCING 1 LINE
           MOVE 'OTVORENIH STAVKI ......................:'
             TO AC-LABEL
           MOVE WS-CNT-ITEMS TO AC-COUNT
           WRITE AGE-LINE FROM WS-AGE-COUNT AFTER ADVANCING 1 LINE
           MOVE 'KUPACA SA OTVORENIM STAVKAMA ..........:'
             TO AC-LABEL
           MOVE WS-CNT-CUST TO AC-COUNT
           WRITE AGE-LINE FROM WS-AGE-COUNT AFTER ADVANCING 1 LINE
           MOVE 'POSLANO OPOMENA UKUPNO ................:'
             TO AC-LABEL
           MOVE WS-CNT-LETTERS TO AC-COUNT
           WRITE AGE-LINE FROM WS-AGE-COUNT AFTER ADVANCING 1 LINE
           MOVE '  NIVO 1 - PODSJETNIK .................:'
             TO AC-LABEL
           MOVE WS-CNT-LVL (1) TO AC-COUNT
           WRITE AGE-LINE FROM WS-AGE-COUNT AFTER ADVANCING 1 LINE
           MOVE '  NIVO 2 - OPOMENA ....................:'
             TO AC-LABEL
           MOVE WS-CNT-LVL (2) TO AC-COUNT
           WRITE AGE-LINE FROM WS-AGE-COUNT AFTER ADVANCING 1 LINE
           MOVE '  NIVO 3 - PRED UTUZENJE ..............:'
             TO AC-LABEL
           MOVE WS-CNT-LVL (3) TO AC-COUNT
           WRITE AGE-LINE FROM WS-AGE-COUNT AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO AGE-LINE
           WRITE AGE-LINE AFTER ADVANCING 2 LINES.

      *----------------------------------------------------------------*
      * PISMO OPOMENE - JEDNO PO KUPCU, NIVO = NAJVIŠI NIVO STAVKI
      *----------------------------------------------------------------*
       4000-PRINT-LETTER.
           ADD 1 TO WS-CNT-LETTERS
           ADD 1 TO WS-CNT-LVL (WS-LETTER-LEVEL)
           PERFORM 4100-FIND-CUSTOMER
           MOVE NC-COMPANY-1 TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING PAGE
           MOVE NC-COMPANY-2 TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
           MOVE NC-COMPANY-3 TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
           MOVE SPACES TO LR-TEXT
           STRING 'DATUM: ' WS-RUN-DATE-TXT
                  DELIMITED BY SIZE INTO LR-TEXT
           END-STRING
           WRITE LTR-LINE FROM WS-LTR-RIGHT AFTER ADVANCING 1 LINE
      *    ADRESA PRIMAOCA
           MOVE WS-BRK-NAME TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 2 LINES
           IF CM-CONTRACT-NO = WS-BRK-CONTRACT
               MOVE CM-STREET TO LX-TEXT
               WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
               MOVE CM-CITY   TO LX-TEXT
               WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
           END-IF
           MOVE SPACES TO LX-TEXT
           STRING 'UGOVOR: ' WS-BRK-CONTRACT
                  DELIMITED BY SIZE INTO LX-TEXT
           END-STRING
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
      *    NASLOV I TEKST PO NIVOU IZ TABELE
           MOVE DT-TITLE (WS-LETTER-LEVEL) TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 3 LINES
           MOVE ALL '=' TO LX-TEXT (1:30)
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
           MOVE DT-LINE1 (WS-LETTER-LEVEL) TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 2 LINES
           MOVE DT-LINE2 (WS-LETTER-LEVEL) TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE
      *    TABELA DOSPJELIH STAVKI
           WRITE LTR-LINE FROM WS-LTR-DASH AFTER ADVANCING 2 LINES
           WRITE LTR-LINE FROM WS-LTR-COLHDR AFTER ADVANCING 1 LINE
           WRITE LTR-LINE FROM WS-LTR-DASH AFTER ADVANCING 1 LINE
           MOVE ZERO TO WS-LTR-TOTAL
           PERFORM VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > WS-LTR-COUNT
               MOVE LT-INV-NO (WS-LX)   TO LI-INV-NO
               MOVE LT-INV-DATE (WS-LX) TO WS-FMT-IN
               PERFORM 8910-FORMAT-DATE
               MOVE WS-FMT-OUT          TO LI-INV-DATE
               MOVE LT-DUE-DATE (WS-LX) TO WS-FMT-IN
               PERFORM 8910-FORMAT-DATE
               MOVE WS-FMT-OUT          TO LI-DUE-DATE
               MOVE LT-DAYS (WS-LX)     TO LI-DAYS
               MOVE LT-OPEN (WS-LX)     TO LI-OPEN
               ADD  LT-OPEN (WS-LX)     TO WS-LTR-TOTAL
               WRITE LTR-LINE FROM WS-LTR-ITEM-LINE
                     AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE LTR-LINE FROM WS-LTR-DASH AFTER ADVANCING 1 LINE
           MOVE ZERO TO LI-INV-NO
           MOVE SPACES TO LI-INV-DATE LI-DUE-DATE
           MOVE ZERO TO LI-DAYS
           MOVE WS-LTR-TOTAL TO LI-OPEN
           MOVE WS-LTR-ITEM-LINE TO LTR-LINE
           MOVE 'UKUPNO DUG (KM):' TO LTR-LINE (6:17)
           MOVE SPACES TO LTR-LINE (23:25)
           WRITE LTR-LINE AFTER ADVANCING 1 LINE
           MOVE 'AKO STE U MEDJUVREMENU IZVRSILI UPLATU, SMATRAJTE OVO'
             TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 3 LINES
           MOVE 'PISMO BESPREDMETNIM. ZAHVALJUJEMO NA SARADNJI.'
             TO LX-TEXT
           WRITE LTR-LINE FROM WS-LTR-TEXT AFTER ADVANCING 1 LINE.

      *    SEKVENCIJALNO UPARIVANJE SA MATIČNOM (OBJE PO UGOVORU)
       4100-FIND-CUSTOMER.
           PERFORM 8100-READ-CUST
               UNTIL CUST-EOF
                  OR CM-CONTRACT-NO >= WS-BRK-CONTRACT.

       8100-READ-CUST.
           READ CUST-MAST
               AT END
                   SET CUST-EOF TO TRUE
                   MOVE HIGH-VALUES TO CM-CONTRACT-NO
           END-READ.

           COPY GETDATE.

       9000-TERMINATE.
           CLOSE CUST-MAST AGE-RPT DUN-LTR
           MOVE WS-CNT-ITEMS   TO WS-DISP-CNT
           DISPLAY 'NADDUN   - OTVORENIH STAVKI     : ' WS-DISP-CNT
           MOVE WS-CNT-LETTERS TO WS-DISP-CNT
           DISPLAY 'NADDUN   - POSLANO OPOMENA      : ' WS-DISP-CNT
           MOVE ZERO TO RETURN-CODE
           DISPLAY 'NADDUN   - KRAJ, RC= 0'.

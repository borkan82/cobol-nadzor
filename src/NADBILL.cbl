      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NADBILL.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - SERVIS VIDEO NADZORA.
       DATE-WRITTEN.  1999-10-10.
      *================================================================*
      * SISTEM  : NADZOR - UGOVORI O ODRŽAVANJU I NAPLATA
      * PROGRAM : NADBILL - KORAK 020 MJESEČNE OBRADE
      * OPIS    : MJESEČNI OBRAČUN. ZA SVAKI UGOVOR ODLUČUJE DA LI JE
      *           NAKNADA DOSPJELA U OVOM PERIODU (PO CIKLUSU M/Q/Y,
      *           U RITMU OD MJESECA POČETKA), RAČUNA SRAZMJERNI DIO
      *           ZA UGOVORE KOJI POČINJU U TOKU MJESECA, DODAJE
      *           NEFAKTURISANE SERVISNE INTERVENCIJE I PDV, PA:
      *             - ŠTAMPA FAKTURU (JEDNA STRANA PO FAKTURI)
      *             - PIŠE NOVU OTVORENU STAVKU ZA NAPLATU
      *             - PIŠE RED U KNJIGU IZLAZNIH FAKTURA (REGISTAR)
      *           ZAŠTITA OD DUPLE NAPLATE: UGOVOR KOJI JE VEĆ
      *           FAKTURISAN ZA PERIOD (CM-LAST-BILLED) SE PRESKAČE,
      *           PA JE PONOVNO POKRETANJE ISTOG PERIODA BEZBJEDNO.
      * ULAZ    : CUSTIN   - MATIČNI UGOVORI (IZ NADUPD)
      *           CTLIN    - KONTROLNI SLOG (ZADNJI BROJ FAKTURE)
      * IZLAZ   : CUSTOUT  - MATIČNI UGOVORI POSLIJE OBRAČUNA
      *           CTLOUT   - NOVI KONTROLNI SLOG
      *           NEWINV   - NOVE OTVORENE STAVKE (OPENREC)
      *           INVPRT   - FAKTURE ZA ŠTAMPU, 80 KOLONA
      *           BILREG   - KNJIGA IZLAZNIH FAKTURA, 132 KOLONE
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-03-10 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-IN      ASSIGN TO CUSTIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-CIN.
           SELECT CUST-OUT     ASSIGN TO CUSTOUT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-COUT.
           SELECT CTL-IN       ASSIGN TO CTLIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-CTLI.
           SELECT CTL-OUT      ASSIGN TO CTLOUT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-CTLO.
           SELECT NEW-INV      ASSIGN TO NEWINV
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-INV.
           SELECT INV-PRINT    ASSIGN TO INVPRT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-PRT.
           SELECT BIL-REG      ASSIGN TO BILREG
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-REG.

       DATA DIVISION.
       FILE SECTION.
       FD  CUST-IN.
           COPY CUSTREC.
       FD  CUST-OUT.
       01  CUST-OUT-REC                PIC X(200).
       FD  CTL-IN.
           COPY CTLREC.
       FD  CTL-OUT.
       01  CTL-OUT-REC                 PIC X(30).
       FD  NEW-INV.
           COPY OPENREC.
       FD  INV-PRINT.
       01  INV-LINE                    PIC X(80).
       FD  BIL-REG.
       01  REG-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-CIN               PIC X(02).
               88  FS-CIN-OK           VALUE '00'.
           05  WS-FS-COUT              PIC X(02).
               88  FS-COUT-OK          VALUE '00'.
           05  WS-FS-CTLI              PIC X(02).
               88  FS-CTLI-OK          VALUE '00'.
           05  WS-FS-CTLO              PIC X(02).
               88  FS-CTLO-OK          VALUE '00'.
           05  WS-FS-INV               PIC X(02).
               88  FS-INV-OK           VALUE '00'.
           05  WS-FS-PRT               PIC X(02).
               88  FS-PRT-OK           VALUE '00'.
           05  WS-FS-REG               PIC X(02).
               88  FS-REG-OK           VALUE '00'.

           COPY NADCONST.
           COPY RUNDATE.

      *    KONTROLNI SLOG SE DRŽI U MEMORIJI TOKOM CIJELE OBRADE
       01  WS-CTL.
           05  WS-LAST-INV-NO          PIC 9(08).

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)   VALUE 'N'.
               88  CUST-EOF            VALUE 'Y'.
           05  WS-PRORATED-SW          PIC X(01)   VALUE 'N'.
               88  FEE-PRORATED        VALUE 'Y'.
           05  WS-DUE-SW               PIC X(01)   VALUE 'N'.
               88  FEE-DUE             VALUE 'Y'.

      *    POMOĆNA POLJA ZA DATUME I CIKLUS
       01  WS-CYCLE-FIELDS.
           05  WS-DT                   PIC 9(08).
           05  WS-DT-R REDEFINES WS-DT.
               10  WS-DT-YYYY          PIC 9(04).
               10  WS-DT-MM            PIC 9(02).
               10  WS-DT-DD            PIC 9(02).
           05  WS-RUN-MONTHS           PIC 9(06).
           05  WS-START-MONTHS         PIC 9(06).
           05  WS-MONTH-DIFF           PIC S9(05).
           05  WS-CYCLE-M              PIC 9(02).
           05  WS-DIM                  PIC 9(02).
           05  WS-DAYS-USED            PIC 9(02).
           05  WS-FIRST-OF-MONTH       PIC 9(08).
           05  WS-FIRST-OF-NEXT        PIC 9(08).
           05  WS-END-MONTHS           PIC 9(06).
           05  WS-END-YYYY             PIC 9(04).
           05  WS-END-MM               PIC 9(02).
           05  WS-DUE-INT              PIC S9(09)  COMP.

      *    IZNOSI JEDNE FAKTURE
       01  WS-INVOICE-FIELDS.
           05  WS-FEE                  PIC 9(07)V99.
           05  WS-QTY-MONTHS           PIC 9(02)V999.
           05  WS-EXTRA                PIC 9(07)V99.
           05  WS-NET                  PIC 9(07)V99.
           05  WS-VAT                  PIC 9(07)V99.
           05  WS-GROSS                PIC 9(07)V99.
           05  WS-DUE-DATE             PIC 9(08).
           05  WS-DUE-TXT              PIC X(10).
           05  WS-FEE-DESC             PIC X(46).
           05  WS-EXTRA-DESC           PIC X(46).
           05  WS-MM-ED                PIC 99.
           05  WS-MM2-ED               PIC 99.
           05  WS-PTR                  PIC 9(02).
           05  WS-DD-ED                PIC 99.
           05  WS-CNT-ED               PIC ZZ9.

       01  WS-TOTALS.
           05  WS-CNT-READ             PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-INV              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-SKIP             PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-NOTDUE           PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-TOT-FEE              PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-EXTRA            PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-NET              PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-VAT              PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-GROSS            PIC 9(09)V99 VALUE ZERO.

       01  WS-DISP-CNT                 PIC ZZ,ZZ9.

      *----------------------------------------------------------------*
      * FAKTURA - 80 KOLONA
      *----------------------------------------------------------------*
       01  WS-INV-HDR.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  IH-LEFT                 PIC X(40).
           05  FILLER                  PIC X(17)   VALUE SPACES.
           05  IH-LABEL                PIC X(10).
           05  IH-VALUE                PIC X(12).
       01  WS-INV-CUST.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  IC-LABEL                PIC X(09).
           05  IC-TEXT                 PIC X(70).
       01  WS-INV-DASH.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(76)   VALUE ALL '-'.
           05  FILLER                  PIC X(03)   VALUE SPACES.
       01  WS-INV-ITEM-HDR.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(46)  VALUE 'OPIS'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(07)  VALUE '   KOL.'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(09)  VALUE '   CIJENA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '     IZNOS'.
           05  FILLER  PIC X(04)  VALUE SPACES.
       01  WS-INV-ITEM.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  II-DESC                 PIC X(46).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  II-QTY                  PIC ZZ9.999 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  II-PRICE                PIC ZZ,ZZ9.99 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  II-AMOUNT               PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04)   VALUE SPACES.
       01  WS-INV-TOTAL.
           05  FILLER                  PIC X(41)   VALUE SPACES.
           05  IT-LABEL                PIC X(25).
           05  IT-AMOUNT               PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04)   VALUE SPACES.

      *----------------------------------------------------------------*
      * KNJIGA IZLAZNIH FAKTURA - 132 KOLONE
      *----------------------------------------------------------------*
       01  WS-REG-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RH1-COMPANY             PIC X(40).
           05  FILLER                  PIC X(91)   VALUE
                   'NADZOR - KNJIGA IZLAZNIH FAKTURA'.
       01  WS-REG-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)   VALUE
                   'PROGRAM: NADBILL'.
           05  FILLER                  PIC X(19)   VALUE
                   'OBRACUNSKI PERIOD: '.
           05  RH2-PERIOD              PIC X(07).
           05  FILLER                  PIC X(05)   VALUE SPACES.
           05  FILLER                  PIC X(14)   VALUE
                   'DATUM OBRADE: '.
           05  RH2-DATE                PIC X(10).
           05  FILLER                  PIC X(36)   VALUE SPACES.
       01  WS-REG-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'FAKTURA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'UGOVOR'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(30)  VALUE 'KUPAC'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(01)  VALUE 'C'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '   NAKNADA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(09)  VALUE '   SERVIS'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '  OSNOVICA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(09)  VALUE '      PDV'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '    UKUPNO'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'VALUTA'.
           05  FILLER  PIC X(17)  VALUE SPACES.
       01  WS-REG-DASH.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(114)  VALUE ALL '-'.
           05  FILLER                  PIC X(17)   VALUE SPACES.
       01  WS-REG-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-INV-NO               PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-CONTRACT             PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-NAME                 PIC X(30).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-CYCLE                PIC X(01).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-FEE                  PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-EXTRA                PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-NET                  PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-VAT                  PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-GROSS                PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RD-DUE                  PIC X(10).
           05  FILLER                  PIC X(17)   VALUE SPACES.
       01  WS-REG-COUNT.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  RC-LABEL                PIC X(40).
           05  RC-COUNT                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(85)   VALUE SPACES.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 3000-PROCESS-CONTRACT
               UNTIL CUST-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * DATUM, PERIOD, BROJ DANA U MJESECU, KONTROLNI SLOG
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'NADBILL  - START MJESECNOG OBRACUNA'
           PERFORM 8900-GET-RUN-DATE
           COMPUTE WS-RUN-MONTHS = WS-RUN-YYYY * 12 + WS-RUN-MM
      *    BROJ DANA U MJESECU = PRVI SLJEDEĆEG MINUS PRVI OVOG
           COMPUTE WS-FIRST-OF-MONTH = WS-RUN-PERIOD * 100 + 1
           IF WS-RUN-MM = 12
               COMPUTE WS-FIRST-OF-NEXT =
                       (WS-RUN-YYYY + 1) * 10000 + 101
           ELSE
               COMPUTE WS-FIRST-OF-NEXT = WS-FIRST-OF-MONTH + 100
           END-IF
           COMPUTE WS-DIM =
                   FUNCTION INTEGER-OF-DATE (WS-FIRST-OF-NEXT)
                 - FUNCTION INTEGER-OF-DATE (WS-FIRST-OF-MONTH)
      *    VALUTA PLAĆANJA ISTA ZA SVE FAKTURE OVE OBRADE
           COMPUTE WS-DUE-INT = WS-RUN-INT + NC-PAYMENT-DAYS
           COMPUTE WS-DUE-DATE = FUNCTION DATE-OF-INTEGER (WS-DUE-INT)
           MOVE WS-DUE-DATE TO WS-FMT-IN
           PERFORM 8910-FORMAT-DATE
           MOVE WS-FMT-OUT TO WS-DUE-TXT
           DISPLAY 'NADBILL  - PERIOD ' WS-RUN-PERIOD
                   ', DATUM ' WS-RUN-DATE-TXT
                   ', VALUTA ' WS-DUE-TXT
           OPEN INPUT  CUST-IN CTL-IN
                OUTPUT CUST-OUT CTL-OUT NEW-INV INV-PRINT BIL-REG
           IF NOT FS-CIN-OK  OR NOT FS-CTLI-OK OR NOT FS-COUT-OK
              OR NOT FS-CTLO-OK OR NOT FS-INV-OK OR NOT FS-PRT-OK
              OR NOT FS-REG-OK
               DISPLAY 'NADBILL  - GRESKA OTVARANJA DATOTEKA'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           READ CTL-IN
               AT END
                   DISPLAY 'NADBILL  - KONTROLNI SLOG NE POSTOJI'
                   MOVE 12 TO RETURN-CODE
                   STOP RUN
           END-READ
           MOVE CTL-LAST-INV-NO TO WS-LAST-INV-NO
           PERFORM 6000-PRINT-REG-HEADERS
           PERFORM 8100-READ-CUST.

      *----------------------------------------------------------------*
      * JEDAN UGOVOR
      *----------------------------------------------------------------*
       3000-PROCESS-CONTRACT.
           MOVE ZERO TO WS-FEE WS-EXTRA WS-QTY-MONTHS
           MOVE 'N'  TO WS-PRORATED-SW
           IF CM-LAST-BILLED >= WS-RUN-PERIOD
      *        VEĆ FAKTURISANO ZA OVAJ PERIOD - NE NAPLAĆUJE SE PONOVO
               ADD 1 TO WS-CNT-SKIP
           ELSE
               IF CM-ACTIVE
                   PERFORM 3100-CALC-FEE
               END-IF
               MOVE CM-PENDING-EXTRA TO WS-EXTRA
               IF WS-FEE > ZERO OR WS-EXTRA > ZERO
                   PERFORM 4000-CREATE-INVOICE
                   MOVE WS-RUN-PERIOD TO CM-LAST-BILLED
                   MOVE ZERO          TO CM-PENDING-EXTRA
                                         CM-PENDING-CNT
               ELSE
                   ADD 1 TO WS-CNT-NOTDUE
               END-IF
           END-IF
           WRITE CUST-OUT-REC FROM CUST-RECORD
           PERFORM 8100-READ-CUST.

      *----------------------------------------------------------------*
      * NAKNADA: DOSPIJEVA KAD JE (MJESECI OD POČETKA) DJELJIVO SA
      * DUŽINOM CIKLUSA. PRVI MJESEC SE NAPLAĆUJE SRAZMJERNO DANIMA.
      *----------------------------------------------------------------*
       3100-CALC-FEE.
           MOVE 'N' TO WS-DUE-SW
      *    UGOVOR KOJI POČINJE POSLIJE DATUMA OBRADE JOŠ NE DOSPIJEVA
           IF CM-START-DATE NOT > WS-RUN-DATE
               MOVE CM-START-DATE TO WS-DT
               COMPUTE WS-START-MONTHS = WS-DT-YYYY * 12 + WS-DT-MM
               COMPUTE WS-MONTH-DIFF = WS-RUN-MONTHS - WS-START-MONTHS
               EVALUATE TRUE
                   WHEN CM-QUARTERLY  MOVE 3  TO WS-CYCLE-M
                   WHEN CM-YEARLY     MOVE 12 TO WS-CYCLE-M
                   WHEN OTHER         MOVE 1  TO WS-CYCLE-M
               END-EVALUATE
               IF FUNCTION MOD (WS-MONTH-DIFF, WS-CYCLE-M) = ZERO
                   SET FEE-DUE TO TRUE
               END-IF
           END-IF
           IF FEE-DUE
               IF WS-MONTH-DIFF = ZERO
                   SET FEE-PRORATED TO TRUE
                   COMPUTE WS-DAYS-USED = WS-DIM - WS-DT-DD + 1
                   COMPUTE WS-FEE ROUNDED =
                           CM-MONTHLY-FEE * (WS-CYCLE-M - 1)
                         + CM-MONTHLY-FEE * WS-DAYS-USED / WS-DIM
                   COMPUTE WS-QTY-MONTHS ROUNDED =
                           WS-CYCLE-M - 1 + WS-DAYS-USED / WS-DIM
               ELSE
                   COMPUTE WS-FEE = CM-MONTHLY-FEE * WS-CYCLE-M
                   MOVE WS-CYCLE-M TO WS-QTY-MONTHS
               END-IF
               PERFORM 3200-BUILD-FEE-DESC
           END-IF.

      *    OPIS STAVKE: "ODRZAVANJE VIDEO NADZORA 10.2000" ILI
      *    "ODRZAVANJE 10.2000 - 12.2000", UZ NAPOMENU "(OD 15.10.)"
      *    KAD JE PRVI MJESEC OBRAČUNAT SRAZMJERNO
       3200-BUILD-FEE-DESC.
           MOVE SPACES TO WS-FEE-DESC
           MOVE 1 TO WS-PTR
           MOVE WS-RUN-MM TO WS-MM-ED
           IF WS-CYCLE-M = 1
               STRING 'ODRZAVANJE VIDEO NADZORA ' WS-MM-ED '.'
                      WS-RUN-YYYY
                      DELIMITED BY SIZE INTO WS-FEE-DESC
                      WITH POINTER WS-PTR
               END-STRING
           ELSE
               COMPUTE WS-END-MONTHS = WS-RUN-MONTHS + WS-CYCLE-M - 1
               COMPUTE WS-END-YYYY = (WS-END-MONTHS - 1) / 12
               COMPUTE WS-END-MM = WS-END-MONTHS - WS-END-YYYY * 12
               MOVE WS-END-MM TO WS-MM2-ED
               STRING 'ODRZAVANJE ' WS-MM-ED '.' WS-RUN-YYYY ' - '
                      WS-MM2-ED '.' WS-END-YYYY
                      DELIMITED BY SIZE INTO WS-FEE-DESC
                      WITH POINTER WS-PTR
               END-STRING
           END-IF
           IF FEE-PRORATED
               MOVE WS-DT-DD  TO WS-DD-ED
               MOVE WS-DT-MM  TO WS-MM-ED
               STRING ' (OD ' WS-DD-ED '.' WS-MM-ED '.)'
                      DELIMITED BY SIZE INTO WS-FEE-DESC
                      WITH POINTER WS-PTR
               END-STRING
           END-IF.

      *----------------------------------------------------------------*
      * FAKTURA: BROJ, PDV, OTVORENA STAVKA, ŠTAMPA, REGISTAR
      *----------------------------------------------------------------*
       4000-CREATE-INVOICE.
           ADD 1 TO WS-LAST-INV-NO
           ADD 1 TO WS-CNT-INV
           COMPUTE WS-NET = WS-FEE + WS-EXTRA
           COMPUTE WS-VAT ROUNDED = WS-NET * NC-VAT-RATE
           COMPUTE WS-GROSS = WS-NET + WS-VAT
           ADD WS-FEE   TO WS-TOT-FEE
           ADD WS-EXTRA TO WS-TOT-EXTRA
           ADD WS-NET   TO WS-TOT-NET
           ADD WS-VAT   TO WS-TOT-VAT
           ADD WS-GROSS TO WS-TOT-GROSS
      *    NOVA OTVORENA STAVKA ZA NAPLATU
           MOVE SPACES          TO OPEN-RECORD
           MOVE WS-LAST-INV-NO  TO OI-INV-NO
           MOVE CM-CONTRACT-NO  TO OI-CONTRACT-NO
           MOVE CM-CUST-NAME    TO OI-CUST-NAME
           MOVE WS-RUN-DATE     TO OI-INV-DATE
           MOVE WS-DUE-DATE     TO OI-DUE-DATE
           MOVE WS-GROSS        TO OI-AMOUNT
           MOVE ZERO            TO OI-PAID OI-DUN-LEVEL
                                   OI-LAST-DUN-DATE
           WRITE OPEN-RECORD
           PERFORM 5000-PRINT-INVOICE
           PERFORM 6100-PRINT-REG-LINE.

      *----------------------------------------------------------------*
      * ŠTAMPA FAKTURE - SVAKA NA NOVOJ STRANI
      *----------------------------------------------------------------*
       5000-PRINT-INVOICE.
           MOVE NC-COMPANY-1    TO IH-LEFT
           MOVE 'RACUN BR. '    TO IH-LABEL
           MOVE WS-LAST-INV-NO  TO IH-VALUE
           WRITE INV-LINE FROM WS-INV-HDR AFTER ADVANCING PAGE
           MOVE NC-COMPANY-2    TO IH-LEFT
           MOVE 'DATUM:    '    TO IH-LABEL
           MOVE WS-RUN-DATE-TXT TO IH-VALUE
           WRITE INV-LINE FROM WS-INV-HDR AFTER ADVANCING 1 LINE
           MOVE NC-COMPANY-3    TO IH-LEFT
           MOVE 'VALUTA:   '    TO IH-LABEL
           MOVE WS-DUE-TXT      TO IH-VALUE
           WRITE INV-LINE FROM WS-INV-HDR AFTER ADVANCING 1 LINE
      *    PODACI O KUPCU
           MOVE 'KUPAC:   '     TO IC-LABEL
           MOVE CM-CUST-NAME    TO IC-TEXT
           WRITE INV-LINE FROM WS-INV-CUST AFTER ADVANCING 2 LINES
           MOVE SPACES          TO IC-LABEL
           MOVE CM-STREET       TO IC-TEXT
           WRITE INV-LINE FROM WS-INV-CUST AFTER ADVANCING 1 LINE
           MOVE CM-CITY         TO IC-TEXT
           WRITE INV-LINE FROM WS-INV-CUST AFTER ADVANCING 1 LINE
           MOVE 'UGOVOR:  '     TO IC-LABEL
           MOVE SPACES          TO IC-TEXT
           STRING CM-CONTRACT-NO '    LOKACIJA: ' CM-SITE-DESC
                  DELIMITED BY SIZE INTO IC-TEXT
           END-STRING
           WRITE INV-LINE FROM WS-INV-CUST AFTER ADVANCING 2 LINES
      *    STAVKE
           WRITE INV-LINE FROM WS-INV-DASH AFTER ADVANCING 1 LINE
           WRITE INV-LINE FROM WS-INV-ITEM-HDR AFTER ADVANCING 1 LINE
           WRITE INV-LINE FROM WS-INV-DASH AFTER ADVANCING 1 LINE
           IF WS-FEE > ZERO
               MOVE WS-FEE-DESC    TO II-DESC
               MOVE WS-QTY-MONTHS  TO II-QTY
               MOVE CM-MONTHLY-FEE TO II-PRICE
               MOVE WS-FEE         TO II-AMOUNT
               WRITE INV-LINE FROM WS-INV-ITEM AFTER ADVANCING 1 LINE
           END-IF
           IF WS-EXTRA > ZERO
               MOVE CM-PENDING-CNT TO WS-CNT-ED
               MOVE SPACES TO WS-EXTRA-DESC
               STRING 'SERVISNE INTERVENCIJE (BROJ: '
                      FUNCTION TRIM (WS-CNT-ED) ')'
                      DELIMITED BY SIZE INTO WS-EXTRA-DESC
               END-STRING
               MOVE WS-EXTRA-DESC  TO II-DESC
               MOVE ZERO           TO II-QTY II-PRICE
               MOVE WS-EXTRA       TO II-AMOUNT
               WRITE INV-LINE FROM WS-INV-ITEM AFTER ADVANCING 1 LINE
           END-IF
           WRITE INV-LINE FROM WS-INV-DASH AFTER ADVANCING 1 LINE
           MOVE 'OSNOVICA'      TO IT-LABEL
           MOVE WS-NET          TO IT-AMOUNT
           WRITE INV-LINE FROM WS-INV-TOTAL AFTER ADVANCING 1 LINE
           MOVE NC-VAT-TXT      TO IT-LABEL
           MOVE WS-VAT          TO IT-AMOUNT
           WRITE INV-LINE FROM WS-INV-TOTAL AFTER ADVANCING 1 LINE
           MOVE 'UKUPNO ZA UPLATU (KM)' TO IT-LABEL
           MOVE WS-GROSS        TO IT-AMOUNT
           WRITE INV-LINE FROM WS-INV-TOTAL AFTER ADVANCING 1 LINE
           MOVE SPACES TO INV-LINE
           STRING ' PRILIKOM UPLATE NAVEDITE BROJ RACUNA '
                  'U POZIVU NA BROJ.'
                  DELIMITED BY SIZE INTO INV-LINE
           END-STRING
           WRITE INV-LINE AFTER ADVANCING 2 LINES.

      *----------------------------------------------------------------*
      * KNJIGA IZLAZNIH FAKTURA
      *----------------------------------------------------------------*
       6000-PRINT-REG-HEADERS.
           MOVE NC-COMPANY-1 TO RH1-COMPANY
           MOVE WS-RUN-DATE-TXT TO RH2-DATE
           MOVE WS-RUN-MM TO WS-MM-ED
           STRING WS-MM-ED '.' WS-RUN-YYYY
                  DELIMITED BY SIZE INTO RH2-PERIOD
           END-STRING
           WRITE REG-LINE FROM WS-REG-HDR1 AFTER ADVANCING PAGE
           WRITE REG-LINE FROM WS-REG-HDR2 AFTER ADVANCING 1 LINE
           WRITE REG-LINE FROM WS-REG-DASH AFTER ADVANCING 1 LINE
           WRITE REG-LINE FROM WS-REG-HDR3 AFTER ADVANCING 1 LINE
           WRITE REG-LINE FROM WS-REG-DASH AFTER ADVANCING 1 LINE.

       6100-PRINT-REG-LINE.
           MOVE WS-LAST-INV-NO  TO RD-INV-NO
           MOVE CM-CONTRACT-NO  TO RD-CONTRACT
           MOVE CM-CUST-NAME    TO RD-NAME
           MOVE CM-CYCLE        TO RD-CYCLE
           MOVE WS-FEE          TO RD-FEE
           MOVE WS-EXTRA        TO RD-EXTRA
           MOVE WS-NET          TO RD-NET
           MOVE WS-VAT          TO RD-VAT
           MOVE WS-GROSS        TO RD-GROSS
           MOVE WS-DUE-TXT      TO RD-DUE
           WRITE REG-LINE FROM WS-REG-DETAIL AFTER ADVANCING 1 LINE.

       6900-PRINT-REG-TOTALS.
           WRITE REG-LINE FROM WS-REG-DASH AFTER ADVANCING 1 LINE
           MOVE SPACES          TO RD-INV-NO RD-CONTRACT RD-CYCLE
                                   RD-DUE
           MOVE 'UKUPNO'        TO RD-NAME
           MOVE WS-TOT-FEE      TO RD-FEE
           MOVE WS-TOT-EXTRA    TO RD-EXTRA
           MOVE WS-TOT-NET      TO RD-NET
           MOVE WS-TOT-VAT      TO RD-VAT
           MOVE WS-TOT-GROSS    TO RD-GROSS
           WRITE REG-LINE FROM WS-REG-DETAIL AFTER ADVANCING 1 LINE
           WRITE REG-LINE FROM WS-REG-DASH AFTER ADVANCING 1 LINE
           MOVE 'UGOVORA OBRADJENO .....................:'
             TO RC-LABEL
           MOVE WS-CNT-READ TO RC-COUNT
           WRITE REG-LINE FROM WS-REG-COUNT AFTER ADVANCING 1 LINE
           MOVE 'IZDATO FAKTURA ........................:'
             TO RC-LABEL
           MOVE WS-CNT-INV TO RC-COUNT
           WRITE REG-LINE FROM WS-REG-COUNT AFTER ADVANCING 1 LINE
           MOVE 'BEZ DOSPJELE NAKNADE U PERIODU ........:'
             TO RC-LABEL
           MOVE WS-CNT-NOTDUE TO RC-COUNT
           WRITE REG-LINE FROM WS-REG-COUNT AFTER ADVANCING 1 LINE
           MOVE 'VEC FAKTURISANO ZA PERIOD (PRESKOCENO):'
             TO RC-LABEL
           MOVE WS-CNT-SKIP TO RC-COUNT
           WRITE REG-LINE FROM WS-REG-COUNT AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO REG-LINE
           WRITE REG-LINE AFTER ADVANCING 2 LINES.

       8100-READ-CUST.
           READ CUST-IN
               AT END
                   SET CUST-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CNT-READ
           END-READ.

           COPY GETDATE.

      *----------------------------------------------------------------*
      * KRAJ: NOVI KONTROLNI SLOG, TOTALI, ZATVARANJE
      *----------------------------------------------------------------*
       9000-TERMINATE.
           PERFORM 6900-PRINT-REG-TOTALS
           MOVE WS-LAST-INV-NO TO CTL-LAST-INV-NO
           MOVE WS-RUN-DATE    TO CTL-LAST-RUN-DATE
           MOVE WS-RUN-PERIOD  TO CTL-LAST-PERIOD
           WRITE CTL-OUT-REC FROM CTL-RECORD
           CLOSE CUST-IN CUST-OUT CTL-IN CTL-OUT
                 NEW-INV INV-PRINT BIL-REG
           MOVE WS-CNT-READ   TO WS-DISP-CNT
           DISPLAY 'NADBILL  - UGOVORA OBRADJENO    : ' WS-DISP-CNT
           MOVE WS-CNT-INV    TO WS-DISP-CNT
           DISPLAY 'NADBILL  - IZDATO FAKTURA       : ' WS-DISP-CNT
           MOVE WS-CNT-SKIP   TO WS-DISP-CNT
           DISPLAY 'NADBILL  - VEC FAKTURISANO      : ' WS-DISP-CNT
           DISPLAY 'NADBILL  - ZADNJI BROJ FAKTURE  : ' WS-LAST-INV-NO
           MOVE ZERO TO RETURN-CODE
           DISPLAY 'NADBILL  - KRAJ, RC= 0'.

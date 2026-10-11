      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NADPAY.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - SERVIS VIDEO NADZORA.
       DATE-WRITTEN.  2000-04-10.
      *================================================================*
      * SISTEM  : NADZOR - UGOVORI O ODRŽAVANJU I NAPLATA
      * PROGRAM : NADPAY - KORAK 030 MJESEČNE OBRADE
      * OPIS    : RASKNJIŽAVANJE UPLATA.
      *           1. MERGE: STARE OTVORENE STAVKE + NOVE FAKTURE IZ
      *              NADBILL (OBJE SORTIRANE PO BROJU FAKTURE).
      *           2. SORT UPLATA IZ IZVODA PO BROJU FAKTURE.
      *           3. BALANCED LINE PO BROJU FAKTURE: UPLATE SE
      *              PRIMJENJUJU NA STAVKE; POTPUNO PLAĆENE STAVKE SE
      *              ZATVARAJU, OSTALE IDU U NOVU DATOTEKU OTVORENIH.
      *           UPLATE ZA NEPOSTOJEĆE FAKTURE I PREPLATE SE
      *           POSEBNO ISPISUJU ZA RUČNU OBRADU U KNJIGOVODSTVU.
      * ULAZ    : OPENOLD  - OTVORENE STAVKE PRETHODNOG MJESECA
      *           NEWINV   - NOVE FAKTURE (IZ NADBILL)
      *           PAYIN    - UPLATE (PAYREC), NESORTIRANO
      * IZLAZ   : OPENMRG  - SPOJENE STAVKE (RADNA DATOTEKA)
      *           PAYSRT   - SORTIRANE UPLATE (RADNA DATOTEKA)
      *           OPENPAY  - OTVORENE STAVKE POSLIJE UPLATA
      *           PAYRPT   - IZVJEŠTAJ O RASKNJIŽAVANJU, 132 KOLONE
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - SVE UPLATE RASPOREĐENE
      *            4 - POSTOJE NERASPOREĐENE UPLATE ILI PREPLATE
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-05-09 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPEN-OLD     ASSIGN TO OPENOLD
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NEW-INV      ASSIGN TO NEWINV
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MERGE-WORK   ASSIGN TO MERGEWK.
           SELECT OPEN-MRG     ASSIGN TO OPENMRG
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-MRG.
           SELECT PAY-IN       ASSIGN TO PAYIN
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-PAY     ASSIGN TO SORTWK.
           SELECT PAY-SRT      ASSIGN TO PAYSRT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-PSRT.
           SELECT OPEN-OUT     ASSIGN TO OPENPAY
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-OUT.
           SELECT PAY-RPT      ASSIGN TO PAYRPT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-RPT.

       DATA DIVISION.
       FILE SECTION.
       FD  OPEN-OLD.
       01  OPEN-OLD-REC                PIC X(90).
       FD  NEW-INV.
       01  NEW-INV-REC                 PIC X(90).
       SD  MERGE-WORK.
       01  MW-RECORD.
           05  MW-INV-NO               PIC 9(08).
           05  FILLER                  PIC X(82).
       FD  OPEN-MRG.
           COPY OPENREC.
       FD  PAY-IN.
       01  PAY-IN-REC                  PIC X(50).
       SD  SORT-PAY.
       01  SP-RECORD.
           05  SP-INV-NO               PIC 9(08).
           05  SP-DATE                 PIC 9(08).
           05  FILLER                  PIC X(34).
       FD  PAY-SRT.
           COPY PAYREC.
       FD  OPEN-OUT.
       01  OPEN-OUT-REC                PIC X(90).
       FD  PAY-RPT.
       01  RPT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-MRG               PIC X(02).
               88  FS-MRG-OK           VALUE '00'.
           05  WS-FS-PSRT              PIC X(02).
               88  FS-PSRT-OK          VALUE '00'.
           05  WS-FS-OUT               PIC X(02).
               88  FS-OUT-OK           VALUE '00'.
           05  WS-FS-RPT               PIC X(02).
               88  FS-RPT-OK           VALUE '00'.

           COPY NADCONST.
           COPY RUNDATE.

      *    STAVKA KOJA SE TRENUTNO OBRAĐUJE (KOPIJA SA PREFIKSOM HI-)
           COPY OPENREC REPLACING ==OPEN-RECORD== BY ==HI-RECORD==
                                  LEADING ==OI-== BY ==HI-==.

       01  WS-KEYS.
           05  WS-OI-KEY               PIC X(08).
           05  WS-PY-KEY               PIC X(08).
           05  WS-ACTIVE-KEY           PIC X(08).

       01  WS-SWITCHES.
           05  WS-ITEM-SW              PIC X(01).
               88  ITEM-EXISTS         VALUE 'Y'.
               88  ITEM-MISSING        VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-REMAINING            PIC S9(07)V99.
           05  WS-OVERPAY              PIC 9(07)V99.
           05  WS-STATUS               PIC X(20).
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-TOTALS.
           05  WS-CNT-OI-IN            PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-OI-OUT           PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-PAY              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-CLOSED           PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-UNMATCHED        PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-TOT-PAY              PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-UNMATCHED        PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-OVERPAY          PIC 9(09)V99 VALUE ZERO.
           05  WS-TOT-OPEN             PIC 9(09)V99 VALUE ZERO.

       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

      *----------------------------------------------------------------*
      * IZVJEŠTAJ - 132 KOLONE
      *----------------------------------------------------------------*
       01  WS-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  HDR1-COMPANY            PIC X(40).
           05  FILLER                  PIC X(91)   VALUE
                   'NADZOR - RASKNJIZAVANJE UPLATA'.
       01  WS-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)   VALUE
                   'PROGRAM: NADPAY'.
           05  FILLER                  PIC X(14)   VALUE
                   'DATUM OBRADE: '.
           05  HDR2-DATE               PIC X(10).
           05  FILLER                  PIC X(67)   VALUE SPACES.
       01  WS-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'FAKTURA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'UGOVOR'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(30)  VALUE 'KUPAC'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'DAT.UPLATE'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '    UPLATA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '   FAKTURA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '  UPLACENO'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '   OSTATAK'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(20)  VALUE 'STATUS'.
           05  FILLER  PIC X(06)  VALUE SPACES.
       01  WS-HDR4.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(125)  VALUE ALL '-'.
           05  FILLER                  PIC X(06)   VALUE SPACES.
       01  WS-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-INV-NO               PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-CONTRACT             PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-NAME                 PIC X(30).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-PAY-DATE             PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-PAY-AMT              PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-INV-AMT              PIC ZZZ,ZZ9.99 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-PAID                 PIC ZZZ,ZZ9.99 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-REMAIN               PIC ZZZ,ZZ9.99 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-STATUS               PIC X(20).
           05  FILLER                  PIC X(06)   VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  TL-LABEL                PIC X(40).
           05  TL-COUNT                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(04)   VALUE SPACES.
           05  TL-AMOUNT               PIC ZZ,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(68)   VALUE SPACES.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-KEY
               UNTIL WS-OI-KEY = HIGH-VALUES
                 AND WS-PY-KEY = HIGH-VALUES
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * MERGE STAVKI, SORT UPLATA, OTVARANJE
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'NADPAY   - START RASKNJIZAVANJA UPLATA'
           PERFORM 8900-GET-RUN-DATE
           MOVE NC-COMPANY-1    TO HDR1-COMPANY
           MOVE WS-RUN-DATE-TXT TO HDR2-DATE
      *    STARE STAVKE I NOVE FAKTURE SU VEĆ SORTIRANE - DOVOLJAN MERGE
           MERGE MERGE-WORK
               ON ASCENDING KEY MW-INV-NO
               USING  OPEN-OLD NEW-INV
               GIVING OPEN-MRG
           SORT SORT-PAY
               ON ASCENDING KEY SP-INV-NO
               ON ASCENDING KEY SP-DATE
               USING  PAY-IN
               GIVING PAY-SRT
           IF SORT-RETURN NOT = ZERO
               DISPLAY 'NADPAY   - GRESKA U SORTU/MERGE, RC '
                       SORT-RETURN
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT  OPEN-MRG PAY-SRT
                OUTPUT OPEN-OUT PAY-RPT
           IF NOT FS-MRG-OK OR NOT FS-PSRT-OK
              OR NOT FS-OUT-OK OR NOT FS-RPT-OK
               DISPLAY 'NADPAY   - GRESKA OTVARANJA DATOTEKA'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-HDR2 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR3 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           PERFORM 8100-READ-ITEM
           PERFORM 8200-READ-PAYMENT.

      *----------------------------------------------------------------*
      * JEDAN BROJ FAKTURE - BALANCED LINE
      *----------------------------------------------------------------*
       2000-PROCESS-KEY.
           IF WS-OI-KEY < WS-PY-KEY
               MOVE WS-OI-KEY TO WS-ACTIVE-KEY
           ELSE
               MOVE WS-PY-KEY TO WS-ACTIVE-KEY
           END-IF
           IF WS-OI-KEY = WS-ACTIVE-KEY
               MOVE OPEN-RECORD TO HI-RECORD
               SET ITEM-EXISTS TO TRUE
               PERFORM 8100-READ-ITEM
           ELSE
               SET ITEM-MISSING TO TRUE
           END-IF
           PERFORM 3000-APPLY-PAYMENT
               UNTIL WS-PY-KEY NOT = WS-ACTIVE-KEY
           IF ITEM-EXISTS
               PERFORM 4000-CLOSE-OR-KEEP
           END-IF.

      *----------------------------------------------------------------*
      * PRIMJENA JEDNE UPLATE
      *----------------------------------------------------------------*
       3000-APPLY-PAYMENT.
           ADD 1         TO WS-CNT-PAY
           ADD PY-AMOUNT TO WS-TOT-PAY
           MOVE PY-INV-NO TO DL-INV-NO
           MOVE PY-DATE   TO WS-FMT-IN
           PERFORM 8910-FORMAT-DATE
           MOVE WS-FMT-OUT TO DL-PAY-DATE
           MOVE PY-AMOUNT TO DL-PAY-AMT
           IF ITEM-MISSING
      *        UPLATA BEZ FAKTURE - ČEKA RUČNU PROVJERU
               ADD 1         TO WS-CNT-UNMATCHED
               ADD PY-AMOUNT TO WS-TOT-UNMATCHED
               MOVE 4 TO WS-RC
               MOVE SPACES TO DL-CONTRACT
               MOVE PY-REF TO DL-NAME
               MOVE ZERO   TO DL-INV-AMT DL-PAID DL-REMAIN
               MOVE 'NEPOZNATA FAKTURA' TO DL-STATUS
           ELSE
               ADD PY-AMOUNT TO HI-PAID
               COMPUTE WS-REMAINING = HI-AMOUNT - HI-PAID
               MOVE HI-CONTRACT-NO TO DL-CONTRACT
               MOVE HI-CUST-NAME   TO DL-NAME
               MOVE HI-AMOUNT      TO DL-INV-AMT
               MOVE HI-PAID        TO DL-PAID
               EVALUATE TRUE
                   WHEN WS-REMAINING > ZERO
                       MOVE WS-REMAINING TO DL-REMAIN
                       MOVE 'DJELIMICNO PLACENO' TO DL-STATUS
                   WHEN WS-REMAINING = ZERO
                       MOVE ZERO TO DL-REMAIN
                       MOVE 'ZATVORENA' TO DL-STATUS
                   WHEN OTHER
                       MOVE ZERO TO DL-REMAIN
                       MOVE 'ZATVORENA, PREPLATA' TO DL-STATUS
               END-EVALUATE
           END-IF
           WRITE RPT-LINE FROM WS-DETAIL AFTER ADVANCING 1 LINE
           PERFORM 8200-READ-PAYMENT.

      *----------------------------------------------------------------*
      * POSLIJE SVIH UPLATA: ZATVORI PLAĆENU STAVKU ILI JE PREPIŠI
      *----------------------------------------------------------------*
       4000-CLOSE-OR-KEEP.
           IF HI-PAID >= HI-AMOUNT
               ADD 1 TO WS-CNT-CLOSED
               IF HI-PAID > HI-AMOUNT
                   COMPUTE WS-OVERPAY = HI-PAID - HI-AMOUNT
                   ADD WS-OVERPAY TO WS-TOT-OVERPAY
                   MOVE 4 TO WS-RC
               END-IF
           ELSE
               WRITE OPEN-OUT-REC FROM HI-RECORD
               ADD 1 TO WS-CNT-OI-OUT
               COMPUTE WS-TOT-OPEN = WS-TOT-OPEN + HI-AMOUNT - HI-PAID
           END-IF.

       8100-READ-ITEM.
           READ OPEN-MRG
               AT END
                   MOVE HIGH-VALUES TO WS-OI-KEY
               NOT AT END
                   ADD 1 TO WS-CNT-OI-IN
                   MOVE OI-INV-NO TO WS-OI-KEY
           END-READ.

       8200-READ-PAYMENT.
           READ PAY-SRT
               AT END
                   MOVE HIGH-VALUES TO WS-PY-KEY
               NOT AT END
                   MOVE PY-INV-NO TO WS-PY-KEY
           END-READ.

           COPY GETDATE.

      *----------------------------------------------------------------*
      * ZBIRNI PODACI
      *----------------------------------------------------------------*
       9000-TERMINATE.
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE SPACES TO WS-TOTAL-LINE
           MOVE 'OTVORENIH STAVKI NA ULAZU (SA NOVIM) ..:'
             TO TL-LABEL
           MOVE WS-CNT-OI-IN TO TL-COUNT
           MOVE ZERO TO TL-AMOUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'UPLATA UKUPNO .........................:'
             TO TL-LABEL
           MOVE WS-CNT-PAY TO TL-COUNT
           MOVE WS-TOT-PAY TO TL-AMOUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  NERASPOREDJENO (NEPOZNATA FAKTURA) ..:'
             TO TL-LABEL
           MOVE WS-CNT-UNMATCHED TO TL-COUNT
           MOVE WS-TOT-UNMATCHED TO TL-AMOUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  PREPLATE ZA POVRAT ILI KOMPENZACIJU .:'
             TO TL-LABEL
           MOVE ZERO TO TL-COUNT
           MOVE WS-TOT-OVERPAY TO TL-AMOUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'ZATVORENIH STAVKI .....................:'
             TO TL-LABEL
           MOVE WS-CNT-CLOSED TO TL-COUNT
           MOVE ZERO TO TL-AMOUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'OSTALO OTVORENIH STAVKI / SALDO .......:'
             TO TL-LABEL
           MOVE WS-CNT-OI-OUT TO TL-COUNT
           MOVE WS-TOT-OPEN   TO TL-AMOUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO RPT-LINE
           WRITE RPT-LINE AFTER ADVANCING 2 LINES
           CLOSE OPEN-MRG PAY-SRT OPEN-OUT PAY-RPT
           MOVE WS-CNT-PAY TO WS-DISP-CNT
           DISPLAY 'NADPAY   - UPLATA OBRADJENO     : ' WS-DISP-CNT
           MOVE WS-CNT-UNMATCHED TO WS-DISP-CNT
           DISPLAY 'NADPAY   - NERASPOREDJENIH      : ' WS-DISP-CNT
           MOVE WS-CNT-CLOSED TO WS-DISP-CNT
           DISPLAY 'NADPAY   - ZATVORENIH STAVKI    : ' WS-DISP-CNT
           MOVE WS-CNT-OI-OUT TO WS-DISP-CNT
           DISPLAY 'NADPAY   - OSTALO OTVORENIH     : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'NADPAY   - KRAJ, RC=' WS-DISP-RC.

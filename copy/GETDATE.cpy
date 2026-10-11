      *================================================================*
      * GETDATE  - PARAGRAFI ZA DATUM OBRADE I FORMATIRANJE DATUMA
      * SISTEM   : NADZOR - PRODUKTRON
      * PREDUSLOV: COPY RUNDATE U WORKING-STORAGE.
      *----------------------------------------------------------------*
      * DATUM OBRADE SE UZIMA IZ VARIJABLE NAD_RUN_DATE (GGGGMMDD),
      * A AKO NIJE ZADANA - SISTEMSKI DATUM. TAKO SE MJESEČNA OBRADA
      * MOŽE PONOVITI ZA BILO KOJI DAN (TESTOVI, NAKNADNI OBRAČUN).
      *================================================================*
       8900-GET-RUN-DATE.
           ACCEPT WS-ENV-DATE FROM ENVIRONMENT 'NAD_RUN_DATE'
           IF WS-ENV-DATE IS NUMERIC
               MOVE WS-ENV-DATE TO WS-RUN-DATE
           ELSE
               MOVE FUNCTION CURRENT-DATE (1:8) TO WS-RUN-DATE
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD (WS-RUN-DATE) NOT = ZERO
               DISPLAY '*** NEISPRAVAN DATUM OBRADE: ' WS-RUN-DATE
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           COMPUTE WS-RUN-PERIOD = WS-RUN-YYYY * 100 + WS-RUN-MM
           COMPUTE WS-RUN-INT = FUNCTION INTEGER-OF-DATE (WS-RUN-DATE)
           MOVE WS-RUN-DATE TO WS-FMT-IN
           PERFORM 8910-FORMAT-DATE
           MOVE WS-FMT-OUT TO WS-RUN-DATE-TXT.

      *    GGGGMMDD -> GGGG-MM-DD, NULA OSTAJE PRAZNA
       8910-FORMAT-DATE.
           IF WS-FMT-IN = ZERO
               MOVE SPACES TO WS-FMT-OUT
           ELSE
               STRING WS-FMT-YYYY '-' WS-FMT-MM '-' WS-FMT-DD
                      DELIMITED BY SIZE INTO WS-FMT-OUT
               END-STRING
           END-IF.

      *================================================================*
      * CTLREC   - KONTROLNI SLOG SISTEMA (JEDAN SLOG)
      * SISTEM   : NADZOR - PRODUKTRON
      * DUŽINA   : 30 BAJTOVA
      *----------------------------------------------------------------*
      * ČUVA ZADNJI DODIJELJENI BROJ FAKTURE, DA BROJEVI BUDU
      * NEPREKINUTI IZ MJESECA U MJESEC.
      *================================================================*
       01  CTL-RECORD.
           05  CTL-LAST-INV-NO         PIC 9(08).
           05  CTL-LAST-RUN-DATE       PIC 9(08).
           05  CTL-LAST-PERIOD         PIC 9(06).
           05  FILLER                  PIC X(08).

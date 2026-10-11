      *================================================================*
      * PAYREC   - SLOG UPLATE IZ IZVODA BANKE
      * SISTEM   : NADZOR - PRODUKTRON
      * DUŽINA   : 50 BAJTOVA
      *----------------------------------------------------------------*
      * KUPAC U POZIVU NA BROJ NAVODI BROJ FAKTURE.
      *================================================================*
       01  PAY-RECORD.
           05  PY-INV-NO               PIC 9(08).
           05  PY-DATE                 PIC 9(08).
           05  PY-AMOUNT               PIC 9(07)V99.
           05  PY-REF                  PIC X(20).
           05  FILLER                  PIC X(05).

      *================================================================*
      * OPENREC  - SLOG OTVORENE STAVKE (NEPLAĆENA FAKTURA)
      * SISTEM   : NADZOR - PRODUKTRON
      * DUŽINA   : 90 BAJTOVA, SORTIRANO PO OI-INV-NO
      *----------------------------------------------------------------*
      * STAVKA OSTAJE U DATOTECI DOK UPLATE NE POKRIJU IZNOS.
      * OI-DUN-LEVEL: 0 = BEZ OPOMENE, 1..3 = NIVO POSLANE OPOMENE
      *================================================================*
       01  OPEN-RECORD.
           05  OI-INV-NO               PIC 9(08).
           05  OI-CONTRACT-NO          PIC X(08).
           05  OI-CUST-NAME            PIC X(30).
           05  OI-INV-DATE             PIC 9(08).
           05  OI-DUE-DATE             PIC 9(08).
      *    IZNOS FAKTURE SA PDV-OM I DO SADA UPLAĆENO
           05  OI-AMOUNT               PIC 9(07)V99.
           05  OI-PAID                 PIC 9(07)V99.
           05  OI-DUN-LEVEL            PIC 9(01).
           05  OI-LAST-DUN-DATE        PIC 9(08).
           05  FILLER                  PIC X(01).

      *================================================================*
      * TRANREC  - SLOG TRANSAKCIJE ZA AŽURIRANJE MATIČNE DATOTEKE
      * SISTEM   : NADZOR - PRODUKTRON
      * DUŽINA   : 150 BAJTOVA
      *----------------------------------------------------------------*
      * ZAJEDNIČKO ZAGLAVLJE + PODRUČJE PODATAKA KOJE SE REDEFINIŠE
      * ZAVISNO OD TIPA TRANSAKCIJE:
      *   A = NOVI UGOVOR         P = PROMJENA CIJENE
      *   X = OTKAZ UGOVORA       S = SERVISNA INTERVENCIJA
      *================================================================*
       01  TRAN-RECORD.
           05  TR-CONTRACT-NO          PIC X(08).
           05  TR-TYPE                 PIC X(01).
               88  TR-ADD              VALUE 'A'.
               88  TR-PRICE            VALUE 'P'.
               88  TR-CANCEL           VALUE 'X'.
               88  TR-SERVICE          VALUE 'S'.
      *    DATUM DOGAĐAJA (POČETAK, OTKAZ, DAN INTERVENCIJE)
           05  TR-DATE                 PIC 9(08).
      *    REDNI BROJ UNOSA - ZA STABILAN REDOSLIJED PRI SORTIRANJU
           05  TR-SEQ                  PIC 9(04).
           05  TR-DATA                 PIC X(129).
      *    A - NOVI UGOVOR
           05  TR-ADD-DATA REDEFINES TR-DATA.
               10  TA-CUST-NAME        PIC X(30).
               10  TA-STREET           PIC X(30).
               10  TA-CITY             PIC X(20).
               10  TA-SITE-DESC        PIC X(30).
               10  TA-CAMERAS          PIC 9(03).
               10  TA-MONTHLY-FEE      PIC 9(05)V99.
               10  TA-CYCLE            PIC X(01).
               10  FILLER              PIC X(08).
      *    P - PROMJENA MJESEČNE NAKNADE
           05  TR-PRICE-DATA REDEFINES TR-DATA.
               10  TP-NEW-FEE          PIC 9(05)V99.
               10  TP-REASON           PIC X(30).
               10  FILLER              PIC X(92).
      *    X - OTKAZ
           05  TR-CANCEL-DATA REDEFINES TR-DATA.
               10  TX-REASON           PIC X(30).
               10  FILLER              PIC X(99).
      *    S - SERVISNA INTERVENCIJA (SATI x SATNICA + MATERIJAL)
           05  TR-SERVICE-DATA REDEFINES TR-DATA.
               10  TS-TECHNICIAN       PIC X(10).
               10  TS-HOURS            PIC 9(02)V9.
               10  TS-MATERIAL         PIC 9(05)V99.
               10  TS-DESC             PIC X(30).
               10  FILLER              PIC X(79).

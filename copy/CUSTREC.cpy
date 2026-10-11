      *================================================================*
      * CUSTREC  - MATIČNI SLOG UGOVORA O ODRŽAVANJU VIDEO NADZORA
      * SISTEM   : NADZOR - PRODUKTRON
      * DUŽINA   : 200 BAJTOVA, SORTIRANO PO CM-CONTRACT-NO
      *----------------------------------------------------------------*
      * JEDAN SLOG = JEDAN UGOVOR ZA JEDNU LOKACIJU KUPCA.
      * CIKLUS NAPLATE: M = MJESEČNO, Q = KVARTALNO, Y = GODIŠNJE.
      * KVARTALNI I GODIŠNJI UGOVORI SE NAPLAĆUJU UNAPRIJED, U RITMU
      * OD MJESECA POČETKA UGOVORA.
      *================================================================*
       01  CUST-RECORD.
           05  CM-CONTRACT-NO          PIC X(08).
           05  CM-CUST-NAME            PIC X(30).
           05  CM-STREET               PIC X(30).
           05  CM-CITY                 PIC X(20).
      *    OPIS LOKACIJE I OPREME (NPR. SKLADIŠTE - 8 KAMERA + DVR)
           05  CM-SITE-DESC            PIC X(30).
           05  CM-CAMERAS              PIC 9(03).
      *    MJESEČNA NAKNADA ZA ODRŽAVANJE, BEZ PDV-A
           05  CM-MONTHLY-FEE          PIC 9(05)V99.
           05  CM-CYCLE                PIC X(01).
               88  CM-MONTHLY          VALUE 'M'.
               88  CM-QUARTERLY        VALUE 'Q'.
               88  CM-YEARLY           VALUE 'Y'.
               88  CM-CYCLE-VALID      VALUE 'M' 'Q' 'Y'.
           05  CM-START-DATE           PIC 9(08).
      *    DATUM OTKAZA, NULA ZA AKTIVNE UGOVORE
           05  CM-END-DATE             PIC 9(08).
           05  CM-STATUS               PIC X(01).
               88  CM-ACTIVE           VALUE 'A'.
               88  CM-CANCELLED        VALUE 'X'.
      *    SERVISNE INTERVENCIJE KOJE JOŠ NISU FAKTURISANE
           05  CM-PENDING-EXTRA        PIC 9(05)V99.
           05  CM-PENDING-CNT          PIC 9(03).
      *    ZADNJI FAKTURISANI PERIOD GGGGMM - ŠTITI OD DUPLE NAPLATE
           05  CM-LAST-BILLED          PIC 9(06).
           05  FILLER                  PIC X(38).

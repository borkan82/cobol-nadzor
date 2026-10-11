      *================================================================*
      * RUNDATE  - DATUM OBRADE I POMOĆNA POLJA ZA DATUME
      * SISTEM   : NADZOR - PRODUKTRON
      * KORISTI SE ZAJEDNO SA PARAGRAFIMA IZ GETDATE.
      *================================================================*
       01  WS-RUN-DATE-AREA.
           05  WS-ENV-DATE             PIC X(08)   VALUE SPACES.
           05  WS-RUN-DATE             PIC 9(08).
           05  WS-RUN-DATE-R REDEFINES WS-RUN-DATE.
               10  WS-RUN-YYYY         PIC 9(04).
               10  WS-RUN-MM           PIC 9(02).
               10  WS-RUN-DD           PIC 9(02).
      *    OBRAČUNSKI PERIOD GGGGMM
           05  WS-RUN-PERIOD           PIC 9(06).
           05  WS-RUN-DATE-TXT         PIC X(10).
      *    DATUM KAO REDNI BROJ DANA - ZA RAČUNANJE RAZLIKA
           05  WS-RUN-INT              PIC S9(09)  COMP.
      *    ULAZ/IZLAZ ZA FORMATIRANJE DATUMA GGGG-MM-DD
       01  WS-FMT-AREA.
           05  WS-FMT-IN               PIC 9(08).
           05  WS-FMT-IN-R REDEFINES WS-FMT-IN.
               10  WS-FMT-YYYY         PIC X(04).
               10  WS-FMT-MM           PIC X(02).
               10  WS-FMT-DD           PIC X(02).
           05  WS-FMT-OUT              PIC X(10).

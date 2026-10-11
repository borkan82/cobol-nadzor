      *================================================================*
      * NADCONST - POSLOVNE KONSTANTE SISTEMA NADZOR
      * SISTEM   : NADZOR - PRODUKTRON
      *----------------------------------------------------------------*
      * SVE STOPE I ROKOVI NA JEDNOM MJESTU - PROMJENA PDV-A ILI
      * SATNICE ZNAČI SAMO IZMJENU OVOG COPYBOOK-A I REKOMPILACIJU.
      *================================================================*
       01  NAD-CONSTANTS.
      *    STOPA PDV-A
           05  NC-VAT-RATE             PIC V99      VALUE .17.
           05  NC-VAT-TXT              PIC X(08)    VALUE 'PDV 17%'.
      *    SATNICA SERVISERA ZA INTERVENCIJE (KM)
           05  NC-HOURLY-RATE          PIC 9(03)V99 VALUE 35.00.
      *    ROK PLAĆANJA FAKTURE U DANIMA
           05  NC-PAYMENT-DAYS         PIC 9(03)    VALUE 15.
      *    DANI TOLERANCIJE PRIJE PRVE OPOMENE
           05  NC-GRACE-DAYS           PIC 9(03)    VALUE 5.
      *    GRANICE ZA NIVO OPOMENE (DANA KAŠNJENJA)
           05  NC-DUN2-DAYS            PIC 9(03)    VALUE 30.
           05  NC-DUN3-DAYS            PIC 9(03)    VALUE 60.
      *    ZAGLAVLJE NA FAKTURAMA I OPOMENAMA
           05  NC-COMPANY-1            PIC X(40)
               VALUE 'PRODUKTRON - SERVIS VIDEO NADZORA'.
           05  NC-COMPANY-2            PIC X(40)
               VALUE 'INDUSTRIJSKA ZONA BB'.
           05  NC-COMPANY-3            PIC X(40)
               VALUE 'ODRZAVANJE SISTEMA VIDEO NADZORA'.

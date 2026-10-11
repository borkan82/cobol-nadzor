#!/bin/sh
#======================================================================
# NADMJES - MJESEČNA OBRADA SISTEMA NADZOR (PRODUKTRON)
# AUTOR   : BORIS JANJETOVIC
#----------------------------------------------------------------------
# Simulacija JCL posla. Svaki korak pokreće jedan COBOL program, a
# datoteke se dodjeljuju preko DD_* varijabli (kao DD kartice).
# Matične datoteke se nikad ne mijenjaju na mjestu: svaki korak čita
# staru generaciju i piše novu (kao GDG na mainframe-u), pa se posao
# uvijek može ponoviti od početka.
#
#   STEP010  NADUPD   - transakcije mjeseca -> novi matični ugovora
#   STEP020  NADBILL  - obračun, fakture, nove otvorene stavke
#   STEP030  NADPAY   - merge stavki, rasknjižavanje uplata
#   STEP040  NADDUN   - starosna struktura duga i opomene
#
# Varijable okruženja (opciono):
#   NAD_DATA      - ulazni podaci (default: ./data)
#   NAD_WORK      - izlaz i radne datoteke (default: ./work)
#   NAD_CUSTMAST  - alternativni matični ugovora (za testove)
#   NAD_RUN_DATE  - datum obrade GGGGMMDD (default: današnji)
#======================================================================
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
BIN=$ROOT/bin
DATA=${NAD_DATA:-$ROOT/data}
WORK=${NAD_WORK:-$ROOT/work}
MAXCC=0

mkdir -p "$WORK"

log() {
    echo "NADMJES $(date '+%H:%M:%S') $*"
}

# step <ime> <program> <max dozvoljeni RC>
step() {
    _name=$1; _prog=$2; _cond=$3
    log "$_name  EXEC PGM=$_prog"
    "$BIN/$_prog"
    _rc=$?
    log "$_name  $_prog  ZAVRSEN, RC=$_rc"
    [ "$_rc" -gt "$MAXCC" ] && MAXCC=$_rc
    if [ "$_rc" -gt "$_cond" ]; then
        log "*** POSAO PREKINUT U KORAKU $_name (RC=$_rc > $_cond) ***"
        log "NADMJES ZAVRSEN, MAXCC=$MAXCC"
        exit "$_rc"
    fi
}

log "===== START POSLA NADMJES ====="
log "PODACI: $DATA"
log "IZLAZ : $WORK"

#---- STEP000: brisanje izlaza prethodne obrade (IEFBR14)
log "STEP000  EXEC PGM=IEFBR14"
rm -f "$WORK"/*.dat "$WORK"/*.txt

#---- STEP010: NADUPD
DD_TRANIN=$DATA/trans.dat
DD_TRANSRT=$WORK/TRANSRT.dat
DD_CUSTOLD=${NAD_CUSTMAST:-$DATA/custmast.dat}
DD_CUSTNEW=$WORK/CUSTNEW.dat
DD_UPDRPT=$WORK/UPDRPT.txt
export DD_TRANIN DD_TRANSRT DD_CUSTOLD DD_CUSTNEW DD_UPDRPT
step STEP010 NADUPD 4

#---- STEP020: NADBILL
DD_CUSTIN=$WORK/CUSTNEW.dat
DD_CUSTOUT=$WORK/CUSTBIL.dat
DD_CTLIN=$DATA/nadctl.dat
DD_CTLOUT=$WORK/NADCTL.dat
DD_NEWINV=$WORK/NEWINV.dat
DD_INVPRT=$WORK/INVOICES.txt
DD_BILREG=$WORK/BILREG.txt
export DD_CUSTIN DD_CUSTOUT DD_CTLIN DD_CTLOUT DD_NEWINV \
       DD_INVPRT DD_BILREG
step STEP020 NADBILL 4

#---- STEP030: NADPAY
DD_OPENOLD=$DATA/openitems.dat
DD_OPENMRG=$WORK/OPENMRG.dat
DD_PAYIN=$DATA/payments.dat
DD_PAYSRT=$WORK/PAYSRT.dat
DD_OPENPAY=$WORK/OPENPAY.dat
DD_PAYRPT=$WORK/PAYRPT.txt
export DD_OPENOLD DD_OPENMRG DD_PAYIN DD_PAYSRT DD_OPENPAY DD_PAYRPT
step STEP030 NADPAY 4

#---- STEP040: NADDUN
DD_OPENIN=$WORK/OPENPAY.dat
DD_OPENTMP=$WORK/OPENTMP.dat
DD_OPENOUT=$WORK/OPENNEW.dat
DD_CUSTMST=$WORK/CUSTBIL.dat
DD_AGERPT=$WORK/AGERPT.txt
DD_DUNLTR=$WORK/DUNLTR.txt
export DD_OPENIN DD_OPENTMP DD_OPENOUT DD_CUSTMST DD_AGERPT DD_DUNLTR
step STEP040 NADDUN 4

#---- Generacije za sljedeći mjesec:
#     CUSTBIL.dat -> custmast, NADCTL.dat -> nadctl, OPENNEW.dat -> openitems
log "===== NADMJES ZAVRSEN, MAXCC=$MAXCC ====="
exit "$MAXCC"

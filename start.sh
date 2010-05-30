#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

echo "Starting Polish..."
erl \
    -sname ${NAME} \
    -pa ./ebin ${NITROGEN_EBIN} ${SIMPLE_BRIDGE_EBIN} ${NPROCREG_EBIN} \
        ${GETTEXT_EBIN} ${EOPENID_EBIN} ${ELOGGER_EBIN} ${TRANE_EBIN} \
    -eval "application:start(nprocreg)" \
    -eval "application:start(polish)"


#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

echo "Starting Nitrogen..."
erl \
    -sname ${NAME} \
    -pa ./ebin ${NITROGEN_EBIN} ${SIMPLE_BRIDGE_EBIN} ${NPROCREG_EBIN} \
        ${GETTEXT_EBIN} ${REDBUG_EBIN} ${YAWS_EBIN} ${EOPENID_EBIN} \
        ${CRONE_EBIN} ${MSC_EBIN} ${ELOGGER_EBIN} \
    -eval "application:start(nprocreg)" \
    -eval "application:start(polish)"


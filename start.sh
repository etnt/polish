#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

if [ $# -eq 1 ]; then
    po_lang_dir="\""$(echo $1 | sed 's/=.*//')"\""
    echo "Starting Polish..."
    erl \
	-polish po_lang_dir $po_lang_dir \
	-sname ${NAME} \
	-pa ./ebin ${NITROGEN_EBIN} ${SIMPLE_BRIDGE_EBIN} ${NPROCREG_EBIN} \
        ${GETTEXT_EBIN} ${EOPENID_EBIN} ${ELOGGER_EBIN} ${TRANE_EBIN} \
	-eval "application:start(nprocreg)" \
	-eval "application:start(polish)"

else
    echo "Usage: ./start.sh absolute_path_to_lang_dir"
fi
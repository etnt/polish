#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

ask_replace_keys=true

if [ $# -eq 1 ]; then
    po_lang_dir="\""$(echo $1 | sed 's/=.*//')"\""
else 
    if [ $# -eq 2 -a $1 = "--no-replace-keys" ]; then
	po_lang_dir="\""$(echo $2 | sed 's/=.*//')"\""
	ask_replace_keys=false
    else
	echo ""
	echo "Usage: ./start.sh [options] absolute_path_to_lang_dir"
	echo ""
	echo "       Options:"
	echo "          --no-replace-keys :  Do not replace keys and wash po files directly"
	echo ""
	exit
    fi
fi

echo "Starting Polish..."
erl \
    -polish po_lang_dir $po_lang_dir ask_replace_keys $ask_replace_keys \
    -sname ${NAME} \
    -pa ./ebin ${YAWS_EBIN} ${GETTEXT_EBIN} ${EOPENID_EBIN} ${ELOGGER_EBIN} \
    -eval "application:start(polish)"

#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

ask_replace_keys=true

if [ $# -ge 1 -a $1 = "test" ]; then
    rm -rf results/ct_run*
    mkdir -p results
    if [ $# -eq 1 ]; then
	run_test -dir test/ -logdir results/ \
	    -pa $(pwd)/ebin $(pwd)/${MOCHIWEB_EBIN} $(pwd)/${GETTEXT_EBIN} \
	    $(pwd)/${EOPENID_EBIN} $(pwd)/${ELOGGER_EBIN}
    elif [ $# -eq 2 ]; then
	run_test -suite $2 -logdir results/ \
	    -pa $(pwd)/ebin $(pwd)/${MOCHIWEB_EBIN} $(pwd)/${GETTEXT_EBIN} \
	    $(pwd)/${EOPENID_EBIN} $(pwd)/${ELOGGER_EBIN}
    elif [ $# -eq 3 ]; then
	run_test -suite $2 -case $3 -logdir results/ \
	    -pa $(pwd)/ebin $(pwd)/${MOCHIWEB_EBIN} $(pwd)/${GETTEXT_EBIN} \
	    $(pwd)/${EOPENID_EBIN} $(pwd)/${ELOGGER_EBIN}
    fi
    mv test/*beam ebin/
elif [ $# -eq 1 -a $1 = "coverage" ]; then
    rm -rf results/ct_run*
    mkdir -p results
    cp ebin/*beam src/
    run_test -dir test/ -cover priv/coverspec -logdir results/ \
	-pa $(pwd)/ebin $(pwd)/${MOCHIWEB_EBIN} $(pwd)/${GETTEXT_EBIN} \
	$(pwd)/${EOPENID_EBIN} $(pwd)/${ELOGGER_EBIN}
    mv test/*beam ebin/
    rm src/*beam
else
    if [ $# -eq 1 ]; then
	po_lang_dir="\""$(echo $1 | sed 's/=.*//')"\""
    elif [ $# -eq 2 -a $1 = "--no-replace-keys" ]; then
	po_lang_dir="\""$(echo $2 | sed 's/=.*//')"\""
	ask_replace_keys=false
    else
	echo ""
	echo "Usage: ./start.sh [options] absolute_path_to_lang_dir"
	echo ""
	echo "       Options:"
	echo "          --no-replace-keys                     :  Do not replace keys and wash po files directly"
	echo ""
	echo ""
	echo "Test usage:"
	echo "       ./start.sh test                          :  Run all testcases"
	echo "       ./start.sh coverage                      :  Run all testcases with coverage"
	echo "       ./start.sh test suite_name               :  Run all testcases in suite"
	echo "       ./start.sh test suite_name testcase_name : Run testcase in suite"
	echo ""
	exit
    fi
    echo "Starting Polish..."
    erl \
	-polish po_lang_dir $po_lang_dir ask_replace_keys $ask_replace_keys \
	-sname ${NAME} \
	-pa ./ebin ${MOCHIWEB_EBIN} ${GETTEXT_EBIN} ${EOPENID_EBIN} ${ELOGGER_EBIN} \
	-eval "application:start(polish)"
fi


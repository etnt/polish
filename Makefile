
include dep.inc

all:
	erl -make

init:
	$(MAKE) -f Makefile.init
	$(MAKE) all

run_test:
	mkdir -p results
	run_test -dir test/ -logdir results/ -pa ./ebin ${MOCHIWEB_EBIN} ${GETTEXT_EBIN} ${EOPENID_EBIN} ${ELOGGER_EBIN} -polish po_lang_dir ./priv
	mv test/*beam ebin/

clean:
	rm -rf ./ebin/*.beam

rm_deps:
	rm -rf dep
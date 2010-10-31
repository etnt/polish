
include dep.inc

all:
	erl -make

init:
	$(MAKE) -f Makefile.init
	$(MAKE) all

run_test:
	mkdir -p results
	run_test -dir test/ -logdir results/ -pa $(PWD)/ebin $(PWD)/${MOCHIWEB_EBIN} $(PWD)/${GETTEXT_EBIN} $(PWD)/${EOPENID_EBIN} $(PWD)/${ELOGGER_EBIN}
	mv test/*beam ebin/

clean:
	rm -rf ./ebin/*.beam

rm_deps:
	rm -rf dep
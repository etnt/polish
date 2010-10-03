
include dep.inc

all:
	erl -make

init:
	$(MAKE) -f Makefile.init
	$(MAKE) all

clean:
	rm -rf ./ebin/*.beam

rm_deps:
	rm -rf dep

include dep.inc

all:
	erl -make

init:
	$(MAKE) -f Makefile.init
	$(MAKE) all

clean:
	rm -rf ./ebin/*.beam

init_clean: clean
	rm -rf ebin dep logs www/nitrogen


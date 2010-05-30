
include dep.inc

all:
	erl -make

init:
	-mkdir ebin
	-mkdir dep
	-mkdir logs
	-mkdir -p www/nitrogen
	-git submodule init
	-git submodule update
	-(cd dep/nitrogen; make compile)
	-(cd dep/elogger; make)
	-(cd dep/eopenid; make)
	-(cd www/nitrogen; ln -s $(NITROGEN_TOP_DIR)/apps/nitrogen/www/nitrogen.css .)
	-(cd www/nitrogen; ln -s $(NITROGEN_TOP_DIR)/apps/nitrogen/www/nitrogen.js .)
	-(cd www/nitrogen; ln -s $(NITROGEN_TOP_DIR)/apps/nitrogen/www/livevalidation.js .)
	-chmod +x ./start.sh
	cp src/polish.app.src ebin/polish.app 
	$(MAKE) all

clean:
	rm -rf ./ebin/*.beam

init_clean: clean
	rm -rf ebin dep logs www/nitrogen


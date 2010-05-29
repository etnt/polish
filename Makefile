
include dep.inc

all:
	erl -make

init:
	-sed -i 's?__polish_path__?'`pwd`'?' dep.inc
	$(MAKE) continue

continue:
	-mkdir ebin
	-mkdir dep
	-mkdir logs
	-mkdir -p www/nitrogen
	-git clone http://github.com/jordi-chacon/nitrogen.git dep/nitrogen
	-git clone http://github.com/etnt/elogger.git dep/elogger
	-git clone http://github.com/etnt/eopenid.git dep/eopenid
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


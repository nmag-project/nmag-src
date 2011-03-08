SHELL=/bin/bash

.PHONY: all build-bin clean

all: nsim nsim-raw nmesh

nsim: nsim.in root.inc nsim-raw
	. ./root.inc && \
	sed -e 's|\$$NMAGBINDIR\$$|'"$$NMAG_BIN"'|g' nsim.in > $@ || rm $@
	chmod u+x nsim

nsim-raw: nsim-raw.in root.inc
	. ./root.inc && \
	sed -e 's|\$$NMAGROOTDIR\$$|'"$$NMAG_ROOT"'|g' nsim-raw.in > $@ || rm $@
	chmod u+x $@

nmesh: nmesh.in root.inc
	. ./root.inc && \
	 sed -e 's|\$$NMAGROOTDIR\$$|'"$$NMAG_ROOT"'|g' nmesh.in > $@ || rm $@
	 chmod u+x $@

root.inc:
	echo "# NMAG_BIN should be set to the Nmag bin directory." > root.inc && \
	echo "NMAG_BIN=`pwd`" >> root.inc && \
	echo "NMAG_ROOT=`cd .. && pwd`" >> root.inc

build-bin:
	rm -f root.inc
	$(MAKE)

clean:
	rm -f nsim nsim-raw root.inc


# (C) 2005, 2006 Matteo Franchin
#
# This is a first attempt to create a makefile to speed-up the compilation
# of the program.
# USAGE:
#   make              # compile and install the code
#   make libuninstall # remove the installed ocaml libraries
#   make clean        # remove the files produced by the compilation
#   make mrproper     # equivalent to make libuninstall clean
# NOTES:
# This makefile provides the functionalities provided before by the scripts
# make-it.sh, unmake-it.sh, mrproper.sh.
# With respect to these script we added some others features:
#  - the building process will stop when an error is encoutered; just
#    to allow us to easily see where the error is;
#  - the building process is faster, because it does not compile all the files
#    from scratch. It will only compile those modules which have changed
#    and needs to be recompiled, taking care of dependencies.

LOG=compilation.log

MODULES = pyfem3 config mt19937 snippets qhull fastfields pycaml nlog mesh mpi_petsc fem sundials_sp ccpla nsim_grammars hlib bem3d nsim_anisotropy nsim

ROOT = ${PWD}
PYTHON=python
NSIM_DEB = $(ROOT)/bin/nsim_debug
NSIM_RAW_EXE = $(ROOT)/bin/nsim-raw
NSIM_EXE=bin/nsim
PYTEST_EXE=/usr/bin/py.test
NSIM_PYTEST = $(ROOT)/bin/nsim --nologfile $(PYTEST_EXE) --
NSIM_EXE1CPU = $(ROOT)/bin/nsim-1cpu
PYFEM = $(ROOT)/pyfem3/pyfem3
NMESHPP = $(ROOT)/bin/nmeshpp
# ---------

WHAT = all

#LOCAL_INST_PATH=$(shell pwd)
#OCAMLFIND_LDCONF:=ignore
#OCAMLPATH:=$(LOCAL_INST_PATH)/site-lib
#OCAMLFIND_DESTDIR:=$(LOCAL_INST_PATH)/site-lib

#export OCAMLFIND_LDCONF
#export OCAMLPATH
#export OCAMLFIND_DESTDIR

include config/common.inc

all: local-deps nsim nmesh2pp

debug: local-deps deps.pyfem3_debug $(NSIM_RAW_EXE) $(NSIM_EXE) \
  $(NSIM_DEB) nmesh2pp

config/configuration.inc: config/configure.py
	(cd config && rm -f configuration.inc && $(PYTHON) configure.py && cd ..)

local-deps: config/configuration.inc
	echo "Checking changed modules..."; \
	list='$(MODULES)'; \
	for subdir in $$list; do \
	  echo -n "Checking $$subdir... "; \
	  test "$$subdir" = . || (cd $$subdir && make deps) >$(LOG) 2>&1 || exit 1; \
	  echo "done!"; \
	done

deps.config: config/deps config/configuration.inc
	(cd config && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.config

deps.mt19937: mt19937/deps deps.config
	(cd mt19937 && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.mt19937

deps.snippets: snippets/deps deps.mt19937
	(cd snippets && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.snippets

deps.qhull: qhull/deps deps.snippets deps.mt19937
	(cd qhull && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.qhull

deps.fastfields: fastfields/deps deps.qhull deps.snippets deps.mt19937
	(cd fastfields && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.fastfields

deps.pycaml: pycaml/deps deps.qhull deps.snippets deps.mt19937
	(cd pycaml && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.pycaml

deps.nlog: nlog/deps deps.snippets deps.pycaml
	(cd nlog && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.nlog

deps.mesh: mesh/deps deps.pycaml deps.nlog deps.fastfields deps.qhull deps.snippets deps.mt19937
	(cd mesh && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.mesh

deps.mpi_petsc: mpi_petsc/deps deps.mesh deps.pycaml deps.fastfields deps.qhull deps.snippets deps.mt19937
	(cd mpi_petsc && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.mpi_petsc

deps.sundials_sp: sundials_sp/deps deps.snippets deps.mpi_petsc
	(cd sundials_sp && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.sundials_sp

deps.nsim_grammars: nsim_grammars/deps
	(cd nsim_grammars && make mrproper libuninstall; \
	make $(WHAT) && umask 0002 && make hacked_libinstall deps) >$(LOG) 2>&1
	touch deps.nsim_grammars

deps.nsim_anisotropy: nsim_anisotropy/deps deps.snippets
	(cd nsim_anisotropy && make mrproper libuninstall; \
	make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.nsim_anisotropy


deps.fem: fem/deps deps.snippets deps.mesh deps.mpi_petsc deps.sundials_sp deps.qhull deps.fastfields deps.nsim_grammars
	(cd fem && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.fem

deps.bem3d: bem3d/deps deps.snippets deps.mesh deps.hlib deps.fem
	(cd bem3d && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.bem3d

deps.nsim: nsim/deps deps.snippets deps.mesh deps.fem deps.bem3d deps.ccpla deps.nlog deps.sundials_sp
	(cd nsim && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.nsim

deps.hlib: hlib/deps
	(cd hlib && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.hlib

deps.ccpla: ccpla/deps deps.snippets deps.mpi_petsc
	(cd ccpla && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.ccpla

deps.ddpla: ddpla/deps deps.mpi_petsc
	(cd ddpla && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.ddpla


$(PYFEM): pyfem3/deps deps.mt19937 deps.snippets deps.qhull deps.fastfields deps.pycaml deps.mesh deps.fem deps.mpi_petsc deps.bem3d deps.ccpla deps.sundials_sp deps.nsim_grammars deps.nsim_anisotropy deps.nsim deps.hlib 
	(cd pyfem3 && make mrproper libuninstall; \
	 make $(WHAT) && umask 0002 && make libinstall deps) >$(LOG) 2>&1
	touch deps.pyfem3

deps.pyfem3_debug: pyfem3/deps deps.mt19937 deps.snippets deps.qhull deps.fastfields deps.pycaml deps.mesh deps.fem deps.mpi_petsc deps.bem3d deps.ccpla deps.sundials_sp deps.nsim_grammars deps.nsim_anisotropy deps.nsim deps.hlib 
	(cd pyfem3 && sed -r 's/native-code$$/byte-code/g' Makefile > Makefile.debug && \
	 make -f Makefile.debug mrproper libuninstall; \
	 make -f Makefile.debug $(WHAT) && umask 0002 && \
	 make -f Makefile.debug libinstall deps) >$(LOG) 2>&1
	touch deps.pyfem3_debug

$(NSIM_RAW_EXE): $(NSIM_RAW_EXE).in $(PYFEM)
	sed -e 's|\$$NMAGROOTDIR\$$|$(ROOT)|g' $(NSIM_RAW_EXE).in > $@ || rm $@
	chmod u+x $(NSIM_RAW_EXE)

$(NSIM_EXE): $(NSIM_RAW_EXE) bin/nsim.in
	sed -e 's|\$$NMAGROOTDIR\$$|$(ROOT)|g' bin/nsim.in > $@ || rm $@
	chmod u+x $@

#$(NSIM_PYTEST): $(NSIM_EXE)
#	rm -f $(NSIM_PYTEST)
#	echo "#!/bin/sh" > $(NSIM_PYTEST)
#	if [ -f config/exports.sh ]; then cat config/exports.sh >> $(NSIM_PYTEST); fi
#	echo "export PYTHONPATH=$(ROOT)/interface:\$$PYTHONPATH" >> $(NSIM_PYTEST)
#	echo $(PYFEM)" $(ROOT)/tests/pytest_main.py \$$*" >> $(NSIM_PYTEST)
#	chmod u+x $(NSIM_PYTEST)

$(NSIM_EXE1CPU): $(PYFEM)
	rm -f $(NSIM_EXE1CPU)
	echo "#!/bin/sh" > $(NSIM_EXE1CPU)
	if [ -f config/exports.sh ]; then cat config/exports.sh >> $(NSIM_EXE1CPU); fi
	echo "export PYTHONPATH=$(ROOT)/interface:\$$PYTHONPATH" >> $(NSIM_EXE1CPU)
	echo "taskset 0x00000001 "$(PYFEM)" \$$*" >> $(NSIM_EXE1CPU)
	chmod u+x $(NSIM_EXE1CPU)

$(NSIM_DEB):
	echo "export PYTHONPATH=$(ROOT)/interface:\$$PYTHONPATH" > $(NSIM_DEB)
	echo -n "ocamldebug" >> $(NSIM_DEB); \
	list='$(MODULES)'; \
	for subdir in $$list; do \
	  echo -n " -I $(ROOT)/$$subdir" >> $(NSIM_DEB); \
	done; \
	echo " "$(PYFEM) "\$$*" >> $(NSIM_DEB)
	chmod u+x $(NSIM_DEB)

interface/nsim/svnversion.py:
	echo "svnversion = '"$$(svnversion -n .)"'" > interface/nsim/svnversion.py

nsim: interface/nsim/svnversion.py $(NSIM_EXE) \
  $(NSIM_EXE1CPU) $(NSIM_PYTEST)

doc:
	cd interface/nmag/manual; make 

nmesh2pp:
	chmod u+x $(NMESHPP)

libuninstall: config/configuration.inc
	list='$(MODULES)'; \
	for subdir in $$list; do \
	  test "$$subdir" = . || (cd $$subdir && make libuninstall); \
	done

clean: config/configuration.inc
	rm -f depsrm -f $(NSIM_RAW_EXE) $(NSIM_EXE) $(NSIM_DEB) $(NSIM_PYTEST)
	list='$(MODULES)'; \
	for subdir in $$list; do \
	  test "$$subdir" = . || (cd $$subdir && make mrproper); \
	done
	rm -f config/configuration.inc

mrproper: libuninstall clean

check:
	@echo "Testing all reasonably fast tests".
	@echo "Skipping tests with name test_slow* and test_mpi*".
	$(NSIM_PYTEST) -k "-test_slow -test_mpi -test_hlib"

checkslow:
	$(NSIM_PYTEST) -k test_slow

checkmpi:
	$(NSIM_PYTEST) -k test_mpi

checkhlib:
	$(NSIM_PYTEST) -k test_hlib

checkall:
	$(NSIM_PYTEST) 


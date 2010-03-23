# Copyright (C) 2010 University of Southampton
#  by Name Surname
#
# Makefile for the example.
#
# I SUGGEST TO AVOID CHANGING THE TARGET NAMES: run.py, mesh.nmesh.h5, etc.
# STICK WITH THESE, IF POSSIBLE. This will help you to avoid redefining things
# and it will make all the examples more uniform and usable. 'make run' will
# then work for all the examples, and will produce the same file
# (run_dat.ndt), etc.

NMAG_ROOT=../..
include $(NMAG_ROOT)/tests/tools.inc

.PHONY: all mesh run force plot clean clean-all

# Standard targets

all: plot

mesh: mesh.nmesh.h5

run: run_dat.ndt

force:
	rm -f run_dat.ndt
	$(MAKE) all

plot: plot.eps

mesh.nmesh.h5: mesh.geo

run_dat.ndt: run.py mesh.nmesh.h5
	$(NSIM) run.py --clean

run.dat: run_dat.ndt
	$(NCOL) run_dat.ndt time H_ext_0 H_ext_1 H_ext_2 \
	  m_Py_0 m_Py_1 m_Py_2 > $@

plot.eps: plot.gnp run.dat
	$(GNUPLOT) plot.gnp

clean:
	rm -f ng.ini run_log.log.old run_progress.txt run_restart.h5 test.out

clean-all: clean
	rm -f mesh.nmesh.h5 plot.eps run.dat run_dat.h5 run_dat.ndt


HERE=$(PWD)
NMAG_ROOT=$(HERE)/../../..
BIN=$(NMAG_ROOT)/bin
INTERFACE_DIR=$(HERE)/../../../interface
NSIM=$(BIN)/nsim
NCOL=$(BIN)/ncol
CUBICMESH=$(NMAG_ROOT)/utils/cubicmesh/cubicmesh
NMESHPP=$(BIN)/nmeshpp

.PHONY: all plot check_nmagfd_root

all: check_nmagfd_root plot

$(CUBICMESH):
	cd `dirname $(CUBICMESH)`&& make

film.nmesh.h5: $(CUBICMESH)
	$(CUBICMESH) film.nmesh,50.0:20,50.0:20,3.0:1
	$(NMESHPP) -c film.nmesh film.nmesh.h5
	rm film.nmesh

check_nmagfd_root:
	@if [ -z "$(NMAGFD_SRC)" -o ! -f "$(NMAGFD_SRC)/m3s/simulation.py" ]; then echo NMAGFD_SRC must be set and must point to the nmagfd source directory; exit 1; fi

fd.dat: run_fdsimulation.py
	export PYTHONPATH=$(INTERFACE_DIR):$(NMAGFD_SRC):$$PYTHONPATH && \
	python run_fdsimulation.py --clean
	$(NCOL) run_fdsimulation H_ext_0 H_ext_1 H_ext_2 \
	  M_Py_0 M_Py_1 M_Py_2 > fd.dat

fe.dat: film.nmesh.h5 run_fesimulation.py
	$(NSIM) run_fesimulation.py --clean
	$(NCOL) run_fesimulation H_ext_0 H_ext_1 H_ext_2 \
	  M_Py_0 M_Py_1 M_Py_2 > fe.dat

plot: fd.dat fe.dat
	gnuplot plot.gnp


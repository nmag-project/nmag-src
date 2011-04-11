OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS =  snippets mesh fem ccpla bem3d nsim_grammars nlog nsimconf

THREADS=yes

INCDIRS=../snippets ../mesh ../fem ../bem3d ../ccpla ../nsim_grammars ../nlog ../mpi_petsc ../hlib ../sundials_sp ../config

CC=${MPICC}

# Where to find petsc and mpich2 libraries
# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

CFLAGS=$(DEBUGFLAGS) $(PETSC_INCFLAGS)
LDFLAGS=$(DEBUGFLAGS) $(PETSC_LDFLAGS)

# OCAMLMKLIB = ocamlmklib -ldopt -lpetscvec -ldopt -lpetscmat 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

RESULT = nsim

SOURCES = nsim.mli nsim.ml

INTERFACE=nsim.ml


all: byte-code-library native-code-library

deps: META nsim.ml nsim.mli
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

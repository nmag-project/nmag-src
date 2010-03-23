OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc

PACKS =  snippets mesh mpi_petsc unix str fem qhull mumag2 nlog nsim

THREADS=yes

CC=mpicc

INCDIRS=../snippets ../mesh ../mpi_petsc ../fem ../ccpla ../nlog ../mumag2

# Where to find petsc and mpich2 libraries
# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

CFLAGS=$(DEBUGFLAGS) $(PETSC_INCFLAGS)
LDFLAGS=$(DEBUGFLAGS) $(PETSC_LDFLAGS)

# OCAMLMKLIB = ocamlmklib -ldopt -lpetscvec -ldopt -lpetscmat 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

RESULT = mumag3

SOURCES = mumag3.mli mumag3.ml

all: byte-code-library native-code-library

deps: META mumag3.ml mumag3.mli
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

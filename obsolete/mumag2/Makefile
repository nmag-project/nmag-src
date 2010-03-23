OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc

PACKS =  snippets mesh mpi_petsc unix str fem qhull sundials nlog

THREADS=yes

CC=mpicc

INCDIRS = ../snippets ../mesh ../mpi_petsc ../fem ../sundials ../nlog

# Where to find petsc and mpich2 libraries
# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

CFLAGS=$(DEBUGFLAGS) $(PETSC_INCFLAGS)
LDFLAGS=$(DEBUGFLAGS) $(PETSC_LDFLAGS)

# OCAMLMKLIB = ocamlmklib -ldopt -lpetscvec -ldopt -lpetscmat 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

RESULT = mumag2

SOURCES = mumag2.mli mumag2.ml mumag2_stubs.c

all: byte-code-library native-code-library

deps: META mumag2.ml mumag2.mli
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc

PACKS =  snippets mesh fem ccpla nsim bem3d

THREADS=yes

INCDIRS=../snippets ../mesh ../fem ../bem3d ../ccpla ../nsim

CC=mpicc

# Where to find petsc and mpich2 libraries
# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

CFLAGS=$(DEBUGFLAGS) $(PETSC_INCFLAGS)
LDFLAGS=$(DEBUGFLAGS) $(PETSC_LDFLAGS)

# OCAMLMKLIB = ocamlmklib -ldopt -lpetscvec -ldopt -lpetscmat 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

RESULT = mumag

SOURCES = mumag.mli mumag.ml

all: byte-code-library native-code-library

deps: META mumag.ml mumag.mli
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS =  snippets mesh fem

THREADS=yes

INCDIRS=../snippets ../ccpla ../mesh ../fem ../mpi_petsc ../nsim_grammars ../hlib

# Where to find petsc and mpich2 libraries
# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

CFLAGS=$(DEBUGFLAGS) $(PETSC_INCFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS) $(PETSC_LDFLAGS)

# OCAMLMKLIB = ocamlmklib -ldopt -lpetscvec -ldopt -lpetscmat 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
#OCAMLNCFLAGS= -unsafe
#OCAMLNLDFLAGS= -unsafe

OCAMLNCFLAGS = -ccopt -O3 -ccopt -fomit-frame-pointer
OCAMLNLDFLAGS = -ccopt -O3 -ccopt -fomit-frame-pointer

OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=bem3d.ml

RESULT = bem3d

SOURCES = bem3d.mli bem3d.ml bem3d_stubs.c

all: byte-code-library native-code-library

deps: META bem3d.ml bem3d.mli bem3d_stubs.c
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

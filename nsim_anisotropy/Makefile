OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS =  snippets

THREADS=yes

INCDIRS=../snippets

# Where to find petsc and mpich2 libraries
# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

# OCAMLMKLIB = ocamlmklib -ldopt -lpetscvec -ldopt -lpetscmat 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
#OCAMLNCFLAGS= -unsafe
#OCAMLNLDFLAGS= -unsafe

OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=nsim_anisotropy.ml

RESULT = nsim_anisotropy

SOURCES = nsim_anisotropy.mli nsim_anisotropy.ml

all: byte-code-library native-code-library

deps: META nsim_anisotropy.ml nsim_anisotropy.mli	
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

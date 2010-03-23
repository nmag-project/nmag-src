OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS = snippets mpi_petsc bigarray unix

THREADS=yes

INCDIRS = ../mpi_petsc ../snippets

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

# CC= mpicc
# CFLAGS=$(DEBUGFLAGS)  -I /usr/lib/petscdir/2.2.0/include -I 
# LDFLAGS=$(DEBUGFLAGS)  -lpetscvec -lpetscmat -lpetscksp -lpetscdm -lpetsc

# this comes from ../config/MakefileOS.*

# CFLAGS=$(DEBUGFLAGS)  -I/usr/lib/petscdir/$(PETSCVERSION)/include -I/usr/local/petsc/$(PETSCVERSION)/include -I /usr/include/cvode -I /usr/include/sundials -I /usr/lib/mpich/include -I../mpi_petsc

CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS) $(PETSC_INCFLAGS) -I$(EXTRA_INCLUDE_PATH) -I../mpi_petsc -I../snippets

THREADS=yes

SOURCES = sundials_sp_stubs.c sundials_sp.mli sundials_sp.ml

RESULT = sundials_sp

# LDFLAGS=$(DEBUGFLAGS)  -lsundials_cvode -lsundials_nvecparallel

LDFLAGS=$(DEBUGFLAGS) -ldl

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=sundials_sp.ml

# OCAMLMKLIB = ocamlmklib -ldopt -lsundials_cvode -ldopt -lsundials_nvecparallel

OCAMLMKLIB = ocamlmklib -ldopt "$(LDFLAGS)"

#all: doc top byte-code-library native-code-library

all: byte-code-library native-code-library

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so *.top deps

deps: META sundials_sp.ml sundials_sp.mli sundials_sp_stubs.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

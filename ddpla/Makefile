OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

# Note: This module MUST NOT depend on snippets!
PACKS =  mpi_petsc bigarray unix
MLIARGS= -I ../mpi_petsc -I ../config

THREADS=yes

INCDIRS = ../mpi_petsc ../config

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

CC=mpicc

# CFLAGS=$(DEBUGFLAGS)  -I /usr/lib/petscdir/2.2.0/include -I 

LDFLAGS=$(DEBUGFLAGS) $(MPICH_LDFLAGS) $(PMPICH_LDFLAGS) $(LAPACK_LDFLAGS) $(BLAS_LDFLAGS)

# -lpetscvec -lpetscmat -lpetscksp -lpetscdm -lpetsc

# this comes from ../config/MakefileOS.*

# CFLAGS=$(DEBUGFLAGS)  -I/usr/lib/petscdir/$(PETSCVERSION)/include -I/usr/local/petsc/$(PETSCVERSION)/include -I /usr/include/cvode -I /usr/include/sundials -I /usr/lib/mpich/include -I../mpi_petsc

CFLAGS=$(DEBUGFLAGS) $(PETSC_INCFLAGS) -I../mpi_petsc

THREADS=yes

SOURCES = ddpla.mli ddpla.ml

RESULT = ddpla

# LDFLAGS=$(DEBUGFLAGS)  -lsundials_cvode -lsundials_nvecserial
# LDFLAGS=$(DEBUGFLAGS)  -ldl

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=ddpla.ml

# OCAMLMKLIB = ocamlmklib -ldopt -lsundials_cvode -ldopt -lsundials_nvecserial
# OCAMLMKLIB = ocamlmklib -ldopt -ldl

.PHONY: mrproper reinterface

#all: doc top byte-code-library native-code-library

all: byte-code-library native-code-library top

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so *.top deps

deps: META ddpla.ml ddpla.mli
	touch deps

include $(OCAMLMAKEFILE)


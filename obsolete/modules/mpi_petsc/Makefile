# (C) 2005 Dr. Thomas Fischbacher
#
# Now, this makefile works, but it is *quite* ugly and has to be re-done!

OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

THREADS=yes

CC=${MPICC}

# CFLAGS: -fPIC: generate [P]osition [I]ndependent [C]ode (default for darwin
#                and mac os)
# CFLAGS: -I: Where to find c-header files (files with extension *.h)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS) -fPIC $(PETSC_INCFLAGS) -I$(EXTRA_INCLUDE_PATH) $(METIS_INCFLAGS)

# List of the libraries used for linking
# LDFLAGS=$(DEBUGFLAGS)  -lpetscvec -lpetscmat -lpetscksp -lpetscts -lpetscsnes -lpetscdm -lpetsc
LDFLAGS=$(DEBUGFLAGS) $(MPICH_LDFLAGS) $(PETSC_LDFLAGS) $(METIS_LDFLAGS) ${MISSINGFLAGS}

# Where to find petsc and mpich2 libraries
LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH) $(MPICH2_LIBRARY_PATH)

SOURCES= mpi_petsc.ml mpi_petsc.mli collcomm.c comm.c groups.c init.c msgs.c utils.c petsc_stubs.c petsc_error.c

INCDIRS = ../config
PACKS = bigarray nsimconf

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

RESULT = mpi_petsc

OCAMLMKLIB=ocamlmklib -ldopt "$(LDFLAGS)"

# Note: we only build native code!
# For some archs, we may prefer bytecode though...
#all: top byte-code-library native-code-library byte-code doc

# XXX Note: for now, I cannot do "make top". Have to find out why!
#all: petsc_error.o petsc_error.ml petsc_error.mli top native-code-library byte-code-library doc

# XXX petsc_error.cmi is required for petsc_stubs.o - Note: have to re-structure the makefile here!

#all: petsc_stubs.o petsc_error.o petsc_error.cmi $(SOURCES) byte-code-library top native-code-library doc

# Do not make top, as this confuses gentoo!
all: petsc_stubs.o petsc_error.o petsc_error.cmi $(SOURCES) byte-code-library native-code-library

mrproper: clean
	rm -f *~ petsc_error.c petsc_error.o petsc_error.ml petsc_error.mli *.cmi *.cmo mpi_petsc.* deps

petsc_error.c: petsc_error.pl
	perl petsc_error.pl $(PETSC_INCLUDE_PATH)

petsc_error.ml: petsc_error.pl
	perl petsc_error.pl $(PETSC_INCLUDE_PATH)

petsc_error.mli: petsc_error.pl
	perl petsc_error.pl $(PETSC_INCLUDE_PATH)

mpi_petsc.ml: mpi.ml petsc.ml petsc_error.ml
	cat  mpi.ml petsc_error.ml petsc.ml > mpi_petsc.ml

mpi_petsc.mli: mpi_petsc.ml
	ocamlc -i -I ../config mpi_petsc.ml >mpi_petsc.mli

deps: META petsc_error.pl mpi_petsc.ml collcomm.c comm.c groups.c init.c msgs.c utils.c petsc_stubs.c petsc_error.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

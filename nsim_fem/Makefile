OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

CC = ${MPICC}

PACKS =  snippets mesh mpi_petsc unix str qhull nsim_grammars fastfields sundials_sp
# Note that we require qhull for plotting postscript
# files of meshes only!

THREADS=yes

INCDIRS = ../snippets ../mesh ../mpi_petsc ../nsim_grammars ../fastfields ../nlog ../qhull ../sundials_sp


CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS) -fPIC $(PETSC_INCFLAGS) -I$(EXTRA_INCLUDE_PATH)

# List of the libraries used for linking
LDFLAGS=$(DEBUGFLAGS) $(MPICH_LDFLAGS) $(PMPICH_LDFLAGS) $(PETSC_LDFLAGS) \
        $(METIS_LDFLAGS) $(LAPACK_LDFLAGS) $(BLAS_LDFLAGS)

# LIBDIRS=$(EXTRA_LIBRARY_PATH) $(PETSC_LIBRARY_PATH)

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLLIBPATH=

RESULT = nsim_fem

SOURCES = nsim_fem.mli nsim_fem.ml nsim_fem_stubs.c

INTERFACE=nsim_fem.ml


#all: top byte-code-library native-code-library native-code byte-code doc
# Again, gentoo has problems making a toplevel...

all: byte-code-library native-code-library native-code byte-code

deps: META fem.mli fem.ml fem_stubs.c
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

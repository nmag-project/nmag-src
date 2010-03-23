OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

# CC= mpicc
# CFLAGS=$(DEBUGFLAGS)  -I /usr/lib/petscdir/2.2.0/include -I 
# LDFLAGS=$(DEBUGFLAGS)  -lpetscvec -lpetscmat -lpetscksp -lpetscdm -lpetsc

# this comes from ../config/MakefileOS.*

THREADS=yes

INCDIRS=$(QHULL_INCLUDE_PATH)
LIBDIRS=$(QHULL_LIBRARY_PATH)

LIBS = unix str

SOURCES = qhull_stubs.c qhull.mli qhull.ml

RESULT = qhull

# Ugly problem: dllqhull_stubs.so will not have libqhull.so linkage if we only set
# LDFLAGS=$(DEBUGFLAGS)  -lqhull. Rather, we have to override OCAMLMKLIB and provide it with
# the appropriate parameter. THIS IS NOT HOW THINGS SHOULD BE!

# NOTE: on the other hand, we need LDFLAGS, since otherwise, "make top" will fail!

CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS) $(QHULL_LDFLAGS)

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)

OCAMLMKLIB=ocamlmklib -ldopt "$(LDFLAGS)"

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=qhull.ml

#all: doc top byte-code-library native-code-library

all: byte-code-library native-code-library

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so *.top deps

deps: META qhull.ml qhull.mli qhull_stubs.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS =

THREADS=yes

INCDIRS =

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.


CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS) $(PETSC_INCFLAGS) -I$(EXTRA_INCLUDE_PATH)

THREADS=yes

SOURCES = hlib_stubs.c hlib.mli hlib.ml

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=hlib.ml

RESULT = hlib

LDFLAGS=$(DEBUGFLAGS) -ldl

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)


# OCAMLMKLIB = ocamlmklib -ldopt -lsundials_cvode -ldopt -lsundials_nvecserial

OCAMLMKLIB = ocamlmklib -ldopt "$(LDFLAGS)"

#all: doc top byte-code-library native-code-library

all: byte-code-library native-code-library

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so *.top deps

deps: META hlib.ml hlib.mli hlib_stubs.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

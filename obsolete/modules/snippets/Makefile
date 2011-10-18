OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

#PACKS = extlib

PACKS = unix str bigarray

THREADS=yes

LIBS = 

OCAMLLIBPATH=

INCDIRS=

LIBDIRS=

EXTLIBDIRS=

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=snippets.ml

RESULT = snippets

SOURCES = snippets_stubs.c objsize.c snippets.ml snippets.mli

all: byte-code-library native-code-library

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so *.o deps

deps: META snippets.ml snippets.mli snippets_stubs.c objsize.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

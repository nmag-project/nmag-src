OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

SOURCES = mt19937.mli mt19937.ml mt19937_impl.c mt19937_stubs.c 

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=mt19937.ml

RESULT = mt19937

THREADS=yes

all: mt19937.h byte-code-library native-code-library top

mrproper: clean
	rm -rf *~ *.cmi *.cmo doc deps

deps: META mt19937.ml mt19937.mli mt19937.h mt19937_stubs.c mt19937_impl.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

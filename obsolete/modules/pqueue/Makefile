OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

SOURCES = pqueue.mli pqueue.ml

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS)

# We do not "make reinterface" - interface was written manually!
## ml sources of the mli files to be generated with 'make reinterface'
#INTERFACE=mt19937.ml

RESULT = pqueue

THREADS=yes

all: byte-code-library native-code-library top

mrproper: clean
	rm -rf *~ *.cmi *.cmo doc deps

deps: META pqueue.ml pqueue.mli
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

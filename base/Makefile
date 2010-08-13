OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS=unix str bigarray

THREADS=yes

LIBS=
OCAMLLIBPATH=
INCDIRS=
LIBDIRS=
EXTLIBDIRS=

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS)

RESULT=base

SOURCES=deferred.mli deferred.ml ba_stubs.c ba.mli ba.ml base.mli base.ml

all: base.mli byte-code-library native-code-library

base.mli: ba.cmi deferred.cmi
	ocamlc -i base.ml > base.mli

mrproper: clean
	rm -f *~ *.cmi *.cmo *.cma *.cmxa *.top *.so *.o *.a deps
	rm -f base.mli

deps: META $(SOURCES)
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

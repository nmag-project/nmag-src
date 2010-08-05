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

SOURCES=ba.ml ba.mli deferred.ml deferred.mli base.ml

all: byte-code-library native-code-library

base.mli: base.ml ba.mli deferred.mli
	ocamlc -i base.ml > base.mli

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so *.o deps

deps: META ba.ml ba.mli deferred.ml deferred.mli base.ml
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

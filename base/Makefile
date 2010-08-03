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

RESULT=deferred

SOURCES=deferred.ml deferred.mli

all: byte-code-library native-code-library

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so *.o deps

deps: META deferred.ml deferred.mli
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

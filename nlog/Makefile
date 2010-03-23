OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

PACKS = unix

THREADS=yes

LIBS = 

OCAMLLIBPATH=

INCDIRS=

LIBDIRS=

EXTLIBDIRS=

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS)
LDFLAGS=$(DEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=nlog.ml

RESULT = nlog

SOURCES =  nlog.ml nlog.mli

all: byte-code-library native-code-library

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so deps

deps: nlog.mli nlog.ml
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

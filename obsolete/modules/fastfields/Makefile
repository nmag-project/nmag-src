OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

#LDFLAGS=$(DEBUGFLAGS)  -L/usr/X11R6/lib -lX11

OCAMLMKLIB = ocamlmklib -ldopt ${DLFLAGS}

PACKS = unix bigarray

THREADS=yes

LIBS = 

OCAMLLIBPATH=

INCDIRS=

LIBDIRS=

EXTLIBDIRS=

CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS) -DGCC_FLAGS_SHLIB='$(GCC_FLAGS_SHLIB)'
LDFLAGS=$(DEBUGFLAGS)

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=fastfields.ml

RESULT = fastfields

SOURCES = fastfields.mli fastfields.ml fastfields_stubs.c

all: byte-code-library native-code-library

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so deps

deps: META fastfields.ml fastfields.mli fastfields_stubs.c
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

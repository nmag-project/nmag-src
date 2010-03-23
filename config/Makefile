OCAMLMAKEFILE = ../OCamlMakefile

include ./common.inc

THREADS=yes

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS)
LDFLAGS=$(DEBUGFLAGS)

PACKS=bigarray

RESULT=nsimconf

SOURCES=nsimconf.ml nsimconf.mli

.PHONY: mrproper

all: nsimconf.mli byte-code-library native-code-library

nsimconf.mli: nsimconf.ml
	ocamlc -i $< > nsimconf.mli

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so deps

deps: META nsimconf.ml
	touch deps

include $(OCAMLMAKEFILE)

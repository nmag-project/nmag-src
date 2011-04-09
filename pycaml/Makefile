OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

OCAMLLDFLAGS=$(PYCAML_OPT_DARWIN)
CLIBS=$(PYCAML_CLIBS) "pthread" ${DLLIB} $(UTIL_CLIBS) "m" "c"

PACKS=unix str bigarray

SOURCES=pycaml.ml pycaml.mli pycaml_stubs.c
RESULT=pycaml
THREADS=yes
#NO_CUSTOM=NO_CUSTOM
LDFLAGS=$(DEBUGFLAGS) -l$(PYCAML_CLIBS)
CFLAGS=$(DEBUGFLAGS) $(NSIM_CFLAGS) -I../config $(NUMPY_INCFLAGS) -fPIC

#EXTLIBDIRS=\ $(LIBDIRS)
#OCAMLLDFLAGS=$(DEBUGFLAGS) -linkall
INCDIRS=$(PYTHON_INCLUDE_PATH)
LIBDIRS=$(PYTHON_LIBRARY_PATH)

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS=$(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS=$(OCAMLDEBUGFLAGS)

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=pycaml.ml

all: pycaml_stubs.h native-code-library byte-code-library top

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so *.top deps

deps: META pycaml.ml pycaml.mli pycaml_stubs.c pycaml_stubs.h
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

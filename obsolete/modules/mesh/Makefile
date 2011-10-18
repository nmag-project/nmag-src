OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS=bigarray snippets base unix str qhull mt19937 gsl nlog

THREADS=yes

OCAMLDOCFLAGS=-I ../mt19937

INCDIRS=../snippets ../base ../mt19937 ../qhull ../nlog

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS=$(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS=$(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS)
LDFLAGS=$(DEBUGFLAGS)

OCAMLLIBPATH=

# ml sources of the mli files to be generated with 'make reinterface'
INTERFACE=periodic.ml correct_broken_mesh.ml

RESULT = mesh

SOURCES = periodic.mli periodic.ml \
  correct_broken_mesh.mli correct_broken_mesh.ml \
  mesh0.ml mesh0.mli simplex.ml simplex.mli \
  mesh.mli mesh.ml

all: top byte-code-library native-code-library

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc deps

deps: META $(SOURCES)
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

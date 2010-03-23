OCAMLMAKEFILE = ../OCamlMakefile

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

#PACKS = extlib

PACKS = snippets unix pycaml voodoo lablgtk2 lablgtk2.init lablgtk2.gtkgl fem_element mpi_petsc fastfields timestep mumag pyfem

THREADS = yes

LIBS = 

OCAMLLIBPATH=

INCDIRS=

LIBDIRS=

EXTLIBDIRS=

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS = -g
OCAMLBLDFLAGS = -g
RESULT = field_scope

SOURCES = field_scope.ml field_scope.mli

all: byte-code-library native-code-library

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so

.PHONY: mrproper

include $(OCAMLMAKEFILE)

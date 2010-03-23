OCAMLMAKEFILE = ../OCamlMakefile

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

PACKS =   mt19937 snippets qhull delaunay mesh meshphysics pycaml unix str

OCAMLLIBPATH="`$(OCAMLC) -where`:/usr/local/lib/ocaml/3.08.3"

# Note: I have problems with ocamlfind not finding own installed modules.
# Hence, for now, we will have to go with a hackish solution until I find out how to
# do this properly - T.F.

RESULT = mumag

SOURCES = Mumag.mli Mumag.ml

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS = -g
OCAMLBLDFLAGS = -g

# all: top byte-code-library native-code-library native-code byte-code

all: byte-code

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so doc

.PHONY: mrproper

include $(OCAMLMAKEFILE)

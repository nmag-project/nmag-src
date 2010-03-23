OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS =
OCAMLDOCFLAGS=

THREADS=yes

INCDIRS =

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= $(OCAMLDEBUGFLAGS)
OCAMLBLDFLAGS= $(OCAMLDEBUGFLAGS)
CFLAGS=$(DEBUGFLAGS)
LDFLAGS=$(DEBUGFLAGS)

OCAMLLIBPATH=

RESULT = diffop_parser

SOURCES = diffop_parser.mli diffop_lexer.mll diffop_parser.mly

LIBINSTALL_FILES=diffop_lexer.cmi  diffop_parser.a  diffop_parser.cma  diffop_parser.cmi  diffop_parser.cmxa  diffop_parser.mli

# Note: we only build native code!
# For some archs, we may prefer bytecode though...
#all: top byte-code-library native-code-library byte-code doc

# Again, gentoo cannot make a toplevel...
#all: top byte-code-library native-code-library native-code doc
all: byte-code-library native-code-library native-code

diffop_parser.mli: diffop_parser.ml
	cat HACK_TYPES diffop_parser.mli > dp.mli
	mv dp.mli diffop_parser.mli

mrproper: clean
	rm -rf *~ *.mli *.cmi *.cmo *.top diffop *.so doc deps

deps: META diffop_lexer.mll diffop_parser.mly HACK_TYPES
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

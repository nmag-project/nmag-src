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

RESULT = nsim_grammars

SOURCES = ddiffop_parser.mli ddiffop_lexer.mll ddiffop_parser.mly\
	  localeqn_parser.mli localeqn_lexer.mll localeqn_parser.mly\
	  dimensions_parser.mli dimensions_lexer.mll dimensions_parser.mly\

LIBINSTALL_FILES=ddiffop_lexer.cmi  ddiffop_parser.a ddiffop_parser.cma ddiffop_parser.cmi\
		 ddiffop_parser.cmxa  ddiffop_parser.mli\
		 localeqn_lexer.cmi  localeqn_parser.a localeqn_parser.cma localeqn_parser.cmi\
		 localeqn_parser.cmxa  localeqn_parser.mli\
		 dimensions_lexer.cmi  dimensions_parser.a dimensions_parser.cma dimensions_parser.cmi\
		 dimensions_parser.cmxa  dimensions_parser.mli\
                 nsim_grammars.cma nsim_grammars.cmxa nsim_grammars.a

nsim_grammars.cma: ddiffop_parser.cma localeqn_parser.cma dimensions_parser.cma
	ocamlc -a -o nsim_grammars.cma $^

ddiffop_parser.cmxa: ddiffop_parser.ml ddiffop_lexer.ml ddiffop_parser.cmi
	ocamlopt -o ddiffop_parser.cmxa -a -thread ddiffop_parser.ml ddiffop_lexer.ml

localeqn_parser.cmxa: localeqn_parser.ml localeqn_lexer.ml localeqn_parser.cmi
	ocamlopt -o localeqn_parser.cmxa -a -thread localeqn_parser.ml localeqn_lexer.ml

dimensions_parser.cmxa: dimensions_parser.ml dimensions_lexer.ml dimensions_parser.cmi
	ocamlopt -o dimensions_parser.cmxa -a -thread dimensions_parser.ml dimensions_lexer.ml

hack:    ddiffop_parser.cmxa ddiffop_parser.cma\
	 localeqn_parser.cmxa localeqn_parser.cma\
	 dimensions_parser.cmxa dimensions_parser.cma
	ocamlopt -o nsim_grammars.cmxa -a -thread ddiffop_parser.ml ddiffop_lexer.ml\
	 localeqn_parser.ml localeqn_lexer.ml\
	 dimensions_parser.ml dimensions_lexer.ml

hacked_libinstall: hack libinstall


# Note: we only build native code!
# For some archs, we may prefer bytecode though...
#all: top byte-code-library native-code-library byte-code doc

# Again, gentoo cannot make a toplevel...
#all: top byte-code-library native-code-library native-code doc
all: byte-code-library native-code-library native-code

ddiffop_parser.ml ddiffop_parser.mli: ddiffop_parser.mly
	ocamlyacc ddiffop_parser.mly
	cat ddiffop_types.mli ddiffop_parser.mli > _ddiffop_parser.mli
	mv _ddiffop_parser.mli ddiffop_parser.mli
	cat ddiffop_types.mli ddiffop_parser.ml > _ddiffop_parser.ml
	mv _ddiffop_parser.ml ddiffop_parser.ml

ddiffop_parser.cma: ddiffop_parser.cmo ddiffop_lexer.cmo
	ocamlc -a -o ddiffop_parser.cma ddiffop_parser.cmo ddiffop_lexer.cmo


localeqn_parser.ml localeqn_parser.mli: localeqn_parser.mly
	ocamlyacc localeqn_parser.mly
	cat localeqn_types.mli localeqn_parser.mli > _localeqn_parser.mli
	mv _localeqn_parser.mli localeqn_parser.mli
	cat localeqn_types.mli localeqn_parser.ml > _localeqn_parser.ml
	mv _localeqn_parser.ml localeqn_parser.ml

localeqn_parser.cma: localeqn_parser.cmo localeqn_lexer.cmo
	ocamlc -a -o localeqn_parser.cma localeqn_parser.cmo localeqn_lexer.cmo


dimensions_parser.ml dimensions_parser.mli: dimensions_parser.mly
	ocamlyacc dimensions_parser.mly
	cat dimensions_types.mli dimensions_parser.mli > _dimensions_parser.mli
	mv _dimensions_parser.mli dimensions_parser.mli
	cat dimensions_types.mli dimensions_parser.ml > _dimensions_parser.ml
	mv _dimensions_parser.ml dimensions_parser.ml

dimensions_parser.cma: dimensions_parser.cmo dimensions_lexer.cmo
	ocamlc -a -o dimensions_parser.cma dimensions_parser.cmo dimensions_lexer.cmo


ddiffop_example_bc: ddiffop_parser.cmi ddiffop_parser.cmo ddiffop_lexer.cmo ddiffop_example.cmo
	ocamlc -o ddiffop_example  ddiffop_parser.cmo ddiffop_lexer.cmo ddiffop_example.cmo

mrproper: clean
	rm -rf *~ ddiffop_parser.mli ddiffop_parser.ml ddiffop_lexer.ml\
	 localeqn_parser.mli localeqn_parser.ml localeqn_lexer.ml\
	 dimensions_parser.mli dimensions_parser.ml dimensions_lexer.ml\
	 ddiffop_example *.cmi *.cmo *.top ddiffop *.so doc deps nsim_grammars.* \
	*.cma *.cmxa

deps: META ddiffop_lexer.mll ddiffop_parser.mly ddiffop_types.mli\
      localeqn_lexer.mll localeqn_parser.mly localeqn_types.mli\
      dimensions_lexer.mll dimensions_parser.mly dimensions_types.mli
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

CC="cc"

PYTHON_CFLAGS="-fPIC -Wall -O3 -fomit-frame-pointer -malign-double -fstrict-aliasing -ffast-math -I../config -I/usr/lib/pymodules/python2.7/numpy/core/include -I/usr/lib/pymodules/python2.7/numpy/core/include -I/usr/include/python2.7 -DPIC -DNATIVE_CODE"

NSIMEXEC_LDFLAGS="-lpython2.7 -lqhull"

CC=cc
CPP=cpp

OCAMLFIND=ocamlfind
OCAMLOPT=$(OCAMLFIND) ocamlopt
OCAMLOPT_CC_C=$(OCAMLOPT) -cc $(CC) -ccopt "-Wall"
OCAMLOPT_CC=$(OCAMLOPT) -cc $(CC) -pp $(CPP)
OCAMLOPT_C=$(OCAMLOPT) -c
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

NSIMEXEC_OBJECTS=\
  mt19937.cmx mt19937_impl.o mt19937_stubs.o \
  snippets.cmx snippets_stubs.o objsize.o \
  fastfields.cmx fastfields_stubs.o \
  qhull.cmx qhull_stubs.o \
  deferred.cmx ba.cmx base.cmx ba_stubs.o \
  nlog.cmx \
  mesh0.cmx simplex.cmx periodic.cmx correct_broken_mesh.cmx mesh.cmx \
  ddiffop_parser.cmx \
  fem.cmx \
  pycaml.cmx pycaml_stubs.o

.PHONY: all clean

all: nsimexec

%.cmi: %.mli
	$(OCAMLOPT_C) $< -o $@

%.cmx: %.ml
	$(OCAMLOPT_C) $< -o $@

%.o: %.c
	$(OCAMLOPT_CC_C) $< -o $@

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml: %.mly
	$(OCAMLYACC) $<

mt19937_impl.o: mt19937_impl.c mt19937.h
mt19937_stubs.o: mt19937_stubs.c mt19937.h
mt19937.cmx: mt19937.ml mt19937.cmi

snippets.cmx: snippets.ml snippets.cmi

fastfields.cmx: fastfields.ml fastfields.cmi

qhull.cmx: qhull.ml qhull.cmi

deferred.cmx: deferred.ml deferred.cmi
ba.cmx: ba.ml ba.cmi
base.mli: ba.cmi deferred.cmi
	ocamlc -i base.ml > base.mli
base.cmx: base.ml base.cmi

nlog.cmx: nlog.ml nlog.cmi

mesh0.cmx: mesh0.ml mesh0.cmi
correct_broken_mesh.cmx: correct_broken_mesh.ml correct_broken_mesh.cmi
simplex.cmi: simplex.mli mesh0.cmi
simplex.cmx: simplex.ml simplex.cmi
periodic.cmx: periodic.ml periodic.cmi
mesh.cmx: mesh.ml mesh0.cmi correct_broken_mesh.cmi simplex.cmi \
          periodic.cmi mesh.cmi

fem.cmx: fem.ml fem.cmi

ddiffop_lexer.cmi: ddiffop_lexer.ml
ddiffop_parser.cmi: ddiffop_parser.mli ddiffop.cmi
ddiffop_parser.cmx: ddiffop_parser.ml ddiffop_parser.cmi

pycaml_stubs.o: pycaml_stubs.c pycaml_stubs.h
	$(OCAMLOPT_CC_C) \
	  -ccopt $(PYTHON_CFLAGS) \
	  -o pycaml_stubs.o pycaml_stubs.c 

pycaml.cmi: pycaml.mli
pycaml.cmx: pycaml.ml pycaml.cmi

nsimexec: $(NSIMEXEC_OBJECTS)
	$(OCAMLOPT) \
	  -cc $(CC) \
	  -package str,bigarray -linkpkg \
	  -ccopt $(NSIMEXEC_LDFLAGS) \
	  $(NSIMEXEC_OBJECTS) \
	  -o nsimexec

clean:
	rm -f nsimexec *.cmi *.cmo *.cmx *.o

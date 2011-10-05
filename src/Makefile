CC="cc"

PYTHON_CFLAGS="-fPIC -Wall -O3 -fomit-frame-pointer -malign-double -fstrict-aliasing -ffast-math -I../config -I/usr/lib/pymodules/python2.7/numpy/core/include -I/usr/lib/pymodules/python2.7/numpy/core/include -I/usr/include/python2.7 -DPIC -DNATIVE_CODE"

NSIMEXEC_LDFLAGS="-lpython2.7 -lqhull"

OCAMLFIND=ocamlfind
OCAMLOPT=$(OCAMLFIND) ocamlopt
OCAMLOPT_CC_C=$(OCAMLOPT) -cc $(CC)

NSIMEXEC_OBJECTS=\
  mt19937.cmx mt19937_impl.o mt19937_stubs.o \
  snippets.cmx snippets_stubs.o objsize.o \
  fastfields.cmx fastfields_stubs.o \
  qhull.cmx qhull_stubs.o \
  pycaml.cmx pycaml_stubs.o

.PHONY: all clean

all: nsimexec

%.cmi: %.mli
	$(OCAMLOPT) -c $< -o $@

%.cmx: %.ml
	$(OCAMLOPT) -c $< -o $@

%.o: %.c
	$(OCAMLOPT_CC_C) $< -o $@

mt19937_impl.o: mt19937_impl.c mt19937.h
mt19937_stubs.o: mt19937_stubs.c mt19937.h
mt19937.cmx: mt19937.ml mt19937.cmi
snippets.cmx: snippets.ml snippets.cmi
fastfields.cmx: fastfields.ml fastfields.cmi
qhull.cmx: qhull.ml qhull.cmi

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

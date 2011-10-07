CC="cc"

CC_SHLIB_FLAGS=-fPIC -shared

PETSC_INCLUDE_PATH=/usr/include/petsc
PYTHON_CFLAGS="-fPIC -Wall -O3 -fomit-frame-pointer -malign-double -fstrict-aliasing -ffast-math -I../config -I/usr/lib/pymodules/python2.7/numpy/core/include -I/usr/lib/pymodules/python2.7/numpy/core/include -I/usr/include/python2.7 -I/usr/include/mpi -DPIC -DNATIVE_CODE"

CFLAGS="-I/usr/include/mpi -I$(PETSC_INCLUDE_PATH) -DGCC_FLAGS_SHLIB='$(CC_SHLIB_FLAGS)' -Wall -O3 -ffast-math"

NSIMEXEC_LDFLAGS="-lpython2.7 -lqhull -lpetsc -lpetscvec -lpetscmat -lpetscksp -lpetscts -lpetscsnes -lpetscdm -lmetis -lparmetis"

CC=cc
CPP=cpp
PERL=perl

OCAMLFIND=ocamlfind
OCAMLOPT=$(OCAMLFIND) ocamlopt
OCAMLOPT_CC_C=$(OCAMLOPT) -cc $(CC) -ccopt $(CFLAGS)
OCAMLOPT_CC=$(OCAMLOPT) -cc $(CC) -pp $(CPP)
OCAMLOPT_C=$(OCAMLOPT) -c
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

NSIMEXEC_OBJECTS=\
  nsimconf.cmx \
  mt19937.cmx mt19937_impl.o mt19937_stubs.o \
  snippets.cmx snippets_stubs.o objsize.o \
  fastfields.cmx fastfields_stubs.o \
  qhull.cmx qhull_stubs.o \
  deferred.cmx ba.cmx base.cmx ba_stubs.o \
  nlog.cmx \
  mesh0.cmx simplex.cmx periodic.cmx correct_broken_mesh.cmx mesh.cmx \
  ddiffop.cmx ddiffop_lexer.cmx ddiffop_parser.cmx \
  localeqn.cmx localeqn_parser.cmx  localeqn_lexer.cmx \
  dimensions.cmx dimensions_parser.cmx dimensions_lexer.cmx \
  collcomm.o comm.o groups.o init.o msgs.o utils.o petsc_stubs.o \
  petsc_error.o mpi_petsc.cmx \
  sundials_sp.cmx sundials_sp_stubs.o \
  fem.cmx fem_stubs.o \
  hlib.cmx hlib_stubs.o mgdesc.o \
  bem3d.cmx bem3d_stubs.o \
  nsim_anisotropy.cmx \
  ccpla.cmx \
  nsim.cmx \
  pycaml.cmx pycaml_stubs.o \
  bindings.cmx pypetsc.cmx pyfem.cmx nsimexec.cmx

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

#mpi_petsc.ml mpi_petsc.mli 
petsc_error.c petsc_error.ml petsc_error.mli: petsc_error.pl
	$(PERL) petsc_error.pl $(PETSC_INCLUDE_PATH)

fem.cmx: fem.ml fem.cmi mesh.cmi

ddiffop.cmx: ddiffop.ml ddiffop.cmi
ddiffop_lexer.cmx: ddiffop_lexer.ml ddiffop_parser.cmi
ddiffop_parser.cmi: ddiffop_parser.mli ddiffop.cmi
ddiffop_parser.cmx: ddiffop_parser.ml ddiffop_parser.cmi

localeqn.cmx: localeqn.ml localeqn.cmi
localeqn_parser.ml localeqn_parser.mli: localeqn_parser.mly
	$(OCAMLYACC) localeqn_parser.mly
localeqn_parser.cmi: localeqn_parser.mli localeqn.cmi
localeqn_lexer.cmx: localeqn_lexer.ml localeqn_parser.cmi
localeqn_parser.cmx: localeqn_parser.ml localeqn_parser.cmi

dimensions.cmx: dimensions.ml dimensions.cmi
dimensions_parser.ml dimensions_parser.mli: dimensions_parser.mly
	$(OCAMLYACC) dimensions_parser.mly
dimensions_parser.cmi: dimensions_parser.mli dimensions.cmi
dimensions_parser.cmx: dimensions_parser.ml dimensions_parser.cmi
dimensions_lexer.cmx: dimensions_parser.ml dimensions_parser.cmi

mpi_petsc.ml: mpi.ml petsc.ml petsc_error.ml
	cat  mpi.ml petsc_error.ml petsc.ml > mpi_petsc.ml

#mpi_petsc.mli: mpi_petsc.ml
#        ocamlc -i -I ../config mpi_petsc.ml >mpi_petsc.mli

sundials_sp.cmx: sundials_sp.ml sundials_sp.cmi

hlib.cmx: hlib.ml hlib.cmi

bem3d.cmx: bem3d.ml bem3d.cmi fem.cmi mesh.cmi

nsim_anisotropy.cmx: nsim_anisotropy.ml nsim_anisotropy.cmi

ccpla.cmx: ccpla.ml ccpla.cmi

nsim.cmx: nsim.ml nsim.cmi bem3d.cmi mesh.cmi fem.cmi

pycaml_stubs.o: pycaml_stubs.c pycaml_stubs.h
	$(OCAMLOPT_CC_C) \
	  -ccopt $(PYTHON_CFLAGS) \
	  -o pycaml_stubs.o pycaml_stubs.c 

pycaml.cmi: pycaml.mli
pycaml.cmx: pycaml.ml pycaml.cmi

pyfem.cmx: pyfem.ml fem.cmi mesh.cmi

nsimexec.cmx: nsimexec.ml nsimexec.cmi snippets.cmi mesh.cmi fem.cmi

nsimexec: $(NSIMEXEC_OBJECTS)
	$(OCAMLOPT) \
	  -cc $(CC) \
	  -package str,bigarray -linkpkg \
	  -ccopt $(NSIMEXEC_LDFLAGS) \
	  $(NSIMEXEC_OBJECTS) \
	  -o nsimexec

clean:
	rm -f nsimexec *.cmi *.cmo *.cmx *.o

OCAMLMAKEFILE = ../OCamlMakefile

include ../config/configuration.inc
include ../config/common.inc

PACKS=nsimconf snippets unix str qhull mt19937 fastfields pycaml mesh fem mpi_petsc nlog ccpla sundials_sp nsim_grammars nsim_anisotropy nsim hlib

THREADS=yes

OCAMLDOCFLAGS= -I ../mt19937

INCDIRS = ../snippets ../sundials_sp

CFLAGS=$(DEBUGFLAGS)
#CFLAGS= -O3 -march=i686

# Note: this will modify our command line arguments in such a way that we can be started via "mpirun"!
CC=${MPICC}

#If we use this, we need to create this directory first!
#Postpone that problem until we decide we need to use rpath.
#LDFLAGS=$(DEBUGFLAGS)  -Wl,-rpath /tmp/NSIMTAGRPATHBEGINXXXXXXXXXXXXXXXXXXXXXXxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXNSIMTAGRPATHEND

LDFLAGS=$(DEBUGFLAGS)

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS= -g
OCAMLBLDFLAGS= -g
OCAMLLIBPATH=

# OCAMLNCFLAGS = -unsafe -ccopt -O3 -ccopt -march=i686
# OCAMLNLDFLAGS = -unsafe -ccopt -O3 -ccopt -march=i686


RESULT = pyfem3

SOURCES = bindings.ml pypetsc.ml pyfem3.mli pyfem3.ml 

# Note: we only build native code!
# For some archs, we may prefer bytecode though...
#all: top byte-code-library native-code-library byte-code doc

# Again, gentoo cannot make a toplevel...
#all: top byte-code-library native-code-library native-code doc
all: byte-code-library native-code-library native-code

deps: META pyfem3.ml pyfem3.mli
	touch deps

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.top *.so *.a pyfem3 ipyfem3 *.cmxa *.cma *.cmx *.o doc deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)

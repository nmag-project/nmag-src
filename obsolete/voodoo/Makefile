OCAMLMAKEFILE = ../OCamlMakefile

MAKEFILE_OS = ../config/MakefileOSconfig

include ../config/MakefileOSconfig

# We put those in so that they are in place right from the start.
# Do not want to see any surprises later on.

# CC= mpicc
# CFLAGS= -I /usr/lib/petscdir/2.2.0/include -I 
# LDFLAGS= -lpetscvec -lpetscmat -lpetscksp -lpetscdm -lpetsc

# this comes from ../config/MakefileOS.*


INCDIRS=/usr/lib/glib-2.0/include /usr/lib/gtk-2.0/include /usr/include/glib-2.0 /usr/include/gtk-2.0 /usr/include/cairo /usr/include/pango-1.0 /usr/include/atk-1.0

#LIBDIRS=$(VOODOO_LIB_DIR)

LIBS = unix str

THREADS=yes

SOURCES = voodoo_stubs.c voodoo.mli voodoo.ml

RESULT = voodoo

LDFLAGS=-L/usr/lib -L/usr/X11R6/lib -lgtk -lgdk -rdynamic -lgmodule -lglib -ldl -lXi -lXext -lX11 -lm

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS = -g
OCAMLBLDFLAGS = -g


OCAMLMKLIB = ocamlmklib -ldopt -L/usr/lib -ldopt -L/usr/X11R6/lib -ldopt -lgtk-x11-2.0 -ldopt -lgdk-x11-2.0 -ldopt -rdynamic -ldopt -lgmodule -ldopt -lglib-2.0 -ldopt -ldl -ldopt -lXi -ldopt -lXext -ldopt -lX11 -ldopt -lm

#all: doc top byte-code-library native-code-library

all: doc byte-code-library native-code-library

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so *.top

.PHONY: mrproper

include $(OCAMLMAKEFILE)

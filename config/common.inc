.PHONY: default_target lu li mp ri reinterface

# These variables are used to reconfigure ocamlfind to operate on local
# directories (normally it tries to write under /usr/lib/ocaml
# This works only for Makefile-s called on the subdirectories.
LOCAL_INST_PATH=$(shell pwd)/..
OCAMLFIND_LDCONF:=ignore
OCAMLPATH:=$(LOCAL_INST_PATH)/site-lib
OCAMLFIND_DESTDIR:=$(LOCAL_INST_PATH)/site-lib

export OCAMLFIND_LDCONF
export OCAMLPATH
export OCAMLFIND_DESTDIR

# Remember that make will execute the first target defined in the Makefile.
# We then have two options: include the Makefile at the end of each Makefile
# or include it at the begin and stick to the convention that every Makefile
# which include this one, should have a target 'all' which is supposed to be
# the default target.
default_target: all

li: libinstall

lu: libuninstall

mp: mrproper

ri: reinterface

reinterface:
	for IF in $(INTERFACE); do \
	  ocamlc -i $(INCFLAGS) $$IF > "$$IF"i; \
	done

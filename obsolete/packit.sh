#!/bin/sh

make mrproper
rm -rf interface/nmesh/doc/visualfigures
rm /tmp/ocaml.tar.bz2*
cd ..
tar cvjf /tmp/ocaml.tar.bz2 ocaml
gpg -c /tmp/ocaml.tar.bz2
scp /tmp/ocaml.tar.bz2.gpg tf@141.84.136.30:

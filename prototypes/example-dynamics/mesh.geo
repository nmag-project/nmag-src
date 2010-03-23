# Copyright (C) 2010 University of Southampton
#  by Name Surname
# ^^^ remove the copyright notices if the mesh is not worth to copyright
#     (if it is just a sphere or cube, for example)
#
# Netgen geometry specification: just one sphere

algebraic3d

solid main = sphere (0, 0, 0; 20) -maxh=3.0;

tlo main;


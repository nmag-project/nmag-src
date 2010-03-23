#!/bin/sh
mpicc -o test_petsc -I /usr/lib/petscdir/2.2.0/include/ ex2.c -lpetscvec -lpetscmat -lpetscts -lpetscsnes -lpetscksp -lpetscdm -lpetsc
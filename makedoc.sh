#!/bin/sh

echo "This script will create a file doc.pdf and directory doc/index.html "
echo "which contain the documenation for all modules (ideally)."

rm -f doc.*

ocamldoc -latex -o doc.tex -I snippets -I mt19937 -I diffop_parser -I qhull -I fastfields -I fem -I mpi_petsc -I pyfem2 -I log -I timestep -I mumag2 -I mesh snippets/snippets.mli qhull/qhull.mli mt19937/mt19937.mli pycaml/pycaml.mli mesh/mesh.mli fastfields/fastfields.mli fem/fem.mli log/log.mli mpi_petsc/mpi_petsc.mli pyfem2/pyfem2.mli timestep/timestep.mli mumag2/mumag2.mli diffop_parser/diffop_parser.mli voodoo/voodoo.mli 

pdflatex doc.tex
pdflatex doc.tex

echo "Documentation is in doc.pdf"

#create html documentation in subdirectory doc

mkdir -p doc

ocamldoc -html -keep-code -colorize-code -o index -d doc -I snippets -I mt19937 -I qhull -I diffop_parser -I fastfields -I fem -I log -I mpi_petsc -I timestep -I mumag2 -I mesh -I pyfem2 snippets/snippets.mli qhull/qhull.mli mt19937/mt19937.mli pycaml/pycaml.mli mesh/mesh.mli fastfields/fastfields.mli fem/fem.mli log/log.mli mpi_petsc/mpi_petsc.mli pyfem2/pyfem2.mli timestep/timestep.mli mumag2/mumag2.mli diffop_parser/diffop_parser.mli voodoo/voodoo.mli 



echo "Documentation is in doc/index.html"
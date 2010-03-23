all:
	# Complie Caml source. It calls a function which is defined in snippets, but the -I ../snippets is a bit strange.
	ocamlopt -I ../snippets -I ../pycaml -I ../nlog -I ../qhull -I ../mt19937 -I ../fastfields -I ../mesh -I ../fem -I ../mpi_petsc -I ../sundials_sp -I ../sundials_ccpla -I ../nsim -I ../ccpla -I ../nsim_grammars -I ../nsim_anisotropy -I ../config -ccopt -fPIC nsimcore.ml -c 

	# Compile the glue which sticks together Ocaml and Python (a Python extension module calling Caml callbacks)
	gcc -ggdb -Wall -c nsimcore.c -I/usr/include/python2.5 -I/usr/lib/ocaml -o cnsimcore.o 

	# Link in some standard utilities
	ocamlopt -ccopt -L/usr/lib/ocaml/gsl -o bignsimcore.so -output-obj unix.cmxa str.cmxa bigarray.cmxa gsl/gsl.cmxa ../config/nsimconf.cmx ../snippets/snippets.cmx  ../pycaml/pycaml.cmx  ../nlog/nlog.cmx  ../qhull/qhull.cmx  ../mt19937/mt19937.cmx  ../fastfields/fastfields.cmx  ../mesh/periodic.cmx ../mesh/correct_broken_mesh.cmx ../mesh/mesh.cmx ../mpi_petsc/mpi_petsc.cmx ../nsim_grammars/ddiffop_parser.cmx ../nsim_grammars/ddiffop_lexer.cmx ../nsim_grammars/localeqn_parser.cmx ../nsim_grammars/localeqn_lexer.cmx ../nsim_grammars/dimensions_parser.cmx ../nsim_grammars/dimensions_lexer.cmx ../fem/fem.cmx ../sundials_sp/sundials_sp.cmx ../hlib/hlib.cmx ../bem3d/bem3d.cmx ../ccpla/ccpla.cmx ../nsim/nsim.cmx ../nsim_anisotropy/nsim_anisotropy.cmx nsimcore.cmx 

	# Stick all the necessary bits together to make a Python module
	gcc -shared --whole-archive  bignsimcore.so -L/usr/lib/ocaml/3.10.2 -L/usr/lib/ocaml/3.10.2/gsl ../snippets/snippets_stubs.o ../sundials_sp/sundials_sp_stubs.o ../mpi_petsc/petsc_stubs.o ../mpi_petsc/msgs.o ../mpi_petsc/comm.o ../mpi_petsc/collcomm.o ../mpi_petsc/utils.o ../mpi_petsc/init.o ../mpi_petsc/petsc_error.o ../fem/fem_stubs.o ../pycaml/pycaml_stubs.o ../mt19937/mt19937_stubs.o ../mt19937/mt19937_impl.o ../fastfields/fastfields_stubs.o ../qhull/qhull_stubs.o ../hlib/hlib_stubs.o ../bem3d/bem3d_stubs.o ../snippets/objsize.o cnsimcore.o -L/usr/lib/ocaml -L/usr/lib/python2.5/config -lpython2.5 -lunix -lasmrun -o nsimcore.so -lstr -lbigarray -L/usr/lib/ocaml/gsl/ -lmlgsl -lqhull -lpetsc -lpetscvec -lpetscmat -lpetscts -lpetscksp -lpetscdm -lpetscsnes -lgsl -lmetis

	# Check that it works
	python2.5 -c "import nsimcore; print nsimcore.version()";
	cp ../tests/regression/nmag/bigbar/bar30_30_100.nmesh.h5 .
	PYTHONPATH=$$PWD/../interface/ python2.5 bigbar.py
	python2.5 ../bin/ncol bigbar time M_Py_0 M_Py_1 M_Py_2 > output
	echo 'plot "output" u 1:2 w l, "" u 1:3 w l, "" u 1:4 w l, "../tests/regression/nmag/bigbar/results/nmag4_M_r5664.dat" u 1:2, "" u 1:3, "" u 1:4; pause 10' | gnuplot

clean:
	rm -f nsimcore.o nsimcore.so bignsimcore.so nsimcore.cmx nsimcore.cmi *~ cnsimcore.o *.pyc bigbar_* bar30_30_100.nmesh.h5 convergence.txt  interactive-session_log.log output


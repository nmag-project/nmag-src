.PHONY: run_touching run_sep default

default: run_touching

touching_cubes.nmesh.h5: touching_cubes.geo
	netgen -geofile=touching_cubes.geo -moderate -meshfiletype="Neutral Format" -meshfile=touching_cubes.neu -batchmode
	nmeshimport --netgen touching_cubes.neu touching_cubes.nmesh.h5

sep_cubes.nmesh.h5: seo_cubes.geo
	netgen -geofile=sep_cubes.geo -moderate -meshfiletype="Neutral Format" -meshfile=sep_cubes.nmesh.h5 -batchmode
	nmeshimport --netgen sep_cubes.neu sep_cubes.nmesh.h5

run_touching: touching_cubes.nmesh.h5
	nsim touching_cubes.py --clean --loglevel=debug

run_sep: sep_cubes.nmesh.h5
	nsim sep_cubes.py --clean


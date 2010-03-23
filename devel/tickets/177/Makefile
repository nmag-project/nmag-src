BIN=../../../bin
NSIM=$(BIN)/nsim 
NMESHIMPORT=$(BIN)/nmeshimport
NMESHPP=$(BIN)/nmeshpp


sphereincube.nmesh.h5: sphereincube.neutral
	$(NSIM) $(NMESHIMPORT) --netgen sphereincube.neutral sphereincube.nmesh.h5


sphereincube.neutral: sphereincube.geo
	netgen -geofile=sphereincube.geo -meshfiletype="Neutral Format" -meshfile=sphereincube.neutral -batchmode


mesh.vtk: sphereincube.nmesh.h5
	$(NSIM) $(NMESHPP) --vtk sphereincube.nmesh.h5 mesh.vtk

mayavi: mesh.vtk
	mayavi -d mesh.vtk -m SurfaceMap


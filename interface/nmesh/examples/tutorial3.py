import nmesh

cigar = nmesh.ellipsoid( [4,2] )

bbox = [[-5,-5],[5,5]]

mesh = nmesh.mesh(objects=[cigar], bounding_box=bbox, a0=0.5,
		  mesh_bounding_box=True)

nmesh.visual.plot2d_ps( mesh, "tutorial3.ps")

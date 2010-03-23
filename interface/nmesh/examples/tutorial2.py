import nmesh

cigar = nmesh.ellipsoid( [4,2] )

bbox = [[-5,-5],[5,5]]

mesh = nmesh.mesh(objects=[cigar], bounding_box=bbox,
		  mesh_bounding_box=True)

mesh.save("tutorial2.nmesh")

nmesh.visual.plot2d_ps( mesh, "tutorial2.ps")



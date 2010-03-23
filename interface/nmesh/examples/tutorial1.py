import nmesh

cigar = nmesh.ellipsoid( [4,2] )

bbox = [[-4,-2],[4,2]]

mesh = nmesh.mesh(objects=[cigar],bounding_box=bbox)

nmesh.visual.plot2d_ps( mesh, "tutorial1.ps")

import nmesh

large = nmesh.box( [-4,-4], [4,4] )
small = nmesh.box( [-2,-2], [2,2] )
diff = nmesh.difference(large,[small])

bbox=[[-4,-4],[4,4]]
mesh = nmesh.mesh(bounding_box=bbox, objects = [diff] )
nmesh.visual.plot2d_ps( mesh, "fixedpoints_faulty.ps")


#can provide some 'fixed points' to avoid round corners
fixed_points = [[-2,-2],[2,-2],[-2,2],[2,2]]
mesh = nmesh.mesh(bounding_box=bbox, objects = [diff], fixed_points=fixed_points )
nmesh.visual.plot2d_ps( mesh, "fixedpoints.ps")


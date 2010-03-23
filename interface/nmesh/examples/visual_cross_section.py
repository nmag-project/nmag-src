"""Compounds outer_skin() with order_mesh() to obtain a cross-section
   of the outer skin of an ellipsoid.

   Author: James Kenny           Last modified: $Date$
"""
import nmesh

ellipsoid = nmesh.ellipsoid([1.5,2.5,2])

# create mesh
bbox = [[-1.5,-2.5,-2],[1.5,2.5,2]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5)

# now extract the outer layer of cells and order the mesh in the y-axis
# to see an outer skin with only elements containing at least 2 surface
# points, the user should uncomment the end of the 2nd following line
mesh_info = mesh.tolists()
mesh_info = nmesh.visual.outer_skin(mesh_info)#, condition='>=2')


# visualise with in2circ as a solid and order it in the y-axis
v = nmesh.visual.solid_in2circ(mesh_info, order=1)

    





"""This example demonstrates the 'order_mesh()' function which enables the user   to ensure that the mesh simplices are indexed in such a way that the Filter
   'ExtractUnstructuredGrid' can be employed to view the interior of the mesh.
"""
import nmesh

ellipsoid = nmesh.ellipsoid([0.75,1.25,1])

# create mesh
bbox = [[-0.75,-1.25,-1],[0.75,1.25,1]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5)

# now order the mesh in the y-axis and display it
# the user should change the values "CellMaximum" and "CellMinimum" in
# the dialog box that appears in MayaVi - then click 'apply'
mesh_info = mesh.tolists()
v = nmesh.visual.solid_in2circ(mesh_info, order=1)



    





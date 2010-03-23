import Numeric, math
import nmesh


def scale_list_of_lists( vec_data, factor ):
    """silly function -- just to change the vector data somehow.

    In real life, the simulation would provide this data, obviously."""
    v2=[]
    for i in range(len(vec_data)):
        v2.append(vec_data[i] + Numeric.array([-1,0,0])*factor)

    return v2


#create mesh
ellipsoid = nmesh.ellipsoid([1,1,0.5])
bbox = [[-1,-1,-1],[1,1,1]]
mesh = nmesh.mesh(objects=[ellipsoid],bounding_box=bbox,a0=0.5,cache_name="visual_plot_vectorfield")

#visualise
import nmesh.visual
meshinfo=mesh.tolists()

#create some vector field defind on vertices (=nodes)
vec_data = []
points=meshinfo[0][2]
origin = Numeric.array([0,0,0])
for point in points:
    vec_data.append(Numeric.array(point) - origin) 

#plot the vector field with changing data
for i in range(200):
    #modify vector data from iteration to iteration
    fac = math.sin(i/100.0*2*math.pi)
    vec_data2=scale_list_of_lists( vec_data, fac )

    #plot the updated vector field on the mesh
    myv = nmesh.visual.plot_vectorfield_on_mesh( meshinfo, vec_data2)

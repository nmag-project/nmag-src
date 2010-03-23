import nmesh

box1 = nmesh.box( [0.0,0.0],[3.0,3.0] )
box2 = nmesh.box( [-1.0,-1.0],[-3.0,-3.0] )
circle = nmesh.ellipsoid(length=[1.5,1.5],transform=[("shift",[-2.,2.])])

bbox = [[-4,-4],[4,4]]

#define call back function
def my_fun(piece_nr, iteration_nr, mesh_info):
    print "** In callback function: Piece %d, Step %d \n" % \
          (piece_nr, iteration_nr)
    
    print "Points = %d\nSimplices = %d\nSurface elements = %d\n" % \
          (len(mesh_info[0][2]),len(mesh_info[2][2]),len(mesh_info[4][2]))

#Call callback function every 5 iterations
mesh = nmesh.mesh(objects = [box1,box2,circle], bounding_box=bbox, \
                  a0=0.5, callback = (my_fun,5))

nmesh.visual.plot2d_ps(mesh,"callback.ps")


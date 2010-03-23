import nmesh

squares = []

for i in range(4):
    xshift = i*6
    yshift = 0
    squares.append( nmesh.box( [-2,-2],[2,2],
                               transform=[("shift",[xshift,yshift])]
                               )
                    )

bbox = [[-5,-5],[23,5]]
mesh = nmesh.mesh(objects=squares,bounding_box=bbox)

# plot mesh
nmesh.visual.plot2d_ps(mesh,"shift.ps")

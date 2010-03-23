import nmesh


cube1 = nmesh.box( [-0.,-0.,-0.],[1.0,1.0,1.0])
cube2 = nmesh.box( [1.,-0.,-0.],[2.0,1.0,1.0])

simply_points = [ [0.,0.,0.],
                  [0.,0.,1.],
                  [0.,1.,0.],
                  [0.,1.,1.],
                  [1.,0.,0.],
                  [1.,0.,1.],
                  [1.,1.,0.],
                  [1.,1.,1.],
                  [0.4,1.0,0.5],
                  [1.,0.,0.],
                  [1.,0.,1.],
                  [1.,1.,0.],
                  [1.,1.,1.],
                  [2.,0.,0.],
                  [2.,0.,1.],
                  [2.,1.,0.],
                  [2.,1.,1.],
                  [1.6,1.0,0.5],
                 ]




bbox = [[0.0,0.0, 0.0],[2.0,1.0,1.0]]

mesh = nmesh.mesh(objects = [cube2,cube1], bounding_box=bbox,
                  a0=0.6, simply_points = simply_points)

mesh.save("test-broken-mesh.nmesh")


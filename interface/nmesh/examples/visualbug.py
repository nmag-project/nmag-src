import nmesh

# rotation of the object is performed
# around the [0,0,1] axis (z-axis). 
box = nmesh.box( [0,0,0],[2,1,1]) #, transform=[("rotate",[0,1],0)])

bbox = [[-3,-3,-3],[3,3,3]]
mesh = nmesh.mesh(objects=[box],bounding_box=bbox,mesh_bounding_box=False,a0=0.5)

mesh.save('visualbug.nmesh')

#note that surface element 23 is this;
#In [19]: mesh_info[4][2][23]
#Out[19]: ([61, 34, 59], ([1.0, 1.0, 1.0], 1.0), ([1.0, 1.0, 1.0], 1.0), 1)

#Now point 61 looks like this:
#mesh_info[0][2][61]
#Out[21]: [1.0385231224921063, 0.41784602829813045, 0.77908298307823132]

#In [24]: mesh_info[3][2][61]
#Out[24]: [1]

#So it appears to belong only to one body (and thus is not added to coordsTranslate, see if statement in line 354)

#and can therefore not be found when we need it in line 


#create 3d-plot of surfaces and export eps, this fails:
vis = nmesh.visual.show_surfaces_mayavi(mesh)

with this message:
#      nmesh:2006-09-13 11:35:28,416    main.py   42    INFO Have writtend mesh to visualbug.nmesh
#Traceback (most recent call last):
#  File "<string>", line 1, in ?
#  File "visualbug.py", line 29, in ?
#    vis = nmesh.visual.show_surfaces_mayavi(mesh)
#  File "/home/fangohr/cvs/ocaml/interface/nmesh/visual.py", line 1637, in show_surfaces_mayavi
#    mesh_info2 = surface_only(mesh.tolists())
#  File "/home/fangohr/cvs/ocaml/interface/nmesh/visual.py", line 368, in surface_only
#    translate.append(coordsTranslate[j])
#KeyError: 61


raw_input()
nmesh.visual.export_visualisation(vis,"visualbug.eps")


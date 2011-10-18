"""This file reads a file of the Surface Mesh Format created by Netgen and
converts it into a file, which can be read by the Library HLib.
The filename of the Netgen file is taken as command line argument. The
filename of the created HLib file will have the same name preceded with
hlib_"""

import sys
from numpy import zeros



if (2 == len(sys.argv)):
    filename = sys.argv[1]
else:
    print "\nThere should just be one command line argument, program aborted!\n"
    exit(1)

file = open(filename,"r")

mesh = file.readlines()
line = mesh[0].split()
if("surfacemesh" != line[0]):
    print "\nThe file should be a surface mesh file created by Netgen!\n"
    exit(1)
line = mesh[1].split()
vertices_vol = int(line[0])
vertice = zeros((vertices_vol,3),dtype=float)
filerow = 2
for i in range(vertices_vol):
    line = mesh[filerow].split()
    vertice[i,0] = float(line[0]);
    vertice[i,1] = float(line[1]);
    vertice[i,2] = float(line[2]);
    filerow = filerow + 1;

line = mesh[filerow].split()
tri_number = int(line[0])
filerow = filerow + 1
triangle = zeros((tri_number,3),dtype=int)
tri2edge = zeros((tri_number,3),dtype=int)
node_number = 0
node_check = zeros(vertices_vol,dtype=int) #for check that surface nodes at beginning of array vertice
for i in range(tri_number):
    line = mesh[filerow].split()
    triangle[i,0] = int(line[0]) - 1  #Netgen indexing starts with 1, hlib with 0 -> conversion
    if (triangle[i,0] >= node_number):
        node_number = triangle[i,0] + 1
    if (node_check[triangle[i,0]] == 0):
        node_check[triangle[i,0]] = 1
        
    triangle[i,1] = int(line[1]) - 1
    if (triangle[i,1] >= node_number):
        node_number = triangle[i,1] + 1
    if(node_check[triangle[i,1]] == 0):
        node_check[triangle[i,1]] = 1
        
    triangle[i,2] = int(line[2]) - 1
    if (triangle[i,2] >= node_number):
        node_number = triangle[i,2] + 1
    if(node_check[triangle[i,2]] == 0):
        node_check[triangle[i,2]] = 1
        
    filerow = filerow + 1

file.close()

#check wether the first node_number entries in the array vertice are surface nodes
for i in range(vertices_vol):
    if (node_check[i] == 0 and i < node_number):
        print "Error: The surface nodes are a block at the beginning.\n"
        exit(1)

#fill the array of surface nodes
node = zeros((node_number,3),dtype=float)
for i in range(node_number):
    node[i,:] = vertice[i,:]

filename = "hlib_" + filename  #The filename of the new file

edge_number = 3*tri_number / 2;
edge  = zeros((edge_number,2),dtype=int)
edge1 = zeros((2*edge_number,2),dtype=int)
j=0
for i in range(tri_number):
    edge1[j,0] = triangle[i,0]
    edge1[j,1] = triangle[i,1]
    j=j+1
    edge1[j,0] = triangle[i,1]
    edge1[j,1] = triangle[i,2]
    j=j+1
    edge1[j,0] = triangle[i,2]
    edge1[j,1] = triangle[i,0]
    j=j+1

k=0
for i in range(edge_number):
    while (0 == edge1[k,0] and 0 == edge1[k,1]):
        k=k+1
    edge[i,:] = edge1[k,:]
    edge1[k,0] = 0
    edge1[k,1] = 0
    for j in range(k+1,2*edge_number):
        if (edge[i,0] in edge1[j,:]) and (edge[i,1] in edge1[j,:]):
            edge1[j,0] = 0
            edge1[j,1] = 0
            break

for i in range(tri_number):
    for j in range(3):
        i1=triangle[i][(j+1)%3]
        i2=triangle[i][(j+2)%3]
        for k in range(edge_number):
            if ((edge[k][0]==i1 and edge[k][1]==i2) or (edge[k][0]==i2 and edge[k][1]==i1)): 
                tri2edge[i][j] = k
                break

outputfile = open(filename,"w")

outputfile.write("%8d %8d %8d\n" % (node_number, edge_number, tri_number))
for i in range(node_number):
    outputfile.write("%14.6f %14.6f %14.6f\n" % (node[i,0],node[i,1],node[i,2]))

for i in range(edge_number):
    outputfile.write("%8d %8d\n" % (edge[i,0],edge[i,1]))

for i in range(tri_number):
    outputfile.write("%8d %8d %8d %8d %8d %8d\n" % (triangle[i,0],triangle[i,1],triangle[i,2],\
                                                    tri2edge[i,0],tri2edge[i,1],tri2edge[i,2]))

outputfile.close()


print "\nPROGRAM TEST HAS BEEN SUCCESSFUL!!\n"

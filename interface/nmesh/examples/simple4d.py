import nmesh

ellipsoid = nmesh.ellipsoid([0.75,1.25,1,0.9])

bbox = [[-1,-1.5,-1.5,-1],[1,1.5,1.5,1]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,
                  a0=0.5)

mesh.save("simple4d.nmesh")
print "should have saved mesh now"

#create gnuplot plot for manual (in other file not relevant here)


import os, os.path, sys, time 

filename="run_simple4d/simple4d.nmesh"
if not os.path.exists(filename):
    print "You need to run simple4d.py first to compute %s" % filename
    sys.exit(1)

def gnuplot_2d_points( points, filename ):

    print "Starting to write %s" % filename,
    print time.asctime()
    
    """Given a list of pairs like
      points = [ [x0,y0], [x1,y1], [x2,y2], ..., [xN,yN]]

      and a filename, this will create a postscriptfile of name
      filename which contains a gnuplot plot of the positions of
      the points xi,yi.
      """
    
    assert len(points)>0, "need some points to plot"
    assert len(points[0])==2, "can only deal with 2d set of points"
    
    tmpfilename1 = 'run_simple4d/_points.dat'
    f=open(tmpfilename1,'w')
    for point in points:
        f.write("%f %f\n" % (point[0],point[1]))
    f.close()

    tmpfilename2 = 'run_simple4d/_myplottmp.gnu'
    f=open(tmpfilename2,'w')
    gnuplot_header="""set terminal postscript eps noenhanced monochrome blacktext dashed dashlength 1.0 linewidth 1.0 defaultplex palfuncparam 2000,0.003 butt "Helvetica" 14
set output '%s'
plot "%s"\n """ % (filename,tmpfilename1)
    f.write(gnuplot_header)
    f.close()

    os.system("gnuplot %s" % tmpfilename2)
    os.remove(tmpfilename1)
    os.remove(tmpfilename2)





import nmesh

print "About to load mesh from file", time.asctime()
mesh = nmesh.load("run_simple4d/simple4d.nmesh")


print "About to convert mesh to lists", time.asctime()
lists = mesh.tolists()

print "Finished converting mesh to lists", time.asctime()

points = lists[0][2]

#extract 2d subsets

print "About to extract projects into 2d planes"
x01 = map( lambda x : [x[0],x[1]], points)
x12 = map( lambda x : [x[1],x[2]], points)
x23 = map( lambda x : [x[2],x[3]], points)
x30 = map( lambda x : [x[3],x[0]], points)
print "Finished extracting 2d projections", time.asctime()

#each of these plots shows a filled circle. this is what we
#expect when projecting the 4d points into any of the two
#dimensions.

gnuplot_2d_points( x01, "simple4d_proj01.eps" )
gnuplot_2d_points( x12, "simple4d_proj12.eps" )
gnuplot_2d_points( x23, "simple4d_proj23.eps" )
gnuplot_2d_points( x30, "simple4d_proj30.eps" )


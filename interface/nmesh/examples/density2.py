import nmesh

box1= nmesh.ellipsoid([1.5,1.5])
box2= nmesh.box([-2.0,6.0],[2.0,7.0])

densitystring="""
double upper=7.1;
double lower=5.9;
double left =-2.1;
double right=2.1;
if ((x[0] < right) && (x[0]>left) && (x[1]>lower) && (x[1]<upper)) 
  {density = 4.0;}  /* mesh upper box with 4 times higher density 
	               Note that sudden changes in the density will
                       result in poor mesh quality */
else {
  double sigma=1.0; 
  double xpos=0.0; 
  double ypos=0.0;
  double xdev = xpos-x[0];
  double ydev = ypos-x[1];
  double rdev2 = xdev*xdev+ydev*ydev;
  /* increase density towards centre of sphere */
  density=1.0+4.0*exp(-rdev2/(sigma*sigma));
}
"""

mesh = nmesh.mesh( objects=[box1,box2], bounding_box=[[-3,-3],[3,9]],
                   mesh_bounding_box=True, a0=0.7,density = densitystring)

nmesh.visual.plot2d_ps(mesh,"density2.ps")

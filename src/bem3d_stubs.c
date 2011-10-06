/* (C) 2007 Dr. Thomas Fischbacher */

#define PI 3.141592653589793238462643383279502884

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <math.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#define V_LEN(v) (sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]))
#define V_SUB(target,x,y) (target[0]=x[0]-y[0],target[1]=x[1]-y[1],target[2]=x[2]-y[2])
#define SPROD(x,y) (x[0]*y[0]+x[1]*y[1]+x[2]*y[2])
#define XPROD(target,x,y) (target[0]=x[1]*y[2]-x[2]*y[1],target[1]=x[2]*y[0]-x[0]*y[2],target[2]=x[0]*y[1]-x[1]*y[0])

#define MAXGEO(x) do {double ax=fabs(x);if(ax>max_geometry)max_geometry=ax;}while(0)
#define MAXGEOL(x) do {long double ax=fabsl(x);if(ax>max_geometry)max_geometry=ax;}while(0)

CAMLprim value caml_bem3d_raw_lindholm(value ml_min_dist, value ml_args)
{
  CAMLparam2(ml_min_dist,ml_args);

  double min_dist;

  static double outward_surface_normal[3],observer[3],
    p0[3],p1[3],p2[3],
    r0[3],r1[3],r2[3],
    s0[3],s1[3],s2[3],
    xi0[3],xi1[3],xi2[3],
    buffer0[3],buffer1[3],buffer2[3],
    v_zeta[3], eta0[3],eta1[3],eta2[3],
    v_log[3],gamma[3][3],lh[3];

  static double zeta, norm, inv_norm, len_buffer2,
    area, angle, r0l, r1l, r2l, s0l, s1l, s2l,
    c01, c12, c20, sp01, sp12, sp20, numerator, denominator, z;

  CAMLlocal3(ml_osn,ml_store_lh012,ml_observer);
  CAMLlocal3(ml_p0,ml_p1,ml_p2);

  min_dist=Double_val(ml_min_dist);

  ml_osn=Field(ml_args,0);
  ml_store_lh012=Field(ml_args,1);
  ml_observer=Field(ml_args,2);
  ml_p0=Field(ml_args,3);
  ml_p1=Field(ml_args,4);
  ml_p2=Field(ml_args,5);

  Store_double_field(ml_store_lh012,0,0.0);
  Store_double_field(ml_store_lh012,1,0.0);
  Store_double_field(ml_store_lh012,2,0.0);
  /* ^ This allows us to quit early should something go wrong. */

  
  outward_surface_normal[0]=Double_field(ml_osn,0);
  outward_surface_normal[1]=Double_field(ml_osn,1);
  outward_surface_normal[2]=Double_field(ml_osn,2);

  observer[0]=Double_field(ml_observer,0);
  observer[1]=Double_field(ml_observer,1);
  observer[2]=Double_field(ml_observer,2);

  p0[0]=Double_field(ml_p0,0);
  p0[1]=Double_field(ml_p0,1);
  p0[2]=Double_field(ml_p0,2);

  p1[0]=Double_field(ml_p1,0);
  p1[1]=Double_field(ml_p1,1);
  p1[2]=Double_field(ml_p1,2);

  p2[0]=Double_field(ml_p2,0);
  p2[1]=Double_field(ml_p2,1);
  p2[2]=Double_field(ml_p2,2);

  V_SUB(r0,p0,observer);
  V_SUB(r1,p1,observer);
  V_SUB(r2,p2,observer);

  r0l=V_LEN(r0);
  r1l=V_LEN(r1);
  r2l=V_LEN(r2);
  
  if(r0l<=min_dist || r1l<=min_dist || r2l<=min_dist) {CAMLreturn(Val_unit);}
  /* "observer is on a triangle vertex" */

  V_SUB(s0,r2,r1);
  V_SUB(s1,r0,r2);
  V_SUB(s2,r1,r0);

  s0l=V_LEN(s0);
  if(s0l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s0l;}
  xi0[0]=s0[0]*inv_norm,xi0[1]=s0[1]*inv_norm,xi0[2]=s0[2]*inv_norm;

  s1l=V_LEN(s1);
  if(s1l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s1l;}
  xi1[0]=s1[0]*inv_norm,xi1[1]=s1[1]*inv_norm,xi1[2]=s1[2]*inv_norm;

  s2l=V_LEN(s2);
  if(s2l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s2l;}
  xi2[0]=s2[0]*inv_norm,xi2[1]=s2[1]*inv_norm,xi2[2]=s2[2]*inv_norm;

  V_SUB(buffer0,p1,p0);
  V_SUB(buffer1,p2,p0);

  XPROD(buffer2,buffer0,buffer1);
  
  len_buffer2=sqrt(buffer2[0]*buffer2[0]+buffer2[1]*buffer2[1]+buffer2[2]*buffer2[2]);
  if(len_buffer2==0.0) {CAMLreturn(Val_unit);}

  area=0.5*len_buffer2;
  inv_norm=1.0/len_buffer2;
  v_zeta[0]=buffer2[0]*inv_norm, v_zeta[1]=buffer2[1]*inv_norm, v_zeta[2]=buffer2[2]*inv_norm;

  c01=SPROD(xi0,xi1);
  c12=SPROD(xi1,xi2);
  c20=SPROD(xi2,xi0);

  zeta=SPROD(v_zeta,r0);

  XPROD(eta0,v_zeta,xi0);
  XPROD(eta1,v_zeta,xi1);
  XPROD(eta2,v_zeta,xi2);

  v_log[0]=log((r1l+r2l+s0l)/(r1l+r2l-s0l));
  v_log[1]=log((r2l+r0l+s1l)/(r2l+r0l-s1l));
  v_log[2]=log((r0l+r1l+s2l)/(r0l+r1l-s2l));

  gamma[0][0] = 1.0, gamma[0][1] = c01, gamma[0][2] = c20,
  gamma[1][0] = c01, gamma[1][1] = 1.0, gamma[1][2] = c12,
  gamma[2][0] = c20, gamma[2][1] = c12, gamma[2][2] = 1.0;

  sp01=SPROD(r0,r1);
  sp12=SPROD(r1,r2);
  sp20=SPROD(r2,r0);

  numerator=r0l*r1l*r2l+r0l*sp12+r1l*sp20+r2l*sp01;
  denominator=sqrt(2.0*(r1l*r2l+sp12)*(r2l*r0l+sp20)*(r0l*r1l+sp01));
  
  if(denominator==0.0) {CAMLreturn(Val_unit);}

  angle=(denominator==0.0? 0.0 : numerator/denominator);
  if(angle<-1.0) {angle = -1.0;}
  else if(angle>1.0) {angle=1.0;}
  angle=2.0*acos(angle);

  if(zeta<0.0) {angle=-angle;}

  denominator=1.0/(8.0*PI*area);

  lh[0]=s0l*denominator*(angle*SPROD(eta0,r1)-zeta*SPROD(gamma[0],v_log));
  lh[1]=s1l*denominator*(angle*SPROD(eta1,r2)-zeta*SPROD(gamma[1],v_log));
  lh[2]=s2l*denominator*(angle*SPROD(eta2,r0)-zeta*SPROD(gamma[2],v_log));

  /* fprintf(stderr,"OK %10.8f %10.8f %10.8f\n",lh[0],lh[1],lh[2]);fflush(stderr); */

  if(SPROD(v_zeta,outward_surface_normal)<0.0)
    {
      Store_double_field(ml_store_lh012,0,-lh[0]);
      Store_double_field(ml_store_lh012,1,-lh[1]);
      Store_double_field(ml_store_lh012,2,-lh[2]);
    }
  else
    {
      Store_double_field(ml_store_lh012,0,lh[0]);
      Store_double_field(ml_store_lh012,1,lh[1]);
      Store_double_field(ml_store_lh012,2,lh[2]);
    }

  /*
  fprintf(stderr,"DDD LH contrib area=%f angle=%f,\n eta0=%f %f %f\n r1=%f %f %f\n observer=%f %f %f\n p0=%f %f %f\n p1=%f %f %f\n p2=%f %f %f\n lh012=%f %f %f\n s[012]l=%f %f %f\n v:log=%f %f %f\n angle_summands=%f %f %f\n zeta_summands=%f %f %f\n", area, angle, eta0[0],eta0[1],eta0[2], r1[0],r1[1],r1[2],observer[0],observer[1],observer[2],p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],lh[0],lh[1],lh[2],s0l,s1l,s2l,v_log[0],v_log[1],v_log[2],(s0l/(area*8*PI))*(angle*SPROD(eta0,r1)),(s1l/(area*8*PI))*(angle*SPROD(eta1,r2)),(s2l/(area*8*PI))*(angle*SPROD(eta2,r0)),(s0l/(area*8*PI))*(zeta*SPROD(gamma[0],v_log)),(s1l/(area*8*PI))*(zeta*SPROD(gamma[1],v_log)),(s2l/(area*8*PI))*(zeta*SPROD(gamma[2],v_log)));fflush(stderr);
  */


  CAMLreturn(Val_unit);
}

/* Same code (cut&paste, unfortunately), with long double precision: */

CAMLprim value caml_bem3d_raw_lindholm_longdouble(value ml_min_dist, value ml_args)
{
  CAMLparam2(ml_min_dist,ml_args);

  double min_dist;

  static long double outward_surface_normal[3],observer[3],
    p0[3],p1[3],p2[3],
    r0[3],r1[3],r2[3],
    s0[3],s1[3],s2[3],
    xi0[3],xi1[3],xi2[3],
    buffer0[3],buffer1[3],buffer2[3],
    v_zeta[3], eta0[3],eta1[3],eta2[3],
    v_log[3],gamma[3][3],lh[3], max_geometry=0.0, geo_eps;

  static long double zeta, norm, inv_norm, len_buffer2,
    area, angle, r0l, r1l, r2l, s0l, s1l, s2l,
    c01, c12, c20, sp01, sp12, sp20, numerator, denominator, z;

  CAMLlocal3(ml_osn,ml_store_lh012,ml_observer);
  CAMLlocal3(ml_p0,ml_p1,ml_p2);

  min_dist=Double_val(ml_min_dist);

  ml_osn=Field(ml_args,0);
  ml_store_lh012=Field(ml_args,1);
  ml_observer=Field(ml_args,2);
  ml_p0=Field(ml_args,3);
  ml_p1=Field(ml_args,4);
  ml_p2=Field(ml_args,5);

  Store_double_field(ml_store_lh012,0,0.0);
  Store_double_field(ml_store_lh012,1,0.0);
  Store_double_field(ml_store_lh012,2,0.0);
  /* ^ This allows us to quit early should something go wrong. */

  /* We sill short-circuit the computation when some denominator
     becomes zero or the like.  But actually, if we think about cross
     products, a number close to zero may actually represent some true
     zero. So, we need a notion of "small enough to be considered as
     zero".  In order to find this, we use "max_geometry" to find the
     largest relevant geometrical number (or something roughly like it)
     and take 1e-14 of that.
   */
  
  outward_surface_normal[0]=Double_field(ml_osn,0);
  outward_surface_normal[1]=Double_field(ml_osn,1);
  outward_surface_normal[2]=Double_field(ml_osn,2);

  observer[0]=Double_field(ml_observer,0);
  observer[1]=Double_field(ml_observer,1);
  observer[2]=Double_field(ml_observer,2);

  p0[0]=Double_field(ml_p0,0);
  p0[1]=Double_field(ml_p0,1);
  p0[2]=Double_field(ml_p0,2);

  p1[0]=Double_field(ml_p1,0);
  p1[1]=Double_field(ml_p1,1);
  p1[2]=Double_field(ml_p1,2);

  p2[0]=Double_field(ml_p2,0);
  p2[1]=Double_field(ml_p2,1);
  p2[2]=Double_field(ml_p2,2);

  MAXGEOL(observer[0]); MAXGEOL(observer[1]); MAXGEOL(observer[2]);
  MAXGEOL(p0[0]); MAXGEOL(p0[1]); MAXGEOL(p0[2]);
  MAXGEOL(p1[0]); MAXGEOL(p1[1]); MAXGEOL(p1[2]);
  MAXGEOL(p2[0]); MAXGEOL(p2[1]); MAXGEOL(p2[2]);

  geo_eps=max_geometry*1e-13;

  /*
  fprintf(stderr,"max_geometry=%g, geo_eps=%g\n",(double)max_geometry,(double)geo_eps);fflush(stderr); / * DDD * /
  fprintf(stderr," p0=%.16f %.16f %.16f\n p1=%.16f %.16f %.16f\n p2=%.16f %.16f %.16f\n obs=%.16f %.16f %.16f\n",
	  (double)p0[0],(double)p0[1],(double)p0[2],
	  (double)p1[0],(double)p1[1],(double)p1[2],
	  (double)p2[0],(double)p2[1],(double)p2[2],
	  (double)observer[0],(double)observer[1],(double)observer[2]);
  fflush(stderr);
  */

  V_SUB(r0,p0,observer);
  V_SUB(r1,p1,observer);
  V_SUB(r2,p2,observer);

  r0l=V_LEN(r0);
  r1l=V_LEN(r1);
  r2l=V_LEN(r2);
  
  if(r0l<=min_dist || r1l<=min_dist || r2l<=min_dist) 
    {
      /* fprintf(stderr, "PHI early exit - triangle vertex!\n");fflush(stderr); / * DDD */
      CAMLreturn(Val_unit);
    }

  V_SUB(s0,r2,r1);
  V_SUB(s1,r0,r2);
  V_SUB(s2,r1,r0);

  s0l=V_LEN(s0);
  if(s0l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s0l;}
  xi0[0]=s0[0]*inv_norm,xi0[1]=s0[1]*inv_norm,xi0[2]=s0[2]*inv_norm;

  s1l=V_LEN(s1);
  if(s1l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s1l;}

  xi1[0]=s1[0]*inv_norm,xi1[1]=s1[1]*inv_norm,xi1[2]=s1[2]*inv_norm;

  s2l=V_LEN(s2);
  if(s2l==0.0) {CAMLreturn(Val_unit);} else {inv_norm=1.0/s2l;}

  xi2[0]=s2[0]*inv_norm,xi2[1]=s2[1]*inv_norm,xi2[2]=s2[2]*inv_norm;

  V_SUB(buffer0,p1,p0);
  V_SUB(buffer1,p2,p0);

  XPROD(buffer2,buffer0,buffer1);
  
  len_buffer2=sqrtl(buffer2[0]*buffer2[0]+buffer2[1]*buffer2[1]+buffer2[2]*buffer2[2]);
  if(len_buffer2==0.0) {CAMLreturn(Val_unit);}

  area=0.5*len_buffer2;
  inv_norm=1.0/len_buffer2;
  v_zeta[0]=buffer2[0]*inv_norm, v_zeta[1]=buffer2[1]*inv_norm, v_zeta[2]=buffer2[2]*inv_norm;

  c01=SPROD(xi0,xi1);
  c12=SPROD(xi1,xi2);
  c20=SPROD(xi2,xi0);

  zeta=SPROD(v_zeta,r0);

  if(fabsl(zeta)<geo_eps)
    {
      /* fprintf(stderr, "PHI early exit - ZETA=0!\n");fflush(stderr); / * DDD */
      CAMLreturn(Val_unit);
    }

  XPROD(eta0,v_zeta,xi0);
  XPROD(eta1,v_zeta,xi1);
  XPROD(eta2,v_zeta,xi2);

  v_log[0]=logl((r1l+r2l+s0l)/(r1l+r2l-s0l));
  v_log[1]=logl((r2l+r0l+s1l)/(r2l+r0l-s1l));
  v_log[2]=logl((r0l+r1l+s2l)/(r0l+r1l-s2l));

  gamma[0][0] = 1.0, gamma[0][1] = c01, gamma[0][2] = c20,
  gamma[1][0] = c01, gamma[1][1] = 1.0, gamma[1][2] = c12,
  gamma[2][0] = c20, gamma[2][1] = c12, gamma[2][2] = 1.0;

  sp01=SPROD(r0,r1);
  sp12=SPROD(r1,r2);
  sp20=SPROD(r2,r0);

  numerator=r0l*r1l*r2l+r0l*sp12+r1l*sp20+r2l*sp01;
  denominator=sqrtl(2.0*(r1l*r2l+sp12)*(r2l*r0l+sp20)*(r0l*r1l+sp01));
  
  if(denominator<geo_eps) {CAMLreturn(Val_unit);}
  
  angle=numerator/denominator;
  if(angle<-1.0) {angle = -1.0;}
  else if(angle>1.0) {angle=1.0;}
  angle=2.0*acosl(angle);

  if(zeta<0.0) {angle=-angle;}
  
  denominator=1.0/(8.0*PI*area);
  
  lh[0]=s0l*denominator*(angle*SPROD(eta0,r1)-zeta*SPROD(gamma[0],v_log));
  lh[1]=s1l*denominator*(angle*SPROD(eta1,r2)-zeta*SPROD(gamma[1],v_log));
  lh[2]=s2l*denominator*(angle*SPROD(eta2,r0)-zeta*SPROD(gamma[2],v_log));

  /* fprintf(stderr,"OK %10.8f %10.8f %10.8f\n",lh[0],lh[1],lh[2]);fflush(stderr); */

  if(SPROD(v_zeta,outward_surface_normal)<0.0)
    {
      Store_double_field(ml_store_lh012,0,-(double)lh[0]);
      Store_double_field(ml_store_lh012,1,-(double)lh[1]);
      Store_double_field(ml_store_lh012,2,-(double)lh[2]);
    }
  else
    {
      Store_double_field(ml_store_lh012,0,(double)lh[0]);
      Store_double_field(ml_store_lh012,1,(double)lh[1]);
      Store_double_field(ml_store_lh012,2,(double)lh[2]);
    }

  CAMLreturn(Val_unit);
}

/* ================================================================= */

/* NOTE: this is unused at present, but will replace the above function(s)
   in the future. The underlying idea is two-fold:

   (1) We want to be able to apply a linear transformation to the
   displaced image points of the surface (to implement nontrivial
   crystal symmetries).

   (2) We do not have to redo the triangle-specific coordinate system
   gamma over and over again.


   Args is:

   p0
   p1
   p2
   triangle-outward-surface-normal: float array, length 3
   store_lh012: (float array array of length 3, each entry having
                length #observers - we add contributions to that array),

   ortho_transformation: orthogonal transformation to be applied to the
                         displaced copy of the triangle (Note: we instead
			 apply the inverse transformation to the observer)

   greyfactors: float array
   displacements: float array array

   observer_points: float array array

   At the higher level, we will call this over and over again, once
   for every row+ortho_transformation, to build the BEM matrix.

 */

CAMLprim value caml_bem3d_raw_lindholm_block_pbc(value ml_min_dist, value ml_args)
{
  CAMLparam2(ml_min_dist,ml_args);

  int j,k, nr_observer_points, nr_displacements;
  double min_dist, factor, max_geometry=0.0, geo_eps;

  static double v[3], outward_surface_normal[3],observer[3],
    p0[3],p1[3],p2[3],
    r0[3],r1[3],r2[3],
    s0[3],s1[3],s2[3],
    xi0[3],xi1[3],xi2[3],
    v_zeta[3], eta0[3],eta1[3],eta2[3],
    v_log[3],gamma[3][3],lh[3],
    otrans[3][3];

  static double zeta, norm, inv_norm, len,
    area, angle, r0l, r1l, r2l, s0l, s1l, s2l,
    c01, c12, c20, sp01, sp12, sp20, numerator, denominator,
    area_denominator,greyfactor;

  CAMLlocal3(ml_p0,ml_p1,ml_p2);
  CAMLlocal3(ml_osn,ml_store_lh012,ml_ortho_trans);
  CAMLlocal3(ml_greyfactors,ml_displacements,ml_observer_points);

  min_dist=Double_val(ml_min_dist);

  ml_p0=Field(ml_args,0);
  ml_p1=Field(ml_args,1);
  ml_p2=Field(ml_args,2);
  ml_osn=Field(ml_args,3);
  ml_store_lh012=Field(ml_args,4);
  ml_ortho_trans=Field(ml_args,5);
  ml_greyfactors=Field(ml_args,6);
  ml_displacements=Field(ml_args,7);
  ml_observer_points=Field(ml_args,8);

  p0[0]=Double_field(ml_p0,0);
  p0[1]=Double_field(ml_p0,1);
  p0[2]=Double_field(ml_p0,2);

  p1[0]=Double_field(ml_p1,0);
  p1[1]=Double_field(ml_p1,1);
  p1[2]=Double_field(ml_p1,2);

  p2[0]=Double_field(ml_p2,0);
  p2[1]=Double_field(ml_p2,1);
  p2[2]=Double_field(ml_p2,2);

  MAXGEO(p0[0]); MAXGEO(p0[1]); MAXGEO(p0[2]);
  MAXGEO(p1[0]); MAXGEO(p1[1]); MAXGEO(p1[2]);
  MAXGEO(p2[0]); MAXGEO(p2[1]); MAXGEO(p2[2]);

  outward_surface_normal[0]=Double_field(ml_osn,0);
  outward_surface_normal[1]=Double_field(ml_osn,1);
  outward_surface_normal[2]=Double_field(ml_osn,2);

  for(j=0;j<3;j++)for(k=0;k<3;k++)otrans[j][k]=Double_field(Field(ml_ortho_trans,j),k);

  nr_displacements=Wosize_val(ml_displacements);
  nr_observer_points=Wosize_val(ml_observer_points);

  /* Setup triangle coordinate system */

  V_SUB(s0,p2,p1);
  V_SUB(s1,p0,p2);
  V_SUB(s2,p1,p0);

  s0l=V_LEN(s0);
  if(s0l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s0l;}
  xi0[0]=s0[0]*inv_norm,xi0[1]=s0[1]*inv_norm,xi0[2]=s0[2]*inv_norm;

  s1l=V_LEN(s1);
  if(s1l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s1l;}
  xi1[0]=s1[0]*inv_norm,xi1[1]=s1[1]*inv_norm,xi1[2]=s1[2]*inv_norm;

  s2l=V_LEN(s2);
  if(s2l==0.0){CAMLreturn(Val_unit);} else {inv_norm=1.0/s2l;}
  xi2[0]=s2[0]*inv_norm,xi2[1]=s2[1]*inv_norm,xi2[2]=s2[2]*inv_norm;

  XPROD(v_zeta,s1,s2); /* s2 = p1-p0; s1 = p0-p2, hence s1 x s2 = (p1-p0)x(p2-p0) */
  
  len=sqrt(v_zeta[0]*v_zeta[0]+v_zeta[1]*v_zeta[1]+v_zeta[2]*v_zeta[2]);
  if(len==0.0) {CAMLreturn(Val_unit);}

  area=0.5*len;
  inv_norm=1.0/len;
  v_zeta[0]=v_zeta[0]*inv_norm, v_zeta[1]=v_zeta[1]*inv_norm, v_zeta[2]=v_zeta[2]*inv_norm;

  c01=SPROD(xi0,xi1);
  c12=SPROD(xi1,xi2);
  c20=SPROD(xi2,xi0);

  XPROD(eta0,v_zeta,xi0);
  XPROD(eta1,v_zeta,xi1);
  XPROD(eta2,v_zeta,xi2);

  gamma[0][0] = 1.0, gamma[0][1] = c01, gamma[0][2] = c20,
  gamma[1][0] = c01, gamma[1][1] = 1.0, gamma[1][2] = c12,
  gamma[2][0] = c20, gamma[2][1] = c12, gamma[2][2] = 1.0;

  area_denominator=1.0/(8.0*PI*area);

  /* Finished setup triangle coordinate system */

  for(j=0;j<nr_displacements;j++)
  {
    greyfactor=Double_field(ml_greyfactors,j);
    for(k=0;k<nr_observer_points;k++)
    {
      v[0]= Double_field(Field(ml_displacements,j),0)+Double_field(Field(ml_observer_points,k),0);
      v[1]= Double_field(Field(ml_displacements,j),1)+Double_field(Field(ml_observer_points,k),1);
      v[2]= Double_field(Field(ml_displacements,j),2)+Double_field(Field(ml_observer_points,k),2);

      /* We apply an orthogonal transformation to the triangle's vertex coordinates
        before we compute Lindholm contributions. This allows us to consider
        lattices with structures like this:
        
        b d b d b d b d
        
        p q p q p q p q
        
        b d b d b d b d
        
        p q p q p q p q
        
        Actually, as the problem of obtaining potentials from triangles with given
        dipole distribution certainly is isotropic, we turn the entire configuration
        around in space by applying the inverse (= transposed) transformation to
        the observer point. This way, the triangle stays in its original configuration
        (where we laborously computed the matrix gamma), and the observer moves 
        in space.
      */
	  
      observer[0] = v[0]*otrans[0][0]+v[1]*otrans[1][0]+v[2]*otrans[2][0];
      observer[1] = v[1]*otrans[0][1]+v[1]*otrans[1][1]+v[2]*otrans[2][1];
      observer[2] = v[2]*otrans[0][2]+v[1]*otrans[1][2]+v[2]*otrans[2][2];

      MAXGEO(observer[0]); MAXGEO(observer[1]); MAXGEO(observer[2]);
      geo_eps=max_geometry*1e-13;

      V_SUB(r0,p0,observer);
      V_SUB(r1,p1,observer);
      V_SUB(r2,p2,observer);
      
      r0l=V_LEN(r0);
      r1l=V_LEN(r1);
      r2l=V_LEN(r2);
	  
      if(r0l<=min_dist || r1l<=min_dist || r2l<=min_dist)continue;
      /* "observer is on a triangle vertex" - ignore this one */

      v_log[0]=log((r1l+r2l+s0l)/(r1l+r2l-s0l));
      v_log[1]=log((r2l+r0l+s1l)/(r2l+r0l-s1l));
      v_log[2]=log((r0l+r1l+s2l)/(r0l+r1l-s2l));

      sp01=SPROD(r0,r1);
      sp12=SPROD(r1,r2);
      sp20=SPROD(r2,r0);

      numerator=r0l*r1l*r2l+r0l*sp12+r1l*sp20+r2l*sp01;
      denominator=sqrt(2.0*(r1l*r2l+sp12)*(r2l*r0l+sp20)*(r0l*r1l+sp01));
	  
      if(denominator==0.0)continue;

      angle=(denominator==0.0? 0.0 : numerator/denominator);
      if(angle<-1.0) {angle = -1.0;}
      else if(angle>1.0) {angle=1.0;}
      angle=2.0*acos(angle);

      zeta=SPROD(v_zeta,r0);
      if(zeta<0.0) {angle=-angle;}
	  
      lh[0]=Double_field(Field(ml_store_lh012,0),k);
      lh[1]=Double_field(Field(ml_store_lh012,1),k);
      lh[2]=Double_field(Field(ml_store_lh012,2),k);

      /*
      fprintf(stderr,"DDD LH contrib row=%d, area=%f %f angle=%f\n r1=%f %f %f\n eta0=%f %f %f\n observer=%f %f %f\n p0=%f %f %f\n p1=%f %f %f\n p2=%f %f %f\n lh012=%f %f %f (sign=%s)\n s[012]l=%f %f %f\n v_log=%f %f %f\n greyfactor=%f\n angle_summands=%f %f %f\n zeta_summands=%f %f %f\n",k, area, area_denominator, angle, r1[0],r1[1],r1[2],eta0[0],eta0[1],eta0[2], observer[0],observer[1],observer[2],p0[0],p0[1],p0[2],p1[0],p1[1],p1[2],p2[0],p2[1],p2[2],greyfactor*(s0l*area_denominator*(angle*SPROD(eta0,r1)-zeta*SPROD(gamma[0],v_log))),greyfactor*(s1l*area_denominator*(angle*SPROD(eta1,r2)-zeta*SPROD(gamma[1],v_log))),greyfactor*(s2l*area_denominator*(angle*SPROD(eta2,r0)-zeta*SPROD(gamma[2],v_log))),(SPROD(v_zeta,outward_surface_normal)<0.0?"MINUS":"PLUS"),s0l,s1l,s2l,v_log[0],v_log[1],v_log[2],greyfactor,s0l*area_denominator*(angle*SPROD(eta0,r1)),s1l*area_denominator*(angle*SPROD(eta1,r2)),s2l*area_denominator*(angle*SPROD(eta2,r0)),(s0l*area_denominator*(zeta*SPROD(gamma[0],v_log))),(s1l*area_denominator*(zeta*SPROD(gamma[1],v_log))),(s2l*area_denominator*(zeta*SPROD(gamma[2],v_log))));fflush(stderr);
      */

      factor = greyfactor*area_denominator;
      if(SPROD(v_zeta,outward_surface_normal)<0.0)factor = -factor;

      if(fabs(zeta)<geo_eps)factor=0;
      
      lh[0] += factor*s0l*(angle*SPROD(eta0,r1)-zeta*SPROD(gamma[0],v_log));
      lh[1] += factor*s1l*(angle*SPROD(eta1,r2)-zeta*SPROD(gamma[1],v_log));
      lh[2] += factor*s2l*(angle*SPROD(eta2,r0)-zeta*SPROD(gamma[2],v_log));
      
      Store_double_field(Field(ml_store_lh012,0),k,lh[0]);
      Store_double_field(Field(ml_store_lh012,1),k,lh[1]);
      Store_double_field(Field(ml_store_lh012,2),k,lh[2]);
    }
  }

  CAMLreturn(Val_unit);
}

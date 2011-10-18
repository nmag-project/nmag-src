#include <stdio.h>
#include "laplacebem.h"
#include "bem3d.h"
#include "supermatrix.h"
#include "sparsematrix.h"
#include "cluster.h"
#include "surfacebem.h"
#include "krylov.h"
#include "hca.h"
#include "hcoarsening.h"

int main()
{
  int algorithm = 4;
  int nfdeg = 3; 
  int nmin = 30;
  double eta = 2.0; 
  double eps_aca = 0.0000001;
  double eps = 0.001;
  int p=4;
  int kmax=p*p*p;
  double *buffer, *result, *inputvector;
  FILE* ofile;
  int i;
  double size_smx;
    
  pbemgrid3d gr;
  pclustertree ct;
  pcluster  root;
  psurfacebemfactory sbf;
  psupermatrix smx;
  int *dof2idx;

  gr = read_bemgrid3d("hlib_sphere40.surf");
  prepare_bemgrid3d(gr);
  if(!oriented_bemgrid3d(gr))
    fprintf(stderr,"The grid gr is not oriented.\n");
  if(!closed_bemgrid3d(gr))
    fprintf(stderr,"The grid gr is not closed.\n");

  buffer = (double*) malloc(sizeof(double)*gr->vertices);
  result = (double*) malloc(sizeof(double)*gr->vertices);
  inputvector = (double*) malloc(sizeof(double)*gr->vertices);
  for(i=0; i<gr->vertices; i++) {
    buffer[i] = 0.0;
    result[i] = 0.0;
    inputvector[i] = 1.0;
  }

  fprintf(stderr,"Building vertexcluster ...\n"); fflush(stderr);
  ct = buildvertexcluster_bemgrid3d(gr,HLIB_REGULAR,nmin,0);
  dof2idx = ct->dof2idx;
  root = ct->root;
 
  fprintf(stderr,"Building the surfacebemfactory...\n"); fflush(stderr);
  sbf = new_surfacebemfactory_dlp_collocation(gr,
					      HLIB_LINEAR_BASIS,ct,
					      HLIB_LINEAR_BASIS,ct,
					      nfdeg,nfdeg,p,0.0);
  
  fprintf(stderr,"Filling the supermatrix...\n"); fflush(stderr);
  smx = onthefly_hca_coarsen_supermatrix(root,root,sbf,eps_aca,
					 kmax,eps,1,algorithm,0,
					 eta,0);

  outputsvd_supermatrix(smx,"supermatrix.ps");
  write_supermatrix("supermatrix.dat",smx);
  
  size_smx = getsize_supermatrix(smx) / 1024.0 / 1024.0;
  fprintf(stderr,"The size of smx is %f MB.\n",size_smx);

  eval_supermatrix(smx,inputvector,buffer);
  for(i=0; i<gr->vertices; i++)
    result[dof2idx[i]] = buffer[i];
    
  ofile = fopen("singleresult.dat","w");
  for(i=0; i<gr->vertices; i++)
    fprintf(ofile,"%f\n",result[i]);
  fclose(ofile);
  free(result);
  
  return 0;

}

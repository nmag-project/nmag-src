#include <stdio.h>
#include "laplacebem.h"
#include "bem3d.h"
#include "supermatrix.h"
#include "sparsematrix.h"
#include "cluster.h"
#include "surfacebem.h"
#include "krylov.h"
#include "hca.h"

int main()
{
  int nfdeg=3; 
  int nmin=20;
  double eta=2.0, eps=0.0001;
  int p=4;
  int kmax=p*p*p;
    
  pbemgrid3d gr;
  pclustertree ct ;
  pcluster  root ;
  pblockcluster bcluster;
  psurfacebemfactory sbf ;
  psupermatrix smx;
  
  printf("Reading from the file...\n");
  gr=read_bemgrid3d("hlib_bigbar.surf");
  printf("Reading finished...");
  prepare_bemgrid3d(gr);
  if(!oriented_bemgrid3d(gr)) 
    {
      printf("The grid is not oriented.\n");
    }
  if(!closed_bemgrid3d(gr))
    {
      printf("The grid is not closed.\n");
  }
  
  printf("Building the vertex cluster...\n");
  ct=buildvertexcluster_bemgrid3d(gr,HLIB_REGULAR,nmin,0);
  root=ct->root;
  
  printf("Creating the surfacebemfactory object ...");
  sbf=new_surfacebemfactory_dlp_collocation(gr,HLIB_LINEAR_BASIS,ct,
					    HLIB_LINEAR_BASIS,ct,nfdeg,
					    nfdeg, p, 0.0);

  printf("Building the blockcluster...\n");
  bcluster=build_blockcluster(root,root,HLIB_MAXADMISSIBILITY,
			      HLIB_BLOCK_HOMOGENEOUS,eta, 0);
  
  printf("Building the empty supermatrix...\n");
  smx=build_supermatrix_from_blockcluster(bcluster,0,0.0);
  
  printf("Filling the supermatrix...\n");
  /*fill_surfacebem_supermatrix(smx,root,root,sbf);*/
  hcafill_surfacebem_supermatrix(smx,root,root,sbf,eps,kmax);
  
  return 0;
}

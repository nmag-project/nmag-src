/* (C) 2007 Dr. Thomas Fischbacher */

/* XXX IMPORTANT NOTE: have to see that this is sufficiently
   self-contained that we are independent of the hlib includes, so one
   can compile our code without having to have hlib around! */

/* Hlib includes -- probably more than we actually need */
/*
#include "h2virtual.h"
#include "basic.h"
#include "bem3d.h"
#include "cluster.h"
#include "clusterbasis.h"
#include "h2virtual.h"
#include "laplacebem.h"
#include "surfacebem.h"
#include "supermatrix.h"
#include "sparsematrix.h"
#include "hcoarsening.h"
#include "aca.h"
#include "krylov.h"
*/
/* end Hlib includes */

typedef enum {
  HLIB_DEFAULT,
  HLIB_GEOMETRIC,
  HLIB_REGULAR,
  HLIB_REGULARBOX,
  HLIB_CARDINALITY,
  HLIB_PCA
} ClusterStrategy;

typedef enum {
  HLIB_CONSTANT_BASIS,
  HLIB_LINEAR_BASIS
} BEMBasis;


typedef enum {
  HLIB_MINADMISSIBILITY,
  HLIB_WEAKADMISSIBILITY,
  HLIB_MAXADMISSIBILITY
} BlockAdmissiblityCriterion;

typedef enum {
  HLIB_BLOCK_HOMOGENEOUS,
  HLIB_BLOCK_INHOMOGENEOUS
} BlockHomogeneity;

typedef enum {
  HLIB_ACA,
  HLIB_ACAP,
  HLIB_INTERPOL,
  HLIB_HCAI,
  HLIB_HCAII
} HLIB_HCA_VARIANT;

typedef enum {
  HLIB_FROBENIUS_ABSOLUTE,	/* Ensure |A-A'|_F <= eps globally */
  HLIB_FROBENIUS_RELATIVE,	/* Truncate if |A-A'|_F <= eps * |A|_F locally */
  HLIB_EUCLIDEAN_RELATIVE	/* Truncate if |A-A'|_2 <= eps * |A|_2 locally */
} TruncationStrategy;

typedef struct {
  double (*x)[3];
  int (*e)[2];
  int (*t)[3];
  int (*s)[3];
  int vertices;
  int edges;
  int triangles;
  double *gdet;
  double (*n)[3];
} bemgrid3d;


typedef void *pcluster;

typedef struct {
  int ndof;
  int nidx;
  int clusters;
  int *dof2idx;
  int *idx2dof;
  pcluster root;
} clustertree;

typedef bemgrid3d *pbemgrid3d;
typedef clustertree *pclustertree;
typedef void *psurfacebemfactory;
typedef void *pblockcluster;
typedef void *psparsematrix;
typedef void *psupermatrix;
typedef void *puniformmatrix;
typedef void *prkmatrix;
typedef void *pfullmatrix;
typedef void *ph2recompression;

typedef pbemgrid3d (*ty_new_bemgrid3d)(int,int,int);
typedef int (*ty_oriented_bemgrid3d)(pbemgrid3d);
typedef void (*ty_del_bemgrid3d)(pbemgrid3d);
typedef void (*ty_prepare_bemgrid3d)(pbemgrid3d);
typedef pclustertree (*ty_buildvertexcluster_bemgrid3d)(pbemgrid3d,ClusterStrategy,int,int);
typedef void (*ty_del_clustertree)(pclustertree);
typedef psurfacebemfactory (*ty_new_surfacebemfactory_dlp_collocation)(pbemgrid3d,
								       BEMBasis,
								       pclustertree,
								       BEMBasis,
								       pclustertree,
								       int, int, int,
								       double);
typedef void (*ty_del_surfacebemfactory)(psurfacebemfactory);

typedef pblockcluster (*ty_build_blockcluster)(pcluster, pcluster,
					       BlockAdmissiblityCriterion,
					       BlockHomogeneity, double, int);
typedef void (*ty_del_blockcluster)(pblockcluster);


typedef psupermatrix (*ty_build_supermatrix_from_blockcluster)(pblockcluster, int, double);
typedef void (*ty_del_supermatrix)(psupermatrix);
typedef void (*ty_hcafill_surfacebem_supermatrix)(psupermatrix, pcluster,
						  pcluster, psurfacebemfactory,
						  double, int);
typedef void (*ty_ihcafill_surfacebem_supermatrix)(psupermatrix,pcluster,
						   pcluster,psurfacebemfactory,
						   double,int);
typedef void (*ty_acafill_surfacebem_supermatrix)(psupermatrix,pcluster,
						  pcluster,psurfacebemfactory,
						  double, int);
typedef void (*ty_bufacafill_surfacebem_supermatrix)(psupermatrix,pcluster,pcluster,
						     psurfacebemfactory,double,int);
typedef void (*ty_bufacafillold_surfacebem_supermatrix)(psupermatrix,pcluster,pcluster,
							psurfacebemfactory,double,int);

typedef void (*ty_fill_surfacebem_supermatrix)(psupermatrix,pcluster,pcluster,psurfacebemfactory);
typedef psupermatrix (*ty_onthefly_hca_coarsen_supermatrix)(pcluster,pcluster,
						 psurfacebemfactory,double,
						 int,double,int,
						 HLIB_HCA_VARIANT,int,
						 double,int);
typedef psupermatrix (*ty_coarsen_hca_from_blockcluster)(pblockcluster,
							 psurfacebemfactory,
							 double, double,
							 int, int);
typedef ph2recompression (*ty_newrecompression_surfacebem)(psurfacebemfactory,double,double,\
							   int,TruncationStrategy);
typedef psupermatrix (*ty_virtual2_supermatrix)(pcluster,pcluster,ph2recompression,pblockcluster);
typedef void (*ty_write_supermatrix)(char *, psupermatrix);
typedef psupermatrix (*ty_read_supermatrix)(char *);
typedef void (*ty_eval_supermatrix)(psupermatrix, double*, double*);
typedef double (*ty_getentry_supermatrix)(psupermatrix,int,int);
typedef unsigned int (*ty_getsize_supermatrix)(psupermatrix);
typedef unsigned int (*ty_getsizefull_supermatrix)(psupermatrix);
typedef unsigned int (*ty_getsizerk_supermatrix)(psupermatrix);
typedef void (*ty_outputsvd_supermatrix)(psupermatrix,char*);

/** Used to initialise bemgrid3d objects. */
typedef struct {
  size_t       nr_vertices;
  pbemgrid3d   gr;
  pclustertree ct;

} gridbuilder_t;

typedef struct {
  gridbuilder_t *row,
                *col;

  psurfacebemfactory sbf;
  pblockcluster bcluster;
  psupermatrix smx;
  double *pbuffer_lhs;
  double *pbuffer_rhs;
} hmatrix_interna;

/*Code HLib parallel*/
/*
typedef psurfacebemfactory (*ty_new_surfacebemfactory_dlp_collocation)(pbemgrid3d,
BEMBasis,
pclustertree,
pbemgrid3d,
BEMBasis,
pclustertree,
int, int, int,
double);

typedef struct {
pbemgrid3d gr_row;
pbemgrid3d gr_col;
pclustertree ct_row;
pclustertree ct_col;
psurfacebemfactory sbf;
pblockcluster bcluster;
psupermatrix smx;
int nr_vertices_row;
int nr_vertices_col;
double *pbuffer_lhs;
double *pbuffer_rhs;
} hmatrix_interna;
*/

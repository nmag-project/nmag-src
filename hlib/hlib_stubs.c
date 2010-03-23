/* (C) 2007 Thomas Fischbacher, Hans Fangohr, SES */

/* Remark: commented code which was written for a parallel execution of HLib 
   in hlib_stubs.h and hlib_stubs.c
   These code bits can be found by searching for "Code: HLib parallel"
*/

#include "hlib_stubs.h"

#include <math.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <caml/bigarray.h>

#include <unistd.h>
#include <stdio.h>
#include <float.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

#include <dlfcn.h>

#include "petsc.h"
#include "petscvec.h"
#include "petscsys.h"

/* define DEBUG to 2 to see all the debug messages */
#define DEBUG 0

#define Store_c_field(block,offset,x) (Field(block,offset)=(value)x)

static char *err_exn_name="ocaml_exn_hlib_caml_interface";

static ty_new_bemgrid3d dyn_new_bemgrid3d = 0;
static ty_oriented_bemgrid3d dyn_oriented_bemgrid3d = 0;
static ty_del_bemgrid3d dyn_del_bemgrid3d = 0;
static ty_prepare_bemgrid3d dyn_prepare_bemgrid3d = 0;
static ty_buildvertexcluster_bemgrid3d dyn_buildvertexcluster_bemgrid3d = 0;
static ty_del_clustertree dyn_del_clustertree = 0;
static ty_new_surfacebemfactory_dlp_collocation dyn_new_surfacebemfactory_dlp_collocation = 0;
static ty_del_surfacebemfactory dyn_del_surfacebemfactory = 0;
static ty_build_blockcluster dyn_build_blockcluster = 0;
static ty_del_blockcluster dyn_del_blockcluster = 0;
static ty_build_supermatrix_from_blockcluster dyn_build_supermatrix_from_blockcluster = 0;
static ty_del_supermatrix dyn_del_supermatrix = 0;
static ty_hcafill_surfacebem_supermatrix dyn_hcafill_surfacebem_supermatrix = 0;
static ty_ihcafill_surfacebem_supermatrix dyn_ihcafill_surfacebem_supermatrix = 0;
static ty_acafill_surfacebem_supermatrix dyn_acafill_surfacebem_supermatrix = 0;
static ty_bufacafill_surfacebem_supermatrix dyn_bufacafill_surfacebem_supermatrix = 0;
static ty_bufacafillold_surfacebem_supermatrix dyn_bufacafillold_surfacebem_supermatrix = 0;
static ty_fill_surfacebem_supermatrix dyn_fill_surfacebem_supermatrix = 0;
static ty_onthefly_hca_coarsen_supermatrix dyn_onthefly_hca_coarsen_supermatrix = 0;
static ty_coarsen_hca_from_blockcluster dyn_coarsen_hca_from_blockcluster = 0;
static ty_newrecompression_surfacebem dyn_newrecompression_surfacebem = 0;
static ty_virtual2_supermatrix dyn_virtual2_supermatrix = 0;
static ty_write_supermatrix dyn_write_supermatrix = 0;
static ty_read_supermatrix dyn_read_supermatrix = 0;
static ty_eval_supermatrix dyn_eval_supermatrix = 0;
static ty_getentry_supermatrix dyn_getentry_supermatrix = 0;
static ty_getsize_supermatrix dyn_getsize_supermatrix = 0;
static ty_getsizefull_supermatrix dyn_getsizefull_supermatrix = 0;
static ty_getsizerk_supermatrix dyn_getsizerk_supermatrix = 0;
static ty_outputsvd_supermatrix dyn_outputsvd_supermatrix = 0;

static int libhmatrix_is_initialized=0;

static void own_raise_with_string(value x, char *s)
{
  fprintf(stderr,"HLIB Exception: '%s'\n",s);fflush(stderr); /* DDD */

  raise_with_string(*caml_named_value(err_exn_name),
		    "hlib was not initialized!");
}

static int init_libhmatrix(char *path_libhmatrix)
{
  void *libhmatrix;

  if(libhmatrix_is_initialized)return 0;


  if(0==(libhmatrix=dlopen(path_libhmatrix,RTLD_LAZY)))
    {
      return -2;
    }

  if(0==(dyn_new_bemgrid3d=dlsym(libhmatrix,"new_bemgrid3d")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_oriented_bemgrid3d=dlsym(libhmatrix,"oriented_bemgrid3d")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_del_bemgrid3d=dlsym(libhmatrix,"del_bemgrid3d")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_prepare_bemgrid3d=dlsym(libhmatrix,"prepare_bemgrid3d")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_buildvertexcluster_bemgrid3d=dlsym(libhmatrix,"buildvertexcluster_bemgrid3d")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_del_clustertree=dlsym(libhmatrix,"del_clustertree")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_new_surfacebemfactory_dlp_collocation=dlsym(libhmatrix,"new_surfacebemfactory_dlp_collocation")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_del_surfacebemfactory=dlsym(libhmatrix,"del_surfacebemfactory")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_build_blockcluster=dlsym(libhmatrix,"build_blockcluster")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_del_blockcluster=dlsym(libhmatrix,"del_blockcluster")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_build_supermatrix_from_blockcluster=dlsym(libhmatrix,"build_supermatrix_from_blockcluster")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_del_supermatrix=dlsym(libhmatrix,"del_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_hcafill_surfacebem_supermatrix=dlsym(libhmatrix,"hcafill_surfacebem_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_ihcafill_surfacebem_supermatrix=dlsym(libhmatrix,"ihcafill_surfacebem_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_acafill_surfacebem_supermatrix=dlsym(libhmatrix,"acafill_surfacebem_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_bufacafill_surfacebem_supermatrix=dlsym(libhmatrix,"bufacafill_surfacebem_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_bufacafillold_surfacebem_supermatrix=dlsym(libhmatrix,"bufacafillold_surfacebem_supermatrix")))

  if(0==(dyn_fill_surfacebem_supermatrix=dlsym(libhmatrix,"fill_surfacebem_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_onthefly_hca_coarsen_supermatrix=dlsym(libhmatrix,"onthefly_hca_coarsen_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_coarsen_hca_from_blockcluster=dlsym(libhmatrix,"coarsen_hca_from_blockcluster")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_newrecompression_surfacebem=dlsym(libhmatrix,"newrecompression_surfacebem")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_virtual2_supermatrix=dlsym(libhmatrix,"virtual2_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_write_supermatrix=dlsym(libhmatrix,"write_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_read_supermatrix=dlsym(libhmatrix,"read_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_eval_supermatrix=dlsym(libhmatrix,"eval_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_getentry_supermatrix=dlsym(libhmatrix,"getentry_supermatrix")))
    {dlclose(libhmatrix);return -1; };
  
  if(0==(dyn_getsize_supermatrix=dlsym(libhmatrix,"getsize_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_getsizefull_supermatrix=dlsym(libhmatrix,"getsizefull_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_getsizerk_supermatrix=dlsym(libhmatrix,"getsizerk_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  if(0==(dyn_outputsvd_supermatrix=dlsym(libhmatrix,"outputsvd_supermatrix")))
    {dlclose(libhmatrix);return -1; };

  libhmatrix_is_initialized=1;

  return 0;
}




static void libhmatrix_checkinit(void)
{
  if(!libhmatrix_is_initialized)
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			"libhmatrix was not initialized!");
    }
}


static void finalize_hmatrix(value block)
{
  hmatrix_interna *hmatrix;

  hmatrix=(hmatrix_interna *)Field(block,1);

  if(hmatrix!=0)
    {
      /* In principle, the internal "hmatrix" data structure allows us
	 to retain part of the scaffolding that was originally used to
	 build the supermatrix. We may or may not want to keep this.
	 This de-allocator can deal with either situation.
      */
      /*Code: HLib parallel*/
      /*
	if(hmatrix->gr_row) {
	dyn_del_bemgrid3d(hmatrix->gr_row);
	hmatrix->gr_row = NULL;
	hmatrix->gr_col = NULL;
	}
	if(hmatrix->gr_row)dyn_del_bemgrid3d(hmatrix->gr_row);
	if(hmatrix->gr_col)dyn_del_bemgrid3d(hmatrix->gr_col);
	if(hmatrix->ct_row)dyn_del_clustertree(hmatrix->ct_row);
	if(hmatrix->ct_col)dyn_del_clustertree(hmatrix->ct_col);
      */
      if(hmatrix->gr)dyn_del_bemgrid3d(hmatrix->gr);
      if(hmatrix->ct)dyn_del_clustertree(hmatrix->ct);
      if(hmatrix->sbf)dyn_del_surfacebemfactory(hmatrix->sbf);
      if(hmatrix->bcluster)
	{
	  fprintf(stderr,"DDD NOTE: deleting block cluster (Do I really have to do that, or is that a mistake?)\n");fflush(stderr);
	  dyn_del_blockcluster(hmatrix->bcluster);
	}
      if(hmatrix->smx)dyn_del_supermatrix(hmatrix->smx);
      if(hmatrix->pbuffer_lhs)free(hmatrix->pbuffer_lhs);
      if(hmatrix->pbuffer_rhs)free(hmatrix->pbuffer_rhs);
      free(hmatrix);
      Store_c_field(block,1,0);
    }
}


static void aiee(char *msg)
{
  fprintf(stderr,"AIEE - FATAL FAILURE: %s\n",msg);
  exit(1);
}



CAMLprim value caml_hlib_init(value ml_hmatrix_path)
{
  CAMLparam1(ml_hmatrix_path);

  int ret;

  ret=init_libhmatrix(String_val(ml_hmatrix_path));

  if(ret==0)CAMLreturn(Val_unit);

  if(ret==-2)
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			"hlib initialization: dlopen() failed!");
    }
  else if(ret==-1)
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			"hlib initialization: dlsym() failed!");
    }
  else	/* for future extensions... */
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			"hlib initialization failed!");
    }

  /* Never reached, but important to keep GCC happy (w.r.t. warnings) */
  CAMLreturn(Val_unit);
}



CAMLprim value caml_hlib_raw_make_hmatrix(value ml_vertices,
					  value ml_triangles,
					  value ml_edges,
					  value ml_triangle_edges,
					  value ml_args)
{
  CAMLparam5(ml_vertices,ml_triangles,ml_edges,ml_triangle_edges,ml_args);
  int number_vertices, number_triangles, number_edges, number_triangle_edges,j,k;
  hmatrix_interna *hmatrix; 
  
  int algorithm;
  double eta;
  double eps_aca;
  double eps;
  int nfdeg;
  int nmin;
  int p ;
  int kmax;
  double size_smx, size_rk, size_full, size_fullbem,
         megabyte=1024*1024;
  FILE *memory_info;
  /*
    FILE *bemfile;
    double buffer;
  */
#if DEBUG > 0
  fprintf(stderr,"Entering function caml_hlib_raw_make_hmatrix!\n"); fflush(stderr);
#endif

#ifdef HAVE_GETTIMEOFDAY
  long int sec, microsec;
  struct timeval time1, time2;
  double delta_t;
  FILE *timing_info;
  
  
  /*Here the first time measurement is taken for the timings.*/
  i=gettimeofday(&time1,NULL);
  if(i!=0)
    {
      fprintf(stderr,"caml_hlib_raw_make_hmatrix: The second time measurement did not work. %d returned.\n",i); fflush(stderr);
      exit(1);
    }
#endif
  
  CAMLlocal5(ml_algorithm,ml_nfdeg,ml_nmin,ml_eta,ml_eps_aca);
  CAMLlocal3(ml_eps,ml_p,ml_kmax);

  ml_algorithm=Field(ml_args,0);
  ml_nfdeg=Field(ml_args,1);
  ml_nmin=Field(ml_args,2);
  ml_eta=Field(ml_args,3);
  ml_eps_aca=Field(ml_args,4);
  ml_eps=Field(ml_args,5);
  ml_p=Field(ml_args,6);
  ml_kmax=Field(ml_args,7);

  algorithm=Int_val(ml_algorithm);
  nfdeg=Int_val(ml_nfdeg);
  nmin=Int_val(ml_nmin);
  eta=Double_val(ml_eta);
  eps_aca=Double_val(ml_eps_aca);
  eps=Double_val(ml_eps);
  p=Int_val(ml_p);
  kmax=Int_val(ml_kmax);
  
  if(algorithm > 1 && algorithm != 5)
    {
      kmax = p*p*p;
    }

#if DEBUG > 1
  fprintf(stderr, "algorithm=%d, eta=%f, kmax=%d, eps_aca=%f, eps=%f.\n",
	  algorithm, eta, kmax, eps_aca, eps);
  fflush(stderr);
#endif

  libhmatrix_checkinit();

  number_vertices = Wosize_val(ml_vertices);
  number_triangles = Wosize_val(ml_triangles);
  number_edges = Wosize_val(ml_edges);
  number_triangle_edges = Wosize_val(ml_triangle_edges);

  if(0==(hmatrix=calloc(1,sizeof(hmatrix_interna))))
    /* Note that we make sure that all entries initially are null pointers. */
    {
      aiee("malloc() failure!");
    }

  hmatrix->nr_vertices = number_vertices;
  hmatrix->gr = dyn_new_bemgrid3d(number_vertices,number_edges,number_triangles);

#if DEBUG > 1
  fprintf(stderr,"caml_hlib_raw_make_hmatrix: 2 after new_bemgrid3d\n"); fflush(stderr);
#endif

  if(    0==(hmatrix->pbuffer_lhs=malloc(number_vertices*sizeof(double)))
      || 0==(hmatrix->pbuffer_rhs=malloc(number_vertices*sizeof(double))))
    {
      aiee("malloc() failure!");
    }

  for(j=0;j<number_vertices;j++)
    {
      if(Wosize_val(Field(ml_vertices,j))!=3*Double_wosize)
	{
	  dyn_del_bemgrid3d(hmatrix->gr);
	  free(hmatrix);
	  own_raise_with_string(*caml_named_value(err_exn_name),
				"hmatrix: encountered wrong number of coordinates for vertex!");
	}

      for(k=0;k<3;k++)
	{
	  hmatrix->gr->x[j][k]=Double_field(Field(ml_vertices,j),k);
	}
    }

  for(j=0;j<number_edges;j++)
    {
      if(Wosize_val(Field(ml_edges,j))!=2)
	{
	  dyn_del_bemgrid3d(hmatrix->gr);
	  free(hmatrix);
	  own_raise_with_string(*caml_named_value(err_exn_name),
				"hmatrix: encountered wrong number of indices for edge!");
	}

      for(k=0;k<2;k++)
	{
	  hmatrix->gr->e[j][k]=Int_val(Field(Field(ml_edges,j),k));
	}
    }


  for(j=0;j<number_triangles;j++)
    {
      if(Wosize_val(Field(ml_triangles,j))!=3)
	{
	  dyn_del_bemgrid3d(hmatrix->gr);
	  free(hmatrix);
	  own_raise_with_string(*caml_named_value(err_exn_name),
				"hmatrix: encountered wrong number of indices for triangle!");
	}

      for(k=0;k<3;k++)
	{
	  hmatrix->gr->t[j][k]=Int_val(Field(Field(ml_triangles,j),k));
	}
    }


  for(j=0;j<number_triangle_edges;j++)
    {
      if(Wosize_val(Field(ml_triangle_edges,j))!=3)
	{
	  dyn_del_bemgrid3d(hmatrix->gr);
	  free(hmatrix);
	  own_raise_with_string(*caml_named_value(err_exn_name),
				"hmatrix: encountered wrong number of indices for triangle edges!");
	}

      for(k=0;k<3;k++)
	{
	  hmatrix->gr->s[j][k]=Int_val(Field(Field(ml_triangle_edges,j),k));
	}
    }
  
  dyn_prepare_bemgrid3d(hmatrix->gr);

#if DEBUG > 1
  fprintf(stderr,"caml_hlib_raw_make_hmatrix: Created grid.\n"); 
  fflush(stderr);
#endif
  hmatrix->ct = dyn_buildvertexcluster_bemgrid3d(hmatrix->gr,HLIB_REGULAR,nmin,0);
#if DEBUG > 1
  fprintf(stderr,"caml_hlib_raw_make_hmatrix: after creating the vertexclusters.\n"); 
  fflush(stderr);
#endif

  hmatrix->sbf = dyn_new_surfacebemfactory_dlp_collocation(hmatrix->gr,
							   HLIB_LINEAR_BASIS, 
							   hmatrix->ct,
							   HLIB_LINEAR_BASIS, 
							   hmatrix->ct,
							   nfdeg, nfdeg, 
							   p, 0.0);
#if DEBUG > 1
  fprintf(stderr,"After creating the surfacebemfactory...\n");
  fflush(stderr);
  /*
    XXX NOTE: I would like to do that, but for this to work, I would
    have to put the MONSTROUS surfacebemfactory definition into
    hlib_stubs.h, and I am not very confident this will not see major
    changes in the future.
  */
  

  fprintf(stderr,"Now creating the blockclustertree.\n"); fflush(stderr);					      
#endif

  hmatrix->bcluster = dyn_build_blockcluster(hmatrix->ct->root, 
					     hmatrix->ct->root,
					     HLIB_MAXADMISSIBILITY,
					     HLIB_BLOCK_INHOMOGENEOUS,
					     eta, 0);

#if DEBUG > 1
  fprintf(stderr,"Creating the supermatrix from the blockcluster.\n");
#endif
  hmatrix->smx = dyn_coarsen_hca_from_blockcluster(hmatrix->bcluster,
						   hmatrix->sbf,
						   eps_aca,eps,1,kmax);

#if DEBUG > 1
  fprintf(stderr,"After building the supermatrix.\n"); fflush(stderr);
#endif
  size_smx = dyn_getsize_supermatrix(hmatrix->smx) / megabyte;
  size_rk = dyn_getsizerk_supermatrix(hmatrix->smx) / megabyte;
  size_full = dyn_getsizefull_supermatrix(hmatrix->smx) / megabyte;
  size_fullbem = 8.0 * number_vertices * number_vertices / megabyte;

  memory_info = fopen("memory_info.dat", "w");
  fprintf(memory_info,"Number of surface nodes:              %6d\n",
          number_vertices);
  fprintf(memory_info,"Size of hierarchical matrix:       %6.2f MB\n",
          size_smx);
  fprintf(memory_info,"Total size of inadmissible leaves: %6.2f MB\n",
          size_full);
  fprintf(memory_info,"Total size of admissible leaves:   %6.2f MB\n",
          size_rk);
  fclose(memory_info);

  fprintf(stderr,"HLib: Memory footprint of hierarchical matrix: %f MB.\n",
	  size_smx);
  fflush(stderr);
  fprintf(stderr,"HLib: Equivalent full matrix would require: %f MB.\n",
          size_fullbem);
  fflush(stderr);
  fprintf(stderr,"HLib: The compression rate is %4.2f %%.\n",
          100.0*(1.0-size_smx/size_fullbem));
  fflush(stderr);

  CAMLlocal1(block);

  block = alloc_final(2, finalize_hmatrix, sizeof(void*), 10*sizeof(void*));

#if DEBUG > 1
  fprintf(stderr,"caml_hlib_raw_make_hmatrix: 12 somewhere\n"); fflush(stderr);
#endif

  Store_c_field(block, 1, hmatrix);

#if DEBUG > 1
  fprintf(stderr,"Leaving caml_hlib_raw_make_hmatrix\n"); fflush(stderr);
#endif
  
#ifdef HAVE_GETTIMEOFDAY
  i=gettimeofday(&time2,NULL);
  if(i!=0)
    {
      fprintf(stderr,"caml_hlib_raw_make_hmatrix: The second time measurement did not work. %d returned.\n",i); fflush(stderr);
      exit(1);
    }

  microsec = time2.tv_usec - time1.tv_usec;
  if(microsec < 0)
    {
      microsec = 1000000 + microsec;
      sec = time2.tv_sec - time1.tv_sec-1;
    }
  else
    {
      sec = time2.tv_sec - time1.tv_sec;
    }
  
  delta_t = 1.0*sec + 1e-6*microsec;

  timing_info = fopen("timing.dat","w");
  fprintf(timing_info,"%18.8f\n",delta_t);
  fclose(timing_info);
#endif

  CAMLreturn(block);
}


CAMLprim value caml_hlib_write_hmatrix(value ml_name,
				       value ml_hmx)
{
  CAMLparam2(ml_name,ml_hmx);

  hmatrix_interna *hmx;

  libhmatrix_checkinit();

  hmx=(hmatrix_interna *)Field(ml_hmx,1);

  if(!hmx)
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			    "hmatrix: tried to write invalid supermatrix!");
    }
  
  dyn_write_supermatrix(String_val(ml_name),hmx->smx);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_hlib_read_hmatrix(value ml_name)
{
  CAMLparam1(ml_name);

  hmatrix_interna *hmatrix;
  psupermatrix smx;

  libhmatrix_checkinit();

  smx=dyn_read_supermatrix(String_val(ml_name));
  
  if(!smx)
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			    "hmatrix: low-level read_supermatrix() failed!");
    }

  if(0==(hmatrix=calloc(1,sizeof(hmatrix_interna))))
    /* Note that we make sure that all entries initially are null pointers. */
    {
      aiee("malloc() failure!");
    }
  
  hmatrix->smx=smx;
  
  CAMLlocal1(block);
  
  block = alloc_final(2, finalize_hmatrix, sizeof(void*), 10*sizeof(void*));
  
  Store_c_field(block, 1, hmatrix);
  
  CAMLreturn(block);
}

/*
 NOTE: this may crash if vector are of inappropriate size! */

CAMLprim value caml_hlib_apply_hmatrix(value ml_hmx,
				       value ml_biga_target,
				       value ml_biga_src)
{
  CAMLparam3(ml_hmx,ml_biga_target, ml_biga_src);
  
  hmatrix_interna *hmx;
  double *data_src, *data_target;
  int j;

  libhmatrix_checkinit();

  hmx=(hmatrix_interna *)Field(ml_hmx,1);
  
  if(!hmx)
    {
      own_raise_with_string(*caml_named_value(err_exn_name),
			    "hmatrix: tried to write invalid supermatrix!");
    }
  
  data_src=Data_bigarray_val(ml_biga_src);
  data_target=Data_bigarray_val(ml_biga_target);
  
  for(j=0;j<hmx->nr_vertices;j++)
    {
      hmx->pbuffer_rhs[hmx->ct->idx2dof[j]] = data_src[j];
    }
  
  dyn_eval_supermatrix(hmx->smx,hmx->pbuffer_rhs,hmx->pbuffer_lhs);
  
  for(j=0;j<hmx->nr_vertices;j++)
    {
      data_target[j]=hmx->pbuffer_lhs[hmx->ct->idx2dof[j]];
    }
  
  CAMLreturn(Val_unit);
}

/*Code: HLib parallel*/
 /*

 CAMLprim value caml_hlib_apply_hmatrix(value ml_hmx,
 value ml_biga_target,
 value ml_biga_src)
 {
 CAMLparam3(ml_hmx,ml_biga_target, ml_biga_src);
 
 hmatrix_interna *hmx;
 double *data_src, *data_target;
 int j;
 
 libhmatrix_checkinit();
 
 hmx=(hmatrix_interna *)Field(ml_hmx,1);
 
 if(!hmx)
 {
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: tried to write invalid supermatrix!");
 }
 
 data_src=Data_bigarray_val(ml_biga_src);
 data_target=Data_bigarray_val(ml_biga_target);
 
 for(j=0;j<hmx->nr_vertices_col;j++)
 {
 hmx->pbuffer_rhs[hmx->ct_col->idx2dof[j]] = data_src[j];
 }
 
 dyn_eval_supermatrix(hmx->smx,hmx->pbuffer_rhs,hmx->pbuffer_lhs);
 
 for(j=0;j<hmx->nr_vertices_row;j++)
 {
 data_target[j]=hmx->pbuffer_lhs[hmx->ct_row->idx2dof[j]];
 }
 
 CAMLreturn(Val_unit);
 }
 
 CAMLprim value caml_hlib_raw_make_hmatrix_strip(value ml_row_info,
 value ml_col_info,
 value ml_args)
 {
 
 CAMLparam3(ml_row_info,ml_col_info,ml_args);
 
 hmatrix_interna *hmatrix; 
 int number_vertices_row, number_triangles_row, number_edges_row; 
 int number_triangle_edges_row;
 int number_vertices_col, number_triangles_col, number_edges_col;
 int number_triangle_edges_col;
 int algorithm;
 double eta;
 double eps_aca;
 double eps;
 int nfdeg;
 int nmin;
 int p ;
 int kmax;
 double size_smx;
 int j,k;
 
 CAMLlocal4(ml_vertices_row, ml_triangles_row, ml_edges_row,
 ml_triangle_edges_row);
 CAMLlocal4(ml_vertices_col, ml_triangles_col, ml_edges_col, 
 ml_triangle_edges_col);
 
 ml_vertices_row = Field(ml_row_info,0);
 ml_triangles_row = Field(ml_row_info,1);
 ml_edges_row = Field(ml_row_info,2);
 ml_triangle_edges_row = Field(ml_row_info,3);
 
 number_vertices_row = Wosize_val(ml_vertices_row);
 number_triangles_row = Wosize_val(ml_triangles_row);
 number_edges_row = Wosize_val(ml_edges_row);
 number_triangle_edges_row = Wosize_val(ml_triangle_edges_row);
 
 ml_vertices_col = Field(ml_col_info,0);
 ml_triangles_col = Field(ml_col_info,1);
 ml_edges_col = Field(ml_col_info,2);
 ml_triangle_edges_col = Field(ml_col_info,3);
 
 number_vertices_col = Wosize_val(ml_vertices_col);
 number_triangles_col = Wosize_val(ml_triangles_col);
 number_edges_col = Wosize_val(ml_edges_col);
 number_triangle_edges_col = Wosize_val(ml_triangle_edges_col);
 
 algorithm = Int_val(Field(ml_args,0));
 nfdeg = Int_val(Field(ml_args,1));
 nmin = Int_val(Field(ml_args,2));
 eta = Double_val(Field(ml_args,3));
 eps_aca = Double_val(Field(ml_args,4));
 eps = Double_val(Field(ml_args,5));
 p = Int_val(Field(ml_args,6));
 kmax = Int_val(Field(ml_args,7));
 
 if(algorithm > 1 && algorithm != 5)
 {
 kmax = p*p*p;
 }
 
 libhmatrix_checkinit();
 
 if(0==(hmatrix=calloc(1,sizeof(hmatrix_interna))))
 {
 aiee("malloc() failure!");
 }
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix_strip: ");
 fprintf(stderr,"Creating grid object for row cluster.\n");
 fflush(stderr);
 
 hmatrix->nr_vertices_row = number_vertices_row;
 hmatrix->gr_row = dyn_new_bemgrid3d(number_vertices_row, number_edges_row, 
 number_triangles_row);
 
 for(j=0;j<number_vertices_row;j++)
 {
 if(Wosize_val(Field(ml_vertices_row,j))!=3*Double_wosize)
 {
 dyn_del_bemgrid3d(hmatrix->gr_row);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number of vertex coordinates!");
 }
 
 for(k=0;k<3;k++)
 {
 hmatrix->gr_row->x[j][k]=Double_field(Field(ml_vertices_row,j),k);
 }
 }
 
 for(j=0;j<number_edges_row;j++)
 {
 if(Wosize_val(Field(ml_edges_row,j))!=2)
 {
 dyn_del_bemgrid3d(hmatrix->gr_row);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number of edge indices!");
 }
 for(k=0;k<2;k++)
 {
 hmatrix->gr_row->e[j][k]=Int_val(Field(Field(ml_edges_row,j),k));
 }
 }
 
 for(j=0;j<number_triangles_row;j++)
 {
 if(Wosize_val(Field(ml_triangles_row,j))!=3)
 {
 dyn_del_bemgrid3d(hmatrix->gr_row);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number of triangle indices!");
 }
   
 for(k=0;k<3;k++)
 {
 hmatrix->gr_row->t[j][k]=Int_val(Field(Field(ml_triangles_row,j),k));
 }
 }
 
 for(j=0;j<number_triangle_edges_row;j++)
 {
 if(Wosize_val(Field(ml_triangle_edges_row,j))!=3)
 {
 dyn_del_bemgrid3d(hmatrix->gr_row);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number triangle2edge indices!");
 }
 
 for(k=0;k<3;k++)
 {
 hmatrix->gr_row->s[j][k]=Int_val(Field(Field(ml_triangle_edges_row,j),k));
 }
 }
 dyn_prepare_bemgrid3d(hmatrix->gr_row);
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix_strip: ");
 fprintf(stderr,"Creating grid object for column cluster.\n");
 fflush(stderr);
 
 hmatrix->nr_vertices_col = number_vertices_col;
 hmatrix->gr_col = dyn_new_bemgrid3d(number_vertices_col, number_edges_col, 
 number_triangles_col);
 
 for(j=0;j<number_vertices_col;j++)
 {
 if(Wosize_val(Field(ml_vertices_col,j))!=3*Double_wosize)
 {
 dyn_del_bemgrid3d(hmatrix->gr_col);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number of vertex coordinates!");
 }
 
 for(k=0;k<3;k++)
 {
 hmatrix->gr_col->x[j][k]=Double_field(Field(ml_vertices_col,j),k);
 }
 }
   
 for(j=0;j<number_edges_col;j++)
 {
 if(Wosize_val(Field(ml_edges_col,j))!=2)
 {
 dyn_del_bemgrid3d(hmatrix->gr_col);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number of edge indices!");
 }
 for(k=0;k<2;k++)
 {
 hmatrix->gr_col->e[j][k]=Int_val(Field(Field(ml_edges_col,j),k));
 }
 }
 
 for(j=0;j<number_triangles_col;j++)
 {
 if(Wosize_val(Field(ml_triangles_col,j))!=3)
 {
 dyn_del_bemgrid3d(hmatrix->gr_col);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number of triangle indices!");
 }
 
 for(k=0;k<3;k++)
 {
 hmatrix->gr_col->t[j][k]=Int_val(Field(Field(ml_triangles_col,j),k));
 }
 }
   
 for(j=0;j<number_triangle_edges_col;j++)
 {
 if(Wosize_val(Field(ml_triangle_edges_col,j))!=3)
 {
 dyn_del_bemgrid3d(hmatrix->gr_col);
 free(hmatrix);
 own_raise_with_string(*caml_named_value(err_exn_name),
 "hmatrix: wrong number triangle2edge indices!");
 }
 
 for(k=0;k<3;k++)
 {
 hmatrix->gr_col->s[j][k]=Int_val(Field(Field(ml_triangle_edges_col,j),k));
 }
 }
 dyn_prepare_bemgrid3d(hmatrix->gr_col);
 
 if(    0==(hmatrix->pbuffer_lhs=malloc(number_vertices_row*sizeof(double)))
 || 0==(hmatrix->pbuffer_rhs=malloc(number_vertices_col*sizeof(double))))
 {
 aiee("malloc() failure!");
 }
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix_strip: ");
 fprintf(stderr,"building vertexclusters for rows and columns.\n");
 fflush(stderr);
 hmatrix->ct_row = dyn_buildvertexcluster_bemgrid3d(hmatrix->gr_row,
 HLIB_REGULAR,nmin,0);
 hmatrix->ct_col = dyn_buildvertexcluster_bemgrid3d(hmatrix->gr_col,
 HLIB_REGULAR,nmin,0);
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix_strip: ");
 fprintf(stderr,"Initialise surfacebemfactory.\n");
 fflush(stderr);
 hmatrix->sbf = dyn_new_surfacebemfactory_dlp_collocation(hmatrix->gr_row,
 HLIB_LINEAR_BASIS, 
 hmatrix->ct_row,
 hmatrix->gr_col,
 HLIB_LINEAR_BASIS, 
 hmatrix->ct_col,
 nfdeg, nfdeg,
 p, 0.0);
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix_strip: ");
 fprintf(stderr,"Creating the blockclustertree from row and column cluster.\n"); 
 fflush(stderr);					      
 
 hmatrix->bcluster = dyn_build_blockcluster(hmatrix->ct_row->root, 
 hmatrix->ct_col->root,
 HLIB_MAXADMISSIBILITY,
 HLIB_BLOCK_INHOMOGENEOUS,
 eta, 0);
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix_strip: ");
 fprintf(stderr,"Creating the hierarchical matrix from blockclustertree.\n"); 
 fflush(stderr);
 hmatrix->smx = dyn_coarsen_hca_from_blockcluster(hmatrix->bcluster,
 hmatrix->sbf,
 eps_aca,eps,1,kmax);
 
 size_smx = dyn_getsize_supermatrix(hmatrix->smx)/1024.0/1024.0;
 fprintf(stderr,"HLib: Memory footprint of hierarchical matrix: "); 
 fprintf(stderr,"%f MB.\n",size_smx);
 
 CAMLlocal1(block);
 block = alloc_final(2, finalize_hmatrix, sizeof(void*), 10*sizeof(void*));
 Store_c_field(block, 1, hmatrix);
 
 fprintf(stderr,"caml_hlib_raw_make_hmatrix: Leaving function.\n"); 
 fflush(stderr);
 
 
 CAMLreturn(block);
 }
 */


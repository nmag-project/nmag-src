/* (C) 2007 Thomas Fischbacher, Hans Fangohr, SES */

/* Remark: commented code which was written for a parallel execution of HLib
   in hlib_stubs.h and hlib_stubs.c
   These code bits can be found by searching for "Code: HLib parallel"
*/

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <float.h>
#include <time.h>
#include <sys/time.h>
#include <dlfcn.h>
#include <assert.h>

/* OCaml includes */
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

/* Other includes */
#include "petsc.h"
#include "petscvec.h"
#include "petscsys.h"

#include "hlib_stubs.h"

/* define DEBUG to 2 to see all the debug messages */
#define DEBUG 0

/* Variadic macros require C99, I think... */
#if DEBUG > 1
#  define DEBUGMSG(...) \
     do {fprintf(stderr, __VA_ARGS__); fflush(stderr);} while(0)
#else
#  define DEBUGMSG(...)
#endif

#define Store_c_field(block, offset, x) \
  (Field((block), (offset)) = (value) (x))

/** Used for exceptions generated from the C code below */
static char *err_exn_name = "ocaml_exn_hlib_caml_interface";

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

/* Experiment */
typedef pclustertree (*ty_buildcustomcluster_bemgrid3d)(pbemgrid3d,int,int,int);
static ty_buildcustomcluster_bemgrid3d dyn_buildcustomcluster_bemgrid3d = 0;




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

  if(0==(dyn_buildcustomcluster_bemgrid3d=dlsym(libhmatrix,"buildcustomcluster_bemgrid3d")))
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

static void aiee(char *msg)
{
  fprintf(stderr, "AIEE - FATAL FAILURE IN HLIB MODULE: %s\n", msg);
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

static void *safe_malloc(size_t size)
{
  void *ptr = malloc(size);
  if (ptr != NULL)
    return ptr;

  else
    aiee("malloc() failure!");
    assert(0);
    return NULL; /* Just to make some C compilers happy */
}


/** Finalise a gridbuilder_t object. */
static void gridbuilder_finish(gridbuilder_t *gb)
{
  if (gb->gr != NULL)
    dyn_del_bemgrid3d(gb->gr);
  if (gb->ct != NULL)
    dyn_del_clustertree(gb->ct);
}

/** Destroy a gridbuilder_t object. */
static void gridbuilder_destroy(gridbuilder_t *gb)
{
  if (gb != NULL) {
    gridbuilder_finish(gb);
    free(gb);
  }
}

static gridbuilder_t *gridbuilder_create(void) {
  gridbuilder_t *gb = safe_malloc(sizeof(gridbuilder_t));
  gb->gr = (pbemgrid3d) NULL;
  gb->ct = (pclustertree) NULL;
  return gb;
}

/** Initialise a gridbuilder_t object. */
static value gridbuilder_init(gridbuilder_t *gb,
                              value ml_vertices,
                              value ml_triangles,
                              value ml_edges,
                              value ml_triangle_edges)
{
  CAMLparam4(ml_vertices, ml_triangles, ml_edges, ml_triangle_edges);

  size_t nr_vertices = Wosize_val(ml_vertices),
         nr_triangles = Wosize_val(ml_triangles),
         nr_edges = Wosize_val(ml_edges),
         nr_triangle_edges = Wosize_val(ml_triangle_edges);

  int j, k;

  DEBUGMSG("Entering function populate_bemgrid3d!\n");

  /* Create an empty bemgrid3d object */
  gb->nr_vertices = nr_vertices;
  gb->gr = dyn_new_bemgrid3d(nr_vertices, nr_edges, nr_triangles);

# define MY_FAIL(msg) \
  do {gridbuilder_finish(gb); \
      own_raise_with_string(*caml_named_value(err_exn_name),(msg));} while (0)

  if (gb->gr == NULL)
    MY_FAIL("new_bemgrid3d returned NULL pointer.");

  for (j = 0; j < nr_vertices; j++)
    {
      if (Wosize_val(Field(ml_vertices, j)) != 3*Double_wosize)
        MY_FAIL("encountered wrong number of coordinates for vertex.");

      for (k = 0; k < 3; k++)
        gb->gr->x[j][k] = Double_field(Field(ml_vertices, j), k);

    }

  for (j = 0; j < nr_edges; j++)
    {
      if (Wosize_val(Field(ml_edges, j)) != 2)
        MY_FAIL("encountered wrong number of indices for edge!");

      for (k = 0; k < 2; k++)
        gb->gr->e[j][k] = Int_val(Field(Field(ml_edges, j), k));
    }


  for (j = 0; j < nr_triangles; j++)
    {
      if (Wosize_val(Field(ml_triangles, j)) != 3)
        MY_FAIL("encountered wrong number of indices for triangle!");

      for (k = 0; k < 3; k++)
        gb->gr->t[j][k] = Int_val(Field(Field(ml_triangles, j), k));
    }


  for (j = 0; j < nr_triangle_edges; j++)
    {
      if (Wosize_val(Field(ml_triangle_edges, j)) != 3)
        MY_FAIL("encountered wrong number of indices for triangle edges!");

      for(k = 0; k < 3; k++)
        gb->gr->s[j][k] = Int_val(Field(Field(ml_triangle_edges, j), k));
    }

  DEBUGMSG("Exiting function populate_bemgrid3d!\n");

# undef MY_FAIL

  CAMLreturn(Val_unit);
}

void gridbuilder_build_vertexcluster(gridbuilder_t *gb, int nmin) {
  /* Compute outer normal vectors and Gram determinants */
  DEBUGMSG("Preparing grid...\n");
  dyn_prepare_bemgrid3d(gb->gr);

  /* Build the cluster tree for the grid defined by a bemgrid3d object,
     using piecewise linear basis functions */
  DEBUGMSG("creating the vertexclusters...\n");
  gb->ct = dyn_buildvertexcluster_bemgrid3d(gb->gr, HLIB_REGULAR, nmin, 0);
}

void gridbuilder_build_vertexcluster2(gridbuilder_t *gb, int nmin, int nr_rows) {
  /* Compute outer normal vectors and Gram determinants */
  DEBUGMSG("Preparing grid...\n");
  dyn_prepare_bemgrid3d(gb->gr);

  /* Build the cluster tree for the grid defined by a bemgrid3d object,
     using piecewise linear basis functions */
  DEBUGMSG("creating the vertexclusters...\n");
  gb->ct = dyn_buildcustomcluster_bemgrid3d(gb->gr, nr_rows, nmin, 0);
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
      if (hmatrix->col != hmatrix->row)
        gridbuilder_destroy(hmatrix->col);
      gridbuilder_destroy(hmatrix->row);
      hmatrix->col = hmatrix->row = NULL;

      if(hmatrix->sbf)dyn_del_surfacebemfactory(hmatrix->sbf);
      if(hmatrix->bcluster)
        {
          fprintf(stderr,"DDD NOTE: deleting block cluster (Do I really have "
                         "to do that, or is that a mistake?)\n");
          fflush(stderr);
          dyn_del_blockcluster(hmatrix->bcluster);
        }
      if(hmatrix->smx)dyn_del_supermatrix(hmatrix->smx);
      if(hmatrix->pbuffer_lhs)free(hmatrix->pbuffer_lhs);
      if(hmatrix->pbuffer_rhs)free(hmatrix->pbuffer_rhs);
      free(hmatrix);
      Store_c_field(block,1,0);
    }
}

typedef struct {
  int    failure;
  int    have_timing[2];
  struct timeval timing[2];

} timemeasurement_t;

/** NOTE: Not sure whether we want really to do all this from C.
    At least I removed this from the main make_hmatrix function, just to make
    the body cleaner to read. */
static void take_time_measurement(timemeasurement_t *tm, int phase)
{
#ifdef HAVE_GETTIMEOFDAY
  assert(phase == 0 || phase == 1);

  /*Here the first time measurement is taken for the timings.*/
  int failure = gettimeofday(& tm->timing[phase], NULL);

  tm->failure = failure;
  tm->have_timing[phase] = !failure;
  if (phase == 0)
    tm->have_timing[1] = 0;

#else
  tm->failure = 1;
  tm->have_timing[1] = tm->have_timing[0] = 0;
#endif
}

static void fprint_time_measurement(const char *filename,
                                    timemeasurement_t *tm)
{
  FILE *f = fopen(filename, "w");
  if (f == NULL) {
    fprintf(stderr, "Cannot fopen \"%s\".", filename);
    return;
  }

#ifdef HAVE_GETTIMEOFDAY
  if (tm->failure) {
    fprintf(f, "Failure: gettimeofday failed.\n");

  } else if (tm->have_timing[0] == 0 || tm->have_timing[1] == 0) {
    fprintf(f, "Error: timings were not both taken.\n");

  } else {
    long int microsec = tm->timing[1].tv_usec - tm->timing[0].tv_usec,
             sec = tm->timing[1].tv_sec - tm->timing[0].tv_sec;
    double delta_t;

    if (microsec < 0) {
      microsec = 1000000 + microsec;
      sec -= 1;
    }

    delta_t = sec + 1e-6*microsec;
    fprintf(f, "%18.8f\n", delta_t);
  }

#else
  fprintf(f, "Not available: gettimeofday not found.\n");
#endif

  fclose(f);
}

static void fprint_memory_info(const char *filename, hmatrix_interna *hm,
                               FILE *extra) {
  int nr_vertices = hm->row->nr_vertices;
  double megabyte = 1024*1024,
         size_smx = dyn_getsize_supermatrix(hm->smx)/megabyte,
         size_rk = dyn_getsizerk_supermatrix(hm->smx)/megabyte,
         size_full = dyn_getsizefull_supermatrix(hm->smx)/megabyte,
         size_fullbem = 8.0*nr_vertices*nr_vertices/megabyte;

  FILE *f = (filename != NULL) ? fopen(filename, "w") : NULL;
  if (f != NULL) {
    fprintf(f, "%38s%6d\n", "Number of surface nodes:", nr_vertices);
    fprintf(f, "%38s%6.2f MB\n", "Size of hierarchical matrix:", size_smx);
    fprintf(f, "%38s%6.2f MB\n",
            "Total size of inadmissible leaves:", size_full);
    fprintf(f, "%38s%6.2f MB\n", "Total size of admissible leaves:", size_rk);
    fclose(f);

  } else {
    fprintf(stderr, "Cannot fopen \"%s\".", filename);
    return;
  }

  if (extra != NULL) {
    fprintf(extra, "HLib: Memory footprint of hierarchical matrix: %f MB.\n",
            size_smx);
    fprintf(extra, "HLib: Equivalent full matrix would require: %f MB.\n",
            size_fullbem);
    fprintf(extra, "HLib: The compression rate is %4.2f %%.\n",
            100.0*(1.0 - size_smx/size_fullbem));
    fflush(stderr);
  }
}

CAMLprim value caml_hlib_raw_make_hmatrix(value ml_vertices,
					  value ml_triangles,
					  value ml_edges,
					  value ml_triangle_edges,
					  value ml_args)
{
  CAMLparam5(ml_vertices, ml_triangles, ml_edges, ml_triangle_edges, ml_args);
  hmatrix_interna *hmatrix;
  timemeasurement_t tm;
  int  algorithm =    Int_val(Field(ml_args, 0)),
           nfdeg =    Int_val(Field(ml_args, 1)),
            nmin =    Int_val(Field(ml_args, 2));
  double     eta = Double_val(Field(ml_args, 3)),
         eps_aca = Double_val(Field(ml_args, 4)),
             eps = Double_val(Field(ml_args, 5));
  int          p =    Int_val(Field(ml_args, 6)),
            kmax =    Int_val(Field(ml_args, 7)),
     nr_vertices = Wosize_val(ml_vertices);

  DEBUGMSG("caml_hlib_raw_make_hmatrix: function entered!\n");
  DEBUGMSG("algorithm=%d, eta=%f, kmax=%d, eps_aca=%f, eps=%f.\n",
           algorithm, eta, kmax, eps_aca, eps);

  take_time_measurement(& tm, 0);
  if(algorithm > 1 && algorithm != 5)
    kmax = p*p*p;

  libhmatrix_checkinit();

  /* Note that we make sure that all entries initially are null pointers. */
  if (0 == (hmatrix = calloc(1, sizeof(hmatrix_interna))))
    aiee("malloc() failure!");

  /* Now we populate the bemgrid3d object */
  if(    0==(hmatrix->pbuffer_lhs=malloc(nr_vertices*sizeof(double)))
      || 0==(hmatrix->pbuffer_rhs=malloc(nr_vertices*sizeof(double))))
      aiee("malloc() failure!");

  /* Create bemgrid3d object and the blockcluster */
  hmatrix->row = gridbuilder_create();
  gridbuilder_init(hmatrix->row,
                   ml_vertices, ml_triangles, ml_edges, ml_triangle_edges);
  gridbuilder_build_vertexcluster(hmatrix->row, nmin);
  hmatrix->col = (gridbuilder_t *) NULL; /* Matrix is square */

  /* Create a surfacebemfactory object for the double layer potential */
  DEBUGMSG("creating the surfacebemfactory...\n");
  hmatrix->sbf =
    dyn_new_surfacebemfactory_dlp_collocation(hmatrix->row->gr,
                                              HLIB_LINEAR_BASIS,
                                              hmatrix->row->ct,
                                              hmatrix->row->gr,
                                              HLIB_LINEAR_BASIS,
                                              hmatrix->row->ct,
                                              nfdeg, nfdeg, p, 0.0);

  /* Create a block cluster tree from the roots of two cluster trees
     row and col (which are here the same, hmatrix->ct->root). */
  DEBUGMSG("creating the blockclustertree...\n");
  hmatrix->bcluster =
    dyn_build_blockcluster(hmatrix->row->ct->root, hmatrix->row->ct->root,
                           HLIB_MAXADMISSIBILITY, HLIB_BLOCK_INHOMOGENEOUS,
                           eta, 0);

  /* Compute a coarsened H-matrix approximation of an H-matrix implicitly
     defined by the block cluster tree hmatrix->bcluster and the hybrid cross
     approximation of the admissible leaves */
  DEBUGMSG("Creating the supermatrix from the blockcluster...\n");
  hmatrix->smx = dyn_coarsen_hca_from_blockcluster(hmatrix->bcluster,
                                                   hmatrix->sbf,
                                                   eps_aca, eps, 1, kmax);

  DEBUGMSG("Boxing hmatrix object into OCaml value...\n");
  CAMLlocal1(block);
  block = alloc_final(2, finalize_hmatrix, sizeof(void*), 10*sizeof(void*));
  Store_c_field(block, 1, hmatrix);

  DEBUGMSG("Writing out statistics...\n"); /* XXX NOTE: All this should go  */
  take_time_measurement(& tm, 1);          /* somewhere else at some point! */
  fprint_time_measurement("timing.dat", & tm);
  fprint_memory_info("memory_info.dat", hmatrix, stderr);

  DEBUGMSG("caml_hlib_raw_make_hmatrix: returning!\n");
  CAMLreturn(block);
}

CAMLprim value caml_hlib_write_hmatrix(value ml_name, value ml_hmx)
{
  CAMLparam2(ml_name, ml_hmx);

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

/* Code: HLib parallel */
/* NOTE: this may crash if vector are of inappropriate size! */
CAMLprim value caml_hlib_apply_hmatrix(value ml_hmx,
                                       value ml_biga_target,
                                       value ml_biga_src)
{
  CAMLparam3(ml_hmx, ml_biga_target, ml_biga_src);

  hmatrix_interna *hmx;
  double *data_src, *data_target;
  int j;

  libhmatrix_checkinit();

  hmx = (hmatrix_interna *) Field(ml_hmx, 1);
  if (!hmx)
    own_raise_with_string(*caml_named_value(err_exn_name),
                          "hmatrix: tried to write invalid supermatrix!");

  data_src = Data_bigarray_val(ml_biga_src);
  data_target = Data_bigarray_val(ml_biga_target);

  int data_src_len = Caml_ba_array_val(ml_biga_src)->dim[0]; /* <---- temporary code */
  for (j = 0; j < hmx->col->nr_vertices; j++)
    hmx->pbuffer_rhs[hmx->col->ct->idx2dof[j]] = data_src[j % data_src_len];

  dyn_eval_supermatrix(hmx->smx, hmx->pbuffer_rhs, hmx->pbuffer_lhs);

  for (j = 0; j < hmx->row->nr_vertices; j++)
    data_target[j] = hmx->pbuffer_lhs[hmx->row->ct->idx2dof[j]];

  CAMLreturn(Val_unit);
}

CAMLprim value caml_hlib_raw_make_hmatrix_strip(value ml_row_info,
                                                value ml_col_info,
                                                value ml_is_square,
                                                value ml_args)
{
  CAMLparam4(ml_row_info, ml_col_info, ml_is_square, ml_args);
  hmatrix_interna *hmatrix;
  int    is_square = Int_val(ml_is_square);
  int    algorithm =    Int_val(Field(ml_args, 0)),
         nfdeg     =    Int_val(Field(ml_args, 1)),
         nmin      =    Int_val(Field(ml_args, 2));
  double eta       = Double_val(Field(ml_args, 3)),
         eps_aca   = Double_val(Field(ml_args, 4)),
         eps       = Double_val(Field(ml_args, 5));
  int    p         =    Int_val(Field(ml_args, 6)),
         kmax      =    Int_val(Field(ml_args, 7));
  int    nr_vertices_rows, nr_vertices_cols;
  double size_smx;

  CAMLlocal4(ml_vertices_row, ml_triangles_row, ml_edges_row,
             ml_triangle_edges_row);
  CAMLlocal4(ml_vertices_col, ml_triangles_col, ml_edges_col,
             ml_triangle_edges_col);

  ml_vertices_row       = Field(ml_row_info, 0);
  ml_triangles_row      = Field(ml_row_info, 1);
  ml_edges_row          = Field(ml_row_info, 2);
  ml_triangle_edges_row = Field(ml_row_info, 3);
  ml_vertices_col       = Field(ml_col_info, 0);
  ml_triangles_col      = Field(ml_col_info, 1);
  ml_edges_col          = Field(ml_col_info, 2);
  ml_triangle_edges_col = Field(ml_col_info, 3);

  nr_vertices_rows = Wosize_val(ml_vertices_row);
  nr_vertices_cols = Wosize_val(ml_vertices_col);

  if(algorithm > 1 && algorithm != 5)
    kmax = p*p*p;

  libhmatrix_checkinit();

  /* Note that we make sure that all entries initially are null pointers. */
  if (0 == (hmatrix = calloc(1, sizeof(hmatrix_interna))))
    aiee("malloc() failure!");

  /* Now we populate the bemgrid3d object */
  hmatrix->pbuffer_lhs = safe_malloc(nr_vertices_rows*sizeof(double));
  hmatrix->pbuffer_rhs = safe_malloc(nr_vertices_cols*sizeof(double));

  /* Create bemgrid3d object and blockcluster for rows */
  DEBUGMSG("Building bemgrid3d and blockcluster for rows...\n");
  hmatrix->row = gridbuilder_create();
  gridbuilder_init(hmatrix->row, ml_vertices_row, ml_triangles_row,
                   ml_edges_row, ml_triangle_edges_row);
  gridbuilder_build_vertexcluster2(hmatrix->row, nmin, nr_vertices_rows);

  if (is_square) {
    DEBUGMSG("Matrix is square: using same bemgrid3d and blockcluster...\n");
    hmatrix->col = hmatrix->row;

  } else {
    /* Create bemgrid3d object and blockcluster for cols */
    DEBUGMSG("Building bemgrid3d and blockcluster for cols...\n");
    hmatrix->col = gridbuilder_create();
    gridbuilder_init(hmatrix->col, ml_vertices_col, ml_triangles_col,
                     ml_edges_col, ml_triangle_edges_col);
    gridbuilder_build_vertexcluster2(hmatrix->col, nmin, nr_vertices_rows);
  }

  /* Create a surfacebemfactory object for the double layer potential */
  DEBUGMSG("creating the surfacebemfactory...\n");
  hmatrix->sbf =
    dyn_new_surfacebemfactory_dlp_collocation(hmatrix->row->gr,
                                              HLIB_LINEAR_BASIS,
                                              hmatrix->row->ct,
                                              hmatrix->col->gr,
                                              HLIB_LINEAR_BASIS,
                                              hmatrix->col->ct,
                                              nfdeg, nfdeg, p, 0.0);

  /* Create a block cluster tree from the roots of two cluster trees
     row and col. */
  DEBUGMSG("creating the blockclustertree...\n");
  hmatrix->bcluster =
    dyn_build_blockcluster(hmatrix->row->ct->root, hmatrix->col->ct->root,
                           HLIB_MAXADMISSIBILITY, HLIB_BLOCK_INHOMOGENEOUS,
                           eta, 0);

  /* Compute a coarsened H-matrix approximation of an H-matrix implicitly
     defined by the block cluster tree hmatrix->bcluster and the hybrid cross
     approximation of the admissible leaves */
  DEBUGMSG("Creating the supermatrix from the blockcluster...\n");
  hmatrix->smx = dyn_coarsen_hca_from_blockcluster(hmatrix->bcluster,
                                                   hmatrix->sbf,
                                                   eps_aca, eps, 1, kmax);

  DEBUGMSG("Boxing hmatrix object into OCaml value...\n");
  CAMLlocal1(block);
  block = alloc_final(2, finalize_hmatrix, sizeof(void*), 10*sizeof(void*));
  Store_c_field(block, 1, hmatrix);

  DEBUGMSG("caml_hlib_raw_make_hmatrix: returning!\n");
  CAMLreturn(block);
}

CAMLprim value caml_hlib_get_matrix_stats(value ml_hmx)
{
  CAMLparam1(ml_hmx);
  CAMLlocal1(result);
  const size_t size_of_return_arr = 4;
  hmatrix_interna *hmx;

  result = alloc(Double_wosize*size_of_return_arr, Double_array_tag);

#define SET_RETURN_ARR(idx, val) \
  do {assert((idx) >= 0 && (idx) < size_of_return_arr); \
      Store_double_field((result), (idx), (val));} while(0)

  libhmatrix_checkinit();
  hmx = (hmatrix_interna *) Field(ml_hmx, 1);

  if (!hmx) {
    own_raise_with_string(*caml_named_value(err_exn_name),
                          "caml_hlib_get_matrix_stats: invalid supermatrix.");
    abort(); /* To avoid compiler complaints */

  } else {
    size_t nr_vertices_rows = hmx->row->nr_vertices,
           nr_vertices_cols = hmx->col->nr_vertices;
    double megabyte = 1024*1024,
           size_smx = dyn_getsize_supermatrix(hmx->smx)/megabyte,
           size_rk = dyn_getsizerk_supermatrix(hmx->smx)/megabyte,
           size_full = dyn_getsizefull_supermatrix(hmx->smx)/megabyte,
           size_fullbem = 8.0*nr_vertices_cols*nr_vertices_rows/megabyte;

    SET_RETURN_ARR(0, size_smx);
    SET_RETURN_ARR(1, size_rk);
    SET_RETURN_ARR(2, size_full);
    SET_RETURN_ARR(3, size_fullbem);

    CAMLreturn(result);
  }

#undef SET_RETURN_ARR
}

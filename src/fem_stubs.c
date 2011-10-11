/*
  Some Caml Stubs... initial skeleton.

  (C) 2006 Matteo Franchin, Thomas Fischbacher

  NOTES: ...
*/
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include "petsc.h"
#include "petscvec.h"

/* turn on debugging for the moment */
#define DEBUG_TIMESTEP_STUBS

/* Print extra debug messages (removed (fangohr 29/09/2006 10:06))) */
/* #define PRINT_DDD */

/* A useful macro to store objects of which the OCaml garbage collector
   should not worry about
 */
#define Store_c_field(block, offset, x) ( Field(block, offset) = (value) x )


/* For now, we do not care about distributed petsc vectors too much.

   Returns true on success, false when problems occurred
   (such as some petsc vectors already having been freed)
 */

typedef int manipulator_function(double **, double *);

CAMLprim value caml_forall_fem_sites_do(value ml_petsc_vectors,
					value ml_site_structure,
					/* by site: (vec-nr,offset, site-position) */
					value ml_aux_args,
					value ml_manipulator_to_call
					)
{
  CAMLparam4(ml_petsc_vectors, ml_site_structure,
             ml_aux_args, ml_manipulator_to_call);

  int nr_petsc_vectors, nr_sites, site_len,
    i, k,
    index_nr_field, index_offset,
    go_on, dimension, nr_aux_var;

  PetscScalar **vecs_data;
  double **refbuf;
  double *posbuf;
  manipulator_function *f;

  nr_petsc_vectors=Wosize_val(ml_petsc_vectors);
  nr_sites=Wosize_val(ml_site_structure);
  nr_aux_var=Wosize_val(ml_aux_args)/Double_wosize;

  f=(manipulator_function *)Field(ml_manipulator_to_call,0);

#ifdef PRINT_DDD
  fprintf(stderr,"DDD calling manipulator function at 0x%08x\n",f);fflush(stderr);
#endif

  if(nr_sites==0 || f==0)
    CAMLreturn(Val_bool(1));
  /* Nothing to do */

  dimension=Wosize_val(Field(Field(ml_site_structure,0),2))/Double_wosize;

  /* fprintf(stderr,"forall_sites_do: nr_petsc_vectors=%d nr_sites=%d dim=%d manipulator=0x%08x\n",nr_petsc_vectors,nr_sites,dimension,f);fflush(stderr); */

  for(i=0;i<nr_petsc_vectors;i++)
    {
      if(Field(Field(ml_petsc_vectors,i),1)==0)
        CAMLreturn(Val_bool(0)); /* Some Petsc vectors already were freed */
    }

  site_len=Wosize_val(Field(Field(ml_site_structure,0),1));

  if(   0==(refbuf=calloc(site_len,sizeof(double *)))
     || 0==(vecs_data=malloc(nr_petsc_vectors*sizeof(PetscScalar *)))
     || 0==(posbuf=malloc((nr_aux_var+dimension)*sizeof(double))))
    {
      fprintf(stderr,"(fem_stubs.c:caml_for_all_sites_do:) malloc() failure!\n");
      exit(1);
    }

  for(i=0;i<nr_petsc_vectors;i++)
    {
      Vec v=(Vec)(Field(Field(ml_petsc_vectors,i),1));
      VecGetArray(v,&vecs_data[i]);
    }

  go_on=1;

  for(i=0;i<nr_aux_var;i++)
    posbuf[i] = Double_field(ml_aux_args, i);

  for(k=0;k<nr_sites && go_on;k++)
    {
      for(i=0;i<dimension;i++)
        {
          posbuf[i+nr_aux_var]=Double_field(Field(Field(ml_site_structure,k),2),i);
        }

      for(i=0;i<site_len;i++)
        {
          index_nr_field=(String_val(Field(Field(ml_site_structure,k),0)))[i];
          index_offset=Int_val(Field(Field(Field(ml_site_structure,k),1),i));

          refbuf[i] = (index_offset==(-1))? 0:&(vecs_data[index_nr_field][index_offset]);
        }
      /* Now, we prepared all the site pointers.
         It is time to call the function...
       */
      if(f(refbuf,posbuf)==0)go_on=0;
    }

  for(i=nr_petsc_vectors-1;i>=0;i--)
    {
      VecRestoreArray((Vec)(Field(Field(ml_petsc_vectors,i),1)),&vecs_data[i]);
    }

  /* Write-back of aux_args - note that these can be modified by C code! */

  for(i=0;i<nr_aux_var;i++)
    Store_double_field(ml_aux_args, i, posbuf[i]);

  free(refbuf);
  free(vecs_data);
  free(posbuf);

  CAMLreturn(Val_bool(go_on));
}


#if 0
//DEAD CODE, COMMENTED OUT
// NOTE: this function is buggy. min_distance is never set, but apparently
//   enters the computation.

CAMLprim value caml_dipole_evaluator_sumpairs_fast_ccode(value ml_params,
							 value ml_posns,
							 value ml_sources,
							 value ml_result)
{
  CAMLparam4(ml_params,ml_posns,ml_sources,ml_result);
  int dim, result_range_offset, result_range_len,nr_sources,i_s,i_r;
  double min_distance,dist,idist,idist2,idist3,idist5,sprod;
  double v_dist[3],v_dipole[3],v_result_here[3];
  /* Note: may want to try using float instead for these vectors. Does this speed things up? */

  dim=Int_val(Field(ml_params,0));

  if(dim>3)
    {
      fprintf(stderr,"Dipole-pair summation not supported for dimension>3! Doing nothing!");
      CAMLreturn(Val_unit);
    }

  result_range_offset=Int_val(Field(ml_params,1));
  result_range_len=Wosize_val(ml_result)/(Double_wosize*3);
  nr_sources=Wosize_val(ml_sources)/(Double_wosize*3);

  for(i_r=0;i_r<result_range_len;i_r++)
    {
      v_result_here[0]=v_result_here[1]=v_result_here[2]=0.0;
      
      for(i_s=0;i_s<nr_sources;i_s++)
	{
	  v_dist[0] = Double_field(ml_posns,dim*i_s)-Double_field(ml_posns,dim*(i_r+result_range_offset));
	  v_dist[1] = dim<2?
	    0.0:
	    (Double_field(ml_posns,1+dim*i_s)-Double_field(ml_posns,1+dim*(i_r+result_range_offset)));
	  v_dist[2] = dim<3?
	    0.0:
	    (Double_field(ml_posns,2+dim*i_s)-Double_field(ml_posns,2+dim*(i_r+result_range_offset)));
	  
	  dist=sqrt(v_dist[0]*v_dist[0]+v_dist[1]*v_dist[1]+v_dist[2]*v_dist[2]);
	  
	  if(dist>min_distance) /* Note: min_distance=0 means: include everything except self-interaction */
	    {
	      idist= dist=0.0?0.0:(1.0/dist);
	      idist2=idist*idist;
	      idist3=idist2*idist;
	      idist5=idist2*idist3;

	      v_dipole[0]=Double_field(ml_sources,0+3*i_s);
	      v_dipole[1]=Double_field(ml_sources,1+3*i_s);
	      v_dipole[2]=Double_field(ml_sources,2+3*i_s);

	      sprod=v_dipole[0]*v_dist[0]+v_dipole[1]*v_dist[1]+v_dipole[2]*v_dist[2];

	      v_result_here[0] += v_dipole[0]*idist3 - 3.0*v_dist[0]*sprod*idist5;
	      v_result_here[1] += v_dipole[1]*idist3 - 3.0*v_dist[1]*sprod*idist5;
	      v_result_here[2] += v_dipole[2]*idist3 - 3.0*v_dist[2]*sprod*idist5;
	      
	      if(i_r==249)	/* Just print some random dipole data... */
		{
		  printf("DDD Contrib result249 pos=(%8.4f %8.4f %8.4f) src pos=(%8.4f %8.4f %8.4f) strength=(%8.4f %8.4f %8.4f) contrib-p=(%8.4f %8.4f %8.4f) contrib-x=(%8.4f %8.4f %8.4f)\n",
			 Double_field(ml_posns,dim*i_r+0),
			 Double_field(ml_posns,dim*i_r+1),
			 Double_field(ml_posns,dim*i_r+2),
			 Double_field(ml_posns,dim*i_s+0),
			 Double_field(ml_posns,dim*i_s+1),
			 Double_field(ml_posns,dim*i_s+2),
			 Double_field(ml_sources,3*i_s+0),
			 Double_field(ml_sources,3*i_s+1),
			 Double_field(ml_sources,3*i_s+2),
			 v_dipole[0]*idist3,
			 v_dipole[1]*idist3,
			 v_dipole[2]*idist3,
			 - 3.0*v_dist[0]*sprod*idist5,
			 - 3.0*v_dist[1]*sprod*idist5,
			 - 3.0*v_dist[2]*sprod*idist5
			 );
		}

	    }
	}

      if(i_r==249)	/* Just print some random dipole data... */
	{
	  printf("DDD result249 total=(%8.4f %8.4f %8.4f)\n",
		 v_result_here[0],
		 v_result_here[1],
		 v_result_here[2]);
	}

      Store_double_field(ml_result,0+i_r*3, v_result_here[0]);
      Store_double_field(ml_result,1+i_r*3, v_result_here[1]);
      Store_double_field(ml_result,2+i_r*3, v_result_here[2]);
    }

  CAMLreturn(Val_unit);
}
#endif

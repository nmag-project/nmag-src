/* 
   (C) 2006 Dr. Thomas Fischbacher

   Low-level mumag2 stubs that help in the construction of the Jacobi matrix
   
*/

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <mpi.h>

#include <float.h>

/* #include "../mpi_petsc/camlmpi.h" */

#include <caml/bigarray.h>

#include <unistd.h>
#include <stdio.h>


#include "petsc.h"
#include "petscvec.h"
#include "petscmat.h"

/* This is for debugging the Jacobian */
static int ddd_rows_cols[]={0,1,2,3,4,5,6};
static PetscScalar ddd_data[49];


CAMLprim caml_mumag_jacobi_plan_executor(value ml_plan,
					 value ml_mx,
					 value ml_m,
					 value ml_h_total)
{
  CAMLparam4(ml_plan,ml_mx,ml_m,ml_h_total);

  /* XXX Actually, we should check ml_mx etc. with petsc_check_mat(),
     but this is not (yet) exported at the C level in petsc_stubs.c
     So, for now, we do not do this. I suppose this is okay, as this
     implementation of the equation-of-motion code will be replaced
     by something far more general in the future, anyway.
  */

  Mat mx;
  Vec v_m, v_h_total;
  PetscScalar *data_m, *data_h_total;
  int
    nr_matrix_entries_updated=0, plan_length_outer,plan_length_inner,
    p_ix_o,p_ix_i,
    ix_le,ix_ri,ix_h,ix_m,nr_m;
  double coeff;

  CAMLlocal3(plan_inner,plan_entry,plan_indices);

  mx=(Mat)Field(ml_mx,1);
  v_m=(Vec)Field(ml_m,1);
  v_h_total=(Vec)Field(ml_h_total,1);

  VecGetArray(v_m,&data_m);
  VecGetArray(v_h_total,&data_h_total);

  MatZeroEntries(mx);
  
  plan_length_outer=Wosize_val(ml_plan);

  for(p_ix_o=0;p_ix_o<plan_length_outer;p_ix_o++)
    {
      plan_inner=Field(ml_plan,p_ix_o);
      plan_length_inner=Wosize_val(plan_inner);

      for(p_ix_i=0;p_ix_i<plan_length_inner;p_ix_i++)
	{
	  plan_entry=Field(plan_inner,p_ix_i);
	  coeff=Double_val(Field(plan_entry,0));
	  plan_indices=Field(plan_entry,1);
	  ix_le=Int_val(Field(plan_indices,0));
	  ix_ri=Int_val(Field(plan_indices,1));
	  
	  ix_h=Int_val(Field(plan_indices,2));

	  if(ix_h != -1)
	    {
	      coeff*=data_h_total[ix_h];
	    }

	  nr_m=Wosize_val(plan_indices)-3;


	  for(ix_m=0;ix_m<nr_m;ix_m++)
	    {
	      coeff*=data_m[Int_val(Field(plan_indices,3+ix_m))];
	    }

	  if(coeff==0.0)coeff=DBL_MIN;

	  MatSetValue(mx,ix_le,ix_ri,coeff,ADD_VALUES);

	  if(((++nr_matrix_entries_updated) & 0xfff) == 0xfff) /* Do every 4096 entries */
	    {
	      MatAssemblyBegin(mx,MAT_FLUSH_ASSEMBLY);
	      MatAssemblyEnd(mx,MAT_FLUSH_ASSEMBLY);
	    }
	}
    }

  VecRestoreArray(v_h_total,&data_h_total);
  VecRestoreArray(v_m,&data_m);

  MatAssemblyBegin(mx,MAT_FINAL_ASSEMBLY);
  MatAssemblyEnd(mx,MAT_FINAL_ASSEMBLY);

  if(0)
    { /* DDD print a few entries of the Jacobian matrix to check it */
      int i,j;
      MatGetValues(mx,7,ddd_rows_cols,7,ddd_rows_cols,ddd_data);
      for(i=0;i<5;i++)
	{
	  fprintf(stderr,"DDD Jacobian row %d: ",i);
	  for(j=0;j<7;j++)
	    {
	      fprintf(stderr,"  %6.3f",ddd_data[i*7+j]);
	    }
	  fprintf(stderr,"\n");
	}
      fflush(stderr);
    }

  CAMLreturn(Val_unit);
}

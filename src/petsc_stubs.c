/*
  Some Caml Stubs making working with petsc convenient

  (C) 2005, 2006 Dr. Thomas Fischbacher, Giuliano Bordignon

  Philosophy (T.F.):

  Quite in general, one can use one of two different approaches with marshalling
  (i.e. mapping values from a low level system to a high level system):

  (1) Either try to stay as close to the primitive mechanisms of the low level system,

  (2) or make things as convenient as possible using the features of the high level system.

  Advantages of (1):
   - Doesn't accidentally bury any functionality users will want to use.
   - Easier to do, maybe even in a semi-automatic fashion (SWIG etc.)
   - If it has to be, it can even be used without documentation -
      the user will just follow his/her expectations.
      (Not that one should not document wrapped interfaces. But as a matter of fact,
      there are many undocumented ones out there in the wild, which one may want to use!)

  Advantages of (2):
   - Can employ the full power of the high level system,
   - Especially, can provide better error detection & handling
   - Good properties of the high level system are not spoilt by low level code
     (= the system still won't segv!)
   - Gives the programmer a free tour of all of the low level system.
     (Good if he wants to learn about it in the first place, a disadvantage otherwise)

  While I generally prefer approach (1), we will choose approach (2) for this
  particular application.


  Note: Every Petsc Thingy is a PetscObject, and can be typecast to
  that. PetscObject is declared a pointer, hence this gives us
  information about the size of PetscVec, KSP, whatever: they just are
  pointers.

  XXX Note: should change/systematize naming conventions: a vector x,
  which is ml-wrapped, should be called ml_x throughout.

*/

/*
  This code requires a C99 compiler: sometimes we are indeed using the macro
  CAMLlocalX after the declaration section of compound statements {...}.
  This will lead to an error for C versions < C99, as CAMLparamX is a macro
  which expands to code which declares new variables.
 */

#include <stdio.h>
#include <float.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <mpi.h>

#include "camlmpi.h"

#include <caml/bigarray.h>

#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>

#include "petsc.h"
#include "petscksp.h"
#include "petscts.h"
#include "petscvec.h"
#include "petscmat.h"
#include "petscsys.h"

#include <parmetis.h>


/* ^ This is just for debugging! */


/* The values for DEBUG_PETSC_STUBS can be set as follows:
 *  2 --> debug message when any of the functions defined in this file
 *        is executed.
 *  1 --> similar to 1, but excluding frequently executed function, such as:
 *        caml_petsc_mat_set, caml_petsc_mat_get, ...
 *  0 --> no debug messages at all;
 */
#define DEBUG_PETSC_STUBS 0

/* better: do {statement1; statement2;} while(0)
 * than:   statement1, statement2;
 * The first follows the oldest standard of the C language.
 */
#define DEBUG_PRINT(name) \
  do {fprintf(stderr,"petsc_stubs: %s\n",name); fflush(stderr);} while(0)

#if DEBUG_PETSC_STUBS > 0
#  define DEBUG_LOG1(x) DEBUG_PRINT(x)
#  define DEBUG_LOG(x) DEBUG_PRINT(x)
#else
#  define DEBUG_LOG1(x)
#  define DEBUG_LOG(x)
#endif

#if DEBUG_PETSC_STUBS > 1
#  define DEBUG_LOG2(x) DEBUG_PRINT(x)
#else
#  define DEBUG_LOG2(x)
#endif

#define Store_c_field(block,offset,x) (Field(block,offset)=(value)x)

/* Some macros to check for Petsc versions */
#include <petscversion.h>

#define VER_DIGIT_GE(l, r, next) ((l) > (r) || ((l) == (r) && (next)))

#define VER_GE(l1, l2, l3, r1, r2, r3) \
  VER_DIGIT_GE(l1, r1, VER_DIGIT_GE(l2, r2, l3 >= r3))

#define VER_GT(l1, l2, l3, r1, r2, r3) \
  VER_DIGIT_GE(l1, r1, VER_DIGIT_GE(l2, r2, l3 > r3))

#define PETSC_VERSION_GE(major, minor, subminor) \
  VER_GE(PETSC_VERSION_MAJOR, PETSC_VERSION_MINOR, PETSC_VERSION_SUBMINOR, \
         (major), (minor), (subminor))

#define PETSC_VERSION_LE(major, minor, subminor) \
  VER_GE((major), (minor), (subminor), \
         PETSC_VERSION_MAJOR, PETSC_VERSION_MINOR, PETSC_VERSION_SUBMINOR)

#define PETSC_VERSION_GT(major, minor, subminor) \
  VER_GT(PETSC_VERSION_MAJOR, PETSC_VERSION_MINOR, PETSC_VERSION_SUBMINOR, \
         (major), (minor), (subminor))

#define PETSC_VERSION_LT(major, minor, subminor) \
  VER_GT((major), (minor), (subminor), \
         PETSC_VERSION_MAJOR, PETSC_VERSION_MINOR, PETSC_VERSION_SUBMINOR)

#if PETSC_VERSION_LE(2, 2, 0)
typedef int PetscErrorCode;
/*
 Comment ad 2.2.0:
 It's in the petsc manual as if it were a properly defined type,
 but it is not. Those $%!*#$$ &&!&%#$$!!!
 */
#endif

static char *err_exn_name="ocaml_exn_petsc_caml_interface";

static char *err_notinited="Petsc is not initialized!";
static char *err_vec="Invalid Petsc vector!";
static char *err_mat="Invalid Petsc matrix!";
static char *err_ts="Invalid Petsc TS!";
static char *err_ksp="Invalid Petsc KSP!";

extern char *_caml_dispatch_petsc_error_str(int);
/* ^ This is from the machine-generated source petsc_error.c! */

static int caml_petsc_is_initialized=0;

extern int _caml_petsc_error_nr_to_caml(int);
extern int _caml_petsc_error_nr_from_caml(int);

/* ====== CPU CYCLES ====== */

static inline double get_cpu_cycle_counter(void)
{
  uint64_t count=0;

#ifdef __tune_i686__
  /* XXX actually, this #ifdef is somewhat inappropriate. Should use something better! */

  __asm__ volatile("rdtsc\n\t" : "=A"(count));
#endif

  return ((double)count);
}

typedef struct {
  double cycles_ksp_solve;
  double cycles_mat_mult;
  double cycles_linear;
} petsc_cpu_cycles;

static petsc_cpu_cycles cpu_cycles_counted = {0.0,0.0,0.0};

CAMLprim value caml_petsc_reset_cpu_cycle_counters(value ml_unit)
{
  CAMLparam1(ml_unit);

  DEBUG_LOG("caml_petsc_reset_cpu_cycle_counters");
  cpu_cycles_counted.cycles_ksp_solve=0.0;
  cpu_cycles_counted.cycles_mat_mult=0.0;
  cpu_cycles_counted.cycles_linear=0.0;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_get_cpu_cycle_counters(value ml_unit)
{
  CAMLparam1(ml_unit);

  CAMLlocal1(result);

  DEBUG_LOG("caml_petsc_get_cpu_cycle_counters");

  result=alloc_tuple(3);

  Store_field(result,0,copy_double(cpu_cycles_counted.cycles_ksp_solve));
  Store_field(result,1,copy_double(cpu_cycles_counted.cycles_mat_mult));
  Store_field(result,2,copy_double(cpu_cycles_counted.cycles_linear));

  CAMLreturn(result);
}

static inline void petsc_checkinit(void)
{
  if(!caml_petsc_is_initialized)
    raise_with_string(*caml_named_value(err_exn_name),err_notinited);
}

static inline void check_petsc_custom_block(value block, char *msg)
{
  if(0==(void *)Field(block,1))
    raise_with_string(*caml_named_value(err_exn_name),msg);
}

static inline void petsc_check_vec(value block)
{
  check_petsc_custom_block(block,err_vec);
}

static inline void petsc_check_mat(value block)
{
  check_petsc_custom_block(block,err_mat);
}

static inline void petsc_check_ts(value ml_ts)
{
  if(0==(void *)Field(Field(ml_ts,0),1))
    raise_with_string(*caml_named_value(err_exn_name),err_ts);
}

static inline void petsc_check_ksp(value ml_ksp)
{
  if(0==(void *)Field(Field(ml_ksp,0),1))
    raise_with_string(*caml_named_value(err_exn_name),err_ksp);
}


static void aiee(char *msg)
{
  fprintf(stderr,"AIEE - FATAL FAILURE: %s\n",msg);
  exit(1);
}

/* This will be registered with PetscPushErrorHandler.
   The idea is to raise an appropriate OCaml exception
   when a Petsc error occurs.
*/
int petsc_error(int line,
		const char *func_name,
		const char *file_name,
		const char *dir_name,
		PetscErrorCode nr_generic,
		int nr_specific,
		const char *message,
		void *closure_arg_ignored)
{
  /* XXX Note that this should properly unwind the Ocaml/C call stack!
     I strongly suppose so, but for now this is only an assumption
     that has to be checked!

     -- at least, it looks pretty good when just using it.
   */
  CAMLlocal1(ex);
  const char *my_message = (message != NULL ?
			    NULL : "PETSc exception");
  /* ^^^ note that message may be NULL (in particular when a signal is raised
         by the OS, e.g. SIGTERM) */

  ex = alloc_tuple(3);
  Store_field(ex, 0, Val_int(_caml_petsc_error_nr_to_caml(nr_generic)));
  Store_field(ex, 1, Val_int(nr_specific));
  Store_field(ex, 2, copy_string(my_message));
  raise_with_arg(*caml_named_value("ocaml_exn_petsc"), ex);

  /* PETSc documentation doesn't say what this function should return, but we
     know it must be a PetscError. It is then natural to return nr_generic. */
  return nr_generic;
}

/** Convert an integer (OCaml) to a MatStructure value */
static MatStructure matstructure_from_int(int i) {
  switch(i) {
  case 1: return SAME_NONZERO_PATTERN;
  case 2: return SAME_PRECONDITIONER;
  default: return DIFFERENT_NONZERO_PATTERN;
  }
}

CAMLprim value caml_petsc_init(value args, value rcfile,
			       value help_string, value do_install_sighandler)
{
  CAMLparam4(args,rcfile,help_string, do_install_sighandler);

  char **argv=0, **argv_to_petsc=0;
  int argc=0,argc_to_petsc=0;
  int i;

  DEBUG_LOG("caml_petsc_init");

  /* First, make this idempotent: */
  if(caml_petsc_is_initialized!=0)
    {
      CAMLreturn(Val_bool(0));
      /* This already was initialized! */
    }

  argc=Wosize_val(args);
  /* this is in CAML-words.
     See:
     http://pauillac.inria.fr/cdrom/ftp/caml/ocaml-3.04-refman.txt
  */

  if(0==argc)
    {
      raise_with_arg(*caml_named_value("ocaml_exn_petsc_caml_interface"),
		     copy_string("Refusing to pass empty argv to Petsc - it would SEGV!"));
    }


  if(0==(argv=malloc((1+argc)*sizeof(char*))))
    {
      aiee("malloc() failure!");
    }

  for(i=0;i<argc;i++)
    {
      argv[i]=String_val(Field(args,i));
    }
  argv[argc]=0;

  argv_to_petsc=argv;
  argc_to_petsc=argc;

  PetscInitialize(&argc_to_petsc,
		  &argv_to_petsc,
		  String_val(rcfile),
		  String_val(help_string));

  if(Bool_val(do_install_sighandler))
    {
      /* printf("DDD petsc installing sighandler!\n");fflush(stdout); / * DDD */
      PetscPushErrorHandler(&petsc_error,0);
    }
  else
    {
      /* printf("DDD petsc NOT installing sighandler!\n");fflush(stdout); / * DDD */
    }

  caml_petsc_is_initialized=1;

  /*
    XXX Here, we might try to analyze the args that petsc left over
    and pass them back to ocaml. For now, I see no point in that.
  */

  free(argv);

  CAMLreturn(Val_bool(1));
  /* Initialization performed, and it succeeded. */
}

CAMLprim value caml_petsc_finalize(value ml_unit)
{
  CAMLparam1(ml_unit);

  DEBUG_LOG("caml_petsc_finalize");

  petsc_checkinit();

  PetscFinalize();
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_check_initialized(value ml_unit)
{
  CAMLparam1(ml_unit);
  CAMLreturn(Val_bool(caml_petsc_is_initialized));
}


CAMLprim value caml_petsc_get_comm_world(value ml_unit)
{
  CAMLparam1(ml_unit);

  DEBUG_LOG("caml_petsc_get_comm_world");
  CAMLreturn(caml_mpi_alloc_comm_nonfinalizing(PETSC_COMM_WORLD));
}

CAMLprim value caml_petsc_get_comm_self(value ml_unit)
{
  CAMLparam1(ml_unit);

  DEBUG_LOG("caml_petsc_get_comm_self");
  CAMLreturn(caml_mpi_alloc_comm_nonfinalizing(PETSC_COMM_SELF));
}


static void finalize_petsc_vec(value block)
{
  Vec v;

  DEBUG_LOG1("finalize_petsc_vec");
  v=(Vec)Field(block,1);
  if(v!=0)
    {
      const char *name;

      PetscObjectGetName((PetscObject)v,&name);
      /* fprintf(stderr,"Finalizing vec '%s' %p\n",name,v);fflush(stderr); */

      VecDestroy(v);
    }
}

static void no_finalize_petsc(value block)
{
  DEBUG_LOG1("no_finalize_petsc");
}



static void finalize_petsc_mat(value block)
{
  Mat m;
  /* XXX NOTE: auto-finalizing matrices leads to a bug at the moment! */

  DEBUG_LOG1("finalize_petsc_mat");
  m=(Mat)Field(block,1);
  if(m!=0)
    {
      const char *name;

      PetscObjectGetName((PetscObject)m,&name);

      /* fprintf(stderr,"Finalizing mat '%s' %p\n",name,m);fflush(stderr); */

      MatDestroy(m);
    }
}

static void finalize_petsc_ksp_block(value block)
{
  KSP ksp;

  /* XXX NOTE: auto-finalizing KSPs leads to a crash at the moment!
     XXX is this still true - should not be so anymore now, as I fixed that bug?! Check!
  */

  DEBUG_LOG1("finalize_petsc_ksp_block");
  ksp=(KSP)Field(block,1);
  if(ksp!=0)
    {
      const char *name;

      PetscObjectGetName((PetscObject)ksp,&name);

      /* fprintf(stderr,"Finalizing ksp '%s' %p\n",name,ksp);fflush(stderr); */

      KSPDestroy(ksp);
    }
}

static void finalize_petsc_matnullspace_block(value block)
{
  MatNullSpace nullsp;

  /* XXX NOTE: auto-finalizing KSPs leads to a crash at the moment!
     XXX is this still true - should not be so anymore now, as I fixed that bug?! Check!
  */

  DEBUG_LOG1("finalize_petsc_matnullspace_block");
  nullsp=(MatNullSpace)Field(block,1);
  if(nullsp!=0)
    {
      const char *name;
      PetscObjectGetName((PetscObject)nullsp,&name);
      /* fprintf(stderr,"DDD Finalizing MatNullSpace '%s' %p\n",name,nullsp);fflush(stderr); */

      MatNullSpaceDestroy(nullsp);
    }
}


/* Note: a TS is special, as the block will contain global roots
   holding:

   - the callback ML function for ts_rhs_function
   - the callback ML function for ts_rhs_jacobian
 */
static void finalize_petsc_ts_block(value block)
{
  TS ts;
  value *ml_cell1, *ml_cell2;

  DEBUG_LOG1("finalize_petsc_ts_block");

  ts=(TS)Field(block,1);
  if(ts==0)return;

  ml_cell1=(value *)Field(block,2);
  ml_cell2=(value *)Field(block,3);

  if(ml_cell1) caml_remove_global_root(ml_cell1);
  if(ml_cell2) caml_remove_global_root(ml_cell2);

  Store_c_field(block,2,0);
  Store_c_field(block,3,0);

  if(ml_cell1)
    {
      free(ml_cell1); /* cell1 and cell2 will be from the same malloc block */
    }

  TSDestroy(ts);
}

static void finalize_petsc_matfdcoloring(value block)
{
  MatFDColoring c;

  DEBUG_LOG1("finalize_petsc_matfdcoloring");
  c=(MatFDColoring)Field(block,1);
  if(c!=0)
    {
      MatFDColoringDestroy(c);
    }
}

/* Here, we have a problem:

   The C side callbacks are designed in such a way that they get the
   TS parameter as an argument.

   Unfortunately, we cannot "get a handle" at the proper TS ml-value
   in the C stubs wrapper, which we could pass on to the ml-callback
   function. The reason is that this would "create a reference loop
   around a globally registered GC root", and hence make some values
   non-reclaimable.

   Work-around for now: just do not pass on TS to the ML callback!
 */

static PetscErrorCode _ts_rhs_function_wrapper(TS ts, double dt,
					       Vec v_in, Vec v_out,
					       void *callback_posn)
{
  CAMLparam0();

  value *callbacks_ml=(value *)callback_posn;

  CAMLlocal4(callback,callback_return,ml_v_in,ml_v_out);

  callback=callbacks_ml[0];

  if(callback==Val_unit)
    {
      raise_with_string(*caml_named_value(err_exn_name),"No Callback function for RHS registered!");
    }

  /* Actually, these vector-holding blocks are passed on to OCaml,
     and will be destroyed afterwards again. We must, however, make sure
     that their destruction does not call the vector finalizer.

     This usually is not a big deal, but it means that we have to be careful
     not passing the vectors out of the ML callback function...
   */

  ml_v_in = alloc_final(2, no_finalize_petsc, sizeof(Vec), sizeof(Vec));
  Store_c_field(ml_v_in, 1, v_in);

  ml_v_out = alloc_final(2, no_finalize_petsc, sizeof(Vec), sizeof(Vec));
  Store_c_field(ml_v_out, 1, v_out);

  callback_return=callback3(callback,copy_double(dt),ml_v_in,ml_v_out);

  CAMLreturn(_caml_petsc_error_nr_from_caml(Int_val(callback_return)));
}

static PetscErrorCode _ts_rhs_jacobian_wrapper(TS ts, double dt,
					       Vec v_in, Mat *jacobi, Mat *precond,
					       MatStructure *flag, void *callback_posn)
{
  value callback_args[5];
  value *callbacks_ml=(value *)callback_posn;

  CAMLlocal4(callback,callback_return,ml_dt,ml_v_in);
  CAMLlocal3(ml_jacobi,ml_precond, ml_flag);

  callback=callbacks_ml[0];

  if(callback==Val_unit)
    {
      raise_with_string(*caml_named_value(err_exn_name),"No Callback function for Jacobian registered!");
    }

  ml_dt = copy_double(dt);

  ml_v_in = alloc_final(2, finalize_petsc_vec, sizeof(Vec), sizeof(Vec));
  Store_c_field(ml_v_in, 1, v_in);

  ml_jacobi = alloc_final(2, finalize_petsc_mat, sizeof(Mat), sizeof(Mat));
  Store_c_field(ml_jacobi, 1, *jacobi);

  ml_precond = alloc_final(2, finalize_petsc_mat, sizeof(Mat), sizeof(Mat));
  Store_c_field(ml_precond, 1, *precond);

  *flag = SAME_NONZERO_PATTERN;

  callback_args[0]=ml_dt;
  callback_args[1]=ml_v_in;
  callback_args[2]=ml_jacobi;
  callback_args[3]=ml_precond;

  callback_return=callbackN(callback,4,callback_args);

  return _caml_petsc_error_nr_from_caml(Int_val(callback_return));
}

/* NOTE: this is quite preliminary,
   as are some of the following functions.

   Once I have a better understanding of petsc,
   this may be subject to change...
 */
CAMLprim value caml_petsc_vec_create_v1(value ml_global_size, value ml_name)
{
  CAMLparam2(ml_global_size,ml_name);

  Vec v;

  DEBUG_LOG("caml_petsc_vec_create_v1");
  petsc_checkinit();

  VecCreateSeq(PETSC_COMM_SELF,Int_val(ml_global_size),&v);
  PetscObjectSetName((PetscObject)v,String_val(ml_name));
  VecSetFromOptions(v);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_vec, sizeof(Vec), 15*sizeof(Vec));
  Store_c_field(block, 1,v);

  /* fprintf(stderr,"CREATE VEC '%s' => %p\n",String_val(ml_name),v); */
  CAMLreturn(block);
}

CAMLprim value caml_petsc_vec_create_mpi(value ml_comm,value ml_global_size, value ml_local_size, value ml_name)
{
  CAMLparam4(ml_comm,ml_global_size,ml_local_size,ml_name);
  MPI_Comm comm;
  Vec v;

  DEBUG_LOG("caml_petsc_vec_create_mpi");
  petsc_checkinit();
  comm=Comm_val(ml_comm);

  VecCreateMPI(comm,Int_val(ml_local_size),Int_val(ml_global_size),&v);
  PetscObjectSetName((PetscObject)v,String_val(ml_name));
  VecSetFromOptions(v);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_vec, sizeof(Vec), 15*sizeof(Vec));
  Store_c_field(block, 1,v);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_obj_name(value ml_petsc)
{
  CAMLparam1(ml_petsc);

  PetscObject p;
  const char *name;

  p=(PetscObject)Field(ml_petsc,1);

  if(p==0)
    {
      raise_with_string(*caml_named_value(err_exn_name),"Tried to obtain name for invalid petsc object!");
    }

  PetscObjectGetName(p,&name);

  CAMLreturn(copy_string(name));
}



/* Return an invalid dummy vector object of the proper type.
   What for? We may have to satisfy the type system without
   wanting to initialize petsc.
 */
CAMLprim value caml_petsc_vec_dummy(value ml_unit)
{
  CAMLparam1(ml_unit);

  Vec v=0;

  CAMLlocal1(block);
  DEBUG_LOG1("caml_petsc_vec_dummy");
  block = alloc_final(2, finalize_petsc_vec, sizeof(Vec), 15*sizeof(Vec));
  Store_c_field(block, 1,v);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_mat_dummy(value ml_unit)
{
  CAMLparam1(ml_unit);

  Mat m=0;

  CAMLlocal1(block);
  DEBUG_LOG1("caml_petsc_mat_dummy");
  block = alloc_final(2, finalize_petsc_mat, sizeof(Mat), 15*sizeof(Mat));
  Store_c_field(block, 1,m);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_ksp_dummy(value ml_unit)
{
  CAMLparam1(ml_unit);

  KSP k=0;

  CAMLlocal1(ksp_block);
  DEBUG_LOG1("caml_petsc_ksp_dummy");
  ksp_block = alloc_final(2, finalize_petsc_ksp_block, sizeof(KSP), 15*sizeof(KSP));
  Store_c_field(ksp_block, 1,k);

  CAMLlocal1(ksp_tuple);
  ksp_tuple=alloc_tuple(6); /* (ksp_block,mat_a,mat_p,rhs,solution,nullspace) */
  Store_field(ksp_tuple,0,ksp_block);
  Store_field(ksp_tuple,1,Val_unit);
  Store_field(ksp_tuple,2,Val_unit);
  Store_field(ksp_tuple,3,Val_unit);
  Store_field(ksp_tuple,4,Val_unit);
  Store_field(ksp_tuple,5,Val_unit);

  CAMLreturn(ksp_tuple);
}


CAMLprim value caml_petsc_vec_destroy(value ml_vec)
{
  CAMLparam1(ml_vec);
  Vec vec;

  DEBUG_LOG("caml_petsc_vec_destroy");
  vec=(Vec)Field(ml_vec,1);

  if(vec!=0)
    {
      VecDestroy(vec);
      Store_c_field(ml_vec,1,0);
    }

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_mat_destroy(value ml_mat)
{
  CAMLparam1(ml_mat);
  Mat mat;

  DEBUG_LOG("caml_petsc_mat_destroy");
  mat=(Mat)Field(ml_mat,1);

  if(mat!=0)
    {
      MatDestroy(mat);
      Store_c_field(ml_mat,1,0);
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ksp_destroy(value ml_ksp)
{
  CAMLparam1(ml_ksp);
  KSP ksp;

  DEBUG_LOG("caml_petsc_ksp_destroy");

  ksp=(KSP)Field(Field(ml_ksp,0),1);

  if(ksp!=0)
    {
      KSPDestroy(ksp);
      Store_c_field(Field(ml_ksp,0),1,0);
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_matnullspace_destroy(value ml_mns)
{
  CAMLparam1(ml_mns);
  MatNullSpace mns;

  DEBUG_LOG("caml_petsc_matnullspace_destroy");

  mns=(MatNullSpace)Field(Field(ml_mns,0),1);

  if(mns!=0)
    {
      MatNullSpaceDestroy(mns);
      Store_c_field(Field(ml_mns,0),1,0);
    }

  CAMLreturn(Val_unit);
}



CAMLprim value caml_petsc_ts_destroy(value ml_ts)
{
  CAMLparam1(ml_ts);
  TS ts;

  DEBUG_LOG("caml_petsc_ts_destroy");
  ts=(TS)Field(Field(ml_ts,0),1);

  if(ts)
    {
      Store_field(ml_ts,1,Val_unit);
      Store_field(ml_ts,2,Val_unit);
      Store_field(ml_ts,3,Val_unit);
      /* Kill references */

      TSDestroy(ts);
      Store_c_field(Field(ml_ts,0),1,0);
    }

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_vec_describe(value ml_vec)
{
  CAMLparam1(ml_vec);
  Vec vec;
  char *str;
  CAMLlocal1(ml_str);

  DEBUG_LOG1("caml_petsc_vec_describe");
  vec=(Vec)Field(ml_vec,1);

  ml_str=caml_alloc_string(1+2*sizeof(void *)); /* trailing \x00 */
  str=String_val(ml_str);
  snprintf(str,1+2*sizeof(void *),"%p",vec);

  CAMLreturn(ml_str);
}

CAMLprim value caml_petsc_mat_describe(value ml_mat)
{
  CAMLparam1(ml_mat);
  Mat mat;
  char *str;
  CAMLlocal1(ml_str);

  DEBUG_LOG1("caml_petsc_mat_describe");
  mat=(Mat)Field(ml_mat,1);

  ml_str=caml_alloc_string(1+2*sizeof(void *)); /* trailing \x00 */
  str=String_val(ml_str);
  snprintf(str,1+2*sizeof(void *),"%p",mat);

  CAMLreturn(ml_str);
}

CAMLprim value caml_petsc_mat_view(value ml_mat)
{
  CAMLparam1(ml_mat);
  Mat mat;

  DEBUG_LOG1("caml_petsc_mat_view");
  mat=(Mat)Field(ml_mat,1);
  MatView(mat,PETSC_VIEWER_STDOUT_WORLD);

  CAMLreturn(Val_unit);
}


/* Set one vector entry */
CAMLprim value caml_petsc_vec_set1(value ml_vec, value ml_index, value ml_val)
{
  CAMLparam3(ml_vec,ml_index,ml_val);

  Vec vec;
  PetscScalar x[1];
  PetscInt indices[1];

  DEBUG_LOG("caml_petsc_vec_set1");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);
  x[0]=Double_val(ml_val);
  indices[0]=Int_val(ml_index);

  VecSetValues(vec,1,indices,x,INSERT_VALUES);
  CAMLreturn(Val_unit);
}


/* Increase one vector entry
   Perhaps should be merged with vec_set1 (in the stubs at least).
   On the other hand, should not, if we want to minimize calling efforts
 */
CAMLprim value caml_petsc_vec_inc1(value ml_vec, value ml_index, value ml_val)
{
  CAMLparam3(ml_vec,ml_index,ml_val);

  Vec vec;
  PetscScalar x[1];
  PetscInt indices[1];

  DEBUG_LOG("caml_petsc_vec_inc1");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);
  x[0]=Double_val(ml_val);
  indices[0]=Int_val(ml_index);

  VecSetValues(vec,1,indices,x,ADD_VALUES);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_scale(value ml_vec, value ml_factor)
{
  CAMLparam2(ml_vec,ml_factor);

  Vec vec;
  double x;

  DEBUG_LOG("caml_petsc_vec_scale");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  x=Double_val(ml_factor);

#if PETSC_VERSION_LE(2, 2, 0)
  VecScale(&x,vec);
#else
  VecScale(vec,x);
#endif

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_zero(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;

  DEBUG_LOG("caml_petsc_vec_scale");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecZeroEntries(vec);
  CAMLreturn(Val_unit);
}


/* Convenience - do both assembly steps at once */
CAMLprim value caml_petsc_vec_assemble(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;

  DEBUG_LOG("caml_petsc_vec_assemble");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecAssemblyBegin(vec);
  VecAssemblyEnd(vec);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_assembly_begin(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;

  DEBUG_LOG("caml_petsc_vec_assembly_begin");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecAssemblyBegin(vec);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_assembly_end(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;

  DEBUG_LOG("caml_petsc_vec_assembly_end");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecAssemblyEnd(vec);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_global_size(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;
  PetscInt global_size;

  petsc_checkinit();
  petsc_check_vec(ml_vec);

  DEBUG_LOG1("caml_petsc_vec_global_size");
  vec=(Vec)Field(ml_vec,1);
  VecGetSize(vec,&global_size);

  CAMLreturn(Val_int(global_size));
}


CAMLprim value caml_petsc_vec_duplicate(value ml_vec_orig)
{
  CAMLparam1(ml_vec_orig);

  Vec master, duplicate;

  DEBUG_LOG("caml_petsc_vec_duplicate");
  petsc_checkinit();
  petsc_check_vec(ml_vec_orig);

  master=(Vec)Field(ml_vec_orig,1);

  VecDuplicate(master, &duplicate);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_vec, sizeof(Vec), 15*sizeof(Vec));
  Store_c_field(block, 1,duplicate);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_vec_copy(value ml_vec_src, value ml_vec_dst)
{
  CAMLparam2(ml_vec_src, ml_vec_dst);

  Vec src,dst;

  DEBUG_LOG("caml_petsc_vec_copy");
  petsc_checkinit();
  petsc_check_vec(ml_vec_src);
  petsc_check_vec(ml_vec_dst);

  src=(Vec)Field(ml_vec_src,1);
  dst=(Vec)Field(ml_vec_dst,1);

  VecCopy(src,dst);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_AXPBY(value alpha, value beta, value ml_vec_src, value ml_vec_dst)
{
  CAMLparam4(alpha, beta, ml_vec_src, ml_vec_dst);

  Vec src,dst;
  double alphabeta[2];
  double cycles_before, cycles_after;

  DEBUG_LOG("caml_petsc_vec_AXPBY");
  petsc_checkinit();
  petsc_check_vec(ml_vec_src);
  petsc_check_vec(ml_vec_dst);

  src=(Vec)Field(ml_vec_src,1);
  dst=(Vec)Field(ml_vec_dst,1);

  alphabeta[0]=Double_val(alpha);
  alphabeta[1]=Double_val(beta);

  cycles_before=get_cpu_cycle_counter();

#if PETSC_VERSION_LE(2, 2, 0)
  VecAXPBY(&alphabeta[0],&alphabeta[1],src,dst);
#else
  VecAXPBY(dst,alphabeta[0],alphabeta[1],src);
  /* What the hell are they smoking? */
#endif

  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_linear+=(cycles_after-cycles_before);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_vec_pointwise_mult(value ml_x, value ml_y,value ml_w)
{
  CAMLparam3(ml_x, ml_y,ml_w);
  double cycles_before, cycles_after;

  Vec vw, vx, vy;

  DEBUG_LOG("caml_petsc_vec_pointwise_mult");
  petsc_checkinit();
  petsc_check_vec(ml_w);
  petsc_check_vec(ml_x);
  petsc_check_vec(ml_y);

  vw=(Vec)Field(ml_w,1);
  vx=(Vec)Field(ml_x,1);
  vy=(Vec)Field(ml_y,1);

  cycles_before=get_cpu_cycle_counter();

  VecPointwiseMult(vw,vx,vy);

  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_linear+=(cycles_after-cycles_before);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_vec_pointwise_divide(value ml_x, value ml_y,value ml_w)
{
  CAMLparam3(ml_x, ml_y,ml_w);
  double cycles_before, cycles_after;

  Vec vw, vx, vy;

  DEBUG_LOG("caml_petsc_vec_pointwise_divide");
  petsc_checkinit();
  petsc_check_vec(ml_w);
  petsc_check_vec(ml_x);
  petsc_check_vec(ml_y);

  vw=(Vec)Field(ml_w,1);
  vx=(Vec)Field(ml_x,1);
  vy=(Vec)Field(ml_y,1);

  cycles_before=get_cpu_cycle_counter();

  VecPointwiseDivide(vw,vx,vy);

  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_linear+=(cycles_after-cycles_before);

  CAMLreturn(Val_unit);
}


/*
 Read out a PetSC vector and map it to OCaml.
 XXX Note that we use a somewhat braindead
 method to get the entries.
 */
CAMLprim value caml_petsc_vec_extract(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec, vec_local;
  PetscInt i, local_size,global_size;

  PetscScalar *data;

  DEBUG_LOG("caml_petsc_vec_extract");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecGetLocalSize(vec,&local_size);
  VecGetSize(vec,&global_size);

  if (local_size == global_size)
    vec_local = vec;

  else {
    /* I think the line above is wrong (vec should be vec_local).
       In general, the whole idea is here flawn (VecCopy does only copy vectors
       locally) */
    fprintf(stderr, "Suspended functionality. Contact the nmag developers.");
    assert(0); 
#if 0
    VecCreateSeq(PETSC_COMM_SELF, global_size, & vec);
#else
    VecCreateSeq(PETSC_COMM_SELF, global_size, & vec_local);
#endif
    VecCopy(vec, vec_local);
  }

  CAMLlocal1(result);

  result = alloc(Double_wosize*global_size, Double_array_tag);

  VecGetArray(vec_local, & data);

  for(i = 0; i < global_size; i++)
    Store_double_field(result, i, data[i]);

  VecRestoreArray(vec_local, & data);

  /* If we allocated that vector we have to destroy it. */
  if (local_size != global_size)  
    VecDestroy(vec_local);

  CAMLreturn(result);
}


CAMLprim value caml_petsc_vec_get_own_range(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;
  PetscInt mybase, myend;

  DEBUG_LOG("caml_petsc_vec_get_own_range");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecGetOwnershipRange(vec,&mybase,&myend);

  CAMLlocal1(result);

  result=alloc_tuple(2);

  Store_field(result,0,Val_int(mybase));
  Store_field(result,1,Val_int(myend));

  CAMLreturn(result);
}

/* Map a Petsc Vector onto a Caml Bigarray */

CAMLprim value caml_petsc_vec_as_bigarray_open_raw(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;
  long l_local_size[1];
  PetscInt i_local_size[1];
  int dim;

  PetscScalar *data;
  PetscErrorCode ierr;

  DEBUG_LOG("caml_petsc_vec_as_bigarray_open_raw");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  VecGetSize(vec,&dim);

  ierr=VecGetLocalSize(vec,&i_local_size[0]);
  ierr=VecGetArray(vec,&data);

  l_local_size[0]=i_local_size[0];

  CAMLreturn (alloc_bigarray(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT,
			/* XXX NOTE: THIS RELIES ON PetscScalar == double! */
			     1, data, l_local_size));
}

CAMLprim value caml_petsc_vec_as_bigarray_close_raw(value ml_vec, value ml_biga)
{
  CAMLparam2(ml_vec, ml_biga);

  Vec vec;
  PetscErrorCode ierr;

  DEBUG_LOG("caml_petsc_vec_as_bigarray_close_raw");
  petsc_checkinit();
  petsc_check_vec(ml_vec);

  vec=(Vec)Field(ml_vec,1);

  ierr=VecRestoreArray(vec,(Data_bigarray_val(ml_biga)));

  CAMLreturn(Val_unit);
}

/* Map a Caml Bigarray onto a Petsc Vector */

CAMLprim value caml_petsc_bigarray_as_vec_open_raw(value ml_biga)
{
  CAMLparam1(ml_biga);
  Vec v;

  petsc_checkinit();

  DEBUG_LOG1("caml_petsc_bigarray_as_vec_open_raw");

  VecCreateSeqWithArray(PETSC_COMM_SELF,
			Bigarray_val(ml_biga)->dim[0],
			Data_bigarray_val(ml_biga),
			&v);

  /* Also do PetscObjectSetName(); VecSetFromOptions(); ? */

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_vec, sizeof(Vec), 100000*sizeof(Vec));
  /* note the large size ratio above. There actually is a very good
     reason to do it that way - the petsc vector by itself is just an
     additional handle into the bigarray, does not really prevent the
     BA from being created, and the user has to make sure he will not
     invalidly pass around the vector after the bigarray may have
     been collected.
   */
  Store_c_field(block, 1,v);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_bigarray_as_vec_close_raw(value ml_vec)
{
  CAMLparam1(ml_vec);

  Vec vec;

  petsc_checkinit();
  DEBUG_LOG1("caml_petsc_bigarray_as_vec_close_raw");
  vec=(Vec)Field(ml_vec,1);

  if(vec!=0)
    {
      VecDestroy(vec);
      Store_c_field(ml_vec,1,0);
    }

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_mat_create_raw(value ml_comm, value ml_type, value ml_name, value ml_dims_global_local)
{
  CAMLparam4(ml_comm, ml_type, ml_name, ml_dims_global_local);

  Mat m;
  MPI_Comm comm;
  int g_rows, g_cols, l_rows, l_cols;

  DEBUG_LOG("caml_petsc_mat_create_raw");
  petsc_checkinit();

  g_rows=Int_val(Field(ml_dims_global_local,0));
  g_cols=Int_val(Field(ml_dims_global_local,1));
  l_rows=Int_val(Field(ml_dims_global_local,2));
  l_cols=Int_val(Field(ml_dims_global_local,3));

  if(g_rows==-1)g_rows=PETSC_DECIDE;
  if(g_cols==-1)g_cols=PETSC_DECIDE;
  if(l_rows==-1)l_rows=PETSC_DECIDE;
  if(l_cols==-1)l_cols=PETSC_DECIDE;

  comm=Comm_val(ml_comm);

  MatCreate(comm,&m);

  MatSetSizes(m,l_rows,l_cols,g_rows,g_cols);
  MatSetType(m,String_val(ml_type));

  PetscObjectSetName((PetscObject)m,String_val(ml_name));
  MatSetFromOptions(m);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_mat, sizeof(Mat), sizeof(Mat));
  Store_c_field(block, 1, m);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_mat_set_prealloc(value ml_mx, value ml_diagonal, value ml_off_diagonal)
{
  CAMLparam3(ml_mx, ml_diagonal, ml_off_diagonal);

  Mat m;
  const char *mtype;

  DEBUG_LOG("caml_petsc_mat_set_sizes");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  m=(Mat)Field(ml_mx,1);
  MatGetType(m,&mtype);

  if(0==strcmp(mtype,"seqaij"))
    {
      MatSeqAIJSetPreallocation(m,Int_val(ml_diagonal)+Int_val(ml_off_diagonal),PETSC_NULL);
    }
  else if(0==strcmp(mtype,"mpiaij"))
    {
      MatMPIAIJSetPreallocation(m,Int_val(ml_diagonal),PETSC_NULL,Int_val(ml_off_diagonal),PETSC_NULL);
    }
  else
    {
      raise_with_string(*caml_named_value(err_exn_name),
			"Cannot set up Petsc matrix pre-allocation: unknown matrix type");
    }

  MatSetUpPreallocation(m);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_matinfo_raw(value ml_mx, value ml_type)
{
  CAMLparam2(ml_mx,ml_type);

  Mat m;
  MatInfo minfo;
  int i, nr_entries, arg_type;
  PetscLogDouble *p;

  DEBUG_LOG1("caml_petsc_matinfo_raw");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  m=(Mat)Field(ml_mx,1);
  arg_type=Int_val(ml_type);

  CAMLlocal1(result);

  /* XXX I hope I do all this right... */

  nr_entries=sizeof(MatInfo)/sizeof(PetscLogDouble);

  result=alloc(Double_wosize*nr_entries,Double_array_tag);

  MatGetInfo(m,(arg_type==0?MAT_LOCAL:arg_type==1?MAT_GLOBAL_MAX:MAT_GLOBAL_SUM),
	     &minfo);

  p=(PetscLogDouble *)&minfo;

  for(i=0;i<nr_entries;i++)
    {
      Store_double_field(result,i,p[i]);
    }

  CAMLreturn(result);
}

CAMLprim value caml_petsc_mat_get_row_raw(value ml_mx, value ml_nr_row)
{
  CAMLparam2(ml_mx,ml_nr_row);

  Mat m;
  int row,nr_rows_total,nr_cols_total, i;
  int ncols;
  const int *cols;
  const PetscScalar *vals;

  DEBUG_LOG1("caml_petsc_mat_get_row_raw");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  m=(Mat)Field(ml_mx,1);
  row=Int_val(ml_nr_row);

  CAMLlocal3(result_vals,result_indices,result);

  MatGetSize(m, &nr_cols_total, &nr_rows_total);

  if(row>=nr_rows_total || row < 0)
    {
      raise_with_string(*caml_named_value(err_exn_name),
			"MatGetRow: invalid row number!");
    }

  MatGetRow(m,row,&ncols,&cols,&vals);

  result_vals=alloc(Double_wosize*ncols,Double_array_tag);
  result_indices=alloc_tuple(ncols); /* actually, we use this as an array... */

  for(i=0;i<ncols;i++)
    {
      Store_double_field(result_vals,i,vals[i]);
      Store_field(result_indices,i,Val_int(cols[i]));
    }

  result=alloc_tuple(2);
  Store_field(result,0,result_indices);
  Store_field(result,1,result_vals);

  MatRestoreRow(m,row,&ncols,&cols,&vals);

  CAMLreturn(result);
}





/* Note: Petsc semantics is a bit strange for those functions and their
   interaction with "Matrix Assembly operations".
   Should we try to do something about that?
*/

CAMLprim value caml_petsc_mat_set(value ml_mx, value ml_row, value ml_col, value ml_val)
{
  CAMLparam4(ml_mx,ml_row,ml_col,ml_val);

  Mat mat;
  int row,col;

  DEBUG_LOG2("caml_petsc_mat_set");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  row=Int_val(ml_row);
  col=Int_val(ml_col);

  if(row<0 || col<0) /* simple safeguard... */
    {
      raise_with_string(*caml_named_value(err_exn_name),
			"Refuse to set matrix entry with invalid (negative) index!");
    }

  (void) MatSetValue(mat,row,col,Double_val(ml_val),INSERT_VALUES);
  CAMLreturn(Val_unit);
}


/* These dummy function to measure timings: */
CAMLprim value caml_petsc_mat_set_timing_dummy1(value ml_mx, value ml_row, value ml_col, value ml_val)
{
  /* Note that we do not even bother to mess with the ML stack here! */
  return(Val_unit);
}


CAMLprim value caml_petsc_mat_inc(value ml_mx, value ml_row, value ml_col, value ml_val)
{
  CAMLparam4(ml_mx,ml_row,ml_col,ml_val);

  Mat mat;
  int row,col;

  DEBUG_LOG2("caml_petsc_mat_inc");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  row=Int_val(ml_row);
  col=Int_val(ml_col);

  if(row<0 || col<0) /* simple safeguard... */
    {
      raise_with_string(*caml_named_value(err_exn_name),
			"Refuse to inc matrix entry with invalid (negative) index!");
    }

  (void) MatSetValue(mat,row,col,Double_val(ml_val),ADD_VALUES);
  DEBUG_LOG2("caml_petsc_mat_inc: returning");
  CAMLreturn(Val_unit);
}


/* Faster versions of the functions above which neither do any arg checks
   NOR any stack-registering of ML values. Here, this is okay as we do not cons,
   but the code is nevertheless close to the edge!

   Usually, going this far is not worth the trouble...
 */

CAMLprim value caml_petsc_mat_set_fast(value ml_mx, value ml_row, value ml_col, value ml_val)
{
  Mat mat;
  mat=(Mat)Field(ml_mx,1);

  DEBUG_LOG2("caml_petsc_mat_set_fast");
  (void) MatSetValue(mat,Int_val(ml_row),Int_val(ml_col),Double_val(ml_val),INSERT_VALUES);
  return(Val_unit);
}

CAMLprim value caml_petsc_mat_inc_fast(value ml_mx, value ml_row, value ml_col, value ml_val)
{
  Mat mat;
  DEBUG_LOG2("caml_petsc_mat_inc_fast");
  mat=(Mat)Field(ml_mx,1);
  (void) MatSetValue(mat,Int_val(ml_row),Int_val(ml_col),
                     Double_val(ml_val),
                     ADD_VALUES);
  return(Val_unit);
}


/* Note: we pass ml_data as well as the index arrays as bigarrays.
   This saves us spurious copying.
*/
CAMLprim value caml_petsc_mat_setinc_vals(value ml_mx, value ml_row_indices,
					  value ml_col_indices, value ml_data,
					  value ml_do_increase
					  )
{
  CAMLparam5(ml_mx,ml_row_indices,ml_col_indices,ml_data,ml_do_increase);

  Mat mat;
  int nr_rows,nr_cols;
  /* PetscInt *row_indices, *col_indices; XXX this causes an error! No PetscInt! */
  int *row_indices, *col_indices;
  PetscScalar *data;

  DEBUG_LOG("caml_petsc_mat_setinc_vals");
  petsc_checkinit();
  petsc_check_mat(ml_mx);
  mat=(Mat)Field(ml_mx,1);

  nr_rows=Bigarray_val(ml_row_indices)->dim[0];
  nr_cols=Bigarray_val(ml_col_indices)->dim[0];

  if(nr_rows != Bigarray_val(ml_data)->dim[0]
     || nr_cols != Bigarray_val(ml_data)->dim[1])
    {
      raise_with_string(*caml_named_value(err_exn_name),
			"Row/column index array lengths do not match data array dimensions!");
    }


  row_indices=Data_bigarray_val(ml_row_indices);
  col_indices=Data_bigarray_val(ml_row_indices);
  data=Data_bigarray_val(ml_data);

  MatSetValues(mat, nr_rows, row_indices, nr_cols, col_indices, data,
	       (Bool_val(ml_do_increase)?ADD_VALUES:INSERT_VALUES));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_get(value ml_mx, value ml_row, value ml_col)
{
  CAMLparam3(ml_mx,ml_row,ml_col);

  Mat mat;
  PetscScalar x;

  DEBUG_LOG("caml_petsc_mat_get");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

#if PETSC_VERSION_GE(3, 0, 0)
  (void) MatGetValue(mat, Int_val(ml_row), Int_val(ml_col), & x);
#else
  (void) MatGetValue(mat,Int_val(ml_row),Int_val(ml_col),x);
  /* Note: MatGetValue is a macro, even though one might expect otherwise!
     Hence, no &x!
  */
#endif

  CAMLreturn(copy_double((double)x));
}


CAMLprim value caml_petsc_mat_get_size(value ml_mx)
{
  CAMLparam1(ml_mx);

  Mat mat;
  int cols, rows;

  DEBUG_LOG("caml_petsc_mat_get_size");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  MatGetSize(mat, &cols, &rows);

  CAMLlocal1(result);

  result=alloc_tuple(2);

  Store_field(result,0,Val_int(cols));
  Store_field(result,1,Val_int(rows));

  CAMLreturn(result);
}


CAMLprim value caml_petsc_mat_mult(value ml_mx, value ml_vec_src, value ml_vec_dst)
{
  CAMLparam3(ml_mx,ml_vec_src,ml_vec_dst);

  Mat mat;
  Vec vec_src, vec_dst;
  double cycles_before, cycles_after;

  DEBUG_LOG("caml_petsc_mat_mult");
  petsc_checkinit();
  petsc_check_mat(ml_mx);
  petsc_check_vec(ml_vec_src);
  petsc_check_vec(ml_vec_dst);

  mat=(Mat)Field(ml_mx,1);
  vec_src=(Vec)Field(ml_vec_src,1);
  vec_dst=(Vec)Field(ml_vec_dst,1);

  cycles_before=get_cpu_cycle_counter();

  MatMult(mat,vec_src,vec_dst);

  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_mat_mult+=(cycles_after-cycles_before);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_mult_transpose(value ml_mx, value ml_vec_src, value ml_vec_dst)
{
  CAMLparam3(ml_mx,ml_vec_src,ml_vec_dst);

  Mat mat;
  Vec vec_src, vec_dst;
  double cycles_before, cycles_after;

  DEBUG_LOG("caml_petsc_mat_mult_transpose");
  petsc_checkinit();
  petsc_check_mat(ml_mx);
  petsc_check_vec(ml_vec_src);
  petsc_check_vec(ml_vec_dst);

  mat=(Mat)Field(ml_mx,1);
  vec_src=(Vec)Field(ml_vec_src,1);
  vec_dst=(Vec)Field(ml_vec_dst,1);

  cycles_before=get_cpu_cycle_counter();

  MatMultTranspose(mat,vec_src,vec_dst);

  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_mat_mult+=(cycles_after-cycles_before);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_zero_entries(value ml_mx)
{
  CAMLparam1(ml_mx);

  Mat mat;

  DEBUG_LOG("caml_petsc_mat_zero_entries");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  MatZeroEntries(mat);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_reincarnate_also_known_as_duplicate_and_destroy(value ml_mx)
{
  /* Duplicate matrix and destroy original.

     This will create a smaller petsc matrix if the original had
     preallocation parameters that were larger than necessary.

  */

  CAMLparam1(ml_mx);


  Mat mat_old, mat_new;

  DEBUG_LOG("caml_petsc_mat_reincarnate_also_known_as_duplicate_and_destroy");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat_old=(Mat)Field(ml_mx,1);

  MatDuplicate(mat_old,MAT_COPY_VALUES,&mat_new);

  Store_c_field(ml_mx,1,mat_new);

  MatDestroy(mat_old);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_duplicate(value ml_copy_values, value ml_mx)
{
  CAMLparam2(ml_copy_values,ml_mx);

  Mat mat_src, mat_res;

  DEBUG_LOG("caml_petsc_mat_duplicate");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat_src=(Mat)Field(ml_mx,1);

  MatDuplicate(mat_src,(Bool_val(ml_copy_values)?MAT_COPY_VALUES:MAT_DO_NOT_COPY_VALUES),&mat_res);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_mat, sizeof(Mat), sizeof(Mat));
  Store_c_field(block, 1, mat_res);

  CAMLreturn(block);
}

CAMLprim value caml_petsc_mat_copy(value ml_same_nonzero_pattern, value ml_src, value ml_dst)
{
  CAMLparam3(ml_same_nonzero_pattern,ml_src,ml_dst);

  Mat mat_src, mat_dst;

  DEBUG_LOG("caml_petsc_mat_copy");
  petsc_checkinit();
  petsc_check_mat(ml_src);
  petsc_check_mat(ml_dst);

  mat_src=(Mat)Field(ml_src,1);
  mat_dst=(Mat)Field(ml_dst,1);

  MatCopy(mat_src,mat_dst,(Bool_val(ml_same_nonzero_pattern)?SAME_NONZERO_PATTERN:DIFFERENT_NONZERO_PATTERN));

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_mat_scale(value ml_mx, value ml_coeff)
{
  CAMLparam2(ml_mx,ml_coeff);

  Mat mat;
  PetscScalar coeff;

  DEBUG_LOG("caml_petsc_mat_scale");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);
  coeff=Double_val(ml_coeff);

#if PETSC_VERSION_LE(2, 2, 0)
  MatScale((const PetscScalar*)&coeff,mat);
#else
  MatScale(mat,coeff);
#endif

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_shift(value ml_mx, value ml_coeff)
{
  CAMLparam2(ml_mx,ml_coeff);

  Mat mat;
  PetscScalar coeff;

  DEBUG_LOG("caml_petsc_mat_shift");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);
  coeff=Double_val(ml_coeff);


#if PETSC_VERSION_LE(2, 2, 0)
  MatShift((const PetscScalar*)&coeff,mat);
#else
  MatShift(mat,coeff);
#endif

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_diagonal_scale(value ml_mx, value ml_opt_v_le, value ml_opt_v_ri)
{
  CAMLparam3(ml_mx,ml_opt_v_le, ml_opt_v_ri);

  Mat mat;
  Vec v_le=PETSC_NULL,v_ri=PETSC_NULL;

  DEBUG_LOG("caml_petsc_mat_diagonal_scale");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  if(ml_opt_v_le != Val_unit)
    {
      petsc_check_vec(ml_opt_v_le);
      v_le=(Vec)Field(ml_opt_v_le,1);
    }

  if(ml_opt_v_ri != Val_unit)
    {
      petsc_check_vec(ml_opt_v_ri);
      v_ri=(Vec)Field(ml_opt_v_ri,1);
    }

  MatDiagonalScale(mat,v_le,v_ri);

  CAMLreturn(Val_unit);
}



CAMLprim value caml_petsc_mat_get_ownership_range(value ml_mx)
{
  CAMLparam1(ml_mx);

  Mat mat;
  int mybase, myend;

  DEBUG_LOG("caml_petsc_mat_get_ownership_range");
  petsc_checkinit();

  mat=(Mat)Field(ml_mx,1);

  MatGetOwnershipRange(mat,&mybase,&myend);

  CAMLlocal1(result);

  result=alloc_tuple(2);

  Store_field(result,0,Val_int(mybase));
  Store_field(result,1,Val_int(myend));

  CAMLreturn(result);
}

/* XXX Had to remove this, as wrapping up MatSetOption in such a way does not make sense!
CAMLprim value caml_petsc_mat_set_option(value ml_mx)
{
  CAMLparam1(ml_mx);

  Mat mat;

  DEBUG_LOG("caml_petsc_mat_set_option");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  MatSetOption(mat,MAT_NEW_NONZERO_LOCATION_ERR);

  CAMLreturn(Val_unit);
}
*/

CAMLprim value caml_petsc_mat_assemble(value ml_mx, value ml_do_final)
{
  CAMLparam2(ml_mx,ml_do_final);

  Mat mat;

  DEBUG_LOG("caml_petsc_mat_assemble");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  MatAssemblyBegin(mat,Bool_val(ml_do_final)?MAT_FINAL_ASSEMBLY:MAT_FLUSH_ASSEMBLY);
  MatAssemblyEnd(mat,Bool_val(ml_do_final)?MAT_FINAL_ASSEMBLY:MAT_FLUSH_ASSEMBLY);

  {
    const char *name;
    PetscObjectGetName((PetscObject) mat, & name);

    /* fprintf(stderr,"DDDEEE ocaml_petsc_mat_assemble,name=%s,type=%s\n",name,Bool_val(ml_do_final)?"Final":"Flush");
    fflush(stderr);
    */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_assembly_begin(value ml_mx, value ml_do_final)
{
  CAMLparam2(ml_mx,ml_do_final);

  Mat mat;

  DEBUG_LOG("caml_petsc_mat_assembly_begin");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  MatAssemblyBegin(mat,Bool_val(ml_do_final)?MAT_FINAL_ASSEMBLY:MAT_FLUSH_ASSEMBLY);

  {
    const char *name;
    PetscObjectGetName((PetscObject)mat,&name);

    /*
    fprintf(stderr,"DDDEEE ocaml_petsc_mat_assembly_begin,name=%s,type=%s\n",name,Bool_val(ml_do_final)?"Final":"Flush");
    fflush(stderr);
    */
  }


  fflush(stderr);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mat_assembly_end(value ml_mx, value ml_do_final)
{
  CAMLparam2(ml_mx,ml_do_final);

  Mat mat;

  DEBUG_LOG("caml_petsc_mat_assembly_end");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);

  MatAssemblyEnd(mat,Bool_val(ml_do_final)?MAT_FINAL_ASSEMBLY:MAT_FLUSH_ASSEMBLY);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_mat_mult_add(value ml_mx, value ml_x, value ml_y, value ml_z)
{
  CAMLparam4(ml_mx,ml_x,ml_y,ml_z);

  Mat mx;
  Vec x,y,z;
  double cycles_before, cycles_after;

  DEBUG_LOG("caml_petsc_mat_mult_add");
  petsc_checkinit();
  petsc_check_mat(ml_mx);
  petsc_check_vec(ml_x);
  petsc_check_vec(ml_y);
  petsc_check_vec(ml_z);

  mx=(Mat)Field(ml_mx,1);
  x=(Vec)Field(ml_x,1);
  y=(Vec)Field(ml_y,1);
  z=(Vec)Field(ml_z,1);

  cycles_before=get_cpu_cycle_counter();

  MatMultAdd(mx,x,y,z);

  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_mat_mult+=(cycles_after-cycles_before);

  CAMLreturn(Val_unit);
}

/* mf: used to set MAT_SYMMETRIC
 * Yes, I know: this idea of enumerating manually the values of ml_mat_option
 * is dangerous and not really a good programming practice, but... is there
 * a better solution? In that case feel free to change the following code!
 */
CAMLprim value caml_petsc_mat_set_option(value ml_mx, value ml_mat_option)
{
  CAMLparam2(ml_mx, ml_mat_option);
  Mat mat;
  MatOption mo;
#if PETSC_VERSION_GE(3, 0, 0)
  PetscTruth flg = PETSC_TRUE;
#endif

  DEBUG_LOG("caml_petsc_mat_set_option");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  switch(Int_val(ml_mat_option)) {
  case 1:  mo = MAT_SYMMETRIC; break;
  case 2:  mo = MAT_HERMITIAN; break;
  case 3:  mo = MAT_STRUCTURALLY_SYMMETRIC; break;
#if PETSC_VERSION_GE(3, 0, 0)
  case 4:  mo = MAT_SYMMETRIC; flg = PETSC_FALSE; break;
  case 5:  mo = MAT_HERMITIAN; flg = PETSC_FALSE; break;
  case 6:  mo = MAT_STRUCTURALLY_SYMMETRIC; flg = PETSC_FALSE; break;
#else
  case 4:  mo = MAT_NOT_SYMMETRIC; break;
  case 5:  mo = MAT_NOT_HERMITIAN; break;
  case 6:  mo = MAT_NOT_STRUCTURALLY_SYMMETRIC; break;
#endif
  case 7:  mo = MAT_SYMMETRY_ETERNAL; break;
  default: CAMLreturn(Val_unit); break;
  }

  mat = (Mat) Field(ml_mx, 1);
#if PETSC_VERSION_GE(3, 0, 0)
  MatSetOption(mat, mo, flg);
#else
  MatSetOption(mat, mo);
#endif
  CAMLreturn(Val_unit);
}

/* mf: Can be used to set an option in the petsc option database. */
CAMLprim value caml_petsc_options_set_value(value option_str, value value_str) {
  const char *c_value_str = String_val(value_str);
  CAMLparam2(option_str, value_str);
  DEBUG_LOG("caml_petsc_options_set_value");
  PetscOptionsSetValue(String_val(option_str),
                       *c_value_str == '\0' ? PETSC_NULL : c_value_str);
  CAMLreturn(Val_unit);
}

/* mf: used to specify that the an iterative KSP solver should start from
 *     the initial value given by the user. If this is not called, by default
 *     petsc will start from a zero guess.
 */
CAMLprim value caml_petsc_ksp_set_initial_guess_nonzero(value ml_ksp, value ml_truth) {
  CAMLparam2(ml_ksp, ml_truth);
  PetscTruth petsc_truth = Bool_val(ml_truth) ? PETSC_TRUE : PETSC_FALSE;
  KSP ksp;

  DEBUG_LOG("caml_petsc_ksp_set_initial_guess_nonzero");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  KSPSetInitialGuessNonzero(ksp, petsc_truth);
  CAMLreturn(Val_unit);
}

/* mf: just to check what initial value a KSP solver uses */
CAMLprim value caml_petsc_ksp_get_initial_guess_nonzero(value ml_ksp) {
  CAMLparam1(ml_ksp);
  PetscTruth petsc_truth;
  KSP ksp;

  DEBUG_LOG("caml_petsc_ksp_get_initial_guess_nonzero");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  KSPGetInitialGuessNonzero(ksp, & petsc_truth);
  CAMLreturn(Val_bool(petsc_truth == PETSC_TRUE));
}

/* mf: in magpar this is used to prevent conflicts in the setups
 * of different KSP solvers. This seems not to be a problem to me, but...
 * we'll follow magpar's suggestions naively
 */
CAMLprim value caml_petsc_ksp_set_up(value ml_ksp) {
  CAMLparam1(ml_ksp);
  KSP ksp;

  DEBUG_LOG("caml_petsc_ksp_set_up");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  KSPSetUp(ksp);
  CAMLreturn(Val_unit);
}

/* mf: to check if KSPSolve converged or not */
CAMLprim value caml_petsc_ksp_get_converged_reason(value ml_ksp) {
  CAMLparam1(ml_ksp);
  KSP ksp;
  KSPConvergedReason cr;

  DEBUG_LOG("caml_petsc_ksp_get_converged_reason");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  KSPGetConvergedReason(ksp, & cr);
  CAMLreturn(Val_int(cr));
}


CAMLprim value caml_petsc_ksp_set_pc_type(value ml_ksp,value ml_pc_type) {
  CAMLparam2(ml_ksp,ml_pc_type);
  KSP ksp;
  PC pc;

  DEBUG_LOG("caml_petsc_ksp_set_pc_type");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  KSPGetPC(ksp,&pc);
  PCSetType(pc,String_val(ml_pc_type));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ksp_set_pc_bjacobi_subtype(value ml_ksp,value ml_pc_type) {
  CAMLparam2(ml_ksp,ml_pc_type);
  KSP ksp;
  PC pc, sub_pc;
  int i, nr_local_blocks, ix_first;
  KSP *sub_ksps;
  char *pc_type;

  DEBUG_LOG("caml_petsc_ksp_set_pc_bjacobi_subtype");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  pc_type=String_val(ml_pc_type);

  KSPGetPC(ksp,&pc);
  PCSetType(pc,PCBJACOBI);

  PCBJacobiGetSubKSP(pc,&nr_local_blocks,&ix_first,&sub_ksps);

   for (i=0; i<nr_local_blocks; i++)
     {
       KSPGetPC(sub_ksps[i],&sub_pc);
       PCSetType(sub_pc,pc_type);
     }

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_ksp_set_type(value ml_ksp,value ml_ksp_type) {
  CAMLparam2(ml_ksp,ml_ksp_type);
  KSP ksp;

  DEBUG_LOG("caml_petsc_ksp_set_type");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp = (KSP) Field(Field(ml_ksp, 0), 1);
  KSPSetType(ksp,String_val(ml_ksp_type));
  CAMLreturn(Val_unit);
}

/* This is slightly tricky: a KSP must retain references to its matrices! */
CAMLprim value caml_petsc_ksp_create_raw(value ml_comm,
					 value ml_name, value ml_mat_a,
					 value ml_mat_p, value ml_mat_structure)
{
  CAMLparam5(ml_comm,ml_name, ml_mat_a, ml_mat_p, ml_mat_structure);
  Mat mat_a, mat_p;
  KSP ksp;
  MatStructure mat_struc = matstructure_from_int(Int_val(ml_mat_structure));
  MPI_Comm comm = Comm_val(ml_comm);

  DEBUG_LOG("caml_petsc_ksp_create");
  petsc_checkinit();
  petsc_check_mat(ml_mat_a);
  petsc_check_mat(ml_mat_p);

  mat_a=(Mat)Field(ml_mat_a,1);
  mat_p=(Mat)Field(ml_mat_p,1);

  KSPCreate(comm,&ksp);
  PetscObjectSetName((PetscObject)ksp,String_val(ml_name));

  KSPSetOperators(ksp, mat_a, mat_p, mat_struc);

  /* XXX Note: may want to do KSPGetPC(), PCSetType(),
     KSPSetTolerances() here.
  */
  KSPSetFromOptions(ksp);

  CAMLlocal2(ksp_tuple,ksp_block);

  ksp_block = alloc_final(2, finalize_petsc_ksp_block, sizeof(KSP), sizeof(KSP));
  Store_c_field(ksp_block, 1, ksp);

  ksp_tuple=alloc_tuple(6); /* (ksp_block,mat_a,mat_p,rhs,solution,nullspace) */
  Store_field(ksp_tuple,0,ksp_block);
  Store_field(ksp_tuple,1,ml_mat_a);
  Store_field(ksp_tuple,2,ml_mat_p);
  Store_field(ksp_tuple,3,Val_unit);
  Store_field(ksp_tuple,4,Val_unit);
  Store_field(ksp_tuple,5,Val_unit);

  /*  fprintf(stderr,"Created KSP at %p\n",ksp);fflush(stderr); / * DDD */

  CAMLreturn(ksp_tuple);
}

/* Likewise, creating a MatNullSpace is tricky... */

CAMLprim value caml_petsc_matnullspace_create_raw(value ml_comm,
						  value ml_name,
						  value ml_has_cnst,
						  value ml_vecs)
{
  CAMLparam4(ml_comm,ml_name, ml_has_cnst, ml_vecs);
  MPI_Comm comm;
  MatNullSpace nullsp;
  int nr_vecs,i;
  Vec *vecs;

  DEBUG_LOG("caml_petsc_matnullspace_create_raw");
  comm=Comm_val(ml_comm);

  nr_vecs=Wosize_val(ml_vecs);

  for(i=0;i<nr_vecs;i++)
    {
      petsc_check_vec(Field(ml_vecs,i));
    }

  if(0==(vecs=malloc((1+nr_vecs)*sizeof(Vec))))
    /* We allocate more just in case the malloc() arg were zero.  Now
       that should actually be permissible as well. But we are
       cautious.
    */
    {
      aiee("malloc() failure!");
    }

  for(i=0;i<nr_vecs;i++)
    {
      vecs[i]=(Vec)Field(Field(ml_vecs,i),1);
    }

  MatNullSpaceCreate(comm,Bool_val(ml_has_cnst),nr_vecs,vecs,&nullsp);
  free(vecs);

  DEBUG_LOG("caml_petsc_mat_null_space_create");
  PetscObjectSetName((PetscObject)nullsp,String_val(ml_name));

  CAMLlocal2(nullsp_tuple,nullsp_block);

  nullsp_block = alloc_final(2, finalize_petsc_matnullspace_block, sizeof(MatNullSpace), sizeof(MatNullSpace));
  Store_c_field(nullsp_block, 1, nullsp);

  nullsp_tuple=alloc_tuple(2); /* (nullsp_block,vecs) */
  Store_field(nullsp_tuple,0,nullsp_block);
  Store_field(nullsp_tuple,1,ml_vecs);

  CAMLreturn(nullsp_tuple);
}



CAMLprim value caml_petsc_ksp_get_tolerances(value ml_ksp)
{
  CAMLparam1(ml_ksp);
  KSP ksp;
  PetscReal rtol, atol, dtol;
  int maxits;

  DEBUG_LOG("caml_petsc_ksp_get_tolerances");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp=(KSP)Field(Field(ml_ksp,0),1);
  KSPGetTolerances(ksp,&rtol,&atol,&dtol,&maxits);

  CAMLlocal1(result);
  result=alloc_tuple(4);

  Store_field(result,0,copy_double(rtol));
  Store_field(result,1,copy_double(atol));
  Store_field(result,2,copy_double(dtol));
  Store_field(result,3,Val_int(maxits));

  CAMLreturn(result);
}

/* Our convention: tolerance=0 means: do not set it */
CAMLprim value caml_petsc_ksp_set_tolerances(value ml_ksp, value ml_rtol,
					     value ml_atol, value ml_dtol,
					     value ml_maxits)
{
  CAMLparam5(ml_ksp, ml_rtol, ml_atol, ml_dtol, ml_maxits);
  KSP ksp;
  PetscReal rtol, atol, dtol;
  int maxits;

  DEBUG_LOG("caml_petsc_ksp_set_tolerances");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  rtol=Double_val(ml_rtol);
  atol=Double_val(ml_atol);
  dtol=Double_val(ml_dtol);
  maxits=Int_val(ml_maxits);

  /* This is somewhat strange code, considering what it expands to: */
  if(rtol==0.0)rtol=PETSC_DEFAULT;
  if(atol==0.0)atol=PETSC_DEFAULT;
  if(dtol==0.0)dtol=PETSC_DEFAULT;
  if(maxits==0)maxits=PETSC_DEFAULT;

  ksp=(KSP)Field(Field(ml_ksp,0),1);
  KSPSetTolerances(ksp,rtol,atol,dtol,maxits);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ksp_set_operators(value ml_ksp, value ml_mx,
                                            value ml_mx_precond,
                                            value ml_mat_structure)
{
  CAMLparam4(ml_ksp, ml_mx, ml_mx_precond, ml_mat_structure);
  MatStructure mat_struc = matstructure_from_int(Int_val(ml_mat_structure));
  KSP ksp;
  Mat mx, mx_precond;

  DEBUG_LOG("caml_petsc_ksp_set_operators");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);
  petsc_check_vec(ml_mx);
  petsc_check_vec(ml_mx_precond);

  ksp=(KSP)Field(Field(ml_ksp,0),1);
  mx=(Mat)Field(ml_mx,1);
  mx_precond=(Mat)Field(ml_mx_precond,1);

  KSPSetOperators(ksp, mx, mx_precond, mat_struc);

  /* Tell the GC about these operators */
  Store_field(ml_ksp,1,ml_mx);
  Store_field(ml_ksp,2,ml_mx_precond);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ksp_set_null_space(value ml_ksp, value ml_matnullspace)
{
  CAMLparam2(ml_ksp,ml_matnullspace);
  KSP ksp;
  MatNullSpace nullsp;

  DEBUG_LOG("caml_petsc_ksp_set_null_space");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);

  ksp=(KSP)Field(Field(ml_ksp,0),1);
  nullsp=(MatNullSpace)Field(Field(ml_matnullspace,0),1);

  KSPSetNullSpace(ksp,nullsp);

  /* Tell the GC about the nullspace */
  Store_field(ml_ksp,5,ml_matnullspace);

  CAMLreturn(Val_unit);
}



CAMLprim value caml_petsc_ksp_solve_raw(value ml_ksp, value ml_x, value ml_b)
{
  CAMLparam3(ml_ksp,ml_x,ml_b);
  KSP ksp;
  Vec x,b;
  int nr_iterations;
  double cycles_before, cycles_after;

  DEBUG_LOG("caml_petsc_ksp_solve_raw");
  petsc_checkinit();
  petsc_check_ksp(ml_ksp);
  petsc_check_vec(ml_x);
  petsc_check_vec(ml_b);

  ksp=(KSP)Field(Field(ml_ksp,0),1);
  x=(Vec)Field(ml_x,1);
  b=(Vec)Field(ml_b,1);


  cycles_before=get_cpu_cycle_counter();
#if PETSC_VERSION_LE(2, 2, 0)
  /* Register rhs and solution for the GC inside the KSP */
  Store_field(ml_ksp,3,ml_b);
  Store_field(ml_ksp,4,ml_x);

  KSPSetRhs(ksp,b);
  KSPSetSolution(ksp,x);

  KSPSolve(ksp);
#else
  /* Petsc 2.3.0's interface is much more reasonable.
     If they had designed it like this right from the start,
     we would not have to bend over backwards in this stubs file
     in order to get the GC behaviour right...
   */
  KSPSolve(ksp,b,x);
#endif
  cycles_after=get_cpu_cycle_counter();
  cpu_cycles_counted.cycles_ksp_solve+=(cycles_after-cycles_before);

  KSPGetIterationNumber(ksp,&nr_iterations);

  CAMLreturn(Val_int(nr_iterations));
}

/* ====== MPI scatterv/gatherv ====== */

CAMLprim value caml_petsc_mpi_scatter_vec(value ml_comm,
					  value ml_vec_master,
					  value ml_vec_recv,
					  value ml_ba_lengths,
					  value ml_ba_displacements)
{
  CAMLparam5(ml_comm, ml_vec_master, ml_vec_recv, ml_ba_lengths, ml_ba_displacements);
  /* XXX We could do some speed improvements here - we can ensure we do not cons,
     and hence can get rid of value stack management... EVIL!
  */

  MPI_Comm comm;
  Vec vec_master=0, vec_recv;
  double *data_master, *data_recv;
  int *ba_lengths, *ba_displacements;
  int rank;

  DEBUG_LOG("caml_petsc_mpi_scatter_vec");
  comm=Comm_val(ml_comm);
  MPI_Comm_rank(comm,&rank);

  petsc_checkinit();
  petsc_check_vec(ml_vec_recv);
  vec_recv=(Vec)Field(ml_vec_recv,1);

  ba_lengths=Data_bigarray_val(ml_ba_lengths);
  ba_displacements=Data_bigarray_val(ml_ba_displacements);

  if(rank==0)
    {
      petsc_check_vec(ml_vec_master);
      vec_master=(Vec)Field(ml_vec_master,1);

      VecGetArray(vec_master,&data_master);
      VecGetArray(vec_recv,&data_recv);

      /*printf("MPI_Scatterv(sendbuf=%p, sendcounts={%d}, displ={%d} "
             "sendtype=%d, recvbuf=%p, recvcount=%d, recvtype=%d,"
             "root=%d, comm=%d)\n",
             data_master, ba_lengths[0], ba_displacements[0],
             MPI_DOUBLE, data_recv, ba_lengths[rank],
             MPI_DOUBLE, 0, comm);
      fflush(stdout);*/

      MPI_Scatterv(data_master,
		   ba_lengths,
		   ba_displacements,
		   MPI_DOUBLE,
		   data_recv,
		   ba_lengths[rank],
		   MPI_DOUBLE,
		   0,
		   comm);

      VecRestoreArray(vec_recv,&data_recv);
      VecRestoreArray(vec_master,&data_master);
    }
  else /* client */
    {
      VecGetArray(vec_recv,&data_recv);

      MPI_Scatterv(0,
		   ba_lengths,
		   ba_displacements,
		   MPI_DOUBLE,
		   data_recv,
		   ba_lengths[rank],
		   MPI_DOUBLE,
		   0,
		   comm);

      VecRestoreArray(vec_recv,&data_recv);
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mpi_gather_vec(value ml_comm,
					 value ml_vec_master,
					 value ml_vec_send,
					 value ml_ba_lengths,
					 value ml_ba_displacements)
{
  CAMLparam5(ml_comm, ml_vec_master, ml_vec_send, ml_ba_lengths, ml_ba_displacements);
  /* XXX We could do some speed improvements here - we can ensure we do not cons,
     and hence can get rid of value stack management... EVIL!
  */

  MPI_Comm comm;
  Vec vec_master=0, vec_send;
  double *data_master, *data_send;
  int *ba_lengths, *ba_displacements;
  int rank;

  DEBUG_LOG("caml_petsc_mpi_gather_vec");
  comm=Comm_val(ml_comm);
  MPI_Comm_rank(comm,&rank);

  petsc_checkinit();
  petsc_check_vec(ml_vec_send);
  vec_send=(Vec)Field(ml_vec_send,1);

  ba_lengths=Data_bigarray_val(ml_ba_lengths);
  ba_displacements=Data_bigarray_val(ml_ba_displacements);

  if(rank==0)
    {
      petsc_check_vec(ml_vec_master);
      vec_master=(Vec)Field(ml_vec_master,1);

      VecGetArray(vec_master,&data_master);
      VecGetArray(vec_send,&data_send);

      MPI_Gatherv(data_send,
		  ba_lengths[rank],
		  MPI_DOUBLE,
		  data_master,
		  ba_lengths,
		  ba_displacements,
		  MPI_DOUBLE,
		  0,
		  comm);

      VecRestoreArray(vec_send,&data_send);
      VecRestoreArray(vec_master,&data_master);
    }
  else /* client */
    {
      VecGetArray(vec_send,&data_send);

      MPI_Gatherv(data_send,
		  ba_lengths[rank],
		  MPI_DOUBLE,
		  0,
		  ba_lengths,
		  ba_displacements,
		  MPI_DOUBLE,
		  0,
		  comm);

      VecRestoreArray(vec_send,&data_send);
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_mpi_allgather_vec(value ml_comm,
					    value ml_vec_seq,
					    value ml_vec_par_piece_send,
					    value ml_ba_lengths,
					    value ml_ba_displacements)
{
  CAMLparam5(ml_comm, ml_vec_seq, ml_vec_par_piece_send, ml_ba_lengths, ml_ba_displacements);
  /* XXX We could do some speed improvements here - we can ensure we do not cons,
     and hence can get rid of value stack management... EVIL!
  */

  MPI_Comm comm;
  Vec vec_seq=0, vec_par_piece_send;
  double *data_seq, *data_send;
  int *ba_lengths, *ba_displacements;
  int rank;

  DEBUG_LOG("caml_petsc_mpi_allgather_vec");
  comm=Comm_val(ml_comm);
  MPI_Comm_rank(comm,&rank);

  petsc_checkinit();
  petsc_check_vec(ml_vec_par_piece_send);
  petsc_check_vec(ml_vec_seq);

  vec_seq=(Vec)Field(ml_vec_seq,1);
  vec_par_piece_send=(Vec)Field(ml_vec_par_piece_send,1);

  ba_lengths=Data_bigarray_val(ml_ba_lengths);
  ba_displacements=Data_bigarray_val(ml_ba_displacements);


  VecGetArray(vec_seq,&data_seq);
  VecGetArray(vec_par_piece_send,&data_send);

  MPI_Allgatherv(data_send,
		 ba_lengths[rank],
		 MPI_DOUBLE,
		 data_seq,
		 ba_lengths,
		 ba_displacements,
		 MPI_DOUBLE,
		 comm);

  VecRestoreArray(vec_par_piece_send,&data_send);
  VecRestoreArray(vec_seq,&data_seq);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_matrix_ownership_range(value ml_mx)
{
  CAMLparam1(ml_mx);
  Mat mx;
  int row_start,row_end;

  DEBUG_LOG("caml_petsc_matrix_ownership_range");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mx=(Mat)Field(ml_mx,1);

  MatGetOwnershipRange(mx,&row_start,&row_end);

  CAMLlocal1(result);

  result=alloc_tuple(2);

  Store_field(result,0,Val_int(row_start));
  Store_field(result,1,Val_int(row_end));
  CAMLreturn(result);
}


CAMLprim value caml_petsc_matrix_call_on_rows_raw(value ml_mx,
						  value ml_fun,
						  value ml_start_row,
						  value ml_end_row
						  )
{
  CAMLparam4(ml_mx,ml_fun,ml_start_row,ml_end_row);
  Mat mx;
  int start_row,end_row,start_own,end_own;
  int i,ncols=0;
  const int *cols=0;
  const PetscScalar *vals=0;
  CAMLlocal2(ba_indices,ba_vals);

  DEBUG_LOG("caml_petsc_matrix_call_on_rows_raw");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mx=(Mat)Field(ml_mx,1);

  MatGetOwnershipRange(mx,&start_own,&end_own);

  start_row=Int_val(ml_start_row);
  end_row=Int_val(ml_end_row);

  if(start_row == -1)start_row=start_own;
  if(end_row == -1)end_row=end_own;

  /* Xavier Leroy greenlighted doing things this way, i.e. changing
     the data entries of a bigarray data structure, in an email
     on the caml list. However, this is not explicitly allowed by
     the official documentation. I asked the Caml team to adjust
     the documentation so that this is officially permitted, but
     they did not do so yet.


     Actually... it is not as easy as that. Using this function crashes, definitely.
     The more defensive alternative, on the other hand, does not.
  */
  ba_indices=alloc_bigarray_dims(BIGARRAY_NATIVE_INT | BIGARRAY_C_LAYOUT,
				 1, (int *) cols, ncols);

  ba_vals=alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT,
			      1, (PetscScalar *) vals, ncols);


  for(i=start_row;i<end_row;i++)
    {
      MatGetRow(mx,i,&ncols,&cols,&vals);

      Bigarray_val(ba_indices)->dim[0]=ncols;
      Bigarray_val(ba_vals)->dim[0]=ncols;

      Bigarray_val(ba_indices)->data=(void*)cols;
      Bigarray_val(ba_vals)->data=(void*)vals;

      callback3(ml_fun,Val_int(i),ba_indices,ba_vals);

      MatRestoreRow(mx,i,&ncols,&cols,&vals);
    }

  CAMLreturn(Val_unit);
}



CAMLprim value caml_petsc_matrix_call_on_rows_raw_defensive(value ml_mx,
							    value ml_fun,
							    value ml_start_row,
							    value ml_end_row
							    )
{
  CAMLparam4(ml_mx,ml_fun,ml_start_row,ml_end_row);
  Mat mx;
  int start_row,end_row,start_own,end_own;
  int i,ncols=0;
  const int *cols=0;
  const PetscScalar *vals=0;
  CAMLlocal2(ba_indices,ba_vals);

  DEBUG_LOG("caml_petsc_matrix_call_on_rows_raw_defensive");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mx=(Mat)Field(ml_mx,1);

  MatGetOwnershipRange(mx,&start_own,&end_own);

  start_row=Int_val(ml_start_row);
  end_row=Int_val(ml_end_row);

  if(start_row == -1)start_row=start_own;
  if(end_row == -1)end_row=end_own;


  if (sizeof(int) == sizeof(long)) {
    for(i=start_row;i<end_row;i++) {
      MatGetRow(mx,i,&ncols,&cols,&vals);

      ba_indices=alloc_bigarray_dims(BIGARRAY_NATIVE_INT | BIGARRAY_C_LAYOUT,
				     1, (int *) cols, ncols);

      ba_vals=alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT,
				  1, (PetscScalar *) vals, ncols);

      callback3(ml_fun,Val_int(i),ba_indices,ba_vals);

      MatRestoreRow(mx,i,&ncols,&cols,&vals);
    }

  } else {
    /* Here we have a problem: BIGARRAY_NATIVEINT corresponds
     * to the long C integer. However PetscInt in general is not guaranteed
     * to be the same integer type. In some 64 bit architectures we may
     * then have a mismatch between the two data types.
     * We should then test at the configuration stage and find out what
     * BIGARRAY type corresponds to the C int data type. This would be
     * the optimal solution, but for now we fix the problem by copying
     * the int array to the long array when sizeof(int) != sizeof(long).
     * This is not optimal, but should fix the problem for now!
     * NOTE: the jungle of 64 bit architectures is populated by several
     *  beasts. there are ilp64, lp64, silp64 and many other models
     *  (ilp64 means that int, long and pointers all are 64 bit wide).
     */
    long *long_cols = (long *) NULL;
    int long_size=-1, j;
    for(i = start_row; i < end_row; i++) {
      MatGetRow(mx, i, & ncols, & cols, & vals);
      /* We allocate the array only if we need it! */
      if (ncols > long_size) {
         /* The GNU implementation of free accepts NULL,
          * but one never knows...
          * I also avoid realloc, because I don't need to preserve
          * the old content of the array.
          */
        if (long_cols != (long *) NULL) free(long_cols);
        long_size = ncols;
        long_cols = (long *) malloc(sizeof(long)*ncols);
        long_size = ncols;
      }
      for(j=0; j<ncols; j++) long_cols[j] = (long) cols[j];

      ba_indices=alloc_bigarray_dims(BIGARRAY_NATIVE_INT | BIGARRAY_C_LAYOUT,
                                     1, (long *) long_cols, ncols);

      ba_vals=alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT,
                                  1, (PetscScalar *) vals, ncols);

      callback3(ml_fun,Val_int(i),ba_indices,ba_vals);

      MatRestoreRow(mx, i, & ncols, & cols, & vals);
    }
    if (long_cols != (long *) NULL) free(long_cols);
  }

/*  printf("caml_petsc_matrix_call_on_rows_raw_defensive: exit\n");*/

  CAMLreturn(Val_unit);
}



/* For now, this is not for distributed plans!
   Will need that later as well. (The problem
   is coordinated Assembly flushing!)
 */

CAMLprim value caml_petsc_matrix_execute_polyplan_noassemble(value ml_plan_outer,
							     value ml_mx,
							     value ml_v_sources,
							     value ml_nr_flushes)
{
  CAMLparam4(ml_plan_outer,ml_mx,ml_v_sources,ml_nr_flushes);

  /* XXX Note: we do not check that the plan does not try to make
     out-of-range entries. This may lead to memory corruption!
  */

  Mat mx;
  Vec *v_sources;
  PetscScalar **data_sources;
  int i,k,m,p;
  int nr_sources, nr_plan_rows, nr_plan_cols, nr_contribs, nr_factors, nr_flushes;
  int nr_flushes_done=0;
  int ix_le,ix_ri;
  double coeff, accum_lr,accum_prod;
  char *field_indices;

  DEBUG_LOG("caml_petsc_matrix_execute_polyplan_noassemble");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mx=(Mat)Field(ml_mx,1);

  /* Setup */

  nr_flushes=Int_val(ml_nr_flushes);

  nr_sources=Wosize_val(ml_v_sources);


  if(0==(v_sources=malloc(nr_sources*sizeof(Vec))))
    {
      aiee("malloc() failure!");
    }

  if(0==(data_sources=malloc(nr_sources*sizeof(PetscScalar*))))
    {
      aiee("malloc() failure!");
    }

  for(i=0;i<nr_sources;i++)
    {
      v_sources[i]=(Vec)Field(Field(ml_v_sources,i),1);
      VecGetArray(v_sources[i],&data_sources[i]);
    }

  nr_plan_rows=Wosize_val(ml_plan_outer);

  /* fprintf(stderr,"DDD PPA: nr-rows=%4d!\n",nr_plan_rows);fflush(stderr); */

  CAMLlocal4(plan_inner,plan_entry,plan_lr_v_cfo,offsets);

  for(i=0;i<nr_plan_rows;i++)
    {
      plan_inner=Field(ml_plan_outer,i);

      nr_plan_cols=Wosize_val(plan_inner);

      /* fprintf(stderr,"DDD PPA: row=%4d/%4d, cols=%4d\n",i,nr_plan_rows,nr_plan_cols);fflush(stderr); */

      for(k=0;k<nr_plan_cols;k++)
	{
	  plan_entry=Field(plan_inner,k);
	  ix_le=Int_val(Field(Field(plan_entry,0),0));
	  ix_ri=Int_val(Field(Field(plan_entry,0),1));
	  plan_lr_v_cfo=Field(plan_entry,1);

	  accum_lr=0;

	  nr_contribs=Wosize_val(plan_lr_v_cfo);

	  /* fprintf(stderr,"DDD PPA: row=%4d/%4d col=%4d/%4d nr_entries=%d!\n",i,nr_plan_rows,k,nr_plan_cols,nr_contribs);fflush(stderr); */

	  for(m=0;m<nr_contribs;m++)
	    {
	      coeff=Double_val(Field(Field(plan_lr_v_cfo,m),0));
	      field_indices=String_val(Field(Field(plan_lr_v_cfo,m),1));
	      offsets=Field(Field(plan_lr_v_cfo,m),2);

	      nr_factors=Wosize_val(offsets);

	      accum_prod=coeff;

	      for(p=0;p<nr_factors;p++)
		{
		  accum_prod*=data_sources[(int) field_indices[p]][Int_val(Field(offsets,p))];
		}
	      accum_lr+=accum_prod;
	    }
	  if(accum_lr==0.0)accum_lr=16.0*DBL_MIN; /* Ensure we make all matrix entries
						     that could be nonzero in principle! */

	  /* fprintf(stderr," DDD PPA: Factors: %2d - mx(%4d,%4d) += %8.6f\n",nr_factors,ix_le,ix_ri,accum_lr);fflush(stderr); */

	  (void) MatSetValue(mx,ix_le,ix_ri,accum_lr,ADD_VALUES);
	}

      if ((nr_flushes_done < nr_flushes) && ((i%(nr_plan_rows/nr_flushes))==0))
	{
	  MatAssemblyBegin(mx,MAT_FLUSH_ASSEMBLY);
	  MatAssemblyEnd(mx,MAT_FLUSH_ASSEMBLY);
	  nr_flushes_done++;
	}
    }


  /* Also ensure we have the diagonal entries... */

  for(i=0;i<nr_plan_rows;i++)
    {
      (void) MatSetValue(mx,i,i,DBL_MIN,ADD_VALUES);
    }

  for (;nr_flushes_done<nr_flushes;nr_flushes_done++) {
    MatAssemblyBegin(mx,MAT_FLUSH_ASSEMBLY);
    MatAssemblyEnd(mx,MAT_FLUSH_ASSEMBLY);
  }

  /* Cleanup */

  for(i=0;i<nr_sources;i++)
    {
      VecRestoreArray(v_sources[i],&data_sources[i]);
    }
  free(v_sources);
  free(data_sources);

  CAMLreturn(Val_unit);
}


/* The internal representation of time-steppers is somewhat tricky,
   as these can hold a lot of context (vectors, matrices):

   This context has to be kept referenced for GC, and the functions in
   that context must be locatable from statically-located-in-memory GC
   roots, so that we can use this address as a closure arg in a C
   callback wrapping the ML callback.

   Sounds complicated, slightly is.
*/
CAMLprim value caml_petsc_ts_create(value ml_unit)
{
  CAMLparam1(ml_unit);
  TS ts;
  value *root_cells;

  DEBUG_LOG("caml_petsc_ts_create");
  petsc_checkinit();

  TSCreate(PETSC_COMM_WORLD,&ts);
  TSSetFromOptions(ts);

  if(0==(root_cells=malloc(2*sizeof(value *))))
    {
      aiee("malloc() failure!");
    }

  root_cells[0]=Val_unit;
  root_cells[1]=Val_unit;

  caml_register_global_root(&root_cells[0]);
  caml_register_global_root(&root_cells[1]);

  /* These GC roots hold C callbacks */
  CAMLlocal2(ts_block, ts_tuple);
  ts_block = alloc_final(4, finalize_petsc_ts_block, sizeof(TS), sizeof(TS));
  Store_c_field(ts_block, 1,ts);
  Store_c_field(ts_block, 2, &root_cells[0]);
  Store_c_field(ts_block, 3, &root_cells[1]);

  ts_tuple=alloc_tuple(5);	/* (block, solution_vector, jacobian, precond, matfdcoloring) */
  Store_field(ts_tuple,0,ts_block);
  Store_field(ts_tuple,1,Val_unit);
  Store_field(ts_tuple,2,Val_unit);
  Store_field(ts_tuple,3,Val_unit);
  Store_field(ts_tuple,4,Val_unit);

  CAMLreturn(ts_tuple);
}

CAMLprim value caml_petsc_ts_set_problem_type(value ml_ts,value ml_ptype)
{
  CAMLparam2(ml_ts,ml_ptype);
  int int_ptype;
  TSProblemType ptype;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_problem_type");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);
  int_ptype=Int_val(ml_ptype);

  switch(int_ptype)
    {
    case 0:
      ptype=TS_LINEAR;
      break;
    case 1:
      ptype=TS_NONLINEAR;
      break;
    default:
      raise_with_string(*caml_named_value("ocaml_exn_petsc_caml_interface"),
			"AIEE! Petsc interface inconsistency in caml_petsc_set_problem_type!");
    }

  TSSetProblemType(ts,ptype);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_get_problem_type(value ml_ts)
{
  CAMLparam1(ml_ts);
  int int_ptype;
  TSProblemType ptype;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_get_problem_type");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  TSGetProblemType(ts,&ptype);

  switch(ptype)
    {
    case TS_LINEAR:
      int_ptype=0;
      break;
    case TS_NONLINEAR:
      int_ptype=1;
      break;
    default:
      raise_with_string(*caml_named_value("ocaml_exn_petsc_caml_interface"),
			"AIEE! Petsc interface inconsistency in caml_petsc_get_problem_type!");
    }

  CAMLreturn(Val_int(int_ptype));
}



CAMLprim value caml_petsc_ts_set_type(value ml_ts, value ml_tstype)
{
  CAMLparam2(ml_ts, ml_tstype);
  int int_tstype;
  TSType tstype;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_type");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);
  int_tstype=Int_val(ml_tstype);

  switch(int_tstype)
    {
#if PETSC_VERSION_GE(3, 1, 0)
    /* PETSc people changed interface from v 3.0 to v 3.1:
     * TS_EULER etc were renamed to TSEULER...
     */
    case 0: tstype=TSEULER; break;
    case 1: tstype=TSBEULER; break;
    case 2: tstype=TSPSEUDO; break;
#else
    case 0: tstype=TS_EULER; break;
    case 1: tstype=TS_BEULER; break;
    case 2: tstype=TS_PSEUDO; break;
#endif
    case 3:
      /* tstype=TS_PVODE; XXX NOTE: does not work with petsc2.3.1! Crazy stuff... */
      exit(1);
      break;
    default:
      raise_with_string(*caml_named_value("ocaml_exn_petsc_caml_interface"),
			"AIEE! Petsc interface inconsistency in caml_petsc_ts_set_type!");
    }

  TSSetType(ts,tstype);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_set_initial_time_step(value ml_ts, value ml_time, value ml_dt)
{
  CAMLparam3(ml_ts, ml_time, ml_dt);
  double double_time, double_dt;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_initial_time_step");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  double_time=Double_val(ml_time);
  double_dt=Double_val(ml_dt);

  TSSetInitialTimeStep(ts, double_time, double_dt);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_set_time_step(value ml_ts, value ml_dt)
{
  CAMLparam2(ml_ts, ml_dt);
  double double_dt;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_time_step");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  double_dt=Double_val(ml_dt);

  ts=(TS)Field(ml_ts,1);

  TSSetTimeStep(ts, double_dt);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_get_time_step(value ml_ts)
{
  CAMLparam1(ml_ts);
  double double_dt;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_get_time_step");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  TSGetTimeStep(ts, &double_dt);

  CAMLreturn(copy_double(double_dt));
}

CAMLprim value caml_petsc_ts_set_solution(value ml_ts, value ml_x)
{
  CAMLparam2(ml_ts, ml_x);
  Vec x;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_solution");
  petsc_checkinit();
  petsc_check_vec(ml_x);
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);
  x = (Vec)Field(ml_x,1);


  Store_c_field(ml_ts,1,x);
  TSSetSolution(ts, x);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_set_duration(value ml_ts, value ml_maxsteps, value ml_maxtime)
{
  CAMLparam3(ml_ts, ml_maxsteps, ml_maxtime);
  int int_maxsteps;
  double double_maxtime;
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_duration");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  int_maxsteps = Int_val(ml_maxsteps);
  double_maxtime = Double_val(ml_maxtime);

  TSSetDuration(ts, int_maxsteps, double_maxtime);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_set_up(value ml_ts)
{
  CAMLparam1(ml_ts);
  TS ts;

  DEBUG_LOG("caml_petsc_ts_set_up");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  TSSetUp(ts);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_ts_step(value ml_ts)
{
  CAMLparam1(ml_ts);
  TS ts;
  int steps;
  PetscReal ftime;

  DEBUG_LOG("caml_petsc_ts_step");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  TSStep(ts,&steps,&ftime);

  CAMLlocal1(result);
  result=alloc_tuple(2);
  Store_field(result,0,Val_int(steps));
  Store_field(result,1,copy_double(ftime));

  CAMLreturn(result);
}

/* --- Helpers for TS --- */

/* The jacobian computation wants to know about some matrix coloring.
   This is information about matrix columns that do not share any row
   (for parallelization of the computation).

   There are some types of problems of the "any site affects any site"
   kind, such as micromagnetical simulations (through the demagnetizing
   field interaction between any pair of magnetic moments), for which
   we just would like to use the trivial FDcoloring (every column
   a different color).

   PPP Note: for now, this is a one-processor variant, where we do not
   try to distribute the matrix.

   XXX Do we really need this? I think it can be removed, as I
   absorbed all the relevant stuff into caml_petsc_ts_set_rhs_simple_jacobian...

 */

CAMLprim value caml_petsc_trivial_fdcoloring_size_n(value ml_mx_size)
{
  CAMLparam1(ml_mx_size);
  int mx_size,nr_row,nr_col;
  Mat m;
  ISColoring iscoloring;
  MatFDColoring fdcoloring;

  DEBUG_LOG("caml_petsc_trivial_fdcoloring_size_n");
  petsc_checkinit();

  mx_size=Int_val(ml_mx_size);

#if PETSC_VERSION_LE(2, 2, 0)
  MatCreate(PETSC_COMM_WORLD,
	    PETSC_DECIDE,PETSC_DECIDE,
	    mx_size,mx_size,
	    &m);
#else
  fprintf(stderr,"XXX Using petsc2.3.x MatCreate! XXX Change this - use MatCreateSeqDense!\n");fflush(stderr);
  MatCreate(PETSC_COMM_WORLD,&m);
  MatSetSizes(m,PETSC_DECIDE,PETSC_DECIDE,
	      mx_size,mx_size);
#endif

  /* MatFDColoringCreate wants us to provide a matrix which describes
     the nonzero structure of the jacobian, so we have to
     populate our matrix...
  */

  for(nr_row=0;nr_row<mx_size;nr_row++)
  for(nr_col=0;nr_col<mx_size;nr_col++)
    (void) MatSetValue(m,nr_row,nr_col,1.0,INSERT_VALUES);

  MatAssemblyBegin(m,MAT_FINAL_ASSEMBLY);
  MatAssemblyEnd(m,MAT_FINAL_ASSEMBLY);

  MatGetColoring(m,MATCOLORING_NATURAL,&iscoloring);
  MatFDColoringCreate(m,iscoloring,&fdcoloring);

  /* It's actually a pity we had to build the matrix in the first place,
     as we are destroying it right again.
   */
  MatDestroy(m);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_matfdcoloring, sizeof(MatFDColoring), 10*sizeof(MatFDColoring));
  Store_c_field(block, 1,fdcoloring);

  /* fprintf(stderr,"CREATE MatFDColoring => %p\n",fdcoloring); */
  CAMLreturn(block);
}


CAMLprim value caml_petsc_ts_set_rhs_function(value ml_ts, value ml_rhs_function)
{
  CAMLparam2(ml_ts, ml_rhs_function);
  TS ts;
  value *callback_ptr;

  DEBUG_LOG("caml_petsc_ts_set_rhs_function");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  callback_ptr=(value *)Field(Field(ml_ts,0),2);
  *callback_ptr=ml_rhs_function;

  TSSetRHSFunction(ts,&_ts_rhs_function_wrapper,callback_ptr);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_petsc_ts_set_rhs_jacobian(value ml_ts, value ml_jacobi, value ml_precond, value ml_rhs_jacobian)
{
  CAMLparam4(ml_ts, ml_jacobi, ml_precond, ml_rhs_jacobian);

  TS ts;
  Mat jacobi;
  Mat precond;
  value *callback_ptr;
  /* Note: I think it is okay here to work without CAMLlocal1(...) - after all,
     protecting callbacks from the GC is what all this caml_named_value stuff is about...
  */

  DEBUG_LOG("caml_petsc_ts_set_rhs_jacobian");
  petsc_checkinit();
  petsc_check_ts(ml_ts);
  petsc_check_mat(ml_jacobi);
  petsc_check_mat(ml_precond);

  ts=(TS)Field(Field(ml_ts,0),1);
  jacobi=(Mat)Field(ml_jacobi,1);
  precond=(Mat)Field(ml_precond,1);

  Store_field(ml_ts,2,ml_jacobi);
  Store_field(ml_ts,3,ml_precond);

  callback_ptr=(value *)Field(Field(ml_ts,0),3);
  *callback_ptr=ml_rhs_jacobian;

  TSSetRHSJacobian(ts,jacobi,precond,_ts_rhs_jacobian_wrapper,callback_ptr);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_ts_set_rhs_simple_jacobian(value ml_ts, value ml_j_size)
{
  CAMLparam2(ml_ts,ml_j_size);

  int mx_size, nr_row,nr_col;
  TS ts;
  Mat jacobi;
  ISColoring iscoloring;
  MatFDColoring fdcoloring;

  mx_size=Int_val(ml_j_size);

  DEBUG_LOG("caml_petsc_ts_set_rhs_simple_jacobian");
  petsc_checkinit();
  petsc_check_ts(ml_ts);

  ts=(TS)Field(Field(ml_ts,0),1);

  MatCreate(PETSC_COMM_WORLD,&jacobi);
  MatSetSizes(jacobi,PETSC_DECIDE,PETSC_DECIDE,mx_size,mx_size);

  PetscObjectSetName((PetscObject)jacobi,"jacobian"); /* XXX */
  MatSetFromOptions(jacobi);

  for(nr_row=0;nr_row<mx_size;nr_row++)
  for(nr_col=0;nr_col<mx_size;nr_col++)
    (void) MatSetValue(jacobi,nr_row,nr_col,1.0,INSERT_VALUES);

  MatAssemblyBegin(jacobi,MAT_FINAL_ASSEMBLY);
  MatAssemblyEnd(jacobi,MAT_FINAL_ASSEMBLY);

  MatGetColoring(jacobi,MATCOLORING_NATURAL,&iscoloring);

  MatFDColoringCreate(jacobi,iscoloring,&fdcoloring);

  CAMLlocal2(block_jacobi, block_fdcoloring);
  block_jacobi = alloc_final(2, finalize_petsc_mat, sizeof(Mat), sizeof(Mat));
  Store_c_field(block_jacobi, 1, jacobi);

  block_fdcoloring = alloc_final(2, finalize_petsc_matfdcoloring, sizeof(MatFDColoring), 10*sizeof(MatFDColoring));
  Store_c_field(block_fdcoloring, 1,fdcoloring);

  Store_field(ml_ts,2,block_jacobi);
  Store_field(ml_ts,3,block_jacobi);
  Store_field(ml_ts,4,block_fdcoloring);

  TSSetRHSJacobian(ts,jacobi,jacobi,TSDefaultComputeJacobianColor,fdcoloring);

  CAMLreturn(Val_unit);
}

/* ====== PARMETIS ====== */

/* XXX Does Not Work Yet! */

CAMLprim value caml_parmetis_v3_node_nd_raw(value ml_sites_per_cpu,
					    value ml_csr_offsets,
					    value ml_csr_neighbours,
					    value ml_result_permutation,
					    value ml_result_sizes)
{
  CAMLparam5(ml_sites_per_cpu,
	     ml_csr_offsets,
	     ml_csr_neighbours,
	     ml_result_permutation,
	     ml_result_sizes);

  int c_ordering[1],options[3],
    *sites_per_cpu,
    *csr_offsets, *csr_neighbours,
    *result_permutation, *result_sizes;

  /*MPI_Comm comm=MPI_COMM_WORLD;*/
  MPI_Comm comm=MPI_COMM_SELF;

  DEBUG_LOG("caml_parmetis_v3_node_nd_raw");
  fprintf(stderr,"ocaml: parmetis call #1\n");fflush(stderr);

  fprintf(stderr,"XXX This does not work yet!\n");fflush(stderr);
  CAMLreturn(Val_unit);

  petsc_checkinit();
  /* While parmetis does not use petsc, we make sure it is initialized,
     just to ensure MPI was initialized.
  */

  c_ordering[0]=0;
  options[0]=0; /* Use default options */

  sites_per_cpu=Data_bigarray_val(ml_sites_per_cpu);
  csr_offsets=Data_bigarray_val(ml_csr_offsets);
  csr_neighbours=Data_bigarray_val(ml_csr_neighbours);
  result_permutation=Data_bigarray_val(ml_result_permutation);
  result_sizes=Data_bigarray_val(ml_result_sizes);

  fprintf(stderr,"ocaml: parmetis call #2\n");fflush(stderr);

  ParMETIS_V3_NodeND(sites_per_cpu,
		     csr_offsets,csr_neighbours,
		     c_ordering,options,
		     result_permutation,result_sizes,
		     &comm
		     /* XXX hope it's okay to use this communicator! */
		     );

  fprintf(stderr,"ocaml: parmetis call #3\n");fflush(stderr);

  CAMLreturn(Val_unit);
}


CAMLprim value caml_metis_node_nd_raw(value ml_csr_offsets,
				      value ml_csr_neighbours,
				      value ml_result_permutation,
				      value ml_result_iperm /* Whatever that is... */
				      )
{
  CAMLparam4(ml_csr_offsets,
	     ml_csr_neighbours,
	     ml_result_permutation,
	     ml_result_iperm);

  int nr_sites[1],c_ordering[1],options[3],
    *csr_offsets, *csr_neighbours,
    *result_permutation, *result_iperm;

  DEBUG_LOG("caml_metis_node_nd_raw");

  /* fprintf(stderr,"ocaml: metis call #1\n");fflush(stderr); */

  c_ordering[0]=0;
  options[0]=0; /* Use default options */

  nr_sites[0]=Bigarray_val(ml_csr_offsets)->dim[0]-1;

  csr_offsets=Data_bigarray_val(ml_csr_offsets);
  csr_neighbours=Data_bigarray_val(ml_csr_neighbours);
  result_permutation=Data_bigarray_val(ml_result_permutation);
  result_iperm=Data_bigarray_val(ml_result_iperm);

  /* fprintf(stderr,"ocaml: metis call #2\n");fflush(stderr); */

  METIS_NodeND(nr_sites,
	       csr_offsets,csr_neighbours,
	       c_ordering,options,
	       result_permutation,result_iperm
	       );

  /* fprintf(stderr,"ocaml: metis call #3\n");fflush(stderr); */

  CAMLreturn(Val_unit);
}


/* Note: this is a complicated function with way too many parameters.
    So, the way it was interfaced was through cut&paste from the
    documentation PDF.
 */

CAMLprim value caml_parmetis_v3_part_kway_raw(value ml_args)
{
  CAMLparam1(ml_args);

  CAMLlocal5(ml_ba_csr_xadj,
             ml_ba_csr_adjncy,
             ml_ba_partitioning,
             ml_nr_parts,
             ml_nr_nodes);
  int n;

  idxtype *xadj;
  idxtype *adjncy;

  /* No weights on the nodes */
  int wgtflag = 0;
  idxtype *vwgt = (idxtype *) NULL;
  idxtype *adjwgt = (idxtype *) NULL;

  /* Select C-style numberings for arrays */
  int numflag = 0;

  /* Number of parts */
  int nparts;

  /* (in)  More options: we use the default options
   *  options[0]: 0 means "use the default options"
   *  options[1]: should ParMETIS return report information?
   *  options[2]: Random seed value for the routine
   */
  int options[5] = {0, 0, 15};

  /* (out) Number of edges that are cut by the partitioning */
  int edgecut;

  idxtype *part;

  DEBUG_LOG("caml_parmetis_v3_part_kway_raw");
  ml_ba_csr_xadj = Field(ml_args, 0);
  ml_ba_csr_adjncy = Field(ml_args, 1);
  ml_ba_partitioning = Field(ml_args, 2);
  ml_nr_parts = Field(ml_args, 3);
  ml_nr_nodes = Field(ml_args, 4);

  xadj=Data_bigarray_val(ml_ba_csr_xadj);
  adjncy=Data_bigarray_val(ml_ba_csr_adjncy);

  nparts = Int_val(ml_nr_parts);
  n = Int_val(ml_nr_nodes);

  part = Data_bigarray_val(ml_ba_partitioning);

#if 0
  if (1) {
    printf("Calling ParMETIS_V3_PartKway with:\n");
    printf("vtxdist=[%d, %d], ", vtxdist[0], vtxdist[1]);
    printf("nparts=%d, ", nparts);
    printf("options=[%d, %d, %d], ", options[0], options[1], options[2]);
    printf("ubvec=%f, ", ubvec);
    printf("comm=%d\n", comm);
    fflush(stdout);
  }
#endif

#if 0
  if (1) {
   int i;

   /* As we are using the MPI_COMM_SELF this-node-only communicator to
    * work out mesh distribution on the master only (for now, at least),
    * vtxdist will just be [0, nr_vertices-1]
    */
   idxtype vtxdist[2] = {0, n-1};

    /* Communicator used by ParMETIS */
    MPI_Comm comm = MPI_COMM_SELF;

    /* Number of weights per node */
    int ncon = 1;

    /* Imbalance tolerance: ParMETIS manual suggests 1.05 */
    float ubvec = 1.05;

    /* Float array to decide how to distribute the nodes among the machines
     * Equal values --> equal number of nodes per machine
     * The floats should add up to 1.0
     */
    float *tpwgts;

    if (0 == (tpwgts = malloc(nparts*sizeof(float))))
      {
        aiee("malloc() failed!");
      }

    for(i = 0; i < nparts; i++) tpwgts[i] = 1.0/nparts;

    ParMETIS_V3_PartKway(vtxdist, xadj, adjncy, vwgt, adjwgt,
                         & wgtflag, & numflag, & ncon, & nparts, tpwgts,
                         & ubvec, options, & edgecut, part, & comm);
    free(tpwgts);
  }

#else
  METIS_PartGraphKway(& n, xadj, adjncy, vwgt, adjwgt,
                      & wgtflag, & numflag, & nparts,
                      options, & edgecut, part);

#endif

#if 0
  printf("Returned from ParMETIS_V3_PartKway\n");
  printf("edgecut=%d\n", edgecut);
  fflush(stdout);
#endif

  CAMLreturn(Val_unit);
}

/* Dump a float into a string. Note: we use native format,
   which might imply endian-ness problems.

   Note: this also is in snippets.ml - we have to duplicate it here, as
   mpi_petsc must not depend on snippets!
*/
CAMLprim value caml_petsc_put_double_into_string(value ml_str, value ml_x)
{
  CAMLparam2(ml_str,ml_x);
  double x;
  char *s_dst,*s_src;
  int i;

  DEBUG_LOG("caml_petsc_put_double_into_string");
  if(string_length(ml_str)<8)
    CAMLreturn(Val_unit);
  /* String too short - maybe we should raise an exception,
     but we just do nothing (rather than crashing the heap) */

  x=Double_val(ml_x);
  s_dst=String_val(ml_str);
  s_src=(char*)&x;

  for(i=0;i<8;i++)
    {
      s_dst[i]=s_src[i];
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_write_mat(value ml_mx, value save_file)
{
  CAMLparam2(ml_mx,save_file);

  Mat mat;
  PetscViewer    view_out;

  DEBUG_LOG("caml_petsc_write_mat");
  petsc_checkinit();
  petsc_check_mat(ml_mx);

  mat=(Mat)Field(ml_mx,1);
  PetscViewerBinaryOpen(PETSC_COMM_SELF,String_val(save_file),FILE_MODE_WRITE,&view_out);
  MatView(mat,view_out);
  PetscViewerDestroy(view_out);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_read_mat(value read_file)
{
  CAMLparam1(read_file);

  Mat mat;
  PetscViewer    view_in;

  DEBUG_LOG("caml_petsc_read_mat");
  petsc_checkinit();

  PetscViewerBinaryOpen(PETSC_COMM_SELF,String_val(read_file),FILE_MODE_READ,&view_in);
  MatLoad(view_in,MATSEQAIJ,&mat);
  PetscViewerDestroy(view_in);

  CAMLlocal1(block);
  block = alloc_final(2, finalize_petsc_mat, sizeof(Mat), sizeof(Mat));
  Store_c_field(block, 1, mat);

  CAMLreturn(block);
}

#if PETSC_VERSION_LT(3, 0, 0)
typedef int PetscLogStage;
#endif

CAMLprim value caml_petsc_log_stage_register(value ml_stage_name) {
  CAMLparam1(ml_stage_name);
  const char *stage_name = String_val(ml_stage_name);
  PetscLogStage stage;

  petsc_checkinit();
#if PETSC_VERSION_LT(3, 0, 0)
  PetscLogStageRegister(& stage, stage_name);
#else
  PetscLogStageRegister(stage_name, & stage);
#endif
  CAMLreturn(Val_int((int) stage));
}

CAMLprim value caml_petsc_log_stage_push(value ml_stage) {
  CAMLparam1(ml_stage);
  PetscLogStage stage = (PetscLogStage) Int_val(ml_stage);

  petsc_checkinit();
  PetscLogStagePush(stage);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_petsc_log_stage_pop(value ml_unit) {
  CAMLparam1(ml_unit);
  petsc_checkinit();
  PetscLogStagePop();
  CAMLreturn(Val_unit);
}

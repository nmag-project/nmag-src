/* (C) 2006 Dr. Thomas Fischbacher

   Header definitions required by sundials_stubs.c

   Note that the definitions in here must be kept in sync with
   libsundials definitions. For a rationale, see the comment at the
   beginning of sundials_stubs.c
 */

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif


#define CV_ADAMS 1
#define CV_BDF   2

  /* iter */
#define CV_FUNCTIONAL 1
#define CV_NEWTON     2

  /* itol */
#define CV_SS 1
#define CV_SV 2
#define CV_WF 3

  /* itask */
#define CV_NORMAL         1
#define CV_ONE_STEP       2
#define CV_NORMAL_TSTOP   3
#define CV_ONE_STEP_TSTOP 4

#define CV_SUCCESS               0
#define CV_TSTOP_RETURN          1
#define CV_ROOT_RETURN           2

#define CV_WARNING              99

#define CV_TOO_MUCH_WORK        -1
#define CV_TOO_MUCH_ACC         -2
#define CV_ERR_FAILURE          -3
#define CV_CONV_FAILURE         -4

#define CV_LINIT_FAIL           -5
#define CV_LSETUP_FAIL          -6
#define CV_LSOLVE_FAIL          -7
#define CV_RHSFUNC_FAIL         -8
#define CV_FIRST_RHSFUNC_ERR    -9
#define CV_REPTD_RHSFUNC_ERR    -10
#define CV_UNREC_RHSFUNC_ERR    -11
#define CV_RTFUNC_FAIL          -12

#define CV_MEM_FAIL             -20
#define CV_MEM_NULL             -21
#define CV_ILL_INPUT            -22
#define CV_NO_MALLOC            -23
#define CV_BAD_K                -24
#define CV_BAD_T                -25
#define CV_BAD_DKY              -26

#define CVSPILS_SUCCESS          0

enum { PREC_NONE, PREC_LEFT, PREC_RIGHT, PREC_BOTH };


typedef double realtype;
/* XXX NOTE: here, we assume sundials was built with SUNDIALS_DOUBLE_PRECISION.
   That makes sense, because sundials will be fed directly with petsc data,
   and we will use a petsc version built to use double.
*/

typedef int booleantype;

struct _N_VectorContent_Serial {
  long int length;
  booleantype own_data;
  realtype *data;
};

typedef struct _N_VectorContent_Serial *N_VectorContent_Serial;

struct _N_VectorContent_Parallel {
   long int local_length;
   long int global_length;
   booleantype own_data;
   realtype *data;
   MPI_Comm comm;
};

typedef struct _N_VectorContent_Parallel *N_VectorContent_Parallel;

struct _generic_N_Vector {
  void *content;
  /* struct _generic_N_Vector_Ops *ops; */
  void *ops;
};



typedef struct _generic_N_Vector *N_Vector;

#define NV_CONTENT_S(v)  ( (N_VectorContent_Serial)(v->content) )
#define NV_DATA_S(v)     ( NV_CONTENT_S(v)->data )
#define NV_LENGTH_S(v)   ( NV_CONTENT_S(v)->length )

#define NV_CONTENT_P(v)    ( (N_VectorContent_Parallel)(v->content) )
#define NV_DATA_P(v)   ( NV_CONTENT_P(v)->data )
#define NV_LOCLENGTH_P(v)  ( NV_CONTENT_P(v)->local_length )


typedef int (*CVSpilsJacTimesVecFn)(N_Vector, N_Vector, realtype,
				    N_Vector, N_Vector,
				    void *, N_Vector);


typedef int (*CVSpilsPrecSolveFn)(realtype, N_Vector, N_Vector,
				  N_Vector, N_Vector,
				  realtype, realtype,
				  int, void *, N_Vector);

typedef int (*CVSpilsPrecSetupFn)(realtype, N_Vector, N_Vector,
				  booleantype, booleantype *,
				  realtype, void *,
				  N_Vector, N_Vector,
				  N_Vector);

typedef int (*CVRhsFn)(realtype, N_Vector, N_Vector, void *);



/* This file was generated automatically by extract_from_header.py
   and needs to be reviewed with care before being used in the sundials
   ocaml module.
 */

static void *(*dyn_CVodeCreate)(int lmm, int iter) = 0;
static int (*dyn_CVodeSetFdata)(void *cvode_mem, void *f_data) = 0;
static int (*dyn_CVodeSetMaxOrd)(void *cvode_mem, int maxord) = 0;
static int (*dyn_CVodeSetMaxNumSteps)(void *cvode_mem, long int mxsteps) = 0;
static int (*dyn_CVodeSetInitStep)(void *cvode_mem, realtype hin) = 0;
static int (*dyn_CVodeSetMinStep)(void *cvode_mem, realtype hmin) = 0;
static int (*dyn_CVodeSetMaxStep)(void *cvode_mem, realtype hmax) = 0;
static int (*dyn_CVodeSetStopTime)(void *cvode_mem, realtype tstop) = 0;
static int (*dyn_CVodeSetTolerances)(void *cvode_mem,
                         int itol, realtype reltol, void *abstol) = 0;
static int (*dyn_CVodeMalloc)(void *cvode_mem, CVRhsFn f, realtype t0, N_Vector y0,
                  int itol, realtype reltol, void *abstol) = 0;
static int (*dyn_CVodeReInit)(void *cvode_mem, CVRhsFn f, realtype t0, N_Vector y0,
                  int itol, realtype reltol, void *abstol) = 0;
static int (*dyn_CVode)(void *cvode_mem, realtype tout, N_Vector yout,
            realtype *tret, int itask) = 0;
static int (*dyn_CVodeGetWorkSpace)(void *cvode_mem, long int *lenrw, long int *leniw) = 0;
static int (*dyn_CVodeGetNumSteps)(void *cvode_mem, long int *nsteps) = 0;
static int (*dyn_CVodeGetNumStabLimOrderReds)(void *cvode_mem, long int *nslred) = 0;
static int (*dyn_CVodeGetActualInitStep)(void *cvode_mem, realtype *hinused) = 0;
static int (*dyn_CVodeGetLastStep)(void *cvode_mem, realtype *hlast) = 0;
static int (*dyn_CVodeGetCurrentStep)(void *cvode_mem, realtype *hcur) = 0;
static int (*dyn_CVodeGetCurrentTime)(void *cvode_mem, realtype *tcur) = 0;
static int (*dyn_CVodeGetTolScaleFactor)(void *cvode_mem, realtype *tolsfac) = 0;
static int (*dyn_CVodeGetErrWeights)(void *cvode_mem, N_Vector eweight) = 0;
static int (*dyn_CVodeGetEstLocalErrors)(void *cvode_mem, N_Vector ele) = 0;
static int (*dyn_CVodeGetNumGEvals)(void *cvode_mem, long int *ngevals) = 0;
static int (*dyn_CVodeGetRootInfo)(void *cvode_mem, int *rootsfound) = 0;
static int (*dyn_CVodeGetIntegratorStats)(void *cvode_mem, long int *nsteps,
                              long int *nfevals, long int *nlinsetups,
                              long int *netfails, int *qlast,
                              int *qcur, realtype *hinused, realtype *hlast,
                              realtype *hcur, realtype *tcur) = 0;
static int (*dyn_CVodeGetNumNonlinSolvIters)(void *cvode_mem, long int *nniters) = 0;
static int (*dyn_CVodeGetNumNonlinSolvConvFails)(void *cvode_mem, long int *nncfails) = 0;
static int (*dyn_CVodeGetNonlinSolvStats)(void *cvode_mem, long int *nniters,
                              long int *nncfails) = 0;
static char *(*dyn_CVodeGetReturnFlagName)(int flag) = 0;
static void (*dyn_CVodeFree)(void **cvode_mem) = 0;
static N_Vector (*dyn_N_VMake_Serial)(long int vec_length, realtype *v_data) = 0;
static void (*dyn_N_VDestroy_Serial)(N_Vector v) = 0;
static int (*dyn_CVSpilsSetPreconditioner)(void *cvode_mem, CVSpilsPrecSetupFn pset, 
			     CVSpilsPrecSolveFn psolve, void *P_data) = 0;
static int (*dyn_CVSpilsSetJacTimesVecFn)(void *cvode_mem, 
                            CVSpilsJacTimesVecFn jtimes, void *jac_data) = 0;
static int (*dyn_CVSpilsGetNumPrecEvals)(void *cvode_mem, long int *npevals) = 0;
static int (*dyn_CVSpilsGetNumPrecSolves)(void *cvode_mem, long int *npsolves) = 0;
static int (*dyn_CVSpilsGetNumLinIters)(void *cvode_mem, long int *nliters) = 0;
static int (*dyn_CVSpilsGetNumConvFails)(void *cvode_mem, long int *nlcfails) = 0;
static int (*dyn_CVSpilsGetNumJtimesEvals)(void *cvode_mem, long int *njvevals) = 0;
static int (*dyn_CVSpilsGetNumRhsEvals)(void *cvode_mem, long int *nfevalsLS) = 0;
static N_Vector (*dyn_N_VMake_Parallel)(MPI_Comm comm, 
                          long int local_length,
                          long int global_length,
                          realtype *v_data) = 0;
static void (*dyn_N_VDestroy_Parallel)(N_Vector v) = 0;
static int (*dyn_CVSpgmr)(void *cvode_mem, int pretype, int maxl) = 0;

typedef struct {
    void **library;
    char *sym_name;
    void **target;
} SundialsFnTable;

static SundialsFnTable symbols_table[] = {
  {& libsundials_cvode, "CVodeCreate", (void **) & dyn_CVodeCreate},
  {& libsundials_cvode, "CVodeSetFdata", (void **) & dyn_CVodeSetFdata},
  {& libsundials_cvode, "CVodeSetMaxOrd", (void **) & dyn_CVodeSetMaxOrd},
  {& libsundials_cvode, "CVodeSetMaxNumSteps", (void **) & dyn_CVodeSetMaxNumSteps},
  {& libsundials_cvode, "CVodeSetInitStep", (void **) & dyn_CVodeSetInitStep},
  {& libsundials_cvode, "CVodeSetMinStep", (void **) & dyn_CVodeSetMinStep},
  {& libsundials_cvode, "CVodeSetMaxStep", (void **) & dyn_CVodeSetMaxStep},
  {& libsundials_cvode, "CVodeSetStopTime", (void **) & dyn_CVodeSetStopTime},
  {& libsundials_cvode, "CVodeSetTolerances", (void **) & dyn_CVodeSetTolerances},
  {& libsundials_cvode, "CVodeMalloc", (void **) & dyn_CVodeMalloc},
  {& libsundials_cvode, "CVodeReInit", (void **) & dyn_CVodeReInit},
  {& libsundials_cvode, "CVode", (void **) & dyn_CVode},
  {& libsundials_cvode, "CVodeGetWorkSpace", (void **) & dyn_CVodeGetWorkSpace},
  {& libsundials_cvode, "CVodeGetNumSteps", (void **) & dyn_CVodeGetNumSteps},
  {& libsundials_cvode, "CVodeGetNumStabLimOrderReds", (void **) & dyn_CVodeGetNumStabLimOrderReds},
  {& libsundials_cvode, "CVodeGetActualInitStep", (void **) & dyn_CVodeGetActualInitStep},
  {& libsundials_cvode, "CVodeGetLastStep", (void **) & dyn_CVodeGetLastStep},
  {& libsundials_cvode, "CVodeGetCurrentStep", (void **) & dyn_CVodeGetCurrentStep},
  {& libsundials_cvode, "CVodeGetCurrentTime", (void **) & dyn_CVodeGetCurrentTime},
  {& libsundials_cvode, "CVodeGetTolScaleFactor", (void **) & dyn_CVodeGetTolScaleFactor},
  {& libsundials_cvode, "CVodeGetErrWeights", (void **) & dyn_CVodeGetErrWeights},
  {& libsundials_cvode, "CVodeGetEstLocalErrors", (void **) & dyn_CVodeGetEstLocalErrors},
  {& libsundials_cvode, "CVodeGetNumGEvals", (void **) & dyn_CVodeGetNumGEvals},
  {& libsundials_cvode, "CVodeGetRootInfo", (void **) & dyn_CVodeGetRootInfo},
  {& libsundials_cvode, "CVodeGetIntegratorStats", (void **) & dyn_CVodeGetIntegratorStats},
  {& libsundials_cvode, "CVodeGetNumNonlinSolvIters", (void **) & dyn_CVodeGetNumNonlinSolvIters},
  {& libsundials_cvode, "CVodeGetNumNonlinSolvConvFails", (void **) & dyn_CVodeGetNumNonlinSolvConvFails},
  {& libsundials_cvode, "CVodeGetNonlinSolvStats", (void **) & dyn_CVodeGetNonlinSolvStats},
  {& libsundials_cvode, "CVodeGetReturnFlagName", (void **) & dyn_CVodeGetReturnFlagName},
  {& libsundials_cvode, "CVodeFree", (void **) & dyn_CVodeFree},
  {& libsundials_nvec_serial, "N_VMake_Serial", (void **) & dyn_N_VMake_Serial},
  {& libsundials_nvec_serial, "N_VDestroy_Serial", (void **) & dyn_N_VDestroy_Serial},
  {& libsundials_cvode, "CVSpilsSetPreconditioner", (void **) & dyn_CVSpilsSetPreconditioner},
  {& libsundials_cvode, "CVSpilsSetJacTimesVecFn", (void **) & dyn_CVSpilsSetJacTimesVecFn},
  {& libsundials_cvode, "CVSpilsGetNumPrecEvals", (void **) & dyn_CVSpilsGetNumPrecEvals},
  {& libsundials_cvode, "CVSpilsGetNumPrecSolves", (void **) & dyn_CVSpilsGetNumPrecSolves},
  {& libsundials_cvode, "CVSpilsGetNumLinIters", (void **) & dyn_CVSpilsGetNumLinIters},
  {& libsundials_cvode, "CVSpilsGetNumConvFails", (void **) & dyn_CVSpilsGetNumConvFails},
  {& libsundials_cvode, "CVSpilsGetNumJtimesEvals", (void **) & dyn_CVSpilsGetNumJtimesEvals},
  {& libsundials_cvode, "CVSpilsGetNumRhsEvals", (void **) & dyn_CVSpilsGetNumRhsEvals},
  {& libsundials_nvec_parallel, "N_VMake_Parallel", (void **) & dyn_N_VMake_Parallel},
  {& libsundials_nvec_parallel, "N_VDestroy_Parallel", (void **) & dyn_N_VDestroy_Parallel},
  {& libsundials_cvode, "CVSpgmr", (void **) & dyn_CVSpgmr},
  {(void **) 0, (char *) 0, (void *) 0}
};

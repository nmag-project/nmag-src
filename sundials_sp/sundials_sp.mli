exception Sundials_sp_caml_interface_exn of string
val version : unit -> string
type cvode
type simple_c_array =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
val make_simple_c_array :
  int -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
type mirage_dof_mapping = {
  mdm_expand :
    ba_short:simple_c_array -> ba_long:simple_c_array -> unit -> unit;
  mdm_reduce :
    ba_long:simple_c_array -> ba_short:simple_c_array -> unit -> unit;
  mdm_longbuffer_lts : simple_c_array;
  mdm_longbuffer_stl : simple_c_array;
  mdm_shortbuffer_lts : simple_c_array;
  mdm_shortbuffer_stl : simple_c_array;
}
(*val mirage_dof_wrapped :
  mirage_dof_mapping option ->
  ?debug_name:string ->
  ?use_own_buffers:bool ->
  ('a -> simple_c_array -> simple_c_array -> 'b) ->
  'a -> simple_c_array -> simple_c_array -> 'b;;*)
type preconditioning_type = PREC_NONE | PREC_LEFT | PREC_RIGHT | PREC_BOTH
type ty_fun_rhs_raw = float * simple_c_array * simple_c_array -> unit -> int
type ty_fun_preconditioner_setup_raw =
    bool * float * float * simple_c_array * simple_c_array * simple_c_array *
    simple_c_array * simple_c_array -> unit -> bool * int
type ty_fun_preconditioner_solve_raw =
    int * float * float * float * simple_c_array * simple_c_array *
    simple_c_array * simple_c_array * simple_c_array -> unit -> int
type ty_fun_jacobian_times_vector_raw =
    float * simple_c_array * simple_c_array * simple_c_array *
    simple_c_array * simple_c_array -> unit -> int
external _sundials_init : string -> string -> string -> unit
  = "caml_sundials_sp_init"
val sundials_init :
  path_cvode:string ->
  path_nvec_serial:string -> path_nvec_parallel:string -> unit -> unit
external cvode_make_dummy : unit -> cvode
  = "caml_sundials_sp_cvode_make_dummy"
val dummy_cvode : cvode
external cvode_make_raw_ba :
  Mpi_petsc.communicator * bool * ty_fun_rhs_raw * int * simple_c_array *
  float * float * float -> cvode = "caml_sundials_sp_cvode_make_raw_ba"
external cvode_reinit_raw : cvode -> float -> float -> float -> unit
  = "caml_sundials_sp_cvode_reinit"
external cvode_setup_cvspgmr_raw :
  cvode ->
  int ->
  preconditioning_type ->
  ty_fun_preconditioner_setup_raw -> ty_fun_preconditioner_solve_raw -> unit
  = "caml_sundials_sp_cvode_setup_cvspgmr_raw"
external cvode_setup_jacobi_times_vector_raw :
  cvode -> ty_fun_jacobian_times_vector_raw -> unit
  = "caml_sundials_sp_cvode_setup_jacobi_times_vector_raw"
external cvode_set_max_order : cvode -> int -> unit
  = "caml_sundials_sp_cvode_set_max_ord"
external cvode_set_max_num_steps : cvode -> int -> unit
  = "caml_sundials_sp_cvode_set_max_num_steps"
external cvode_set_init_step : cvode -> float -> unit
  = "caml_sundials_sp_cvode_set_init_step"
external cvode_set_min_step : cvode -> float -> unit
  = "caml_sundials_sp_cvode_set_min_step"
external cvode_set_max_step : cvode -> float -> unit
  = "caml_sundials_sp_cvode_set_max_step"
external cvode_set_tolerances : cvode -> float -> float -> unit
  = "caml_sundials_sp_cvode_set_tolerances"
external cvode_get_num_steps : cvode -> float
  = "caml_sundials_sp_cvode_get_num_steps"
external cvode_get_step_info : cvode -> float * float * float
  = "caml_sundials_sp_cvode_get_step_info"
external cvode_advance_raw_ba :
  (Mpi_petsc.communicator * bool * cvode * simple_c_array
   * int * float * int * bool) -> float
  = "caml_sundials_sp_cvode_advance_raw_ba"
external cvode_get_integrator_stats_raw :
  cvode ->
  float * float * float * float * float * float * float * float * float *
  float = "caml_sundials_sp_cvode_get_integrator_stats"
external cvode_get_precond_stats_raw :
  cvode -> float * float * float * float * float * float
  = "caml_sundials_sp_cvode_get_precond_stats"
external cvode_get_nonlinsolv_stats_raw : cvode -> float * float
  = "caml_sundials_sp_cvode_get_nonlinsolv_stats"
external cvode_get_tol_scale_factor : cvode -> float
  = "caml_sundials_sp_cvode_get_tol_scale_factor"
external cvode_get_num_ord_red : cvode -> float
  = "caml_sundials_sp_cvode_get_num_ord_red"
val cvode_reinit :
  ?initial_time:float ->
  ?rel_tolerance:float -> ?abs_tolerance:float -> cvode -> unit
val cvode_advance :
  ?comm:Mpi_petsc.communicator ->
  cvode -> float -> simple_c_array -> int -> float

val cvode_advance_ng:
  ?comm:Mpi_petsc.communicator -> ?max_nr_steps:int ->
  ?exact_tstop:bool -> ?verbose:bool ->
  cvode -> float -> simple_c_array -> float
val cvode_get_integrator_stats : cvode -> (string * float) array
val cvode_get_precond_stats : cvode -> (string * float) array
val cvode_get_stats : cvode -> (string * float) array
type cvode_context = (string, float option) Hashtbl.t
val default_cvode_context : cvode_context
val cvode_context_copy : cvode_context -> cvode_context
val cvode_context_set_float : cvode_context -> string -> float option -> unit
val cvode_context_set_int : cvode_context -> string -> int option -> unit
val cvode_context_get_float : cvode_context -> string -> float option
val cvode_context_get_int : cvode_context -> string -> int option
val ensure_libraries_are_initialized : unit -> unit
val cvode_setup_simple :
  ?name:string ->
  ?cvode_context:cvode_context ->
  ?use_jacobian:bool ->
  ?debug:(int -> string -> unit) ->
  ?fun_fill_jacobian:(jacobian:Mpi_petsc.matrix ->
                      y:simple_c_array ->
                      ydot:simple_c_array -> time:float -> unit) ->
  ?t_initial:float ->
  y_initial:simple_c_array ->
  fun_rhs:(time:float -> y:simple_c_array -> ydot:simple_c_array -> int) ->
  unit -> cvode
type 'a cvode_par_functions = {
  cpf_exec_in_parallel_return_handles_on_master :
    (unit -> 'a array) -> 'a array;
}

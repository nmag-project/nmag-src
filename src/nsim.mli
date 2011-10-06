val logmsg : Nlog.level_type -> string -> unit
val loginfo : string -> unit
val loginfo2 : string -> unit
val logdebug : string -> unit
val reportmem : string -> unit
val sundials_path : string -> string
type inference_entity = {
  ien_name : string;
  ien_depends_on : inference_entity array;
  ien_prerequisites : inference_entity array;
  ien_remake : unit -> unit;
  ien_also_updates : inference_entity array;
  mutable ien_is_uptodate : bool;
}
val rebuild : inference_entity -> unit
val invalidate : inference_entity -> unit
type linalg_command =
    LC_vec_distribute of string * string
  | LC_vec_collect of string * string
  | LC_pvec_scale of string * float
  | LC_pvec_pointwise of string * (int -> float -> float)
  | LC_pvec_axpby of Ccpla.ccpla_coefficient * string *
      Ccpla.ccpla_coefficient * string
  | LC_iparams_axpby of Ccpla.ccpla_coefficient * string *
      Ccpla.ccpla_coefficient * string
  | LC_pvec_pull of int array * string * string
  | LC_pvec_push of int array * string * string
  | LC_pvec_pull_fem of string * string * string * string
  | LC_pvec_push_fem of string * string * string * string
  | LC_smx_x_pvec of string * string * string
  | LC_dmx_x_pvec of string * string * string
  | LC_vec_pointwise_mult of string * string * string
  | LC_cofield_to_field of string * string
  | LC_jplan_execute of string * string array
  | LC_psolve of string * string * string
  | LC_psite_wise_iparams of string * string array * string array
  | LC_gosub of string
  | LC_callext of (float -> unit)
  | LC_debug_printvec_1cpu of string * string * int
  | LC_start_timer of string
  | LC_stop_timer of string
  | LC_report_timers of string array
  | LC_clear_timers of string array
type fun_derive_me =
    int -> ((int * int) * (float * string * int array) array) array
type local_equations =
    (Fem.dof_name * (float * Fem.dof_name array) array) array
type jacobi_plan =
    ((int * int) * (float * string * int array) array) array array
type las_op_matrix_spec = {
  loms_name : string;
  loms_mwe_name_le : string;
  loms_mwe_name_ri : string;
  loms_mwe_name_mid: string option;
  loms_symbolic_operator : string;
  loms_matoptions : Mpi_petsc.matoption_type array;
}
type las_dense_matrix =
    BEM_PETSC of Ccpla.distributed_resource_handle
  | BEM_HLib of (Hlib.hmatrix * Mpi_petsc.vector * Mpi_petsc.vector)
type las_dense_matrix_spec = {
  ldms_name : string;
  ldms_hlib : bool;
  ldms_hlib_params : int * int * int * int * float * float * float * int * int;
  ldms_mwe_name : string;
  ldms_dof_name : Fem.dof_name;
  ldms_boundary_spec : string;
  ldms_lattice_info :
    (float array array array * (int * float * float array) array) option;
  ldms_matoptions : Mpi_petsc.matoption_type array;
}
type las_ksp_spec = {
  lks_name : string;
  lks_matrix_name : string;
  lks_precond_name : string;
  lks_ksp_type : string option;
  lks_pc_type : string option;
  lks_nullspace : (bool * string array) option;
  lks_initial_guess_nonzero : bool option;
  lks_rtol : float option;
  lks_atol : float option;
  lks_dtol : float option;
  lks_maxits : int option;
}
type las_swex_spec = {
  lss_name : string;
  lss_c_code : string;
  lss_aux_args : string array;
  lss_field_mwes : string array;
  lss_cofield_mwes : string array;
}
type jplan_derivative = JPLAN_OPERATOR of string | JPLAN_EQUATION of string
type las_jacobi_plan_spec = {
  ljps_name : string;
  ljps_debugprint : bool;
  ljps_mwe_lhs : string;
  ljps_mwe_names : string array;
  ljps_derive_me : jplan_derivative array array;
  ljps_eqns : local_equations;
}
type las_timestepper_spec = {
  lts_name : string;
  lts_jacobian : string;
  lts_name_seq_velocities : string;
  lts_nr_primary_fields : int;
  lts_names_phys_field_mwes : string array;
  lts_names_phys_field_buffers : string array;
  lts_phys_field_derivs : (string * string) array;
  lts_jacobi_prealloc_diagonal : int;
  lts_jacobi_prealloc_off_diagonal : int;
  lts_use_jacobian : bool;
  lts_jacobi_equations : local_equations;
  lts_pc_same_nonzero_pattern : bool;
  lts_pc_opt_rtol : float option;
  lts_pc_opt_atol : float option;
  lts_pc_opt_dtol : float option;
  lts_pc_opt_maxit : int option;
  lts_max_order : int;
  lts_krylov_max : int;
}
type linalg_script = {
  las_intensive_params : string array;
  las_mwes : string array;
  las_internal_buffers : (string * string * Fem.dvss * bool * float) array;
  las_op_matrices : las_op_matrix_spec array;
  las_dense_matrices : las_dense_matrix_spec array;
  las_ksps : las_ksp_spec array;
  las_swexs : las_swex_spec array;
  las_jplans : las_jacobi_plan_spec array;
  las_command_sequences :
    (string * (string * string) array * (string * string) array *
     linalg_command array)
    array;
  las_timesteppers : las_timestepper_spec array;
}
type petsc_or_ccpla_matrix =
    POCM_petsc of Mpi_petsc.matrix
  | POCM_ccpla of Ccpla.drh
type linalg_machine = {
  mutable _resources_to_stick_to : Ccpla.distributed_resource_handle list;
  get_mwe : string -> float Fem.mesh_with_elements;
  get_iparam : string -> float;
  set_iparam : string -> float -> unit;
  set_ksp_tolerances :
    string -> float option * float option * float option * int option -> unit;
  get_ksp_tolerances : string -> float * float * float * int;
  get_field : string -> float Fem.fem_field -> unit;
  get_cofield : string -> float Fem.fem_cofield -> unit;
  set_field : string -> float Fem.fem_field -> unit;
  set_cofield : string -> float Fem.fem_cofield -> unit;
  get_jacobian : string -> petsc_or_ccpla_matrix;
  get_potential : string -> string -> float array -> float;
  get_timers : string array -> (string * float) array;
  timestepper_initialize_from_physics :
    string -> float -> float -> float -> unit;
  timestepper_advance : string -> bool -> float -> int -> float;
  timestepper_get_cvode : string -> Sundials_sp.cvode;
  timestepper_timings : string -> string -> (string * float * float) array;
  internal_ccpla_resources: unit -> string array;
  mutable execute_on :
    string ->
    float Fem.fem_field array -> float Fem.fem_cofield array -> unit;
}
val dummy_linalg_machine : linalg_machine
type par_timestepper_timings = {
  mutable ptt_rhs_n : float;
  mutable ptt_rhs_t : float;
  mutable ptt_jacobi_n : float;
  mutable ptt_jacobi_t : float;
  mutable ptt_jv_n : float;
  mutable ptt_jv_t : float;
  mutable ptt_pc_setup_n : float;
  mutable ptt_pc_setup_t : float;
  mutable ptt_pc_solve_n : float;
  mutable ptt_pc_solve_t : float;
  mutable ptt_extra_n : float;
  mutable ptt_extra_t : float;
}
val make_par_timestepper_timings : unit -> par_timestepper_timings
type par_timestepper = {
  pts_name : string;
  pts_timings : par_timestepper_timings;
  mutable pts_time_reached : float;
  mutable pts_cvode : Sundials_sp.cvode option ref;
  mutable pts_set_initial_from_phys :
    ?initial_time:float -> ?rel_tol:float -> ?abs_tol:float -> unit -> unit;
  mutable pts_advance : ?exact_tstop:bool -> float -> int -> float;
  mutable pts_set_tolerances :
    float option * float option * float option * int option ->
    float * float -> unit;
  mutable pts_timing_control : string -> (string * float * float) array;
}
type 'a nsim_ccpla_resource =
    NSIM_RES_swex of
      (float array ->
       Mpi_petsc.vector array -> Mpi_petsc.vector array -> bool)
  | NSIM_RES_mwes of float Fem.mesh_with_elements array
  | NSIM_RES_vivificators of (string * 'a Fem.vivificator) array
  | NSIM_RES_jacobi_operators of (string * Mpi_petsc.matrix) array
  | NSIM_RES_timestepper of par_timestepper
type nsim_ccpla_opcode =
    NSIM_OP_swex_create of
      (string * string * (string * int array * float array) array *
       (string * string list * string) list)
  | NSIM_OP_swex of int * string array
  | NSIM_OP_mwes_create of string * float Fem.mwe_made_by array option *
      float Fem.mesh_with_elements array option
  | NSIM_OP_vivificators_create of string *
      (string * Ddiffop.ddiffop * string * string * string option)
      array
  | NSIM_OP_vivificator_exec of (string * bool * bool * string option)
  | NSIM_OP_jacobi_operators_create of (string * string array)
  | NSIM_OP_fill_bem of las_dense_matrix_spec
  | NSIM_OP_make_timestepper of (string * las_timestepper_spec)
  | NSIM_OP_init_timestepper of (float * float * float)
  | NSIM_OP_advance_timestepper of (bool * float * int)
  | NSIM_OP_hmatrix_mult of
      ((Hlib.hmatrix * Mpi_petsc.vector * Mpi_petsc.vector) *
       Mpi_petsc.vector * Mpi_petsc.vector)
      option
val special_tensor_delta2 :
  ix_ranges:(string * int) array ->
  Localeqn.ix array -> string list * (float * int list) array
val special_tensor_epsilon :
  ix_ranges:('a * 'b) array ->
  Localeqn.ix array -> string list * (float * int list) array
val default_special_tensors :
  (string *
   (ix_ranges:(string * int) array ->
    Localeqn.ix array -> string list * (float * int list) array))
  list
val local_equation_normal_form :
  ?special_tensors:(string *
                    (ix_ranges:(string * int) array ->
                     Localeqn.ix array ->
                     string list * (float * int list) array))
                   list ->
  ix_ranges:(string * int) array ->
  lhs:string * Localeqn.ix array ->
  rhs:Localeqn.tensor_term ->
  unit ->
  ((string * int array) * (float * (string * int array) array) array) array
val parse_localeqn :
  string ->
  Localeqn.local_spec array *
  ((string * int array) * (float * (string * int array) array) array) array
  array
val parsed_eqn_ccode :
  (string * int array) array *
  ((string * int array) * (float * (string * int array) array) array) array
  array -> string
val localeqn_ccode : string -> string
val layout_y_timestepper_from_primary_fields :
  'a Fem.mesh_with_elements array -> int array * (int * int array) array
val pts_jacobi_derive_me__dummy :
  fun_make_entry:'a ->
  callbuff_fun_make_entry_indices:'b -> contrib_other_factors:'c -> unit
val pts_jacobi_derive_me__primary_field :
  fun_make_entry:('a -> 'a -> 'a -> 'a -> 'b -> 'c) ->
  callbuff_fun_make_entry_indices:'a array -> contrib_other_factors:'b -> 'c
val pts_jacobi_derive_me__ddiffop :
  float Fem.mesh_with_elements array ->
  int ->
  'a ->
  Mpi_petsc.matrix ->
  fun_make_entry:(int -> int -> 'a -> int -> float -> 'b) ->
  callbuff_fun_make_entry_indices:int array ->
  contrib_other_factors:float -> unit
val pts_jacobi_derive_me__localpolynomial :
  ('a, 'b) Ccpla.ccpla ->
  'c Fem.mesh_with_elements array ->
  string ->
  fun_make_entry:'d ->
  callbuff_fun_make_entry_indices:'e -> contrib_other_factors:'f -> 'g
val pts_jacobi_vivificator :
  ?nr_cpu:int ->
  v_mwes:'a Fem.mesh_with_elements array ->
  v_derive_me:(fun_make_entry:'b ->
               callbuff_fun_make_entry_indices:int array ->
               contrib_other_factors:float -> 'c)
              array ->
  local_equations:((string * int array) *
                   (float * (string * int array) array) array)
                  array ->
  (float, 'd, 'e) Bigarray.Array1.t array -> 'b -> unit
val local_equations_to_c_code :
  ((string * string array) * (float * (string * string array) array) array)
  array -> string
val nsim_opcode_interpreter :
  ('a, Mpi_petsc.matrix nsim_ccpla_resource) Ccpla.ccpla ->
  nsim_ccpla_opcode ->
  ('a, Mpi_petsc.matrix nsim_ccpla_resource) Ccpla.distributed_resource array ->
  (string * ('b, 'c nsim_ccpla_resource) Ccpla.distributed_resource) array
val nsim_swex_create :
  (nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  string ->
  string ->
  'b Fem.mesh_with_elements array ->
  'b Fem.mesh_with_elements array ->
  string array -> Ccpla.distributed_resource_handle
val nsim_mwes_create :
  (nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  string ->
  float Fem.mesh_with_elements array -> Ccpla.distributed_resource_handle
val nsim_vivificators_create :
  (nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  string ->
  mwes:Ccpla.drh ->
  vivificator_specs:(string * Ddiffop.ddiffop * string * string *
                     string option)
                    array ->
  Ccpla.distributed_resource_handle
val nsim_jacobi_operators_create :
  (nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  string ->
  jacobi_operator_names:string array ->
  jacobi_drhs:Ccpla.drh array -> Ccpla.distributed_resource_handle
val nsim_timestepper_create :
  (nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  prefix:string ->
  vecs_fields:Ccpla.drh array ->
  script_compute_velocities:Ccpla.drh ->
  mwes:Ccpla.drh ->
  timestepper_spec:las_timestepper_spec ->
  jacobi_operators:Ccpla.drh -> Ccpla.distributed_resource_handle
val  nsim_execute_all_CPUs :
  (nsim_ccpla_opcode, 'a) Ccpla.ccpla -> (unit->unit)->unit
val make_linalg_machine :
  ?ccpla:(nsim_ccpla_opcode, 'a nsim_ccpla_resource) Ccpla.ccpla ->
  prefix:string ->
  relevant_mwes:float Fem.mesh_with_elements array ->
  linalg_script -> linalg_machine
val bem_field_strength :
  ?epsilon:float ->
  linalg_machine ->
  bem_name:string -> boundary_vec_name:string -> float array -> float array
val ddd_bem_potential :
  linalg_machine ->
  bem_name:string -> boundary_vec_name:string -> float array -> float

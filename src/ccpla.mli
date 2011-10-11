val debug_mpi_buffer_lengths : bool
val rccpla_logdebug : (string -> unit) ref
val vec_dummy : Mpi_petsc.vector
type ccpla_coefficient = CCCOEFF_var of string | CCCOEFF_float of float
type distributed_resource_handle = DRH of string
type drh = distributed_resource_handle
type ('a, 'b) distributed_resource =
    DRES_petsc_vector of string *
      (Nsimconf.c_int_bigarray1 * Nsimconf.c_int_bigarray1) *
      Mpi_petsc.vector
  | DRES_petsc_vector_multiseq of string * Mpi_petsc.vector
  | DRES_petsc_matrix of string * Mpi_petsc.matrix option ref
  | DRES_petsc_ksp of string * Mpi_petsc.ksp
  | DRES_petsc_matnullspace of string * Mpi_petsc.matnullspace
  | DRES_parameters of string array * float array
  | DRES_sequence of string * ('a, 'b) distributed_command array
  | DRES_opdata of ('b * (unit -> unit))
and ('a, 'b) distributed_command =
    DCOM_getfrom_master of int * drh
  | DCOM_sendto_master of int * drh
  | DCOM_getfrom_master_v of Mpi_petsc.vector option * drh
  | DCOM_sendto_master_v of Mpi_petsc.vector option * drh
  | DCOM_printvec of drh
  | DCOM_printmat of drh
  | DCOM_vec_scale of (drh * float)
  | DCOM_vec_pointwise of (drh * (int -> float -> float))
  | DCOM_push of int array * drh * drh
  | DCOM_pull of int array * drh * drh
  | DCOM_mx_x_vec of drh * drh * drh
  | DCOM_vec_sety_axpby of ccpla_coefficient * ccpla_coefficient * drh *
      drh * drh
  | DCOM_vec_pointwise_mult of drh * drh * drh
  | DCOM_vec_pointwise_divide of drh * drh * drh
  | DCOM_NOP
  | DCOM_QUIT
  | DCOM_HELLO
  | DCOM_exec of (unit -> unit)
  | DCOM_master of (unit -> unit)
  | DCOM_rec_master of (unit -> unit)
  | DCOM_rec_quit
  | DCOM_destroy_resources of string list
  | DCOM_register_sequence of string * ('a, 'b) distributed_command array
  | DCOM_execute_sequence of drh
  | DCOM_register_parameters of string * string array * float array
  | DCOM_manipulate_parameters of drh * (string array -> float array -> unit)
  | DCOM_vec_create of string * int array
  | DCOM_vec_create_multiseq of string * int
  | DCOM_mat_create of string * string * int array * int array
  | DCOM_mat_duplicate of drh * string * bool
  | DCOM_mat_copy of drh * drh * bool
  | DCOM_mat_zero of drh
  | DCOM_mat_flush_ntimes of drh * int
  | DCOM_vec_assembly_begin of drh
  | DCOM_vec_assembly_end of drh
  | DCOM_mat_assembly_begin of drh * bool
  | DCOM_mat_assembly_end of drh * bool
  | DCOM_mat_scale_and_add_identity of drh * float * float
  | DCOM_ksp_create of string * Mpi_petsc.matstructure_type * drh * drh
  | DCOM_matnullspace_create of string * bool * drh array
  | DCOM_ksp_set_initial_guess_nonzero of drh * bool
  | DCOM_ksp_set_matnullspace of drh * drh
  | DCOM_ksp_set_type of drh * string
  | DCOM_ksp_set_pc_type of drh * string
  | DCOM_ksp_set_pc_bjacobi_subtype of drh * string
  | DCOM_ksp_set_tolerances_opt of drh * float option * float option *
      float option * int option
  | DCOM_ksp_manipulate_tolerances of drh *
      (float * float * float * int -> float * float * float * int)
  | DCOM_ksp_set_operators of (drh * drh * drh)
  | DCOM_ksp_set_up of drh
  | DCOM_ksp_solve of drh * drh * drh
  | DCOM_opcode of 'a * drh array
val dres_type : ('a, 'b) distributed_resource -> string
val dres_get_vector : ('a, 'b) distributed_resource -> Mpi_petsc.vector
val dres_get_vector_lengths_offsets :
  ('a, 'b) distributed_resource ->
  Nsimconf.c_int_bigarray1 * Nsimconf.c_int_bigarray1
val dres_get_vector_multiseq :
  ('a, 'b) distributed_resource -> Mpi_petsc.vector
val dres_get_matrix : ('a, 'b) distributed_resource -> Mpi_petsc.matrix
val dres_get_ksp : ('a, 'b) distributed_resource -> Mpi_petsc.ksp
val dres_get_matnullspace :
  ('a, 'b) distributed_resource -> Mpi_petsc.matnullspace
val dres_get_parameters : ('a, 'b) distributed_resource -> string array*float array
val dres_get_sequence :
  ('a, 'b) distributed_resource -> ('a, 'b) distributed_command array
val dres_get_opdata : ('a, 'b) distributed_resource -> 'b
type ('a, 'b) ccpla = {
  ccpla_comm : Mpi_petsc.communicator;
  mutable ccpla_opcode_interpreter :
    'a ->
    ('a, 'b) distributed_resource array ->
    (string * ('a, 'b) distributed_resource) array;
  ccpla_resources : (string, ('a, 'b) distributed_resource) Hashtbl.t;
  ccpla_queue : ('a, 'b) distributed_command array Queue.t ref;
  mutable ccpla_pending_finalizations : string list;
  ccpla_quit : unit -> unit;
  mutable ccpla_register_params :
    string -> string array -> float array -> distributed_resource_handle;
  mutable ccpla_manipulate_params :
    drh -> (string array -> float array -> unit) -> unit;
  mutable ccpla_sequence_create :
    string ->
    ('a, 'b) distributed_command array array -> distributed_resource_handle;
  mutable ccpla_sequence_execute :
    ?local_vectors:Mpi_petsc.vector array ->
    distributed_resource_handle -> unit;
  mutable ccpla_vector_create :
    string -> int array -> distributed_resource_handle;
  mutable ccpla_vector_create_multiseq :
    string -> int -> distributed_resource_handle;
  mutable ccpla_vector_distribute :
    ?local_vectors:Mpi_petsc.vector array ->
    int -> distributed_resource_handle -> unit;
  mutable ccpla_vector_collect :
    ?local_vectors:Mpi_petsc.vector array ->
    int -> distributed_resource_handle -> unit;
  mutable ccpla_matrix_create :
    string -> string -> int array -> int array -> distributed_resource_handle;
  mutable ccpla_matrix_duplicate :
    drh -> string -> bool -> distributed_resource_handle;
  mutable ccpla_matrix_copy : drh -> drh -> bool -> unit;
  mutable ccpla_matrix_scale_and_add_identity : drh -> float -> float -> unit;
  mutable ccpla_matrix_times_vector : drh -> drh -> drh -> unit;
  mutable ccpla_ksp_solve : drh -> drh -> drh -> unit;
  mutable ccpla_matrix_assemble :
    ?final:bool -> distributed_resource_handle -> unit;
  mutable ccpla_vector_assemble : distributed_resource_handle -> unit;
  mutable ccpla_petsc_vector :
    distributed_resource_handle -> Mpi_petsc.vector;
  mutable ccpla_petsc_matrix :
    distributed_resource_handle -> Mpi_petsc.matrix;
  mutable ccpla_petsc_ksp : distributed_resource_handle -> Mpi_petsc.ksp;
  mutable ccpla_iparams :
    distributed_resource_handle -> string array * float array;
  mutable ccpla_petsc_multiseq_vector :
    distributed_resource_handle -> Mpi_petsc.vector;
  mutable ccpla_ksp_create :
    string ->
    ?matrix_structure:Mpi_petsc.matstructure_type ->
    ?tolerances:float option * float option * float option * int option ->
    ?initial_guess_nonzero:bool ->
    ?ksp_type:string ->
    ?pc_type:string ->
    ?pc_subtype:string ->
    ?matnullspace:bool * drh array ->
    distributed_resource_handle ->
    distributed_resource_handle -> distributed_resource_handle;
  mutable ccpla_ksp_set_up : drh -> unit;
  mutable ccpla_ksp_set_operators :
    ?do_register_new_finalizer:bool -> drh -> drh -> drh -> unit;
  mutable ccpla_ksp_manipulate_tolerances :
    drh ->
    (float * float * float * int -> float * float * float * int) -> unit;
}
val ccpla_get_resource :
  ('a, 'b) ccpla -> string -> ('a, 'b) distributed_resource

val ccpla_registered_resources :
  ('a, 'b) ccpla -> string array

val dcom_execute :
  ?local_vectors:Mpi_petsc.vector array ->
  ('a, 'b) ccpla -> ('a, 'b) distributed_command -> unit
val dcom_execute_1 :
  ?local_vectors:Mpi_petsc.vector array ->
  ('a, 'b) ccpla -> ('a, 'b) distributed_command -> unit
val master_process_queue :
  ?local_vectors:Mpi_petsc.vector array -> ('a, 'b) ccpla -> unit
val mpi_get_distributed_command :
  ('a, 'b) ccpla -> ('c, 'd) distributed_command
val slave_mode : ('a, 'b) ccpla -> 'c
val ccpla_vector_create :
  ('a, 'b) ccpla -> string -> int array -> distributed_resource_handle
val ccpla_vector_create_multiseq :
  ('a, 'b) ccpla -> string -> int -> distributed_resource_handle
val ccpla_vector_distribute :
  ('a, 'b) ccpla ->
  ?local_vectors:Mpi_petsc.vector array ->
  int -> distributed_resource_handle -> unit

val ccpla_hello : ('a, 'b) ccpla -> unit

val ccpla_vector_collect :
  ('a, 'b) ccpla ->
  ?local_vectors:Mpi_petsc.vector array ->
  int -> distributed_resource_handle -> unit
val ccpla_matrix_create :
  ('a, 'b) ccpla ->
  string -> string -> int array -> int array -> distributed_resource_handle
val ccpla_matrix_duplicate :
  ('a, 'b) ccpla -> drh -> string -> bool -> distributed_resource_handle
val ccpla_matrix_copy : ('a, 'b) ccpla -> drh -> drh -> bool -> unit
val ccpla_matrix_scale_and_add_identity :
  ('a, 'b) ccpla -> drh -> float -> float -> unit
val ccpla_matrix_times_vector : ('a, 'b) ccpla -> drh -> drh -> drh -> unit
val ccpla_ksp_solve : ('a, 'b) ccpla -> drh -> drh -> drh -> unit
val ccpla_matrix_assemble : ('a, 'b) ccpla -> ?final:bool -> drh -> unit
val ccpla_vector_assemble : ('a, 'b) ccpla -> drh -> unit
val ccpla_petsc_matrix :
  ('a, 'b) ccpla -> distributed_resource_handle -> Mpi_petsc.matrix
val ccpla_petsc_vector :
  ('a, 'b) ccpla -> distributed_resource_handle -> Mpi_petsc.vector
val ccpla_petsc_ksp :
  ('a, 'b) ccpla -> distributed_resource_handle -> Mpi_petsc.ksp
val ccpla_iparams :
  ('a, 'b) ccpla -> distributed_resource_handle -> string array * float array
val ccpla_petsc_multiseq_vector :
  ('a, 'b) ccpla -> distributed_resource_handle -> Mpi_petsc.vector
val ccpla_ksp_create :
  ('a, 'b) ccpla ->
  string ->
  ?matrix_structure:Mpi_petsc.matstructure_type ->
  ?tolerances:float option * float option * float option * int option ->
  ?initial_guess_nonzero:bool ->
  ?ksp_type:string ->
  ?pc_type:string ->
  ?pc_subtype:string ->
  ?matnullspace:bool * drh array -> drh -> drh -> distributed_resource_handle
val ccpla_ksp_set_up : ('a, 'b) ccpla -> drh -> unit
val ccpla_ksp_set_operators :
  ('a, 'b) ccpla ->
  ?do_register_new_finalizer:bool -> drh -> drh -> drh -> unit
val ccpla_register_params :
  ('a, 'b) ccpla ->
  string -> string array -> float array -> distributed_resource_handle
val ccpla_manipulate_params :
  ('a, 'b) ccpla -> drh -> (string array -> float array -> unit) -> unit
val ccpla_sequence_create :
  ('a, 'b) ccpla ->
  string ->
  ('a, 'b) distributed_command array array -> distributed_resource_handle
val ccpla_sequence_execute :
  ('a, 'b) ccpla -> ?local_vectors:Mpi_petsc.vector array -> drh -> unit
val ccpla_ksp_manipulate_tolerances :
  ('a, 'b) ccpla ->
  drh -> (float * float * float * int -> float * float * float * int) -> unit
val _master_set_finalization_alarm : ('a, 'b) ccpla -> unit
val _master_exit : ('a, 'b) ccpla -> unit
val setup :
  ?petsc_argv:string array ->
  string array ->
  (('a, 'b) ccpla ->
   'a ->
   ('a, 'b) distributed_resource array ->
   (string * ('a, 'b) distributed_resource) array) ->
  (unit -> unit) -> ('a, 'b) ccpla
val do_test : ('a, 'b) ccpla -> unit
val do_test2 : ('a, 'b) ccpla -> unit
val do_test3 : ('a, 'b) ccpla -> unit

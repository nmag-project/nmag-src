type c_int_bigarray1 = Nsimconf.c_int_bigarray1
val c_int_bigarray1_create :
  int -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
val c_int_of_int : int -> int32
val c_int_to_int : int32 -> int
val c_int_add : int32 -> int32 -> int32
val default_ddpla_error : 'a -> string -> unit
type ddpla_magictype
type ddpla_pill_payload
type ddpla_pilltype = string
type ddpla_pill = ddpla_pilltype * ddpla_pill_payload
type opcode_argument =
    OA_bool of bool
  | OA_int of int
  | OA_float of float
  | OA_string of string
  | OA_adminfunction of (ddpla -> opcode_argument array -> unit)
  | OA_resource of drh
  | OA_array of opcode_argument array
  | OA_assoc_array of (string * opcode_argument) array
  | OA_abstract_exec of
      (report_error:(?level:string -> string -> string) ->
       unit -> opcode_argument array -> unit)
  | OA_buildscript of dscript
  | OA_complex_data of (ddpla_magictype * string * drh array)
and dres_object =
    DRO_pvec of (c_int_bigarray1 * c_int_bigarray1) * Mpi_petsc.vector
  | DRO_msvec of Mpi_petsc.vector
  | DRO_matrix of Mpi_petsc.matrix
  | DRO_solver of Mpi_petsc.ksp
  | DRO_matnullspace of Mpi_petsc.matnullspace
  | DRO_parameters of string array * float array
  | DRO_sequence of dscript
  | DRO_data of (string * ddpla_magictype)
and dcommand = {
  dc_opcode : string;
  dc_args : (string * opcode_argument) array;
}
and dcompiled = {
  dco_opcode : string;
  dco_param_names : string array;
  dco_params : opcode_argument array;
  dco_exec : opcode_argument array -> unit;
}
and dscript = dcommand array
and dbinary = dcompiled array
and dres = {
  dres_name : string;
  dres_rebuild : dbinary;
  dres_destroy : dres -> unit;
  mutable dres_obj : dres_object option;
  mutable dres_dirtiness : bool;
  mutable dres_prerequisites : distributed_resource_handle list;
  mutable dres_dependents : distributed_resource_handle list;
}
and distributed_resource_handle = DRH of (string * dres option ref)
and ddpla = {
  ddpla_name : string;
  ddpla_comm : Mpi_petsc.communicator;
  ddpla_error : string -> string -> unit;
  ddpla_pill_types :
    (string,
     (ddpla_magictype -> ddpla_pill) * (ddpla_pill -> ddpla_magictype))
    Hashtbl.t;
  ddpla_opcodes :
    (string, string array * (opcode_argument array -> unit)) Hashtbl.t;
  ddpla_resources : (string, dres) Hashtbl.t;
  ddpla_queue : dcommand array Queue.t;
  ddpla_quit : unit -> unit;
  mutable ddpla_new_resources_on_master : drh list;
  mutable ddpla_pending_finalizations : string list;
}
and drh = distributed_resource_handle
val drh_name : distributed_resource_handle -> string
val dro_typename : dres_object -> string
val oa_to_string : opcode_argument -> string
val oa_bool :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> bool
val oa_int :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> int
val oa_float :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> float
val oa_string :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> string
val oa_resource :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> drh
val oa_abstract_exec :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  opcode_argument ->
  report_error:(?level:string -> string -> string) ->
  unit -> opcode_argument array -> unit
val oa_adminfunction :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  opcode_argument -> ddpla -> opcode_argument array -> unit
val oa_buildscript :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> dscript
val oa_complex_data :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  ?required_type:string -> opcode_argument -> ddpla_magictype
val oa_array :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string -> opcode_argument -> opcode_argument array
val oa_int_array :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> int array
val oa_float_array :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> float array
val oa_string_array :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> string array
val oa_resource_array :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string -> param_name:string -> opcode_argument -> drh array
val oa_opt_bool :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  opcode_argument -> ?default:bool -> string -> bool option
val oa_opt_int :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  opcode_argument -> ?default:int -> string -> int option
val oa_opt_float :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  opcode_argument -> ?default:float -> string -> float option
val oa_opt_string :
  report_error:(?level:string -> string -> string) ->
  opcode_name:string ->
  param_name:string ->
  opcode_argument -> ?default:string -> string -> string option
val buildscript_prereqs : dscript -> drh array
val oa_deps : opcode_argument -> drh array
val ddpla_registered_resources : ddpla -> string list
val get : ddpla -> distributed_resource_handle -> dres
val register_dependency :
  parent:distributed_resource_handle ->
  child:distributed_resource_handle -> ddpla -> unit
val ddpla_register_new :
  ?drh:drh ->
  ddpla -> prerequisites:distributed_resource_handle array -> dres -> drh
val _get_type_error :
  string ->
  string -> ddpla -> distributed_resource_handle -> dres_object -> 'a
val get_msvec : ddpla -> distributed_resource_handle -> Mpi_petsc.vector
val get_pvec : ddpla -> distributed_resource_handle -> Mpi_petsc.vector
val get_pvec_lengths_offsets :
  ddpla -> distributed_resource_handle -> c_int_bigarray1 * c_int_bigarray1
val get_matrix : ddpla -> distributed_resource_handle -> Mpi_petsc.matrix
val get_matnullspace :
  ddpla -> distributed_resource_handle -> Mpi_petsc.matnullspace
val get_solver : ddpla -> distributed_resource_handle -> Mpi_petsc.ksp
val get_parameters :
  ddpla -> distributed_resource_handle -> string array * float array
val get_sequence : ddpla -> distributed_resource_handle -> dscript
val get_data :
  ddpla -> string -> distributed_resource_handle -> ddpla_magictype
val known_dependencies : ddpla -> (string * string array) array
val commands_as_script : dscript -> dscript
val compile_script : ddpla -> dcommand array -> dcompiled array
val execute_compiled : ?trace:bool -> ddpla -> dcompiled array -> unit
val execute_script : ?trace:bool -> ddpla -> dcommand array -> unit
val execute_rebuild : ddpla -> distributed_resource_handle -> unit
val ensure_uptodate : ddpla -> distributed_resource_handle -> unit
val spoil_uptodate : ddpla -> distributed_resource_handle -> unit
val mpi_get_distributed_command_sequence : ddpla -> dcommand array
val slave_mode : ddpla -> 'a
val master_process_queue : ddpla -> unit
val _def_opcode :
  ddpla:ddpla ->
  opcode:string ->
  ?args:string array ->
  (report_error:(?level:string -> string -> string) ->
   unit -> opcode_argument array -> unit) ->
  unit
val ddpla_create_skeleton :
  fun_error:(string -> string -> unit) ->
  name:string ->
  parent_communicator:Mpi_petsc.communicator ->
  fun_quit:(unit -> unit) -> unit -> ddpla
val ddpla_add_core_opcodes :
  ?fun_error:('a -> string -> unit) -> ddpla -> unit
val ddpla_create :
  ?fun_error:(string -> string -> unit) ->
  ?name:string ->
  parent_communicator:Mpi_petsc.communicator ->
  fun_quit:(unit -> unit) -> unit -> ddpla
val ddpla_names_of_unsnatched_resources : ddpla -> string list
val ddpla_snatch_new : ddpla -> string -> drh
val ddpla_register_pill : ddpla -> ddpla_pilltype -> unit
val ddpla_exec : ddpla -> dcommand array -> unit
val ddpla_hello : ddpla -> unit

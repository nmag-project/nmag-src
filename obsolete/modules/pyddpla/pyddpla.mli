val pysym_drh : string
val pysym_oa : string
val ocamlpill_from_drh : Ddpla.drh -> Pycaml.pyobject
val drh_from_ocamlpill : Pycaml.pyobject -> 'a
val ocamlpill_from_oa : Ddpla.opcode_argument -> Pycaml.pyobject
val oa_from_ocamlpill : Pycaml.pyobject -> 'a
val oa_from_python_special_named_pair :
  string -> Pycaml.pyobject -> Ddpla.opcode_argument
val process_list_tuple : Pycaml.pyobject -> Ddpla.opcode_argument
val oa_from_python : Pycaml.pyobject -> Ddpla.opcode_argument
val oa_to_python : Ddpla.opcode_argument -> Pycaml.pyobject
val dscript_from_python : Pycaml.pyobject -> Ddpla.dcommand array
val pyddpla_add_opcodes : Ddpla.ddpla -> unit
val _pyddpla_was_initialized : bool ref
val init : unit -> Ddpla.ddpla
val _pyddpla_exec : string -> Pycaml.pyobject
val _pyddpla_snatchable : string -> Pycaml.pyobject
val _pyddpla_snatch_new : string -> Pycaml.pyobject
val _pyddpla_drh_name : string -> Pycaml.pyobject
val _pyddpla_drh_ensure_uptodate : string -> Pycaml.pyobject
val _pyddpla_drh_spoil_uptodate : string -> Pycaml.pyobject
val _pyddpla_dependencies : string -> Pycaml.pyobject
val _ddpla_pyeval_on_drh_msvec : string -> Pycaml.pyobject

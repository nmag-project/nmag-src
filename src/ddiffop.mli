type tensor_index = IX_int of int | IX_name of string
type parsed_difftype =
    PDIFF_none
  | PDIFF_vol of tensor_index
  | PDIFF_boundary of tensor_index
type dof_logic =
    DLOG_true
  | DLOG_and of dof_logic list
  | DLOG_or of dof_logic list
  | DLOG_not of dof_logic
  | DLOG_all of string
  | DLOG_some of string
  | DLOG_nregions of int
type nsi_field = {
  fnsi_name : string;
  fnsi_indices : tensor_index array;
  fnsi_bspec : dof_logic;
}
type field = { f_name : string; f_indices : int array; f_bspec : dof_logic; }
type amendment =
    AMDMT_MXDIM of nsi_field list option * nsi_field list option
  | AMDMT_DIAGONAL_ONES of nsi_field * nsi_field
  | AMDMT_GAUGE_FIX of nsi_field
  | AMDMT_PERIODIC of nsi_field
type diff_field_spec = field * parsed_difftype
type diff_mxdim_spec = string list option
type difftype = DIFF_none | DIFF_vol of int | DIFF_boundary of int
type ddiffop = {
  diff_contribs :
    (field array * (difftype * difftype, float) Hashtbl.t) array;
  diff_mxdim : field array * field array;
  diff_diag_ones : (field * field) array;
  diff_gauge_fix : field array;
  diff_periodic : field array;
}
val multifor : int array -> (int -> int array -> unit) -> unit
val hashtbl_arbitrary_element : ('a, 'b) Hashtbl.t -> ('a * 'b) option
val hashtbl_keys :
  ?sorter:('a -> 'a -> int) -> ('a, 'b) Hashtbl.t -> 'a array
val map_hashtbl_to_array :
  ?sorter:('a -> 'a -> int) ->
  ('b -> 'c -> 'a) -> ('b, 'c) Hashtbl.t -> 'a array
val optionally : ('a -> 'b) -> 'a option -> 'b option
val forall_index_instantiations :
  string array ->
  (string * int) list -> (ix_value:(tensor_index -> int) -> unit) -> unit
val instantiate_field_indices :
  ix_value:(tensor_index -> int) -> nsi_field -> field
val all_symbolic_indices : nsi_field array -> string array
val expand_fields : (string * int) list -> nsi_field list -> field array
val amdmt_get_diag_ones :
  (string * int) list -> amendment list -> (field * field) array
val amdmt_get_gauge_fixed :
  (string * int) list -> amendment list -> field array
val amdmt_get_periodic : (string * int) list -> amendment list -> field array
val amdmt_get_mxdim :
  (string * int) list -> amendment list -> field array * field array
val build_ddiffop :
  (float *
   ((nsi_field * parsed_difftype) * (nsi_field * parsed_difftype) *
    nsi_field option))
  list -> (string * int) list -> amendment list -> ddiffop

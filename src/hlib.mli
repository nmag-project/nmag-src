exception HLib_exn of string
type hmatrix
type hlib_algorithm = ACA_I | ACA_II | Interpolation | HCA_I | HCA_II
val hlib_algorithm_id : hlib_algorithm -> int
type simple_float_bigarray =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
external hlib_init_raw : string -> unit = "caml_hlib_init"
val raw_make_hmatrix :
  float array array ->
  int array array ->
  int array array ->
  int array array ->
  int * int * int * int * float * float * float * int * int -> hmatrix
(* Code: HLib parallel *)
(*
  external raw_make_hmatrix_strip :
  float array array * int array array * int array array * int array array ->
  float array array * int array array * int array array * int array array ->
  int * int * int * float * float * float * int * int -> hmatrix
  = "caml_hlib_raw_make_hmatrix_strip"
 *)
external write_hmatrix : string -> hmatrix -> unit
  = "caml_hlib_write_hmatrix"
external read_hmatrix : string -> hmatrix = "caml_hlib_read_hmatrix"
external get_hmatrix_stats: hmatrix -> float array
  = "caml_hlib_get_matrix_stats"
val get_hmatrix_stats_verbose: hmatrix -> (float*string*string) array
external apply_hmatrix :
  hmatrix -> simple_float_bigarray -> simple_float_bigarray -> unit
  = "caml_hlib_apply_hmatrix"
val hlib_init : string -> unit
val make_hmatrix_from_oriented_triangles :
  ?cluster_strategy:int -> ?algorithm:int -> ?nfdeg:int -> ?nmin:int ->
  ?eta:float -> ?eps_aca:float -> ?eps:float -> ?p:int -> ?kmax:int ->
  float array array -> int array array -> hmatrix
val make_hmatrix :
  ?cluster_strategy:int -> ?algorithm:int -> ?nfdeg:int -> ?nmin:int ->
  ?eta:float -> ?eps_aca:float -> ?eps:float -> ?p:int -> ?kmax:int ->
  ?lattice_info: float array array array * (int * float * float array) array ->
  float array array -> int array array -> float array array -> hmatrix
(* Code: HLib parallel *)
(*
  val make_hmatrix_strip_from_oriented_triangles :
  surface_vertex_distribution:int array ->
  nr_strip:int ->
  ?algorithm:hlib_algorithm ->
  ?nfdeg:int ->
  ?nmin:int ->
  ?eta:float ->
  ?eps_aca:float ->
  ?eps:float ->
  ?p:int -> ?kmax:int -> float array array -> int array array -> hmatrix
val make_hmatrix_strip :
  surface_vertex_distribution:int array ->
  nr_strip:int ->
  ?algorithm:hlib_algorithm ->
  ?nfdeg:int ->
  ?nmin:int ->
  ?eta:float ->
  ?eps_aca:float ->
  ?eps:float ->
  ?p:int ->
  ?kmax:int ->
  float array array -> int array array -> float array array -> hmatrix
 *)

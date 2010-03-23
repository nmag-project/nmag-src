exception HLib_exn of string
type hmatrix
type hlib_algorithm = ACA_I | ACA_II | Interpolation | HCA_I | HCA_II
val hlib_algorithm_id : hlib_algorithm -> int
type simple_float_bigarray =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
external hlib_init_raw : string -> unit = "caml_hlib_init"
external raw_make_hmatrix :
  float array array ->
  int array array ->
  int array array ->
  int array array ->
  int * int * int * float * float * float * int * int -> hmatrix
  = "caml_hlib_raw_make_hmatrix"
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
external apply_hmatrix :
  hmatrix -> simple_float_bigarray -> simple_float_bigarray -> unit
  = "caml_hlib_apply_hmatrix"
val hlib_init : string -> unit
val make_hmatrix_from_oriented_triangles :
  ?algorithm:int ->
  ?nfdeg:int ->
  ?nmin:int ->
  ?eta:float ->
  ?eps_aca:float ->
  ?eps:float ->
  ?p:int -> ?kmax:int -> float array array -> int array array -> hmatrix
val make_hmatrix :
  ?algorithm:int ->
  ?nfdeg:int ->
  ?nmin:int ->
  ?eta:float ->
  ?eps_aca:float ->
  ?eps:float ->
  ?p:int ->
  ?kmax:int ->
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

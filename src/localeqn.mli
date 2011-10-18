type ix = IX_int of int | IX_var of string
type tensor_term =
    Tensor_sum of tensor_term list
  | Tensor_product of tensor_term list
  | Tensor_float of float
  | Tensor_varindexed of (string * ix array)
  | Tensor_func of (string * tensor_term)
type tensor_lvalue = string * ix array
type assignment = tensor_lvalue * tensor_term
type range = string * int
type local_spec = string * int array
type local_eqn = local_spec array * range array * assignment array
val multifor : int array -> (int -> int array -> unit) -> unit
val forall_index_instantiations :
  string array ->
  (string * int) list -> (ix_value:(ix -> int) -> unit) -> unit
val new_tensor_product :
  ?extra_sign:float -> float * tensor_term list -> tensor_term

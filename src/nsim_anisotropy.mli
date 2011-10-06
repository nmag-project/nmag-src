val independent_so_n_scalars : int -> int -> int array array
val vivify_multinomial_coefficient_vector :
  int array array -> float array -> float array -> float
val reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients :
  ?fun_rng:(float -> float) ->
  ?dim:int ->
  ?iterations:int -> int array array -> (float array * float array) array
val taylor_coefficients_of_spherical_function :
  ?fun_rng:(float -> float) ->
  ?dim:int -> int -> (float array -> float) -> (float * int array) array
val _ht_to_poly : (int array, float) Hashtbl.t -> (float * int array) array
val polypotential_gradient :
  ?dim:int -> (float * int array) array -> (float * int array) array array
val unit_sphere_tangential_polypotential_gradient :
  ?dim:int -> (float * int array) array -> (float * int array) array array
val polypotential_equation_rhs :
  ?correction_factor:float -> string -> (float * int array) array -> string
val anisotropy_equations :
  ?factor_mu0:float ->
  ?fun_rng:(float -> float) ->
  ?dim:int ->
  ?name_E:string ->
  ?name_m:string ->
  ?name_H:string -> int -> (float array -> float) -> string * string

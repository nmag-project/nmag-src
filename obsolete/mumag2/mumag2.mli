(* (C) 2007 Dr. Thomas Fischbacher *)


val version : unit -> string
type llg_material = {
  llg_name : string;
  llg_Ms : float;
  llg_J : float;
  llg_c1 : float;
  llg_c2 : float;
  llg_c3 : float;
  llg_stt_c1 : float;
  llg_stt_c2 : float;
  llg_anisotropy : (float array -> float) option;
  llg_anisotropy_order : int;
}
val standard_simplex_subdivide :
  ('a array -> 'a) -> 'a array -> 'a array array
val subdivide_if_surface_angle_larger_than :
  int -> float -> float array -> 'a -> float array array -> bool
val do_for_simplex_subdivisions :
  ?point_meddler:(?storage:float array -> float array array -> float array) ->
  ?fun_how_to_subdivide:((float array array -> float array) ->
                         float array array -> float array array array) ->
  start_level:'a ->
  fun_stop_level:('a -> bool) ->
  ('a -> float array array -> 'a) -> float array array -> unit
val integrate_femfun_numfuns_over_face :
  ?point_meddler:(?storage:float array -> float array array -> float array) ->
  ?fun_how_to_subdivide:((float array array -> float array) ->
                         float array array -> float array array array) ->
  float Fem.femfun ->
  ((float array -> float) * (float array -> float array array -> bool)) array ->
  float array -> Mesh.simplex -> int -> float array
val simplex3d_outward_surface_normal_ext :
  ?normalize:bool -> Mesh.simplex -> int -> float array
val green_kernel_directional_derivative :
  ?nr_coords:int -> int -> float array -> float array -> float array -> float
val array_intersection_eq : 'a array -> 'a array -> 'a list
val lindholm_triangle_contributions :
  ?store_zeta:float array ->
  Mesh.coords -> float array -> float array -> float array -> float array
val lindholm_simplex3d_face_contributions :
  ?lindholm_triangle_contributions:(?store_zeta:float array ->
                                    Mesh.coords ->
                                    float array ->
                                    float array -> float array -> float array) ->
  Mesh.simplex -> int -> Mesh.coords -> float array
val bem_matrix_lindholm :
  ?lindholm_triangle_contributions:(?store_zeta:float array ->
                                    Mesh.coords ->
                                    float array ->
                                    float array -> float array -> float array) ->
  ?inside_regions:Mesh.simplex_region list ->
  ?dof_name:Fem.dof_name ->
  ?petsc_name:string ->
  'a Fem.mesh_with_elements -> Fem.dof array * Mpi_petsc.matrix
val laplace_solver_bem :
  ?lindholm_triangle_contributions:(?store_zeta:float array ->
                                    Mesh.coords ->
                                    float array ->
                                    float array -> float array -> float array) ->
  ?inside_regions:Mesh.simplex_region list ->
  ?petsc_name:string ->
  float Fem.prematrix ->
  ?target:float Fem.fem_field -> float Fem.fem_cofield -> float Fem.fem_field
val demag_fun_diffop_div_m_string :
  string -> string -> llg_material array -> string
val make_demag_fun_3d :
  ?petsc_name:string ->
  ?lindholm_triangle_contributions:(?store_zeta:float array ->
                                    Mesh.coords ->
                                    float array ->
                                    float array -> float array -> float array) ->
  ?fun_make_and_register_mwe_field:(float Fem.mesh_with_elements ->
                                    float Fem.mesh_with_elements *
                                    float Fem.fem_field) ->
  ?sloppy_cofield_to_field:bool ->
  ?name_demag:string ->
  ?name_rho:string ->
  ?name_phi:string ->
  ?diffop_div_m_string:string ->
  ?order:int ->
  ?inside_regions:Mesh.simplex_region list ->
  ?fem_only:bool ->
  llg_material array ->
  float Fem.mesh_with_elements ->
  float Fem.mesh_with_elements ->
  ?compute_field_rho:bool ->
  ?debug_charge_imbalance:bool ->
  ?target:float Fem.fem_field -> float Fem.fem_field -> float Fem.fem_field
val make_exchange_fun :
  ?petsc_name:string ->
  ?sloppy_cofield_to_field:bool ->
  ?name_exchange:string ->
  llg_material array ->
  float Fem.mesh_with_elements ->
  float Fem.mesh_with_elements ->
  (int array, float) Hashtbl.t *
  (?target:float Fem.fem_field -> float Fem.fem_field -> float Fem.fem_field)
val make_fun_to_compute_H_total :
  ?strict_mwe_check:bool ->
  ?basic_equation:string * string * string ->
  ?extra_contribs_by_magnetization_name:(string * string) array ->
  ?intensive_parameter_names:string array ->
  string array ->
  'a Fem.mesh_with_elements array ->
  float array -> 'a Fem.fem_field array -> 'a Fem.fem_cofield array -> bool
val make_fun_to_compute_llg_rhs :
  ?strict_mwe_check:bool ->
  llg_material array ->
  'a Fem.mesh_with_elements array ->
  float array -> 'a Fem.fem_field array -> 'a Fem.fem_cofield array -> bool
val llg_jacobi_plan :
  ?prefix_dofs_m:string ->
  ?prefix_dofs_h:string ->
  (int array, float) Hashtbl.t ->
  float Fem.mesh_with_elements ->
  float Fem.mesh_with_elements ->
  llg_material array -> (float * int array) array array
external jacobi_plan_executor :
  (float * int array) array array ->
  Mpi_petsc.matrix -> Mpi_petsc.vector -> Mpi_petsc.vector -> unit
  = "caml_mumag_jacobi_plan_executor"
type ty_fun_advance_time =
    float array ->
    float ->
    int ->
    float * float * (string * float * float) array * float Fem.fem_field *
    float Fem.fem_field
val mumag_cvode_time_integrator :
  ?petsc_name:string ->
  ?tuning_params:float * float * int * int ->
  ?prefix_dofs_m:string ->
  ?prefix_dofs_h:string ->
  ?field_J_grad_m:float Fem.fem_field * float Fem.fem_field ->
  llg_material array ->
  (int array,float) Hashtbl.t ->
  string array ->
  (?target:float Fem.fem_field ->
   float array -> float Fem.fem_field -> float Fem.fem_field) ->
  float Fem.fem_field ->
  Sundials.cvode *
  (float array ->
   float ->
   int ->
   float * float * (string * float * float) array * float Fem.fem_field *
   float Fem.fem_field)
val independent_so3_scalars : int -> int array array
val vivify_multinomial_coefficient_vector :
  int array array -> float array -> float array -> float
val random_point_on_d_sphere :
  ?fun_rng:(float -> float) -> int -> float array
val reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients :
  ?fun_rng:(float -> float) ->
  ?iterations:int -> int array array -> (float array * float array) array
val taylor_coefficients_of_spherical_function :
  ?fun_rng:(float -> float) ->
  int -> (float array -> float) -> (float * int array) array
val spherical_function_and_derivative_ccode_from_coeffs :
  ?scalar_name:string ->
  ?deriv_name:string ->
  ?field_name:string ->
  ?derivative_coeff:float -> (float * int array) array -> string * string
val anisotropy_ccode_from_function :
  string -> int -> (float array -> float) -> string * string
val make_anisotropy_funs :
  llg_material array ->
  'a Fem.mesh_with_elements ->
  'a Fem.mesh_with_elements ->
  'a Fem.mesh_with_elements ->
  (?target:'a Fem.fem_field -> 'a Fem.fem_field -> 'a Fem.fem_field) *
  (?target:'a Fem.fem_field -> 'a Fem.fem_field -> 'a Fem.fem_field)
val magsim_brain :
  ?petsc_name:string ->
  ?lindholm_triangle_contributions:(?store_zeta:float array ->
                                    Mesh.coords ->
                                    float array ->
                                    float array -> float array -> float array) ->
  ?do_demag:bool ->
  ?fem_only:bool ->
  ?make_exchange_fun:(?petsc_name:string ->
                      ?sloppy_cofield_to_field:bool ->
                      ?name_exchange:string ->
                      llg_material array ->
                      float Fem.mesh_with_elements ->
                      float Fem.mesh_with_elements ->
                      (int array,float) Hashtbl.t *
                      (?target:float Fem.fem_field ->
                       float Fem.fem_field -> float Fem.fem_field)) ->
  ?make_demag_fun_3d:(
    ?petsc_name:string ->
		       ?lindholm_triangle_contributions:(?store_zeta:float array ->
							  Mesh.coords ->
							  float array ->
							  float array -> float array -> float array) ->
		       ?fun_make_and_register_mwe_field:(float Fem.mesh_with_elements ->
							   float Fem.mesh_with_elements *
							     float Fem.fem_field) ->
		       ?sloppy_cofield_to_field:bool ->
		       ?name_demag:string ->
		       ?name_rho:string ->
		       ?name_phi:string ->
		       ?diffop_div_m_string:string ->
		       ?order:int ->
		       ?inside_regions:Mesh.simplex_region list ->
		       ?fem_only:bool ->
		       llg_material array ->
		       float Fem.mesh_with_elements ->
		       float Fem.mesh_with_elements ->
		       ?compute_field_rho:bool ->
		       ?debug_charge_imbalance:bool ->
		       ?target:float Fem.fem_field -> float Fem.fem_field -> float Fem.fem_field
  ) ->
  ?extra_contribs_by_magnetization_name:(string * string) array ->
  ?energy_factor:float ->
  ?enable_spin_transfer_torque:bool ->
  ?tuning_params:float * float * int * int ->
  Mesh.mesh ->
  (llg_material * int) array array ->
  string array ->
  (string, float Fem.mesh_with_elements * float Fem.fem_field) Hashtbl.t *
  (?compute_field_rho:bool ->
   ?debug_charge_imbalance:bool ->
   ?target:float Fem.fem_field ->
   float array -> float Fem.fem_field -> float Fem.fem_field) *
  (float array -> unit) *
  ((Fem.dof_name -> float array -> float) ->
   (Fem.dof_name -> float array -> float) ->
   Sundials.cvode *
   (float array ->
    float ->
    int ->
    float * float * (string * float * float) array * float Fem.fem_field *
    float Fem.fem_field))
val vector_field_max_angle : 'a Fem.fem_field -> (string * float) array


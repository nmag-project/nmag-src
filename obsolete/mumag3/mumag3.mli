(* (C) 2007 Dr. Thomas Fischbacher *)

val mwe_surface_dof_mappings :
  Mesh.simplex_region list ->
  string * 'a -> 'b Fem.mesh_with_elements -> int array * int array
val mwe_dof_3d_surface_triangles_and_space_angles_and_coords :
  Mesh.simplex_region list ->
  'a Fem.mesh_with_elements ->
  'b * int array ->
  (int array * float array) array * float array * Mesh.coords array
val lindholm_triangle_contributions :
  ?store_zeta:float array ->
  float array -> float array -> float array -> float array -> float array
val lindholm_triangle_contributions_ng :
  outward_surface_normal:float array ->
  store_lh012:float array ->
  observer:Mesh.coords -> Mesh.coords -> Mesh.coords -> Mesh.coords -> unit
val lindholm_make_bdof_row :
  ?fun_add_contrib:(int -> float -> unit) ->
  ?observer_vertex_nr:int ->
  (int array * float array) array * float array * Mesh.coords array ->
  Mesh.coords -> unit
val bem_fill_block_of_rows :
  (int array * float array) array * float array * Mesh.coords array ->
  start_row:int -> end_row:int -> Mpi_petsc.matrix -> unit
val bem_matrix :
  fun_create_matrix:(int -> int -> 'a) ->
  fun_assemble_matrix:('a -> bool -> unit) ->
  fun_extract_petsc_matrix:('a -> Mpi_petsc.matrix) ->
  Mesh.simplex_region array -> string * 'b -> 'c Fem.mesh_with_elements -> 'a
val make_H_exch_linalg_machine :
  ?ccpla:(Nsim.nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  prefix:string ->
  ?pass_on_prematrix:((int array, float) Hashtbl.t -> unit) ->
  mwe_m:float Fem.mesh_with_elements ->
  mwe_H_exch:float Fem.mesh_with_elements ->
  (string * 'b * float) array -> Nsim.linalg_machine
val make_H_demag_linalg_machine :
  ?ccpla:(Nsim.nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  prefix:string ->
  mwe_m:float Fem.mesh_with_elements ->
  mwe_H_demag:float Fem.mesh_with_elements ->
  inside_regions:Mesh.simplex_region array ->
  ?fun_make_and_register_mwe_field:(float Fem.mesh_with_elements -> 'b) ->
  (string * float * 'c) array -> Nsim.linalg_machine
val make_ccpla_exchange_fun :
  ?ccpla:(Nsim.nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  ?petsc_name:string ->
  ?sloppy_cofield_to_field:bool ->
  ?name_exchange:string ->
  Mumag2.llg_material array ->
  float Fem.mesh_with_elements ->
  float Fem.mesh_with_elements ->
  (int array, float) Hashtbl.t *
  (?target:float Fem.fem_field -> float Fem.fem_field -> float Fem.fem_field)
val make_ccpla_demag_fun_3d :
  ?ccpla:(Nsim.nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  ?petsc_name:string ->
  ?lindholm_triangle_contributions:'b ->
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
  Mumag2.llg_material array ->
  float Fem.mesh_with_elements ->
  float Fem.mesh_with_elements ->
  ?compute_field_rho:bool ->
  ?debug_charge_imbalance:bool ->
  ?target:float Fem.fem_field -> float Fem.fem_field -> float Fem.fem_field
val global_shape_approximating_lattice_points_and_greyfactors :
  oversampling_steps:int array ->
  fun_global_shape:(float array -> float) ->
  v_lattice:float array array ->
  cell_rescaling:float -> (float array * float) array
val pbc_lindholm_triangle_contributions_ng :
  oversampling_steps:int array ->
  fun_global_shape:(float array -> float) ->
  v_lattice:float array array ->
  cell_rescaling:float ->
  outward_surface_normal:float array ->
  store_lh012:float array ->
  observer:float array -> Mesh.coords -> Mesh.coords -> Mesh.coords -> unit
val pbc_lindholm_triangle_contributions :
  oversampling_steps:int array ->
  fun_global_shape:(float array -> float) ->
  v_lattice:float array array ->
  cell_rescaling:float ->
  ?store_zeta:float array ->
  float array -> float array -> float array -> float array -> float array
val slp_lindholm_triangle_contributions :
  pts_lattice:float array array ->
  ?store_zeta:float array ->
  float array -> float array -> float array -> float array -> float array

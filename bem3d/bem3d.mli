external lindholm_triangle_contributions_C :
  float -> float array array -> unit = "caml_bem3d_raw_lindholm"
external lindholm_triangle_contributions_C_ld :
  float -> float array array -> unit = "caml_bem3d_raw_lindholm_longdouble"
external raw_lindholm_block_pbc :
  float ->
  float array * float array * float array * float array * float array array *
  float array array * float array * float array array * float array array ->
  unit = "caml_bem3d_raw_lindholm_block_pbc"
val mwe_dof_3d_surface_triangles_and_space_angles_and_coords :
  inside_property:string ->
  'a Fem.mesh_with_elements ->
  'b * int array ->
  (int array * float array) array * float array * Mesh.coords array
val lindholm_triangle_contributions :
  ?store_zeta:float array ->
  float array -> float array -> float array -> float array -> float array
val lindholm_triangle_contributions_ng :
  outward_surface_normal:float array ->
  store_lh012:float array ->
  observer:float array -> float array -> float array -> float array -> unit
val lindholm_make_bdof_row :
  ?high_precision:bool ->
  ?min_dist:float ->
  ?fun_add_contrib:(int -> float -> unit) ->
  ?observer_vertex_nr:int ->
  (int array * float array) array * float array * float array array ->
  float array -> unit
val bem_fill_block_of_rows :
  ?min_dist:float ->
  (int array * float array) array * float array * float array array ->
  start_row:int -> end_row:int -> Mpi_petsc.matrix -> unit
val bem_matrix :
  ?min_dist:float ->
  ?geom_info:(int array * Mesh.coords) array * float array *
             Mesh.coords array ->
  fun_create_matrix:(int -> int -> 'a) ->
  fun_assemble_matrix:('a -> bool -> unit) ->
  fun_extract_petsc_matrix:('a -> Mpi_petsc.matrix) ->
  ?field_name:string ->
  ?inside_property:string ->
  ?outside_property:string -> 'b * 'c -> 'd Fem.mesh_with_elements -> 'a
val bem_hmatrix :
  ?algorithm:int ->
  ?nfdeg:int ->
  ?nmin:int ->
  ?eta:float ->
  ?eps_aca:float ->
  ?eps:float ->
  ?p:int ->
  ?kmax:int ->
  ?geom_info:(int array * float array) array * float array *
             Mesh.coords array ->
  ?lattice_info: float array array array*(int * float * float array) array ->
  ?field_name:string ->
  ?inside_property:string ->
  ?outside_property:string ->
  'a * 'b ->
  'c Fem.mesh_with_elements ->
  Hlib.hmatrix * Mpi_petsc.vector * Mpi_petsc.vector
val apply_bem_hmatrix :
  Hlib.hmatrix * Mpi_petsc.vector * Mpi_petsc.vector ->
  Mpi_petsc.vector -> Mpi_petsc.vector -> unit
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
  observer:float array -> float array -> float array -> float array -> unit
val pbc_lindholm_triangle_contributions :
  oversampling_steps:int array ->
  fun_global_shape:(float array -> float) ->
  v_lattice:float array array ->
  cell_rescaling:float ->
  ?store_zeta:float array ->
  float array -> float array -> float array -> float array -> float array
val identity_3d : float array array
val nas_lindholm_all_contributions_by_triangle :
  ?min_dist:float ->
  ?ortho_transformation:float array array ->
  ?greyfactors:float array ->
  ?displacements:float array array ->
  observer_points:Mesh.coords array ->
  p012:Mesh.coords array ->
  outward_surface_normal:float array ->
  store_lh012:float array array -> unit -> unit
val nas_partially_fill_bem_matrix_row_constrained :
  ?min_dist:float ->
  ?nr_flushes:int ->
  ?geom_info:(int array * float array) array * float array *
             Mesh.coords array ->
  ?lattice_info:float array array array * (int * float * float array) array ->
  fun_add_contrib:(int -> int -> float -> unit) ->
  fun_assemble_matrix:(bool -> unit) ->
  ?rows_to_fill:int array ->
  ?field_name:string ->
  ?inside_property:string ->
  ?outside_property:string -> 'a * 'b -> 'c Fem.mesh_with_elements -> unit
val nas_partially_fill_bem_matrix_col_constrained :
  ?min_dist:float ->
  ?nr_flushes:int ->
  ?geom_info:(int array * float array) array * float array *
             Mesh.coords array ->
  ?lattice_info:float array array array * (int * float * float array) array ->
  fun_add_contrib:(int -> int -> float -> unit) ->
  fun_assemble_matrix:(bool -> unit) ->
  ?columns_to_fill:int array ->
  ?field_name:string ->
  ?inside_property:string ->
  ?outside_property:string -> 'a Fem.mesh_with_elements -> unit

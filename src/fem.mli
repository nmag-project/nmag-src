val version : unit -> string
val logmsg : Nlog.level_type -> string -> unit
val loginfo : string -> unit
val loginfo2 : string -> unit
val logdebug : string -> unit
type abssite = int array
type site = int array
type dof_mapping_long_to_short = int array
type dof_mapping_short_to_long = int array
type distribution_information = int array
type dof_region_spec = (string * bool * int option array) array
val print_dof_region_spec : (string * bool * int option array) array -> unit
type dof_region_logic = Ddiffop.dof_logic
type dvss = (string * dof_region_logic) array option
val dvss_to_string: dvss -> string
type dof_name = string * int array
type el_name = string
type dL_power = { dlp_nr_L : int; dlp_nr_dx : int; dlp_pow : int; }
type 'a femfun_summand = {
  ff_coefficient : 'a;
  ff_dL_powers : dL_power array;
  ff_L_powers : int array;
}
type 'a femfun = int * 'a femfun_summand array
val ht_all_femfuns_by_fem :
  (float femfun_summand array, float femfun) Hashtbl.t
val ht_all_femfun_products : (int * int, float femfun) Hashtbl.t
val ht_all_femfun_linsubs : (int * int * int, float femfun) Hashtbl.t
val ht_all_femfun_powers : (int * int, float femfun) Hashtbl.t
val ht_all_femfun_integrals : (int * int, float femfun) Hashtbl.t
val ht_all_femfun_surface_integrals :
  (int * int * int, int * int array * float femfun) Hashtbl.t
val ht_all_femfun_derivatives : (int * int, float femfun) Hashtbl.t
val registered_femfun : float femfun_summand array -> float femfun
type element_construction_plan =
    ECP_alien
  | ECP_empty
  | ECP_renamed
  | ECP_make_element of (string * dof_name * int * int)
  | ECP_fuse_elements of
      (string * element_construction_plan * element_construction_plan)
type 'a element = {
  el_name : el_name;
  el_subfields : dof_name array;
  el_dof_type_ranges : int array;
  el_dof_names : dof_name array;
  el_abssites : abssite array;
  el_abssite_index_by_dof_index : int array;
  el_dof_indices_by_abssite_index : int array array;
  el_dof_funs : 'a femfun array;
  el_made_by : element_construction_plan;
}
type dof = {
  dof_nr : int;
  dof_pos : Mesh.coords;
  dof_site : site;
  mutable dof_disappears_across : (Mesh.simplex * int) list;
  mutable dof_sx_nel_ix_fillcount : int;
  mutable dof_sx_nel_ix : int array;
  dof_in_body : Mesh.simplex_region list;
  mutable dof_volume : float;
}
val dof_sx_nel_ix_truncate : dof -> unit
val dof_sx_nel_ix_extend : ?extra:int -> dof -> unit
val dof_sx_nel_ix_add : dof -> Mesh.simplex -> int -> int -> unit
val dof_sx_nel_ix_iter : 'a array -> dof -> ('a -> int -> int -> 'b) -> unit
val dof_sx_nel_ix_find :
  'a array -> dof -> ('a -> int -> int -> bool) -> 'a * int * int
val dof_sx_nel_ix_reduce :
  'a array -> dof -> ('a -> int -> int -> 'b -> 'b) -> 'b -> 'b
val dof_name_to_string : string * int array -> string
type 'a mwe_made_by =
    MWEMB_make_mwe of (string *
       (int * string array) array * (* properties by region *)
       int array * (Mesh.simplex -> 'a element))
  | MWEMB_mwe_sibling of (string * string * (string * string) array * string)

type ssip_body = (site * int array * string * int) array
type hibernatable_sites_simplex_info_pmid =
    (unit -> ssip_body) * ssip_body option ref

type 'a mesh_with_elements = {
  mwe_mesh : Mesh.mesh;
  mwe_made_by : 'a mwe_made_by;
  mwe_name : string;
  mwe_elements : 'a element array;
  mwe_is_primary : bool;
  mwe_subfields : dof_name array;
  mwe_properties_by_region : (Mesh.simplex_region, string array) Hashtbl.t;
  mwe_sites: site array;
  mwe_pmids: int array;
  mwe_sites_simplex_info_pmid : hibernatable_sites_simplex_info_pmid;
  mwe_dofs : dof array;
  mwe_dof_periodic_images : int array array;
  mwe_dofs_by_site : (site, dof array) Hashtbl.t;
  mwe_nel_by_simplex_index : int array;
  mwe_dof_nrs_by_simplex_index : int array array;
  mwe_dof_neighbour_pairs : (int array * int array) array option ref;
  mwe_dof_funs_g : Mpi_petsc.matrix option ref;
  mwe_dof_funs_g_solver :
    (?output:Mpi_petsc.vector ->
     input:Mpi_petsc.vector -> unit -> Mpi_petsc.vector)
    option ref;
  mwe_dof_funs_g_entries : (int * int * float) array ref;
  mwe_dof_funs_volumes : float array option ref;
  mwe_distribution : int array;
  mwe_dofs_and_distribution_by_dvss :
    (dvss,
     dof_mapping_long_to_short * dof_mapping_short_to_long *
     distribution_information)
    Hashtbl.t;
}
val get_mwe_sites_simplex_info_pmid : 'a mesh_with_elements -> ssip_body
val slim_mwe_sites_simplex_info_pmid : 'a mesh_with_elements -> unit

type opt_field_restriction = dof_region_spec option
type 'a fem_field =
    FEM_field of 'a mesh_with_elements * dvss * Mpi_petsc.vector
type 'a fem_cofield =
    FEM_cofield of 'a mesh_with_elements * dvss * Mpi_petsc.vector
val the_dof_name : 'a mesh_with_elements -> dof -> dof_name
val the_dof_femfun :
  'a mesh_with_elements -> Mesh.simplex -> dof -> 'a femfun
val the_dof_machine : 'a mesh_with_elements -> dof -> int
val dof_belongs_to :
  'a mesh_with_elements -> dof -> Ddiffop.dof_logic -> bool
val dvss_constraint :
  'a mesh_with_elements ->
  dof -> (string * Ddiffop.dof_logic) array option -> bool
val mwe_shortvec_info :
  'a mesh_with_elements ->
  dvss ->
  dof_mapping_long_to_short * dof_mapping_short_to_long *
  distribution_information
val boundary_shortvec_info :
  string ->
  'a mesh_with_elements ->
  string ->
  dof_mapping_long_to_short * dof_mapping_short_to_long *
  distribution_information
val mwe_shortvec_selections_match :
  'a mesh_with_elements -> dvss -> dvss -> bool
val ensure_field_is_unrestricted : 'a fem_field -> unit
val ensure_cofield_is_unrestricted : 'a fem_cofield -> unit
val femfun_zero : float femfun
val make_bare_femfun : int -> int -> float femfun
val dL_as_femfun : int -> int -> int -> float femfun
val elements_are_compatible : 'a element -> 'a element -> bool
val make_element :
  ?element_name:el_name -> int -> string * int array -> int -> float element
val _element_rename_dofs :
  string -> (string * string) array -> 'a element -> 'a element
val empty_element : 'a element
val element_is_empty : 'a element -> bool
val fuse_elements : ?name:el_name -> 'a element -> 'a element -> 'a element
val element_to_string : 'a element -> string
val mwes_are_compatible :
  ?restr1:dvss ->
  ?restr2:dvss -> 'a mesh_with_elements -> 'a mesh_with_elements -> bool
val femfun_to_string : int * float femfun_summand array -> string
val mwe_to_string : 'a mesh_with_elements -> string
val print_mwe : Format.formatter -> 'a mesh_with_elements -> unit
val element_shape_function_power_integral_factor : int array -> float
val femfun_eval: Base.Ba.F.array2 ->
  'a * float femfun_summand array -> float array -> float
val fuse_dL_powers : dL_power array -> dL_power array -> dL_power array
val _femfun_from_hash :
  (int array * dL_power array, float) Hashtbl.t -> float femfun
val femfun_add :
  ?scale1:float ->
  ?scale2:float ->
  'a * float femfun_summand array ->
  'b * float femfun_summand array -> float femfun
val _raw_femfun_mult :
  float femfun_summand array -> float femfun_summand array -> float femfun
val femfun_mult :
  int * float femfun_summand array ->
  int * float femfun_summand array -> float femfun
val femfun_power : int -> float femfun -> int -> float femfun
val _raw_femfun_linsubs :
  int -> float femfun_summand array -> int -> float femfun -> float femfun
val femfun_linsubs :
  int ->
  int * float femfun_summand array -> int -> float femfun -> float femfun
val _raw_femfun_integrate : float femfun_summand array -> int -> float femfun
val femfun_integrate :
  int * float femfun_summand array -> int -> float femfun
(*
val femfun_numerical_integrator :
  (float array * float) array ->
  'a * float femfun_summand array ->
  Mesh.simplex -> (float array -> float) -> float*)
val _raw_femfun_integrate_over_surface :
  int ->
  int * float femfun_summand array -> int -> int * int array * float femfun
val femfun_integrate_over_surface :
  int ->
  int * float femfun_summand array -> int -> int * int array * float femfun
val _raw_femfun_diff_x : float femfun_summand array -> int -> float femfun
val femfun_diff_x : int * float femfun_summand array -> int -> float femfun
val femfun_integrate_over_simplex :
  Mesh.mesh -> 'a * float femfun_summand array -> Mesh.simplex -> float
val femfun_integrate_over_simplex_face :
  Mesh.mesh -> ?bare_integral:bool ->
  int * float femfun_summand array -> Mesh.simplex -> int -> float
val mwe_subfield_info :
  'a mesh_with_elements ->
  string -> (string * int array) * (site * Mesh.coords * dof array) array
val mwe_ensure_has_volumes : float mesh_with_elements -> unit
val fold_out_abssite : int array -> int array -> int array
val abssite_coords : int array -> Mesh.simplex -> float array
val copy_field : 'a fem_field -> 'a fem_field
val copy_cofield : 'a fem_cofield -> 'a fem_cofield
val copy_field_into : 'a fem_field -> 'a fem_field -> 'a fem_field
val copy_cofield_into : 'a fem_cofield -> 'a fem_cofield -> 'a fem_cofield
val make_field :
  ?name:string ->
  ?restriction:dvss ->
  ?constant_value:float -> 'a mesh_with_elements -> 'a fem_field
val make_cofield :
  ?name:string ->
  ?restriction:dvss ->
  ?constant_value:float -> 'a mesh_with_elements -> 'a fem_cofield
val field_axpby :
  'a fem_field ->
  float -> 'a fem_field -> float -> 'a fem_field -> 'a fem_field
val cofield_to_field :
  ?target:float fem_field ->
  ?box_method:bool -> float fem_cofield -> float fem_field
val field_to_cofield :
  ?target:float fem_cofield ->
  ?box_method:bool -> float fem_field -> float fem_cofield
val field_mwe : 'a fem_field -> 'a mesh_with_elements
val cofield_mwe : 'a fem_cofield -> 'a mesh_with_elements
val field_do_scale : 'a fem_field -> float -> unit
val cofield_do_scale : 'a fem_cofield -> float -> unit
val field_push : 'a fem_field -> 'a fem_field -> unit
val field_pull : 'a fem_field -> 'a fem_field -> unit
val cofield_push : 'a fem_cofield -> 'a fem_cofield -> unit
val cofield_pull : 'a fem_cofield -> 'a fem_cofield -> unit
val write_mwe_to_fh : ?handle:out_channel -> 'a -> 'b
val write_mwe : string -> 'a -> unit
val mwe_sibling :
  string ->
  string ->
  (string * string) array -> 'a mesh_with_elements -> 'a mesh_with_elements
val mwe_memstats: 'a mesh_with_elements -> (string * (float*float*float)) array
val mwe_memstats_str: (string * (float*float*float)) array -> string
val make_compatible_field : 'a fem_field -> 'a fem_field
val field_cofield_inner_product : 'a fem_field -> 'a fem_cofield -> float
val square_mesh : int -> int -> float -> float -> Mesh.mesh
val site_face_nrs : Mesh.simplex -> Mesh.point_id array -> int list
val mwe_dof_jumps :
  'a mesh_with_elements -> (int * int, string list) Hashtbl.t
val make_mwe :
  string ->
  ?debug:bool ->
  ?retain_simplex_info_pmid:bool ->
  ?fun_outer_region:(int -> float array -> int) ->
  ?properties_by_region:(int * string array) array ->
  (Mesh.simplex -> 'a element) -> Mesh.mesh -> 'a mesh_with_elements
val mwe_ensure_has_neighbour_pairs : 'a mesh_with_elements -> unit
val mwe_dof_connectivity : 'a mesh_with_elements -> string -> int array array
val mwe_matnullspace_vectors :
  fun_make_vector:(nr_vec:int -> 'a) ->
  fun_add_entry:('a -> int -> float -> 'b) ->
  fun_assemble_vector:('a -> 'c) ->
  'd mesh_with_elements -> string array -> 'a array
val equalize_periodic_field :
  'a fem_field -> 'a fem_field -> string array option -> unit
val integrate_field :
  ?dof_stem:string -> float fem_field -> (dof_name * float) array
val probe_field :
  float fem_field ->
  ?dof_stem:string -> Mesh.coords -> (dof_name * float) array
val build_probe_matrix :
  (int -> int -> 'a) ->
  ('a -> int -> int -> float -> unit) ->
  float fem_field -> string -> float array array -> 'a;;
val _plot_scalar_field_ps_header : string
val mesh2d_plot_scalar_field :
  ?scale:(Mesh.coords -> float array) ->
  ?plot_order:int ->
  ?plot_edges:bool ->
  dof_name -> 'a fem_field -> (float * float array) array -> string -> unit
val debugprint_field : 'a fem_field -> unit
val debugprint_cofield : 'a fem_cofield -> unit
val sample_field :
  ?petsc_name:string ->
  'a mesh_with_elements -> (dof_name -> dof -> float) -> 'a fem_field
val sample_field_type2 :
  ?petsc_name:string ->
  'a mesh_with_elements -> (dof_name -> Mesh.coords -> float) -> 'a fem_field
val set_field : 'a fem_field -> (dof_name -> dof -> float) -> unit
val set_field_uniformly: string -> float fem_field ->
  (float, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> bool
val set_subfield: float fem_field -> string ->
  (float, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit
(*external dipole_evaluator_sumpairs_fast_ccode :
  int * int * float -> float array -> float array -> float array -> unit
  = "caml_dipole_evaluator_sumpairs_fast_ccode"*)
val finer_mesh_from_coarse_mesh :
  int array array array ->
  Mesh.mesh ->
  Mesh.mesh * ((Mesh.point_id array * int array) * int) array * int array
val mesh_simplex_refinement : int -> int -> int array array array
val make_coarse_fine_mwe :
  ?petsc_name:string ->
  string * string ->
  (Mesh.simplex -> float element) ->
  Mesh.mesh * Mesh.mesh ->
  Mesh.simplex_id array ->
  float mesh_with_elements * float mesh_with_elements *
  (?petsc_name:string ->
   ?target:float fem_cofield -> float fem_field -> float fem_cofield) *
  (?petsc_name:string ->
   ?target:float fem_cofield -> float fem_field -> float fem_cofield)
external forall_fem_sites_do :
  Mpi_petsc.vector array ->
  (string * int array * float array) array ->
  float array -> Fastfields.c_funptr -> bool = "caml_forall_fem_sites_do"
val _site_wise_buffer_name : string
val _site_wise_posbuffer_name : string
val _site_wise_position_name : string
val _site_wise_site_pmid : string
val mesh_vertex_machine : Mesh.mesh -> int -> int
val site_wise_structure_and_offsets_and_defines :
  ?parallel:bool ->
  'a mesh_with_elements array ->
  'a mesh_with_elements array ->
  string array ->
  dof_name array * (string * int array * float array) array array *
  (string * string list * string) list
val site_wise_executor :
  (string * int array * float array) array ->
  (string * string list * string) list ->
  string ->
  float array -> Mpi_petsc.vector array -> Mpi_petsc.vector array -> bool
val site_wise_execute :
  ?strict_mwe_check:bool ->
  'a mesh_with_elements array ->
  'a mesh_with_elements array ->
  string array ->
  string -> float array -> 'a fem_field array -> 'a fem_cofield array -> bool
val ccode_for_tensor_norms : 'a mesh_with_elements -> string
val fun_tensor_norms :
  'a mesh_with_elements -> 'b fem_field -> (string * float * float) array
val mwe_simplex_id_to_elem_dof_nr_to_dof_nr :
  'a mesh_with_elements -> int array array
val ddiffop_from_string : string -> Ddiffop.ddiffop
val _dof_belongs_to_field :
  'a mesh_with_elements -> dof -> Ddiffop.field -> bool
val ddiffop_mxdim_index_mapping :
  'a mesh_with_elements ->
  Ddiffop.field array -> int array * int array * int array
type 'a vivificator =
    ?fun_make_matrix:(int -> int -> 'a) ->
    ?fun_finish_matrix:('a -> unit) ->
    ?field_mid:float fem_field ->
    ('a option -> int -> int -> float -> unit) -> 'a option
val ddiffop_vivified :
  ?nr_cpu:int ->
  Ddiffop.ddiffop ->
  ?mwe_mid:float mesh_with_elements ->
  float mesh_with_elements ->
  float mesh_with_elements ->
  ?fun_make_matrix:(int -> int -> 'a) ->
  ?fun_finish_matrix:('a -> unit) ->
  ?field_mid:'b fem_field ->
  ('a option -> int -> int -> float -> unit) -> 'a option
val make_sundials_mirage_dof_mapping :
  ?petsc_name:string ->
  ?subfield_restriction:string array ->
  'a mesh_with_elements -> Sundials_sp.mirage_dof_mapping option

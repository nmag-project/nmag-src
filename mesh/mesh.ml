(*
  (C) 2005, 2006 Dr. Thomas Fischbacher, Giuliano Bordignon,
   Dr. Hans Fangohr, SES, University of Southampton

   TODO NEXT:

   * Change the definition of fem_geo_boundaries so that it also allows
     back-references to objects meshed earlier.

   * We need a way to implement repelling boundaries, especially for PBC.
   (Actually, do we really?)


   We aim at a pretty rich, but opaque mesh data structure,
   for which we will provide a number of accessor/manipulator
   functions.

   Note that this is mostly a combination of the old mesh-generator
   and delaunay modules.

   XXX NOTE: inv-ext-point-coords should already contain all the
   information about hyperplanes, hence there presumably is no need to
   do all this "hyperplane through D points" stuff, and we can save a
   lot of effort, computation time, and code complexity!

   XXX NOTE: the point and simplex IDs of the "mesh" data structure
   are to be considered as internal. This is essential for dynamic
   insert/delete. This however means that we need a mapping of
   internal to external IDs and vice versa!

   XXX NOTE: when dynamically deleting points, how do we make sure we
   do not kill so many points that we no longer can triangulate?!?

   XXX NOTE: Another issue: we should introduce an extra factor
   between the typical length used to determine the number of points
   and the rod length scale. Rationale: In the final mesh, we want our
   points to be under some strain.

   NOTE: the density function provided must be continuous: it must not
   steeply fall to zero outside boundaries. This is necessary as it is
   used to determine proper local rod lengths - where one endpoint may
   lie "just a fuzzy little bit across a boundary". For the initial
   distribution of points, this continuous density function is mapped
   to a second one (internally) which is constrained to the region
   under investigation.


   Ad Mesh Generation

   Strategy: use qhull to get a notion of "nearest neighbours", and
   consider the dynamics of a truss structure of springs that show a
   certain resistance against being compressed.


   Some random thoughts on random, but important details:

   In higher dimensions (starting with 3), we face the problem that
   giving reasonable ranges for edge lengths may give quite bad
   elements. Suppose edge lengths are in the range [0.8 .. 1.2].

   What is the worst element quality a triangle with such edges can have?
   What is the worst element quality for a tetrahedron?

   We might want to introduce element-quality-dependent forces, but
   for this, we would have to have accurate delaunay triangulation
   topology information in every relevant step, which is costly to
   obtain.

   Can we do with approximative topology, i.e. retriangulate every few
   steps only? Only experiment can show.

   Why is this an important question? Depending on how we want to
   attack it, it does influence the intermediate data structures we
   should use.

   XXX Note that there are essentially two different heuristics in use
   that associate simplices to bodies. One just looks at the center of gravity,
   the other one pushes all points out a bit, tucks them in again, and uses information
   about which BCs the points satisfy or violate.

   As it seems, piecewise stepwise meshing does sometimes produce
   intermediate meshes that have simplices that are grossly
   mis-classified. Thinking about our methods, this should not
   be a problem though.

   XXX fun_elements_to_diffop and fun_elements_to_bdiffop may be
   mostly obsolete. Any reason why we still should carry these around?
   Our differential operators now directly implement the Galerkin
   method, and all the complexity goes into the naming of DOFs.



   XXX Note: there is no need to compute the face_eqns - that field
   indeed is mostly obsolete. The n'th face equation can be obtained
   as the n'th row of inv_ext_points_coords, up to normalization.

   We may want to keep it if we want to normalize face equations. As
   we do not do that, let us drop it and introduce a function
   simplex_face_eqn instead. Note that this means that all simplices
   have to be true simplices, and we cannot accomodate a NULL point -
   but we will eventually do without, even if we implement our own
   triangulator.

   Note that if this were CL, we could simply move simplex-face-eqn
   from being a defstruct member accessor to being a simple
   function. With OCaml, the syntax gets into our way.

 *)

open Snippets;;
open Correct_broken_mesh;;
open Base
open Base.Ba

let version () =
  String.concat ":" ["$Id$";
		      (Snippets.version());
		      (Qhull.version());
		      (Mt19937.version());]
;;

(* Removed the md5 hashing, this seems wrong (fangohr 13/02/2007) :
let version () = Snippets.md5
  (String.concat ":" ["$Id$";
		      (Snippets.version());
		      (Qhull.version());
		      (Mt19937.version());])
;;
*)

(* Get logger: *)

(* This is the logger with the handler from Python (relatively slow) *)
let meshlogger = Nlog.getLogger "nmesh.ocaml";;

(* let meshlogger = Nlog.getLogger "dev-mesh";; *)
let logdebug = meshlogger Nlog.Debug;;
let loginfo = meshlogger Nlog.Info;;
let loginfo2 = meshlogger Nlog.Info2;;
let logwarn = meshlogger Nlog.Warn;;

(* Create second (fast) logger which is only used by development team *)
let fastlog = Nlog.getLogger "dev-mesh";;
Nlog.setLogLevel "dev-mesh" Nlog.Error;;

let fastlogdebug = fastlog Nlog.Debug;;
let fastloginfo =  fastlog Nlog.Info;;
let fastlogwarn =  fastlog Nlog.Warn;;

(* These are options for Giuliano
let fastloghandler levelint msg =
  Printf.printf " %d: %s" levelint msg
in
  Nlog.setLogHandler "dev-mesh" fastloghandler;;

Nlog.setLogLevel " dev-mesh" Nlog.Debug;;
Nlog.setLogLevel " dev-mesh" (Nlog.Level 5);;
fastlog (Nlog.Level 7) "This is an extreme debug statement";;

*)




let reportmem tag =
  let (time, vmem, rss) = time_vmem_rss () in
    loginfo2 (* logdebug *)
      (Printf.sprintf "Memory report: T=%7.3f VMEM=%8.0f KB RSS=%8.0f KB %s"
                      time vmem rss tag)
;;


(*

Without logging -> 3 sec
With Python logging -> 17 sec
With Nlog logging -> 5.5 sec
With Null logging -> 4.6 sec
*)

(* --- begin fem geometry --- *)

type point_id = int;;
type simplex_id = int;;
type face_id = int array;;
type face_ix = int;;

(* For faces, an ID is a vector of point coords -
   we also introduce the "face index" as an enumerative integer,
   however.
 *)

type face_name = point_id array;;
type simplex_name = point_id array;;

type coords = float array;;

type coords_L = float array;;

type mesh_site_id = point_id array;;
type mesh_site_ix = int;;

type partitioning_type = int array;;
type dof_id = int;;

type field = float array;;

(* Note that we may want to specify extra fixed points as well at this level.
   Here, we do not.
 *)

(*

A mesh may consist of different "regions" - every simplex does belong
to a region, and it makes sense to ask questions such as "what are the
faces making up the interface between regions (bodies) P and Q in the
mesh?"

It is not entirely clear what the most appropriate data type is to
encode such region information. For now, we will just model this as
follows (XXX should be adjusted later on when we have more experience
how to do things): Bodies are labeled by integers, where 0 means
"outside any body", while (-1) means "such a simplex should not belong
to the mesh". Aside from those, bodies are labeled as 1,2,3,...

What is this 0/(-1) distinction good for (maybe we should provide
other variadic cases for those)?

* When creating a mesh of an object with holes in it, the "inside" of
a hole belongs to the convex hull, but not to the body to be meshed.

It is important to have it in the mesh, but one must disregard all
forces that come from rods "across" the hole. Hence, simplices in
holes are region-0 simplices.

* When meshing a box, some triangulators may resolve circumcircularity
ambiguities by jittering point coordinates a little. This will produce
very very flat artefact simplices at the boundaries that should be
filtered out. This is what region -1 is for.

A good reason NOT to go for a variadic design is that we then have to
make an additional marshalling decision when it comes to exporting
this interface to scripting languages. And it does not really hurt us
to use the (-1)/0 convention.

*)

type simplex_region =
  | Body_Nr of int
;;

type fem_geometry =
    {
     fem_geo_dim: int;
     fem_geo_bounding_box:(float array * float array);
     fem_geo_density: (float array -> float);
     fem_geo_boundaries:((simplex_region * (float array -> float)) array);
     (* XXX NOTE:
	We must extend this in such a way that a boundary
	may either be given as a boundary function,
	or as a back-reference to an object that
	was meshed earlier and is to be submitted to some
	transformation.

	XXX NOTE: maybe the piece_hints below can do this for us...
     *)
     fem_geo_piece_hints: ((float array array * int array array) array);      (* list of points and arrays of indices (defining the simplices) of the loaded mesh *)
     fem_geo_simplex_classificator: (float array array -> simplex_region);
   };;


let make_fem_geometry
    ?(bounding_box:((float array * float array) option))
    ?(density=(fun (_:(float array)) -> 1.0))
    ?(simplex_classificator=(fun _ -> Body_Nr 1))
    ?(boundaries=([||]:((simplex_region * (float array -> float)) array)))
    ?(hints= ([||]:(float array array * int array array) array))
    ()
    =
  let (corner_nw,corner_se) as bb =
    match bounding_box with
    | None -> failwith "Must specify an outer bounding box!"
    | Some x -> x
  in
  let the_dim = Array.length corner_nw in
  let () =
    (if Array.length corner_se <> the_dim
    then failwith "Bounding box corners must have same dimension!"
    else ()
	(* Note that there is no point in also requiring that
	   corner_nw be "coordinate-wise smaller" than corner_se
	 *)
    )
  in
  {fem_geo_dim=the_dim;
   fem_geo_bounding_box=bb;
   fem_geo_density=density;
   fem_geo_boundaries=boundaries;
   fem_geo_piece_hints= hints;
   fem_geo_simplex_classificator=simplex_classificator;
 }
;;

(* --- end fem geometry --- *)


(*
   We want to be able to deal with meshes composed from sub-meshes of
   individual pieces - e.g. a sphere within a shell. Evidently, we
   want to label the individual pieces of a mesh.

   The simplest way to do so which should already take us quite far is
   to just use numerical labels, but it may be desirable to change
   this to other types of labels in the future. Furthermore, we should
   perhaps trade some memory and speed efficiency better type checking
   here:

 *)

(* Purely internal data structures
   XXX No, actually not. Meshphysics will want to reference them!
   We should at least declare them as "not for public use". Better yet,
   provide a clear outside interface which will be used by meshphysics as well!
 *)


type point =
    {
     mp_coords: float array;
     mutable mp_id: point_id;
     mutable mp_belongs_to: simplex array;
     mutable mp_in_body: simplex_region list;
   }
and simplex =
    {
     mutable ms_id: simplex_id;
     (* Note: when we have brought a mesh to "normal form" - i.e. everywhere
	except deeply within some functions that construct and adjust meshes -
	the simplex id will just equal the simplex' index into the simplices
	array.
      *)
     ms_points: point array;
     (*
	XXX just changed this to have simplices directly reference
	their points. The documentation does not yet reflect this.
	Note: This approach has turned out to be necessary,
	and indirection-through-indices impractical, as it otherwise
	would be overly difficult to answer the question
	"what points does a simplex belong to" satisfactorily.
      *)
     ms_neighbours: simplex option array;
     ms_neighbour_backrefs: int array;
     (* If we cross a face and enter a neighbour, we have to
	know the number of the face from the perspective
	of the neighbour that leads back to us.
      *)
     ms_face_ids: face_id array;
     (* A face id is the ordered vector of all point ids. *)
     mutable ms_in_body: simplex_region;
     (* Defaults to Body_Nr (-2), will provide information about
	what mesh piece a simplex belongs to.

	Note: Body_Nr (-2) will mean ERROR: simplex is not/cannot be
	attributed to a body. Reasons may be:

	- Some of the vertices are in the bulk of different bodies.
	- All vertices are on a boundary between the same bodies.
      *)
   }
;;

let simplex_face_eqn (sd, sx_nr) n =
  let mx = Simplex.get_inv_point_matrix sd sx_nr in
  let _, dd = F.dim2 mx
  in Array.init dd (fun i -> (F.get2 mx n i))
;;

type la_functions =
    {
      la_det: float array array -> float;
      la_det_and_inv: float array array -> (float * float array array);
      la_hyperplane_eqn: float array array -> float array;
      la_line_point: float array -> float array -> float -> float array;
      la_simplex_circumcircle_midpoint_radius:
	  float array array -> float array * float;
      la_simplex_incircle_midpoint_radius:
	  float array array -> float array * float;
   }
;;

let make_la_functions dim =
  {
   la_det = determinant dim;
   la_det_and_inv = det_and_inv dim;
   la_hyperplane_eqn = hyperplane_eqn dim;
   la_line_point = line_point_sr (Array.make dim 0.0);
   la_simplex_circumcircle_midpoint_radius=simplex_circumcircle_midpoint_radius dim;
   la_simplex_incircle_midpoint_radius=simplex_incircle_midpoint_radius dim;
 }
;;

type mesh =
    {
      mm_dim: int;
      mutable mm_points: point array;

      mm_la: la_functions;
      mm_make_find_visible: (mesh -> float array -> (simplex * int));
      mutable mm_fem_geometry: fem_geometry option;
	(* Note: the slot above is required, as a mesh should know about its geometry
	   e.g. to answer questions such as "what boundary condition is that given point on",
	   but we make this mutable so that we can zero it out. This way, we can avoid
	   closure serialization issues.
	 *)
      mutable mm_boundaries:
	((simplex_region * simplex_region)*((simplex_id*face_ix) array)) array;
      (* A vector of (simplex_id,nr_face) per combination of regions. *)
      mutable mm_origins: (coords * simplex) array;
      mutable mm_simplices: simplex array;
      mutable mm_mesh0: Mesh0.t;
      mutable mm_simplex_data: Simplex.t;
      mutable mm_region_volumes: float array;
      mutable mm_vertex_distribution: int array; (* how many nodes go to what machine? *)
      (* NOTE: we still have to deal with removal of points and simplices.
	 The way we handle this is to keep a list of all point_ids/simplex_ids
	 which correspond to entries in mm_points/mm_simplices which actually
	 are not used in the current mesh, and hence may be recycled.

	 NOT USED YET - will be used when we think about dynamic mesh adjustment.
      *)
      mutable mm_periodic_points: int array array;   (* array where each entry is the array of point indices which are periodical identical *)
      mutable mm_have_connectivity: bool;
      mutable mm_have_incircle_circumcircle: bool;
      mutable mm_have_regions: bool;
    }
;;

let dummy_mesh =
  {
     mm_dim = 2;
     mm_la = make_la_functions 2;
     mm_make_find_visible = (fun _ -> impossible ());
     mm_fem_geometry = None;
     mm_boundaries = [||];
     mm_origins = [||];
     mm_points = [||];
     mm_simplices = [||];
     mm_mesh0 = Mesh0.dummy;
     mm_simplex_data = Simplex.dummy;
     mm_region_volumes = [||];
     mm_vertex_distribution = [||];
     mm_periodic_points = [||];
     mm_have_connectivity = false;
     mm_have_incircle_circumcircle = false;
     mm_have_regions = false;
    };;


(* TRANSITION FUNCTION *)
let mesh0_from_mesh mesh_points mesh_simplices =
  let point_array = Array.map (fun mp -> mp.mp_coords) mesh_points in
  let region_and_simplex_array =
    Array.map
      (fun ms ->
         let Body_Nr region_nr = ms.ms_in_body
         in (region_nr, Array.map (fun mp -> mp.mp_id) ms.ms_points))
      mesh_simplices
  in
    Mesh0.init_from_arrays point_array region_and_simplex_array
;;

(* DDD NOTE: this is to repair a deeper problem.
   Note that our approach here is not particularly efficient.
   Should re-write simplex_surface_1form_component properly
   sometime later on when we take another close look on the
   meshing module...
*)
let _simplex_surface_orientation_is_outward mesh sx nr_face =
  let mx = Simplex.get_inv_point_matrix mesh.mm_simplex_data sx.ms_id in
  let det = Simplex.get_point_matrix_det mesh.mm_simplex_data sx.ms_id in
  let points = sx.ms_points in
  let dim = Array.length points.(0).mp_coords in
  let coords_opposing_point = points.(nr_face).mp_coords in
  let coords_face_point = points.(if nr_face=0 then 1 else 0).mp_coords in
  let c_op = coords_opposing_point in
  let c_fp = coords_face_point in
  let rec walk n so_far =
    if n=dim then so_far
    else walk (n+1) (so_far+.(c_op.(n)-.c_fp.(n))*.det*.(F.get2 mx nr_face n))
  in
    (walk 0 0.0)<0.0
;;

let simplex_surface_1form_component mesh sx_nr nr_face nr_component =
  (* The "outward-pointing" surface 1-form
     is just given by the first dim components
     of sx.ms_inv_ext_point_coords, multiplied
     with the determinant of the extended point
     coordinates. So:
  *)
  let sx = mesh.mm_simplices.(sx_nr) in
  let mx = Simplex.get_inv_point_matrix mesh.mm_simplex_data sx_nr in
  let d = Simplex.get_point_matrix_det mesh.mm_simplex_data sx_nr in
  let z = d *. (F.get2 mx nr_face nr_component) in
    if _simplex_surface_orientation_is_outward mesh sx nr_face then z else -.z
;;












(*
let reordered_mesh mesh ix_new_by_ix_old =
  let () = Printf.fprintf stderr "XXX NOTE: the reordered_mesh function has not been tested toroughly yet! Tests: (a) re-ordering under involution should give the same mesh back, (b) serialization of re-ordered mesh should be exactly as long as that of original mesh.\n" in
  let nr_points = Array.length mesh.mm_points in
  let () =
    (if nr_points <> Array.length ix_new_by_ix_old
     then failwith "Fatal: mesh point-index re-ordering vector of wrong length!"
     else ())
  in
  let ix_old_by_ix_new =
    let v = Array.make nr_points 0 in
    let () = Array.iteri (fun ix_old ix_new -> v.(ix_new) <- ix_old) ix_new_by_ix_old
    in v
  in
  let old_to_new n = ix_new_by_ix_old.(n) in
  let new_periodicity =
    Array.init
      (Array.length mesh.mm_periodic_points)
      (fun ix_new ->
	 let ix_old = ix_old_by_ix_new.(ix_new) in
	   Array.map old_to_new mesh.mm_periodic_points.(ix_old))
  in
  let a0 = [||] in
  let new_points =
    Array.init nr_points
      (fun n ->
	 let pt_old = mesh.mm_points.(old_to_new n) in
	   {mp_coords=pt_old.mp_coords;
	    mp_id=n;
	    mp_belongs_to=a0;
	    mp_in_body=pt_old.mp_in_body;
	   })
  in
  let new_simplices =
    Array.mapi
      (fun nr_sx old_sx ->
	 {ms_id=nr_sx;
	  ms_points=Array.map (fun pt_old -> new_points.(old_to_new pt_old.mp_id)) old_sx.ms_points;
	  ms_neighbours=Array.map identity old_sx.ms_neighbours;
	  ms_neighbour_backrefs=old_sx.ms_neighbour_backrefs;
	  ms_face_ids=old_sx.ms_face_ids;
	  ms_in_body=old_sx.ms_in_body;
	 })
      mesh.mm_simplices
  in
    (* Simplex neighbour array still points to the old simplices. Replace by new ones: *)
  let () =
    Array.iter
      (fun sx ->
	 let v_neighbours = sx.ms_neighbours in
	   for i=0 to Array.length v_neighbours-1 do
	     match v_neighbours.(i) with
	       | None -> ()
	       | Some old_sx_neighbour ->
		   v_neighbours.(i) <- Some new_simplices.(old_sx_neighbour.ms_id)
	   done)
      new_simplices
  in
    (* Also, make sure new points know what simplices they belong to: *)
  let () =
    Array.iter
      (fun pt_old ->
	 let pt_new = new_points.(old_to_new pt_old.mp_id) in
	   pt_new.mp_belongs_to <-
	     Array.map
	     (fun sx_old -> new_simplices.(sx_old.ms_id))
	     pt_old.mp_belongs_to
      ) mesh.mm_points
  in
  let mesh0 = mesh0_from_mesh new_points new_simplices in
  let simplex_data = Simplex.init mesh0
  in
    (* Make the new mesh... *)
    {
      mm_dim=mesh.mm_dim;
      mm_la=mesh.mm_la;
      mm_make_find_visible=mesh.mm_make_find_visible;
      mm_fem_geometry=mesh.mm_fem_geometry;
      mm_boundaries=mesh.mm_boundaries;
      mm_origins=mesh.mm_origins;
      mm_points=new_points;
      mm_simplices=new_simplices;
      mm_mesh0 = mesh0;
      mm_simplex_data = simplex_data;
      mm_region_volumes=mesh.mm_region_volumes;
      mm_vertex_distribution=mesh.mm_vertex_distribution; (* may no longer make sense! *)
      mm_periodic_points=new_periodicity;
      mm_have_connectivity=mesh.mm_have_connectivity;
      mm_have_incircle_circumcircle=mesh.mm_have_incircle_circumcircle;
      mm_have_regions=mesh.mm_have_regions;
    }
;;
*)

let mesh_boundaries mesh =
  let body_outer_space = Body_Nr (-1) in
  let dim = mesh.mm_dim in
  let nr_vertices = 1+dim in
  let simplices = mesh.mm_simplices in
  let nr_simplices = Array.length simplices in
  let ht_boundaries = Hashtbl.create 100 in
  let rec walk_simplices nr_sx =
    if nr_sx=nr_simplices
    then ()
    else
      let sx = simplices.(nr_sx) in
      let body = sx.ms_in_body in
      let rec walk_vertices nr_v =
	if nr_v = nr_vertices
	then walk_simplices (1+nr_sx)
	else
	  let body_n =
	    match sx.ms_neighbours.(nr_v) with
	      | None ->  body_outer_space
	      | Some sx_n -> sx_n.ms_in_body
	  in
	  let () =
	    (if body_n <> body then
	       hashtbl_push ht_boundaries (body,body_n) (nr_sx,nr_v)
	     else ())
	  in walk_vertices (1+nr_v)
      in walk_vertices 0
  in
  let () = walk_simplices 0 in
  ht_boundaries
    (* Not:
       map_hashtbl_to_array
       ~sorter:(fun (k1,_) (k2,_) -> compare k1 k2)
       (fun k v -> (k,Array.of_list (List.rev v)))
       ht_boundaries
     *)
;;


(* Mesh Generator Data Types *)

type point_state = Immobile | Pinned | Mobile | Boundary
;;

type point_fate =
    Do_Nothing_With_Point | Add_Another_Point | Delete_Point
;;

(* Note: the entries here are mutable so that the relaxation function
   may re-cycle the structure.
 *)
type meshgen_controller_input =
    {
     mutable mci_bound_pt_between_immobiles: bool;
     mutable mci_dimension: int;
     mutable mci_size_last_time_step: float;
     mutable mci_max_node_rel_movement_since_last_triangulation: float;
     mutable mci_largest_rel_movement_in_mesh: float;
     mutable mci_largest_effective_force_in_mesh: float;
     (* Effective forces are just the real forces for interior nodes.
	For nodes close to the boundary, the part
	parallel to the gradient is projected out.
      *)
     mutable mci_node_avg_density: (float * float) array;
   }
;;

type meshgen_controller_command =
  | Force_Equilibrium_Reached of float
  | Step_Limit_Reached of int
  | Pin_Unpin_Nodes of point_state array
  | Determine_Forces of int (* This is done implicitly as well with all the commands below;
			       all these options pass around the current iteration number*)
  | Retriangulate of int
  | Change_points_and_retriangulate of (point_fate array * int)
  | Time_step of (float * int)
;;

(*
   This is a means to configure things we usually do not want to
   bother with, but are glad if we can tweak them when we really
   have to.

   All those fields are mutable so we may just copy the structure and
   adjust one to get a new, adjusted variant.
*)

type 'state mesher_defaults =
    {
     mutable mdefault_controller_initial_points_volume_ratio: float;                        (* functions defined but not used (constraints on pyfem) *)
     mutable mdefault_controller_splitting_connection_ratio: float;                         (* | *)
     mutable mdefault_controller_exp_neigh_force_scale: float;                              (* __ *)

     mutable mdefault_nr_probes_for_determining_volume: int;
     mutable mdefault_boundary_condition_debuglevel: int;
     mutable mdefault_boundary_condition_acceptable_fuzz: float;
     mutable mdefault_boundary_condition_max_nr_correction_steps: int;
     (* - *)
     (* Heuristically determine whether a simplex fulfills is "outside":
	(NOTE: first parameter is scratchpad_simplex point coords array
	large enough to hold the vertices of a simplex plus one more.)
      *)
     mutable mdefault_make_boundary_condition_simplex_classificator:
       int -> (* dim *)
      ((float array -> float) array) -> (* boundary conditions *)
       float ->                         (* smallest_allowed_volume_ratio*)
       float array array -> (* simplex nodes coordinates *)
	point_state array -> (* points states *)
	     simplex_region;
     (* -- *)
     mutable mdefault_controller_movement_max_freedom : float;
     mutable mdefault_controller_topology_threshold: float;
     mutable mdefault_controller_step_limit_min: int;
     mutable mdefault_controller_step_limit_max: int;
     mutable mdefault_controller_max_time_step: float;
     mutable mdefault_controller_time_step_scale: float;
     mutable mdefault_controller_tolerated_rel_movement: float;

     (*KKK*)
     mutable mdefault_controller_shape_force_scale: float;
     mutable mdefault_controller_volume_force_scale: float;
     mutable mdefault_controller_neigh_force_scale: float;
     mutable mdefault_controller_irrel_elem_force_scale: float;

     mutable mdefault_controller_thresh_add: float;
     mutable mdefault_controller_thresh_del: float;
     mutable mdefault_initial_relaxation_weight:  (int -> int -> float -> float -> float);
     mutable mdefault_controller_initial_settling_steps: int;
     mutable mdefault_controller_sliver_correction: float;
     mutable mdefault_controller_smallest_allowed_volume_ratio: float;

     mutable mdefault_controller_handle_point_density_fun:
       (Mt19937.rng -> (float * float) -> float -> float -> point_fate);
     (* XXX some of the relaxation tweaking parameters will have to become
	parameters of the controller. The names should reflect that!
      *)
     mutable mdefault_relaxation_debuglevel: int;
     (* After a point moved by that much, we re-triangulate. *)
     mutable mdefault_relaxation_force_fun: (float -> float);
     mutable mdefault_boundary_node_force_fun: (float -> float);
     mutable mdefault_meshgen_controller:
       ('state mesher_defaults -> Mt19937.rng -> 'state -> meshgen_controller_input ->
	 ('state * meshgen_controller_command));
     mutable mdefault_meshgen_controller_initial: 'state;
   }
;;

(* Debugging control *)

let opt_debuglevel_mesher = ref 0;;


let make_find_visible _ =
  not_implemented_yet "make_find_visible"
;;

(* Formatters *)

let print_point fmt pt =
  Format.fprintf fmt "#<mesh point #%d at %s>" pt.mp_id (float_array_to_string pt.mp_coords)
;;

let print_simplex fmt sx =
  Format.fprintf fmt "#<mesh simplex #%d, containing points %s>"
    sx.ms_id
    (int_array_to_string (Array.map (fun p -> p.mp_id) sx.ms_points))
;;

let print_mesh fmt mesh =
  Format.fprintf fmt "#<mesh, %d points, %d simplices>"
    (Array.length mesh.mm_points)
    (Array.length mesh.mm_simplices)
;;

(* Auxiliary functions *)

let float_array_min arr =
  (* Note: does not work for length-0 arrays! *)
  let len = Array.length arr in
  let rec walk now pos =
    if pos=len then now
    else
      let here = arr.(pos) in
      walk (if here < now then here else now) (1+pos)
  in walk arr.(0) 1
;;


let reduce_array arr =                                             (* function to create an array taking each entry of the  *)
  let len = Array.length arr in                                    (* given array only once (in case they appear more than once) *)
  let rec add_el ix elem_sf =
    if ix = len
    then Array.of_list elem_sf
    else
      let new_elem = arr.(ix) in
      if List.mem new_elem elem_sf (* already in the list *)
      then add_el (1+ix) elem_sf
      else add_el (1+ix) (new_elem::elem_sf)
  in
  add_el 0 []

;;

let register_point ht key =                                         (* function to register a point in a hashtable *)
  try
    Hashtbl.find ht key
  with
    | Not_found ->
	let new_ix = Hashtbl.length ht in
	let () = Hashtbl.add ht key new_ix
	in new_ix
;;

let array_filteri p arr =                                           (* function that filters the entries of an array running also on their indices *)
  let pre_result = Array.copy arr in
  let len = Array.length arr in
  let rec walk nr_good pos_src =
    if pos_src = len
    then Array.sub pre_result 0 nr_good
    else
      if p pos_src arr.(pos_src)
      then
	let () = pre_result.(nr_good) <- arr.(pos_src) in
	  walk (1+ nr_good) (1+ pos_src)
      else
	walk nr_good (1+ pos_src)
  in walk 0 0
;;

(*let is_obj_nan x =
  Obj.tag (Obj.repr x) = Obj.double_tag &&
  (let f = (Obj.magic x : float) in not (f = f))
;;
*)

let report_mesh_quality bins mesh =
  let dim = mesh.mm_dim in
  let quality = Array.init bins (fun i -> 0) in
  let intvl = 1.0 /. (float_of_int bins) in
  let nr_points = Array.length (mesh.mm_points) in
  let nr_simplices = Array.length (mesh.mm_simplices) in
  let get_ic_r = Simplex.get_incircle_radius mesh.mm_simplex_data in
  let get_cc_r = Simplex.get_circumcircle_radius mesh.mm_simplex_data in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let () = forall_simplices
    (fun sx ->
	let ic_r = get_ic_r sx.ms_id in
	let cc_r = get_cc_r sx.ms_id in
	let reduced_ratio = ic_r/.cc_r in
	let ratio = (float_of_int dim)*.reduced_ratio in
	let scaled_ratio = int_of_float ((float_of_int bins) *. ratio) in
	let final_ratio = min (bins-1) scaled_ratio in
	  quality.(final_ratio) <- quality.(final_ratio)+1
      )
    in
    let () = loginfo (Printf.sprintf "\tMesh quality (%d points, %d simplices): " nr_points nr_simplices) in
      for i = 0 to bins-1 do
	loginfo (Printf.sprintf "%6d simplices (%5.1f %%) with quality in range [%3.2f - %3.2f] %!" quality.(i)
		    (100.*.float_of_int(quality.(i))/.(float_of_int nr_simplices))
		    (intvl*.(float_of_int(i)))
		    (intvl*.(float_of_int(i+1))) )
      done
;;


let array_filter_double p arr =                                                                 (* this function splits the entry of an array in two sets, the *)
  let pre_result = Array.copy arr in                                                            (* ones satisfying the condition and the ones which don't, returning *)
  let len = Array.length arr in                                                                 (* both arrays. *)
  let rec walk nr_good nr_bad pos_src =
    if pos_src = len
    then (Array.sub pre_result 0 nr_good, Array.sub pre_result nr_good nr_bad)
    else
      if p arr.(pos_src)
    then
      let () = pre_result.(nr_good) <- arr.(pos_src) in
	walk (1+ nr_good) nr_bad (1+ pos_src)
    else
      let () = pre_result.(len - 1 - nr_bad) <- arr.(pos_src) in
	walk nr_good (1+nr_bad) (1+ pos_src)
  in walk 0 0 0
;;

let norm vec =                                                                                  (* norm of a vector *)
  sqrt(Array.fold_left (fun x y -> x +. y**2. ) 0.0 vec)
;;

let unit_vector vec =                                                                           (* unit vector from a given vector  *)
  let vecnorm = norm vec in
  if vecnorm = 0.
  then failwith "Can't normalise vector with length 0"
  else Array.map (fun e -> e/.vecnorm) vec
;;

let scalar_prod arr1 arr2 =                                                                     (* deprecated: substitute by the function snippets/scalar_product *)
  Array.fold_left (+.) 0.0 (array_pointwise ( *. ) arr1 arr2)
;;

let mx_sum m1 m2 =                                                                              (* entry-wise sum of two matrix with the same dimensions *)
  let rows_m1 = Array.length m1 in
  let cols_m1 = Array.length m1.(0) in
  let new_m =  Array.make_matrix rows_m1 cols_m1 0.0 in
  for i = 0 to rows_m1-1 do
    for j = 0 to cols_m1-1 do
      new_m.(i).(j) <- m1.(i).(j) +. m2.(i).(j)
    done
  done;
  new_m
;;

let outerproduct m =                                                                            (* outer product of two matrix *)
  let mat_dim = Array.length m.(0) in
  let new_m = ref (Array.make_matrix mat_dim mat_dim 0.0) in
  for i=0 to (Array.length m)-1 do
    let pt = [|m.(i)|]  in
    new_m := mx_sum !new_m (mx_mult (mx_transpose pt) pt)
  done;
  !new_m
;;

let center_mass_component pos arr =                                                             (* center of mass of a set of points *)
  let nr_pts = Array.length arr in
  let mean = ref 0.0 in
  let () =
    for i=0 to (nr_pts-1) do
      mean := !mean +. arr.(i).(pos)
    done
  in
  (!mean)/.(float_of_int nr_pts)
;;

let ideal_vol_sx side_len dim =                                                                 (* ideal volume of a simplex given the dimension of the space and *)
  let n = float_of_int dim in                                                                   (* the length of the side *)
  sqrt(n+.1.)/.((float_factorial dim)*.sqrt(2.**n)) *. (side_len**n)
;;

let shift_list_on_the_left lst =                                                                (* given a list, it cycles the entries on the left *)
  let dim = List.length lst in
  let rec fill_list ix list_so_far =
    if ix = dim
    then List.rev list_so_far
    else
      let add_elem =
	if ix = dim-1
	then List.nth lst 0
	else List.nth lst (1+ix)
      in
      fill_list (1+ix) (add_elem::list_so_far)
  in
  fill_list 0 []
;;

let shift_array_on_the_left arr =                                                               (* given an array, it shift the entries on the left *)
  let dim = Array.length arr in
  let new_arr = Array.init dim
      (fun i ->
	if i = dim-1 then arr.(0)
	else arr.(i+1))
  in
  new_arr
;;

(* function to compute the volume for each of the objects in the mesh *)
let _region_volumes simplex_data simplices =
  let get_det = Simplex.get_point_matrix_det simplex_data in
  let max_region =
    Array.fold_left
      (fun sf sx -> let Body_Nr r = sx.ms_in_body in max sf r) 0 simplices
  in
  let volumes = Array.make (1+max_region) 0.0 in
  let dim = Array.length simplices.(0).ms_points.(0).mp_coords in
  let () = array_foreach_do simplices
    (fun sx ->
      let Body_Nr r = sx.ms_in_body in
      let vol = abs_float(get_det sx.ms_id)/.(float_factorial dim) in
	volumes.(r) <- volumes.(r) +. vol)
  in volumes
;;

(* We need this function to be able to re-compute the volumes
from the Python side after scaling the node positions. *)
let mesh_plotinfo_regionvolumes mesh =
  mesh.mm_region_volumes;;


(*
   Note: when we mesh a region with holes in it, Delaunay
   triangulation will create triangles that cross holes.
   Clearly, we do not want that.

   Hence, we need to filter the simplices generated.

 *)

let nearest_neighbours dim =
  let nr_vertices = dim+1 in
  let scratch_points_simplex = Array.init nr_vertices (fun _ -> Array.make dim 0.0) in
    fun
      ?(triangulator=Qhull.delaunay)                                      (* we want to pass also the states of points to the triangulator *)
      ?(simplex_classificator = (fun _ _ _ -> Body_Nr 1))                                  (* so that we can remove most of the flat ones on the interface *)
      ?(fun_filter_region = (fun (Body_Nr x) -> x > 0))
      smallest_allowed_ratio
      points states ->

	let () = logdebug (Printf.sprintf "function: nearest_neighbours (%d points)%!" (Array.length points) )
	in
	let () = logdebug (Printf.sprintf " Points: ") ;
	  for i=0 to (Array.length points)-1 do
	    logdebug (Printf.sprintf "%4d: %s" i (float_array_to_string points.(i));)
	  done;
	in
	let fill_sps sx =                                                                 (* copy the vertices coordinates on a scratchpad *)
	  for i=0 to nr_vertices-1 do
	    for k=0 to dim-1 do
	      scratch_points_simplex.(i).(k) <- points.(sx.(i)).(k);
	    done;
	  done
	in

	let ht_pointstate = Hashtbl.create (Array.length points) in                        (* for each point we want to know the associated state *)
                                                                                           (* after the triangulation, so that the classificator *)
                                                                                           (* can use this information *)
	let () = logdebug (Printf.sprintf "\nnn-points - %d " (Array.length points)) in

	let () = for ix=0 to (Array.length points)-1 do
	    ignore(Hashtbl.add ht_pointstate ix states.(ix))
	  done
	in

	let triangulate_points = triangulator points in

	let sx_states_arr =                                                                (* associate the right state to each point of the new triangulation *)
	  Array.map (fun sx -> Array.map (fun x -> Hashtbl.find  ht_pointstate x) sx ) triangulate_points
	in


	let fun_filter_bad_region = (fun (Body_Nr x) -> x <= 0) in                         (* the simplices are kept if they belong to the *)
	                                                                                   (* region with Body_Nr 1 (given by the classificator) *)
	let all_simplices =
	  Array.mapi                                                                        (* classify the simplex obtained from the delaunay *)
	    (fun ix s ->                                                                    (* triangulation of the given set of points *)
	      let () = fill_sps s in
	      (simplex_classificator smallest_allowed_ratio scratch_points_simplex sx_states_arr.(ix), s)
	    )
	    triangulate_points in                                                           (* here we call the triangulator with also the state of the points *)

	let relevant_simplices =                                                            (* the relevant simplices are extracted from the list *)
	  array_filter (fun (region,_) -> fun_filter_region region) all_simplices           (* the default filter can be overidden! *)
	in

	let irrelevant_simplices =                                                           (* the simplices are classified only wrt the *)
	  array_filter (fun (region,_) -> fun_filter_bad_region region) all_simplices        (* region assigned by the classificator *)
	in
	  (Array.map (fun (region, pt_ixs) -> pt_ixs) relevant_simplices,                    (* return vertices of each simplex *)
	  Array.map (fun (region, pt_ixs) -> pt_ixs) irrelevant_simplices )
;;

let extract_topology_from_simplices relevant_simplices points =                               (* this function creates two lists of lists: *)
	let nr_points = Array.length points in                                                (* - each entry of the list, associated to the *)
	let nr_nn_by_point = Array.make nr_points 0 in                                        (* node with the corresponding index, stores a *)
	let nr_nn_by_point_both_ways = Array.make nr_points 0 in                              (* list of indices corresponding to its neighbours *)
                                                                                              (* each index appears only *once* in all the entire list *)
	let ht_nn = Hashtbl.create nr_points in                                               (* - each entry of the list, associated to the *)
	begin                                                                                 (* node with the corresponding index, stores a *)
	  Array.iter                                                                          (* of indices corresponding to its neighbours *)
	    (fun simplex ->                                                                   (* each node stores each of its neighbours, so that *)
	      do_for_any_two_of simplex                                                       (* each index appears as many times as the number *)
		(* XXX Note that we presumably do have ordering guarantees                    (* of its neighbours *)
		   from Qhull.delaunay and do_for_any_two_of_simplex.
		   Look this up, document it, and USE it here!
		 *)
		(fun x y ->
		  let mx = min x y
		  and my = max x y in
		  let xy = (mx,my) in                                                         (* ensure that we get the pair in lexi ordering! *)
		  if Hashtbl.mem ht_nn xy
		  then ()	                                                              (* Already made the entry for that pair *)
		  else
		    begin
		      Hashtbl.add ht_nn xy 1;
		      nr_nn_by_point.(mx) <- 1+nr_nn_by_point.(mx);
		      nr_nn_by_point_both_ways.(mx) <- 1+nr_nn_by_point_both_ways.(mx);
		      nr_nn_by_point_both_ways.(my) <- 1+nr_nn_by_point_both_ways.(my);
		    end
		))
	    relevant_simplices;
                                                                                             (* Now, we have the nearest-neighbours information *)
	                                                                                     (* in a hash, and furthermore we know how many *)
                                                                                             (* nearest neighbours every point has *)
                                                                                             (* So, we already can allocate the result array *)
											     (* with the proper shape *)
	  let nn_by_point = Array.init nr_points
	    (fun ix -> Array.make nr_nn_by_point.(ix) 0) in
	  let nn_by_point_both_ways = Array.init nr_points
	    (fun ix -> Array.make nr_nn_by_point_both_ways.(ix) 0) in
                                                                                             (* nr_nn_by_point is no longer needed now. *)
											     (* Hence, we recycle it to an array that tells *)
											     (* us for every point with number x the lowest *)
											     (* occupied entry in nn_by_point.(x). Note that *)
											     (* we fill our NN arrays backwards! ???? - GB - *)
	  let nn_lowest_occupied_ix_by_point = nr_nn_by_point in
	  let nn_lowest_occupied_ix_by_point_both_ways = nr_nn_by_point_both_ways in
	  begin
	    Hashtbl.iter
	      (fun (x,y) _ ->                                                                (* (key, value): we use only the key *)
		let ix_x = nn_lowest_occupied_ix_by_point.(x)-1
		in
		let ix_x_2 = nn_lowest_occupied_ix_by_point_both_ways.(x)-1                  (* CLEAR after some thought !!!!! - GB - *)
		and ix_y_2 = nn_lowest_occupied_ix_by_point_both_ways.(y)-1
		in
		begin
		  nn_lowest_occupied_ix_by_point.(x) <- ix_x;
		  nn_by_point.(x).(ix_x) <- y;

		  nn_lowest_occupied_ix_by_point_both_ways.(x) <- ix_x_2;
		  nn_lowest_occupied_ix_by_point_both_ways.(y) <- ix_y_2;
		  nn_by_point_both_ways.(x).(ix_x_2) <- y;
		  nn_by_point_both_ways.(y).(ix_y_2) <- x;
		end
	      )
	      ht_nn;
	    (nn_by_point, nn_by_point_both_ways);
	  end
	end
;;

(*
 Note that for mesh_from_points below, we especially want to be able to
 remove super-slim simplices which are numerical fuzz artefacts.
 Such ones can appear e.g. very close to the boundary of a box.

 One geometric quantity which we will want to know anyway, and which
 will be useful to detect those simplices with little effort is the
 set of center-of-gravity-to-corner vectors ("lines of gravity"). As
 we may later on want to compute different things on these, we
 introduce a function that wraps the bookkeeping.
 *)

let make_simplex_lines_of_gravity_applicator dim =                                (* function which takes the dimension of the space,  *)
										  (* the coordinates of a simplex and a function *)
  let nr_vertices = dim+1 in                                                      (* and applies the function with arguments centerOfMass, *)
  let ext_det_fun = determinant (dim+1) in                                        (* linesOfGravity and their length, and the *)
  let scratchpad_cog = Array.make dim 0.0 in                                      (* volume of the simplex *)
  let scratchpad_logs = Array.init nr_vertices (fun _ -> Array.copy scratchpad_cog) in
  let scratchpad_log_lens = Array.make nr_vertices 0.0 in
  (* It is also useful to have the lengths of those lines readily available. *)
  let scratchpad_ext_coords = Array.init nr_vertices (fun _ -> Array.make (dim+1) 1.0) in
  (* ... as well as the volume. For this, we need extended coordinates.
     Note that the code above already inits the extra coord to 1. *)
  let avg_weight = 1.0/. (float_of_int nr_vertices) in
  fun fun_do_what simplex_points_coords ->
    begin
      for k=0 to dim-1 do
	  scratchpad_ext_coords.(0).(k) <- simplex_points_coords.(0).(k);
	scratchpad_cog.(k) <- simplex_points_coords.(0).(k);
      done;
      for i=1 to nr_vertices-1 do
	for k=0 to dim-1 do
	  scratchpad_ext_coords.(i).(k) <- simplex_points_coords.(i).(k);
	  scratchpad_cog.(k) <- scratchpad_cog.(k) +. simplex_points_coords.(i).(k);
	done;
      done;
      for k=0 to dim-1 do
	scratchpad_cog.(k) <- avg_weight *. scratchpad_cog.(k);
      done;
      (* Now that we have the center of gravity, make the logs. *)
      for i=0 to nr_vertices-1 do
	for k=0 to dim-1 do
	  scratchpad_logs.(i).(k) <- simplex_points_coords.(i).(k) -. scratchpad_cog.(k);
	done;
	scratchpad_log_lens.(i) <- sqrt(euclidean_len_sq scratchpad_logs.(i));
      done;
      let vol = (ext_det_fun scratchpad_ext_coords)/.(float_factorial dim) in
      fun_do_what scratchpad_cog scratchpad_logs scratchpad_log_lens vol
    end
;;

(* For dealing with artefact simplices at box boundaries, this will
   do. Note that our approach is slightly unsafe: if the mesh was bad in
   the first place, we may kill random inner simplices.

   Maybe we should instead try to ensure that the filtered simplices
   all are in some sense close to a boundary. But maybe, a final
   post-processing consistency check step which tests if all the
   boundary points discovered really are close to some boundary would
   deal with most situations where this can become a problem (but
   unfortunately perhaps not all of them...)
 *)

let make_default_boundary_condition_simplex_classificator
    dim
    boundary_conditions
    smallest_allowed_ratio
    points states =
  let rec separate_points ix mobile_sf immobile_sf boundary_sf =
    if ix = dim+1
    then (mobile_sf, immobile_sf, boundary_sf)
    else
      match states.(ix) with
        | Mobile ->  separate_points (ix+1) (mobile_sf+1) immobile_sf boundary_sf
	| Immobile -> separate_points (ix+1) mobile_sf (immobile_sf+1) boundary_sf
	| Boundary -> separate_points (ix+1) mobile_sf immobile_sf (boundary_sf+1)
	| _  -> separate_points (ix+1) mobile_sf immobile_sf boundary_sf (* Pinned : not used *)
  in
  let nr_mob, nr_imm, nr_bou = separate_points 0 0 0 0 in

(*  let () = Printf.printf "mobiles: %d, immobiles: %d, boundary: %d %!"  nr_mob nr_imm nr_bou in
  (* useful for DDD but setting it as a log message costs in terms of mesh generation time *)
*)
  let (cog, logs, log_lens, vol) =

    let nr_vertices = dim+1 in
    let ext_det_fun = determinant (dim+1) in
    let scratchpad_cog = Array.make dim 0.0 in
    let scratchpad_logs = Array.init nr_vertices (fun _ -> Array.copy scratchpad_cog) in
    let scratchpad_log_lens = Array.make nr_vertices 0.0 in
      (* It is also useful to have the lengths of those lines readily available. *)
    let scratchpad_ext_coords = Array.init nr_vertices (fun _ -> Array.make (dim+1) 1.0) in
      (* ... as well as the volume. For this, we need extended coordinates.
	 Note that the code above already inits the extra coord to 1. *)
    let avg_weight = 1.0/. (float_of_int nr_vertices) in
    let () =
      for k=0 to dim-1 do
	scratchpad_ext_coords.(0).(k) <- points.(0).(k);
	scratchpad_cog.(k) <- points.(0).(k);
      done
    in
    let () =
      for i=1 to nr_vertices-1 do
	for k=0 to dim-1 do
	  scratchpad_ext_coords.(i).(k) <- points.(i).(k);
	  scratchpad_cog.(k) <- scratchpad_cog.(k) +. points.(i).(k);
	done
	done
    in
    let () =
      for k=0 to dim-1 do
	scratchpad_cog.(k) <- avg_weight *. scratchpad_cog.(k);
      done
    in
      (* Now that we have the center of gravity, make the logs. *)
    let () =
      for i=0 to nr_vertices-1 do
	for k=0 to dim-1 do
	  scratchpad_logs.(i).(k) <- points.(i).(k) -. scratchpad_cog.(k);
	done;
	scratchpad_log_lens.(i) <- sqrt(euclidean_len_sq scratchpad_logs.(i));
	done
    in
    let scratchpad_vol = (ext_det_fun scratchpad_ext_coords)/.(float_factorial dim) in
      (scratchpad_cog, scratchpad_logs, scratchpad_log_lens, scratchpad_vol)
  in
  let f_dim = float_of_int dim in
  let max_log_len = float_array_max log_lens in
  let expected_volume_order_of_magnitude = max_log_len**f_dim in

  let ratio = abs_float(vol /. expected_volume_order_of_magnitude) in                 (* condition on the volume of the simplices *)

(*
  let () = Printf.printf "ratio: %f \n%!"  ratio in   (* useful for DDD but setting it as a log message costs in terms of mesh generation time *)
*)
  let condition =
    array_all_satisfy
      (fun bc ->
	array_all_satisfy

	  (fun point -> bc point > 0.0 )

	  (Array.map
	      (fun log ->
		array_pointwise (fun l c -> 0.9*.l +. c)                              (* condition on the points inside the simplex very close to *)
		  log cog )                                                           (* vertices (used to prevent the misclassification of simplices *)
	      logs)                                                                   (* on boundaries of concave objects) *)

 	&& bc cog > 0.0
      )
      boundary_conditions
  in
    if condition
    then
      if ratio < smallest_allowed_ratio &&  nr_bou = dim+1
      then
	Body_Nr 0  (* flat simplex on the surface *)
      else
	Body_Nr 1 (* good simplex *)
    else
      Body_Nr 0  (* outside current object *)
;;

(* Begin INTERNAL: add the missing data to a mesh

   XXX Note: face description equations presumably are spurious in
   here. We killed the ms_face_eqns field, and this code should be re-adjusted
   to reflect that change.
 *)

type face_description =
    {mutable simplex_1_extid: simplex_id;
     mutable simplex_2_extid: simplex_id;
     eqn: float array;
   };;


(* Note: the identifier "new" is a token. Hence, I use "nov" below. *)

let mesh_align_external_indices mesh =
  let mapping_pt_novid_oldid = Array.map (fun pt -> pt.mp_id) mesh.mm_points in
  let mapping_pt_oldid_novid = Hashtbl.create (Array.length mesh.mm_points) in
  let mapping_sx_novid_oldid = Array.map (fun sx -> sx.ms_id) mesh.mm_simplices in
  let mapping_sx_oldid_novid = Hashtbl.create (Array.length mesh.mm_simplices) in
  let () =
    Array.iteri
      (fun nov old -> Hashtbl.replace mapping_pt_oldid_novid old nov)
      mapping_pt_novid_oldid
  in
  let () =
    Array.iteri
      (fun nov old -> Hashtbl.replace mapping_sx_oldid_novid old nov)
      mapping_sx_novid_oldid
  in
  let map_pt_old_nov id = Hashtbl.find mapping_pt_oldid_novid id in
  let map_sx_old_nov id = Hashtbl.find mapping_sx_oldid_novid id in
  begin
    Array.iter (fun pt -> pt.mp_id <- map_pt_old_nov pt.mp_id) mesh.mm_points;
    Array.iter
      (fun sx ->
	begin
	  sx.ms_id <- map_sx_old_nov sx.ms_id;
	  for i=0 to Array.length sx.ms_face_ids-1 do
	    for k=0 to Array.length sx.ms_face_ids.(i)-1 do
	      sx.ms_face_ids.(i).(k) <- map_pt_old_nov sx.ms_face_ids.(i).(k);
	    done;
	  done;
	end
      )
      mesh.mm_simplices;
  end
;;

(*
   Why does this function exist at all?

   Reason: there are a lot of useful things that can be done on a
   mesh, or during mesh construction, which do not need all the
   information computed down here, which furthermore is somewhat
   costly to obtain.

   Note: this may re-align external point and simplex ids!
   As a general rule, this should be used at the end of
   a series of simplex and point changing operations, before
   the rest of the world sees the new mesh.
 *)

let mesh_grow_bookkeeping_data
    ?(do_connectivity=false)
    ?(do_incircle_circumcircle=false)
    ?(do_regions=false)
    mesh =
  let () = logdebug ("Ensuring mesh has bookkeeping data") in
  let () = reportmem "Entering mesh_grow_bookkeeping_data" in
  let () = mesh_align_external_indices mesh in
  let dim = mesh.mm_dim in
  let nr_vertices=1+dim in
  let hyperplane = mesh.mm_la.la_hyperplane_eqn in
  let scratch_hp_points = Array.make dim [||] in
  let simplex_midpoint =
    (* Note: this returns the midpoint, but one must not modify that vector! *)
    let scratch_mp = Array.make dim 0.0 in
    let scratch_coords = Array.make nr_vertices [||] in
    (fun sx ->
      begin
	for i=0 to nr_vertices-1 do
	  scratch_coords.(i) <- sx.ms_points.(i).mp_coords;
	done;
	float_arrays_avg ~storage:scratch_mp scratch_coords
      end)
  in
  let nr_vertices_in_simplex = 1+dim in
  let ht_face_description_by_id = Hashtbl.create 100 in
  let ht_point_by_internal_id =
    (
     let h = Hashtbl.create 100 in
     let () = Array.iter (fun p -> Hashtbl.add h p.mp_id p) mesh.mm_points in
     h)
  in
  let simplices = mesh.mm_simplices in
(*  let nr_simplices = Array.length simplices in
    RRR
*)
  let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
  let nr_points = Array.length points_coords in
  let forall_simplices f = Array.iter f simplices in
  let forall_faces simplex f =
    let face_ids = array_all_one_shorter_mapped (fun pt -> pt.mp_id) simplex.ms_points in
    let () = Array.iter (Array.sort compare) face_ids in
      (* Note that it is important to have normalized (which means: sorted) face_ids *)
    Array.iter f face_ids
      (* Note that face IDs use internal point indices, which have nothing to do
	 with external indices!
       *)
  in
  let locate_face_in_simplex sx face_id =
    (* face_id is sorted.
       We need the first internal point id that is not contained in face_id
     *)
    let rec walk nr_vertex =
      if nr_vertex = nr_vertices_in_simplex
      then impossible()
      else
	let pos = array_position sx.ms_points.(nr_vertex).mp_id face_id 0 in
	if pos = (-1) then nr_vertex
	else walk (1+nr_vertex)
    in walk 0
  in
  let face_hyperplane face_id =
    begin
      for i = 0 to dim-1 do
	scratch_hp_points.(i) <- (Hashtbl.find ht_point_by_internal_id face_id.(i)).mp_coords;
      done;
      hyperplane scratch_hp_points
    end
  in
  let link_simplices_via_face (face_id:(int array)) face_desc =
    let sx1 = simplices.(face_desc.simplex_1_extid) in
    let sx2 = simplices.(face_desc.simplex_2_extid) in
    let pos_in_sx1 = locate_face_in_simplex sx1 face_id in
    let pos_in_sx2 = locate_face_in_simplex sx2 face_id in
    begin
      sx1.ms_neighbours.(pos_in_sx1) <- Some simplices.(face_desc.simplex_2_extid);
      sx2.ms_neighbours.(pos_in_sx2) <- Some simplices.(face_desc.simplex_1_extid);
      sx1.ms_neighbour_backrefs.(pos_in_sx1) <- pos_in_sx2;
      sx2.ms_neighbour_backrefs.(pos_in_sx2) <- pos_in_sx1;
      sx1.ms_face_ids.(pos_in_sx1) <- face_id;
      sx2.ms_face_ids.(pos_in_sx2) <- face_id;
    end
  in
  let () = reportmem "Completed compulsory part of mesh_grow_bookkeeping_data" in
  let () =
    if do_connectivity && mesh.mm_have_connectivity = false
    then
      let () = loginfo ("Computing mesh connectivity data") in
      let () = reportmem "Before computing mesh connectivity" in
      let () =
	forall_simplices
	  (fun simplex ->
	    forall_faces simplex
	      (fun (face_id:(int array)) ->
		try
		  let desc = Hashtbl.find ht_face_description_by_id face_id in
		  (* If we have an entry, this can only be as the other side of this face
		     has been processed earlier.
		   *)
		  begin
		    desc.simplex_2_extid <- simplex.ms_id;
		    link_simplices_via_face face_id desc;
		    Hashtbl.remove ht_face_description_by_id face_id;
		    (* ^ Here we are tidy just to keep our memory footprint small. *)
		  end
		with
		| Not_found ->  (* Have to make a new entry *)
		    Hashtbl.add ht_face_description_by_id face_id
		      {
		       simplex_1_extid=simplex.ms_id;
		       simplex_2_extid= (-1);
		       eqn=face_hyperplane face_id;
		     }))
      in
      let () = reportmem "after computing mesh connectivity" in
      mesh.mm_have_connectivity <- true
    else ()
  in
  let () =
    (if do_regions then
      let () = loginfo ("mesh generator: Working out regions for simplices") in
      let () = reportmem "Before working out regions for simplices" in
      let () =
	match mesh.mm_fem_geometry with
	| None ->
	    (* the print statement is there because python doesn't catch
	       the exception raised by ocaml *)
	    let () = logdebug (Printf.sprintf "Mesh needs FEM Geometry information for that operation!%!") in
	      failwith "Mesh needs FEM Geometry information for that operation!"
	| Some fem_geo ->
	    let scratch_point_a = Array.make dim 0.0 in
	    let scratch_point_b = Array.make dim 0.0 in
	    let scratch_points_simplex = Array.init nr_vertices (fun _ -> Array.make dim 0.0) in
	    let log_app = make_simplex_lines_of_gravity_applicator dim in
	    let for_centroid_data_of sx f =
	      begin
		for i=0 to nr_vertices-1 do
		  for k=0 to dim-1 do
		    scratch_points_simplex.(i).(k) <- sx.ms_points.(i).mp_coords.(k);
		  done;
		done;
		log_app f scratch_points_simplex
	      end
	    in
	    let set_point dst origin vec scale =
	      for i=0 to dim-1 do
		dst.(i) <- origin.(i) +. scale *. vec.(i);
	      done;
	    in
	    (* Just to make sure, clear the point-belongs-to-body information
	       beforehand. *)
	    let () =
	      (for i=0 to nr_points-1 do
		mesh.mm_points.(i).mp_in_body<-[]; done)
	    in
	    let pushnew_boundary region pt =
	      try ignore(List.find (fun x -> x = region) pt.mp_in_body)
	      with
	      | Not_found -> pt.mp_in_body <- (region::pt.mp_in_body)
	    in
	    let () =
	      forall_simplices
		(fun sx ->
		  for_centroid_data_of sx
		    (* XXX here, the question arises: which point_ids to use? *)
		    (fun cog v_log v_lens vol ->
		      for i=0 to nr_vertices-1 do
			let point_id = sx.ms_points.(i).mp_id in
			begin
			  (* XXX Note: maybe, we should make that 1% a parameter...
			     I do not think that this is reasonable, though.
			   *)
			  set_point scratch_point_a cog v_log.(i) 1.25;
			  set_point scratch_point_b cog v_log.(i) 0.80;
			  (* Here, we would like to just iterate over all boundary
			     conditions and check whether scratch_point_a or scratch_point_b
			     is "inside". If either is the case, we remember that our
			     point belongs to the corresponding object.

			     However, we also want to deal with the "outer surface" points
			     and properly add (Body_Nr (-1)) when appropriate.
			     For this, we have to check whether a or b is "outside" everything.
			   *)
			  let nr_boundaries = Array.length fem_geo.fem_geo_boundaries in
			  let rec process_boundaries n a_outside_all b_outside_all =
			    if n = nr_boundaries then
			      (if (a_outside_all || b_outside_all)
			      then pushnew_boundary (Body_Nr (-1)) mesh.mm_points.(point_id)
			      else ())
			    else
			      let (region,bc) = fem_geo.fem_geo_boundaries.(n) in
			      let a_outside = ((bc scratch_point_a) < 0.0) in
			      let b_outside = ((bc scratch_point_b) < 0.0) in
			      let () =
				(if (not(a_outside) || not(b_outside))
				then
				  (* Complexity is a non-issue with pushnew_boundary.
				     What points will belong to 10+ boundaries? *)
				  pushnew_boundary region mesh.mm_points.(point_id)
				else ())
			      in process_boundaries (1+n)
				(a_outside_all && a_outside)
				(b_outside_all && b_outside)
			  in process_boundaries 0 true true
			end
		      done;
		    ))
	    in
	    let () =
	      forall_simplices
		(fun sx ->
		  let in_all =
		    lists_intersection ~sorter:compare
		      (Array.to_list (Array.map (fun p -> p.mp_in_body) sx.ms_points))
		  in
		  let sx_region = Body_Nr
		      (
		       match in_all with
		       | [] -> -2
		       | _::_::_ ->
			   let midpoint = simplex_midpoint sx in
			   let first_inside =
			     try
			       let (Body_Nr r,bc) =
				 array_find (fun (region,bc) -> bc midpoint >0.0)
				   fem_geo.fem_geo_boundaries
			       in r
			     with
			     | Not_found -> -1
			   in first_inside
		       | [Body_Nr x] -> x)
		  in
		  sx.ms_in_body <- sx_region)
	    in ()
      in
      let () = Array.iter (fun p -> p.mp_in_body <- List.sort (fun x y -> if x < y then (-1) else if y < x then 1 else 0) p.mp_in_body) mesh.mm_points
      in
      let () = reportmem "after working out regions for simplices" in
      mesh.mm_have_regions <- true
    else ())
  in
  ()
;;

(* End INTERNAL: add the missing data to a mesh *)

let mesh_boundary_points mesh =
(* NOT USED - GREAT!! gb *)
  array_mapfilter
    (fun pt ->
      match pt.mp_in_body with
      | _::_::_ ->			(* at least two entries *)
	  Some
	    ((List.sort compare pt.mp_in_body), pt.mp_coords)
      | _ -> None)
    mesh.mm_points
;;

(* Later on, we will provide our own Delaunay triangulation code.
   For development and for comparison, it is appropriate
   to have functions that generate meshes from Qhull.delaunay data.
 *)

let mesh_from_known_delaunay                                                                   (* function to tell the points about the simplices they *)
    points_coords arr_simplex_region_and_vertices =                                            (* belong to and vice-versa. *)
  let nr_points = Array.length points_coords in
  let point_belongs_to = Array.make nr_points [] in
  let dim = Array.length points_coords.(0)
  in
  let mpoints = Array.mapi                                                                      (* function to initialize the mesh points structure *)
    (fun nr_point coords ->
       {
	mp_coords=coords;
	mp_id=nr_point;
	mp_belongs_to=[||];
	mp_in_body=[];
       })
    points_coords
  in
  let point_nr n = mpoints.(n) in
  let do_for_simplex_vertices_with_counter f = Array.mapi f arr_simplex_region_and_vertices in
  let msimplices = do_for_simplex_vertices_with_counter
    (fun nr_simplex (region,point_indices) ->
      let sorted_ci = Array.copy point_indices in
      let points = Array.map point_nr sorted_ci in                                            (* initialize the points *)
      let () = (* Ensure our coord indices really are sorted. *)
	Array.sort compare sorted_ci in
      (* Remember for all points of this simplex that they belong to this simplex. *)
      let new_simplex =
	{
	  ms_id = nr_simplex;
	  ms_points = points;
	  (* To be adjusted later: *)
	  ms_neighbours = Array.make (dim+1) None;
	  ms_neighbour_backrefs = Array.make (dim+1) (-1);
	  ms_face_ids =
	    array_all_one_shorter
	      (Array.map (fun n -> (point_nr n).mp_id) sorted_ci);
	  (* XXX TODO: We will add code to provide those later! *)
	  ms_in_body=region;
	}
      in
      let () =
	Array.iter                                                                         (* for each point find the simplices it belongs to *)
	  (fun ix -> point_belongs_to.(ix) <- new_simplex::(point_belongs_to.(ix)))
	  point_indices in
	new_simplex
    )
  in
    (* Tell all the points about their simplices *)
  let () = Array.iteri
      (fun n p -> p.mp_belongs_to <- (Array.of_list point_belongs_to.(n)))
      mpoints
  in
  let mesh0 = mesh0_from_mesh mpoints msimplices in
  let simplex_data = Simplex.init mesh0 in
  let mesh =
    {mm_dim=dim;
     mm_la = make_la_functions dim;
     mm_make_find_visible = make_find_visible;
     mm_fem_geometry = None;
     mm_boundaries = [||];
     mm_origins = [||];
     mm_points = mpoints;
     mm_simplices = msimplices;
     mm_mesh0 = mesh0;
     mm_simplex_data = simplex_data;
     mm_region_volumes = _region_volumes simplex_data msimplices;
     mm_vertex_distribution=[|Array.length mpoints|];
     mm_periodic_points = [||];
     mm_have_connectivity=false;
     mm_have_incircle_circumcircle=false;
     mm_have_regions=false;
   }
  in
  let () = mesh_grow_bookkeeping_data ~do_connectivity:true mesh in
  let () = mesh.mm_boundaries <-
    Array.map (fun (x,y) -> (x,Array.of_list y)) (hashtbl_to_array (mesh_boundaries mesh))
  in
    mesh
;;


(* Note: the function f will get called as f (Body_Nr large) (Body_Nr small) simplex nr_face *)
(* NOT USED - GREAT!! gb *)
let for_mesh_interfaces mesh f =
  let simplices = mesh.mm_simplices in
  let nr_neighbours = mesh.mm_dim+1 in
  let body_ext = Body_Nr (-1) in
  let nr_simplices = Array.length simplices in
    for i=0 to nr_simplices-1 do
      let sx = simplices.(i) in
      let body_sx = sx.ms_in_body in
      let neighbours = sx.ms_neighbours in
	for n=0 to nr_neighbours-1 do
	  match neighbours.(n) with
	    | None ->
		f body_sx body_ext sx n
	    | Some sx_n ->
		let body_sx_n = sx_n.ms_in_body in
		  if body_sx_n < body_sx then
		    f body_sx body_sx_n sx n
		  else ()
	done
    done
;;


(* For a spherical triangle, the area (hence space angle) is proportional
   to the excess angle.

   For the computation of one of the two BEM potentials, we need the
   spatial surface angle as seen from a surface vertex. How to obtain this?

   Idea:

   - If the site in question is inside a triangle (i.e. involves averaging 3 vertices),
     the solid angle is 2pi.

   - If the site in question is on an edge (i.e. the average of two vertices),
     the solid angle can be derived from the angle between the surface normals
     of the meeting triangles.

   - If the site in question is a vertex, we have to:

     (1) Find all simplices this vertex belongs to.
     (2) Only take these which belong to the "inside region"
     (3) For every one of these, take the (internal) face opposing our vertex,
         and use the formula (taken from Wikipedia, actually) XXX need a better reference!

   http://en.wikipedia.org/wiki/Solid_angle

   XXX this formula smells fishy - see the Wikipedia discussion page.

   Instead, we now use (3.57) from:
   http://magnet.atp.tuwien.ac.at/scholz/projects/diss/html/node19.html

   (Note: test shows that both formulae do indeed give the same results!)

   Note: the default fun_is_inside function just assumes every simplex that is not vacuum
   to be "inside". This may or may not be reasonable:

   - In many applications, "every simplex is inside", as the non-inside simplices even do not exist.

   - We may want to have non-magnetic materials with boundaries which
     are interesting for other reasons (e.g. heat diffusion), but which
     we want to consider as belonging to the outside as well.

   So, it is good that this is a parameter. We must only remember to use it.
 *)


let solid_angle_3d_at_mesh_vertex ?(fun_is_inside=(fun sx -> sx.ms_in_body <> Body_Nr 0)) mesh_point =
  let simplices = mesh_point.mp_belongs_to in                              (* computes the solid angle spanned by a simplex as seen by the *)
  let nr_simplices = Array.length simplices in                             (* the vertex *)
  let rec walk n angle_so_far =
    if n=nr_simplices then angle_so_far
    else if not (fun_is_inside simplices.(n)) then walk (1+n) angle_so_far
    else
      let sx = simplices.(n) in
      let sx_points = sx.ms_points in
      let ix0 = array_position_if (fun p -> p == mesh_point) sx_points 0 in
      let ix1 = if ix0 <= 0 then 1 else 0
      and ix2 = if ix0 <= 1 then 2 else 1
      and ix3 = if ix0 <= 2 then 3 else 2
      in
(*      let pc0 = sx_points.(ix0).mp_coords
	in
      RRR
*)
      let angle_contrib =
	abs_float (triangle_space_angle_3d                                 (* this is the function used for the computation of the angle *)
		     sx_points.(ix0).mp_coords
		     sx_points.(ix1).mp_coords
		     sx_points.(ix2).mp_coords
		     sx_points.(ix3).mp_coords)
      in
	walk (1+n) (angle_so_far+.angle_contrib)
  in
  let result=walk 0 0.0 in
    result
;;

(* Destructively modify a given float array in such a way
   that the n-th entry is increased by the solid angle
   of material n as seen from a given vertex.
   Nonmeshed outer space is ignored.
*)

(* NOT USED - GREAT!! gb *)

let accumulate_material_solid_angles_3d mesh_point v_solid_angles =
  let simplices = mesh_point.mp_belongs_to in
  let nr_simplices = Array.length simplices in
  let rec walk n =
    if n=nr_simplices then ()
    else
      let sx = simplices.(n) in
      let Body_Nr region = sx.ms_in_body in
      let sx_points = sx.ms_points in
      let ix0 = array_position_if (fun p -> p == mesh_point) sx_points 0 in
      let ix1 = if ix0 <= 0 then 1 else 0
      and ix2 = if ix0 <= 1 then 2 else 1
      and ix3 = if ix0 <= 2 then 3 else 2
      in
(*      let pc0 = sx_points.(ix0).mp_coords
	in
	RRR
*)
      let angle_contrib =
	abs_float (triangle_space_angle_3d
		     sx_points.(ix0).mp_coords
		     sx_points.(ix1).mp_coords
		     sx_points.(ix2).mp_coords
		     sx_points.(ix3).mp_coords)
      in
      let () = v_solid_angles.(region) <- v_solid_angles.(region) +. angle_contrib
      in
	walk (1+n)
  in
    walk 0
;;



(* Note 1: this function just combines mesh_from_known_delaunay and Qhull.delaunay.

   Note 2: There is a need for a filter function in order to remove super-slim
   simplices from flat faces, e.g. of a box. The default provided should be
   somewhat reasonable...
*)

let mesh_from_points
    dim
    boundary_conditions
    ?(triangulator=Qhull.delaunay)
    ?(simplex_classificator = (make_default_boundary_condition_simplex_classificator dim boundary_conditions))
    smallest_allowed_ratio pts_states points_coords  =
  let all_raw_simplices = triangulator points_coords in                                                     (* make a triangulation; the bad simplices will then be pruned *)

  let nr_vertices=dim+1 in                                                                       (* copy the vertices coordinates on a scratchpad *)
  let scratch_simplex_coords = Array.init nr_vertices (fun _ -> Array.make dim 0.0) in
  let init_scratch_simplex_coords sx =
    for i=0 to nr_vertices-1 do
      for k=0 to dim-1 do
	scratch_simplex_coords.(i).(k) <- points_coords.(sx.(i)).(k);
      done;
    done
  in
  let simplices =                                                                                 (* take all the simplices whose body_nr is 0 or 1, *)
    array_filter (fun ((Body_Nr x),s) -> x <> (-1))						  (* as given by the classificator *)
      (Array.map
	 (fun s ->
	   let () = init_scratch_simplex_coords s in
	   let points_states_arr = Array.map (fun pt_ix -> pts_states.(pt_ix)) s                  (* list of states associated to the points of the simplex *)
	   in
	   let count_mob_pts = ref 0 in                                                           (* count mobile points in the list of vertices *)
	   let () = for i=0 to dim do
	       if  points_states_arr.(i) = Mobile
	       then count_mob_pts := 1 + !count_mob_pts
	       else ()
	     done
	   in
	     if !count_mob_pts >= (dim-1)                                                         (* if the mobile points are at least dim-1, the simplex is *)
	     then                                                                                 (* partially in the interior of the object, so that its body_nr is set to 1 *)
	       (Body_Nr 1,s)
	     else
               (simplex_classificator smallest_allowed_ratio scratch_simplex_coords  pts_states,s))                      (* if not, the classificator is called to decide is the simplex *)
	 all_raw_simplices)                                                                       (* is in the outer space (body_nr 0) or should be removed (body_nr -1) *)
  in

    mesh_from_known_delaunay points_coords simplices                                              (* create the mesh out of the list of simplices (giving each simplex) *)
;;

(* XXX MUST BE RE-WRITTEN!

    XXX NOTE: For now, this is overly primitive, indeed O(nr simplices).
   Even with a quite naive approach, we can at least do O(nr simplices)^(1/d),
   and even logarithmic behaviour should be possible.

   Note: simplex surface points belong to more than one simplex.
   We just return one of them, and corresponding coordinates.

   NOTE: Currently not used
 *)
let mesh_simplex_nr_and_L_coords_of_point mesh point_coords =
  let dim = Array.length point_coords in
  let dim_ext = dim+1 in
  let nr_simplices = Array.length mesh.mm_simplices in
  let scratch_pos_ext = Array.make dim_ext 0.0 in
  let get_inv_point_matrix =
    Simplex.get_inv_point_matrix mesh.mm_simplex_data in
  let mx_x_pos_ext_writeto_scratch mx =
    begin
      for i=0 to dim_ext-1 do
	(* We have to clear the vector -
	   but we save some work if we do so by initializing
	   the result vector with contributions from the one extra
	   projective coordinate, which is 1.
	 *)
	scratch_pos_ext.(i) <- (F.get2 mx i dim);
      done;
      for i=0 to dim_ext-1 do
	for k=0 to dim-1 do
	  scratch_pos_ext.(i) <- scratch_pos_ext.(i)
	                         +. (F.get2 mx i k)*.point_coords.(k);
	done;
      done;
    end
  in
  let rec find_point_L_coords nr_simplex =
    if nr_simplex = nr_simplices
    then None
    else
      let mx_iepc = get_inv_point_matrix nr_simplex in
      let () = mx_x_pos_ext_writeto_scratch mx_iepc in
      (*
      let () = Printf.printf "Simplex %3d: %s\n L-coords: %20s backmapped: %20s\n\n"
          nr_simplex
          (String.concat " -- " (Array.to_list (Array.map (fun ix -> float_array_to_string mesh.mm_points.(ix).mp_coords) simplex.ds_points)))
          (float_array_to_string scratch_pos_ext)
          (float_array_to_string (mx_x_vec ((fun (Some x) -> x) simplex.ds_ext_point_coords) scratch_pos_ext))
      in *)
      let pos_first_negative = array_position_if (fun x -> x<0.0) scratch_pos_ext 0
      in
        if pos_first_negative = (-1)
        then Some (nr_simplex, Array.copy scratch_pos_ext)
        else find_point_L_coords (nr_simplex+1)
  in find_point_L_coords 0
;;

let last_addition_deletion_points = ref 0;;                                                           (* function to set the controller default behavior *)
let default_meshgen_controller mdefaults rng so_far input =
  let return x = (so_far+1,x) in
  (* We internally provide code for square and cubic number checks. *)
(*  let is_positive_cubic_number x =
    if x<=0 then false
    else let presumed_root = truncate ((float_of_int x)**(1.0/.3.0)+.0.5) in
    presumed_root*presumed_root*presumed_root = x
    RRR
*)
  let is_positive_square_number x =                                                                   (* function to catch the square numbers: condition *)
    if x<=0 then false                                                                                (* on the mesh step for a retriangulation, starting *)
    else let presumed_root = truncate ((float_of_int x)**(1.0/.2.0)+.0.5) in                          (* counting from step=10 *)
    presumed_root*presumed_root = x
  in
  if so_far=0                                                                                         (* Ok, we do not yet know anything... go on with *)
  then                                                                                                (* the mesh relaxation using a default value for *)
    return (Time_step (mdefaults.mdefault_controller_time_step_scale,so_far+1))                       (* time step *)
  else if so_far >= mdefaults.mdefault_controller_step_limit_max && so_far >= (50 + !last_addition_deletion_points)
  then                                                                                                (* extract mesh only after 50 steps from the *)
    return (Step_Limit_Reached so_far)                                                                (* last insertion or deletion of points, no matter *)
      (* Heuristics: schedule point number changing steps from time to time,                          (* if the max step limit is exceeded *)
	 but more and more infrequently.
	 Present scheme: whenever the step number is of the form (cubic number+10).
       *)
  else if
    is_positive_square_number (so_far-10) && so_far < mdefaults.mdefault_controller_step_limit_max
  then                                                                                                (* step = positive square number: *)
    let () = last_addition_deletion_points := so_far in                                               (* at these steps the mesher checks the density *)
    let fates =                                                                                       (* of points, whose limits are more relaxed at the *)
	(Array.map (fun x -> mdefaults.mdefault_controller_handle_point_density_fun rng x             (* beginning of the relaxation, and after adding or *)
		      ((mdefaults.mdefault_initial_relaxation_weight                                  (* deleting points it retriangulates *)
			  so_far
			  mdefaults.mdefault_controller_initial_settling_steps
			  ((-0.1) *. mdefaults.mdefault_controller_movement_max_freedom )             (* density for the insertion of points *)
			  0.) +. mdefaults.mdefault_controller_thresh_add )

		      ((mdefaults.mdefault_initial_relaxation_weight
			  so_far
			  mdefaults.mdefault_controller_initial_settling_steps                        (* density for the removal of points *)
			  (0.1 *. mdefaults.mdefault_controller_movement_max_freedom )
			  0.) +. mdefaults.mdefault_controller_thresh_del ))
	   input.mci_node_avg_density)

    in
      return (Change_points_and_retriangulate (fates,so_far+1))

  else if
    (so_far > mdefaults.mdefault_controller_step_limit_min &&                                         (* the mesh has reached a point of equilibrium and *)
     input.mci_largest_rel_movement_in_mesh < mdefaults.mdefault_controller_tolerated_rel_movement)   (* it will stop the relaxation as soon as the min *)
  then                                                                                                (* number of steps is reached *)
    let () = loginfo (Printf.sprintf "F-EQ with largest rel movement = %f" input.mci_largest_rel_movement_in_mesh) in
    return (Force_Equilibrium_Reached input.mci_largest_rel_movement_in_mesh)
  else
    if input.mci_max_node_rel_movement_since_last_triangulation >                                     (* max ratio [distFromLastTriang/a0 *density^(1/dim)] *)
      ((mdefaults.mdefault_initial_relaxation_weight                                                  (* the function is compared to the topology threshold, *)
	  so_far                                                                                      (* a value which is "relaxed" in the first steps of the *)
	  mdefaults.mdefault_controller_initial_settling_steps                                        (* relaxation *)
	  mdefaults.mdefault_controller_movement_max_freedom
	  1.) *. mdefaults.mdefault_controller_topology_threshold)
  then
      (so_far+1,Retriangulate (so_far+1))                                                             (* simple retriangulation *)
  else
    let effective_largest_force =
      if so_far<2                                                                                     (* For the initial steps, we just set this to 1, *)
      then 1.0                                                                                        (* to get things going. *)
      else input.mci_largest_effective_force_in_mesh
    in
      if effective_largest_force = 0.0                                                                (* if force = 0 we reached the equilibrium and *)
      then                                                                                            (* we extract the mesh *)
	return (Force_Equilibrium_Reached input.mci_largest_rel_movement_in_mesh)
      else
	return                                                                                        (* do a time step taking the minimum between the *)
	  (Time_step (                                                                                (* max_time_step and the ratio dx/force, that is *)
												      (* time_step_scale/effective_largest_force *)
	      ( min
		  ((mdefaults.mdefault_initial_relaxation_weight
		       so_far
		       mdefaults.mdefault_controller_initial_settling_steps
		       mdefaults.mdefault_controller_movement_max_freedom
		       1.) *. mdefaults.mdefault_controller_max_time_step
		  )
		  ((mdefaults.mdefault_initial_relaxation_weight
		       so_far
		       mdefaults.mdefault_controller_initial_settling_steps
		       mdefaults.mdefault_controller_movement_max_freedom
		       1.)
		    *. mdefaults.mdefault_controller_time_step_scale /. effective_largest_force
		  )
	      ), so_far+1)
	  )
;;


let opt_mesher_defaults=
  ref
    {
      mdefault_controller_initial_points_volume_ratio= 0.9;               (* functions defined but not used (constraints on pyfem) *)
      mdefault_controller_splitting_connection_ratio= 1.6;                (* | *)
      mdefault_controller_exp_neigh_force_scale= 0.9;                     (* __ *)

      mdefault_nr_probes_for_determining_volume=100000;
      mdefault_boundary_condition_acceptable_fuzz=1.0e-6;                 (* each object is defined by a function which is >0 inside and <0 outside *)
      mdefault_boundary_condition_max_nr_correction_steps=200;
      mdefault_boundary_condition_debuglevel=0;
      mdefault_make_boundary_condition_simplex_classificator=
	make_default_boundary_condition_simplex_classificator;
      (* -- *)
      mdefault_relaxation_debuglevel=0;
      mdefault_controller_movement_max_freedom = 3.0;                    (* ...freedom relaxes the conditions on the insertion/deletion of points, *)
      mdefault_controller_topology_threshold=0.2;                        (* the topology threshold and the next time step *)
      mdefault_controller_step_limit_min=500;
      mdefault_controller_step_limit_max=1000;
      mdefault_controller_max_time_step=10.0;
      mdefault_controller_time_step_scale=0.1;
                                                                         (* Roughly: how far should the fastest node move in this time step? *)
									 (* Of course, this should be considerably less than the topology threshold, *)
									 (* or that parameter would be useless. Perhaps, ~4-5 time steps before one *)
									 (* retriangulation is something resaonable one should aim at? Then, Delaunay costs *)
									 (* do not hit us that badly, but we do want to eventually do some real, potentially *)
									 (* topology-changing work anyway. *)
      mdefault_controller_tolerated_rel_movement=0.002;

                                                                         (* default values for the weigth to apply to the forces *)
      mdefault_controller_shape_force_scale=0.1;
      mdefault_controller_volume_force_scale= 0.0;
      mdefault_controller_neigh_force_scale= 1.0;
      mdefault_controller_irrel_elem_force_scale= 1.0;
      mdefault_controller_initial_settling_steps= 100;
                                                                         (* default values for the insertion/deletion of points, and *)
      mdefault_controller_thresh_add = 1.0;                              (* sliver correction (which is the use of longitudinal component of *)
      mdefault_controller_thresh_del = 2.0;                              (* the shape force when the volume of the simplex is very small) *)
      mdefault_controller_sliver_correction= 1.0;
      mdefault_controller_smallest_allowed_volume_ratio= 1.0;

      mdefault_initial_relaxation_weight=                                	  (* linear function from init_val to final_val in the first *)
	(fun iteration_step max_step init_val final_val ->                        (* max_step time steps, then final_val *)
	  init_val +. (final_val -. init_val) *.
	    (min 1. ((float_of_int iteration_step)/.(float_of_int max_step)))
	);

      mdefault_relaxation_force_fun=                                     (* force acting between two mobile nodes: it is simply a repulsing force  *)
	(fun reduced_distance ->
	  if reduced_distance > 1.0
	  then 0.0
	  else 1.0 -. reduced_distance
	);
      mdefault_boundary_node_force_fun=                                  (* force acting on points when interact with a boundary point: the force is *)
	(fun reduced_distance ->                                         (* again repulsive but its value is stronger than the standard one for short *)
	  if reduced_distance > 1.0                                      (* distances; as the author said ... *)
	                                                                 (* Quite strongly repelling potential: "Don't dare coming close to me!" *)
	  then
	    0.0
	  else
            (*1.0 -. reduced_distance *) reduced_distance**(-1.0)-.1.0
	);
      mdefault_controller_handle_point_density_fun=                      (* the function to insert or delete points checks both the density of a point *)
	(fun rng (avg_density, avg_force)  thresh_add thresh_del ->      (* as obtained by the ratio voronoi volume/ideal volume and the (only) force from *)
	  if avg_density < thresh_add (* Virtually no force at that element *)                          (* the neighbours *)
	  then
	    let dice = Mt19937.int rng 100 in
	      if dice < 10 (*+ int_of_float((thresh_add -. avg_density)*.30.0)*)                        (* vith small density the point is inserted with 10% *)
	      then                                                                                      (* probability *)
		let () = logdebug (Printf.sprintf "Dtl (dens_avg=%f) - adding point.%!" avg_density) in
		  (*Do_Nothing_With_Point *)Add_Another_Point
	      else Do_Nothing_With_Point
	  else if avg_force < 0.07 then                                                                 (* if the force from neighbours is lower than 0.07 *)
	    let dice = Mt19937.int rng 100 in                                                           (* the point is inserted with 20% probability *)
	      if dice < 20 (* Chance of 1/5 *)
	      then
		let () = logdebug (Printf.sprintf "Ftl (avg_force=%f) - adding point.%!" avg_force) in
	       (*Do_Nothing_With_Point*)Add_Another_Point
	      else Do_Nothing_With_Point
       else
	    if avg_density > thresh_del (* Density much too high here *)                                (* if density is too large the point is removed with *)
       then                                                                                             (* probability 30% + 10% for each integer exceeding *)
	 let dice = Mt19937.int rng 100 in                                                              (* the threshold *)
	   if dice < 30+int_of_float((avg_density-.thresh_del)*.10.0)  (* Chance of ...*)
	   then
	     let () = logdebug (Printf.sprintf "Dth (dens_avg=%f) - axing point.%!" avg_density) in
	       (*Do_Nothing_With_Point*)Delete_Point
	   else Do_Nothing_With_Point

       else
	 if avg_force > 0.5 (* Density much too high here *)                                            (* if the force from neighbours is greater than 0.5 *)
       then                                                                                             (* the point is removed with probability 40% + 10% for *)
	 let dice = Mt19937.int rng 100 in                                                              (* each integer exceeding 0.5 *)
	   if dice < 40+int_of_float((avg_force-.0.5)*.10.0)  (* Chance of ...*)
	   then
	     let () = logdebug (Printf.sprintf "Fth (avg_force=%f) - axing point.%!" avg_force) in
	       (*Do_Nothing_With_Point*)Delete_Point
	   else Do_Nothing_With_Point


       else Do_Nothing_With_Point);
      mdefault_meshgen_controller=default_meshgen_controller;                                           (* set the initial state of the controller, as well *)
      mdefault_meshgen_controller_initial=0;                                                            (* as the controller itself *)
    }
;;


let copy_mesher_defaults m new_controller new_initial_state =
  {
   mdefault_controller_initial_points_volume_ratio=m.mdefault_controller_initial_points_volume_ratio;   (* functions defined but not used (constraints on pyfem) *)
   mdefault_controller_splitting_connection_ratio=m.mdefault_controller_splitting_connection_ratio;     (* | *)
   mdefault_controller_exp_neigh_force_scale=m.mdefault_controller_exp_neigh_force_scale;               (* __ *)

   mdefault_nr_probes_for_determining_volume=m.mdefault_nr_probes_for_determining_volume;
   mdefault_boundary_condition_acceptable_fuzz=m.mdefault_boundary_condition_acceptable_fuzz;
   mdefault_boundary_condition_max_nr_correction_steps=m.mdefault_boundary_condition_max_nr_correction_steps;
   mdefault_boundary_condition_debuglevel=m.mdefault_boundary_condition_debuglevel;
   mdefault_make_boundary_condition_simplex_classificator=m.mdefault_make_boundary_condition_simplex_classificator;
   (* -- *)
   mdefault_relaxation_debuglevel=m.mdefault_relaxation_debuglevel;
   mdefault_controller_movement_max_freedom = m.mdefault_controller_movement_max_freedom;
   mdefault_controller_topology_threshold=m.mdefault_controller_topology_threshold;
   mdefault_controller_step_limit_min=m.mdefault_controller_step_limit_min;
   mdefault_controller_step_limit_max=m.mdefault_controller_step_limit_max;
   mdefault_controller_max_time_step=m.mdefault_controller_max_time_step;
   mdefault_controller_time_step_scale=m.mdefault_controller_time_step_scale;
   mdefault_controller_tolerated_rel_movement=m.mdefault_controller_tolerated_rel_movement;

   (* GB *)
   mdefault_controller_shape_force_scale=m.mdefault_controller_shape_force_scale;
   mdefault_controller_volume_force_scale=m.mdefault_controller_volume_force_scale;
   mdefault_controller_neigh_force_scale=m.mdefault_controller_neigh_force_scale;
   mdefault_controller_irrel_elem_force_scale=m.mdefault_controller_irrel_elem_force_scale;

   mdefault_controller_thresh_add = m.mdefault_controller_thresh_add;
   mdefault_controller_thresh_del = m.mdefault_controller_thresh_del;
   mdefault_initial_relaxation_weight= m.mdefault_initial_relaxation_weight;
   mdefault_controller_initial_settling_steps= m.mdefault_controller_initial_settling_steps;
   mdefault_controller_sliver_correction = m.mdefault_controller_sliver_correction;
   mdefault_controller_smallest_allowed_volume_ratio = m.mdefault_controller_smallest_allowed_volume_ratio;

   mdefault_relaxation_force_fun=m.mdefault_relaxation_force_fun;
   mdefault_boundary_node_force_fun=m.mdefault_boundary_node_force_fun;
   mdefault_controller_handle_point_density_fun=m.mdefault_controller_handle_point_density_fun;
   mdefault_meshgen_controller=new_controller;
   mdefault_meshgen_controller_initial=new_initial_state;
 };;


(* Note: this is strictly internal: *)
(* NOT USED - GREAT!! gb *)
type meshgenerator_point =
    {
      coords_start: float array; (* Original point position *)
      coords_here: float array; (* Present point position *)
      force_here: float array; (* A vector *)
      (* These two entries below are used to determine
	 whether points have to be axed or inserted: *)
      mutable sum_abs_forces: float;
      mutable nr_forces: int;
      mutable density_here: float;
      mutable state: point_state
    }
;;


(*
  Note 1: this is internal only, not a public function.

  Note 2: for really crazy geometries, this might loop endlessly...

  Note 3: this will destructively modify point.coords_here.

  Note 4: the default for acceptable_fuzz means that we implicitly
  assume that boundary condition functions have values ~ 1, not, say,
  ~ 1e8.
*)

let _enforce_boundary_conditions                                                       (* function which moves the points pushed outside the *)
    mdefaults                                                                          (* boundaries of an object back within the boundaries *)
    scratchpad_coords
    fun_take_gradient_store_result
    bcs point =
  let acceptable_fuzz = mdefaults.mdefault_boundary_condition_acceptable_fuzz in
  let max_nr_correction_steps=mdefaults.mdefault_boundary_condition_max_nr_correction_steps in
  let nr_bcs = Array.length bcs
  and coords = point.coords_here in
  let dim = Array.length coords in
  let rec find_first_violated n =
    if n = nr_bcs
    then (-1)                                                            (* Finished, none violated *)
    else

      if bcs.(n) coords >= (0.0-.acceptable_fuzz)
    then find_first_violated (n+1)
    else
      let () = point.state <- Boundary in                                (* the points which are moved back on the boundary are classified as boundary points *)
	n                                                                (* the number of the boundary in the sequence is returned whenever the point is outside *)
  in
  let rec correct_point nr_step =                                        (* if the point doesn't exceed any boundary condition the function returns -1, *)
    if nr_step >= max_nr_correction_steps                                (* otherwise the number of steps necessary for the euler method to push the point back *)
    then
      ()
    else
      let first_violated = find_first_violated 0 in
      if first_violated = (-1)
      then
	()	                                                          (* Done - all boundary conditions are fine. *)
      else
	let rec correct_this_bc nr_step =
	  if nr_step >= max_nr_correction_steps
	  then
	    nr_step
	  else
	    let bc_val = bcs.(first_violated) coords in
	    if abs_float(bc_val) <= acceptable_fuzz
	    then
 	      nr_step
	    else
	      let () =
		fun_take_gradient_store_result
		  scratchpad_coords bcs.(first_violated) coords
	      in
	      let len_grad_sq = euclidean_len_sq scratchpad_coords in
                                                                	  (* length of gradient tells us how much the function will grow    *)
									  (* if we walk one unit distance in the direction of the gradient. *)
									  (*   								    *)
									  (* As we are too small, we will do so, but we will walk	    *)
									  (* not one unit distance, but					    *)
									  (*   								    *)
									  (* (-bc_val/len_grad) unit distances.				    *)
									  (*   								    *)
									  (* or, what is the same,  (-bc_val/len_grad_sq) times the gradient.*)
		if len_grad_sq = 0.0
		then failwith
		  (Printf.sprintf "Failure: grad(boundary condition)=0 close to the boundary (pos=%s)!" (float_array_to_string coords))
		else
		  let scale = (0.0-.bc_val)/.len_grad_sq in

		    begin
		      for i = 0 to dim-1 do
			coords.(i) <- coords.(i) +. scale*.scratchpad_coords.(i);
		      done;
		      correct_this_bc (nr_step+1);
		    end
	in
	let new_nr_step = correct_this_bc nr_step in
	  correct_point new_nr_step
  in
    correct_point 0
;;

let _enforce_boundary_conditions_reversed                                  (* this function does the same thing as the _enforce_boundary_conditions one *)
    mdefaults                                                              (* but now the points are pushed from the interior of the object on the closest *)
    scratchpad_coords                                                      (* boundary: it is used to push all the points very close to the boundary, on the *)
    fun_take_gradient_store_result                                         (* boundary itself, in the final_mesh_from_points function *)
    bcs coords =
  let () = logdebug (Printf.sprintf " reversed%!") in
  let acceptable_fuzz = mdefaults.mdefault_boundary_condition_acceptable_fuzz in
  let max_nr_correction_steps=mdefaults.mdefault_boundary_condition_max_nr_correction_steps in
  let nr_bcs = Array.length bcs in
  let dim = Array.length coords in
  let rec find_first_violated n =
    if n = nr_bcs then (-1) (* Finished, none violated *)
    else
      if bcs.(n) coords <= acceptable_fuzz
    then find_first_violated (n+1)
    else
      n
  in
  let rec correct_point nr_step =
    if nr_step >= max_nr_correction_steps
    then
      ()
    else
      let first_violated = find_first_violated 0 in
      if first_violated = (-1)
      then
	()	(* Done - all boundary conditions are fine. *)
      else
	let rec correct_this_bc nr_step =
	  if nr_step >= max_nr_correction_steps
	  then
	    nr_step
	  else
	    let bc_val = bcs.(first_violated) coords in
	      if abs_float(bc_val) <= acceptable_fuzz
	      then
 		nr_step
	      else
		let () =
		  fun_take_gradient_store_result
		    scratchpad_coords bcs.(first_violated) coords
		in
		let len_grad_sq = euclidean_len_sq scratchpad_coords in
		let scale =
		  if len_grad_sq = 0.0
		  then 1.0e-6
		  else (0.0-.bc_val)/.len_grad_sq in
		  begin
		    for i = 0 to dim-1 do
		      coords.(i) <- coords.(i) +. scale*.scratchpad_coords.(i);
		    done;
		    correct_this_bc (nr_step+1);
		  end
	in
	let new_nr_step = correct_this_bc nr_step in
	  correct_point new_nr_step
  in
    correct_point 0
;;

let final_mesh_from_points
    dim
    ?(triangulator=Qhull.delaunay)
    ?(simplex_classificator = make_default_boundary_condition_simplex_classificator dim)
    boundary_conditions smallest_allowed_ratio pts_states points_coords all_raw_simplices =

(*  let all_raw_simplices = triangulator points_coords in                                                        (* prepare the arguments for the default classificator *)
*)  let surface_classificator s =
    let simplex_points_states = Array.map (fun pt_ix -> pts_states.(pt_ix)) s in
    let simplex_points_coords = Array.map (fun pt_ix -> points_coords.(pt_ix)) s in
    (simplex_classificator boundary_conditions smallest_allowed_ratio simplex_points_coords simplex_points_states,s)
    in
(* let () = Array.iter (fun s -> let () = Array.iter (fun p_ix -> Printf.printf "%s " (float_array_to_string points_coords.(p_ix))) s in Printf.printf "\n%!") all_raw_simplices in *)
  let simplices =                                                                                              (* apply the function to the  list of simplices *)
    array_filteri (fun ix ((Body_Nr x),s) ->
(*      let () = loginfo (Printf.sprintf "sx %d - final-mesh-from-points: body nr %d%!" ix x) in
*)      x > 0)
      (Array.map (fun sx -> surface_classificator sx) all_raw_simplices)
  in
  let () = Printf.printf "final_mesh_from_points: %d simplices\n%!" (Array.length simplices) in
    mesh_from_known_delaunay points_coords simplices                                                           (* call the function which tells the simplices about *)
;;                                                                                                             (* their points and viceversa *)


  (* Note 1: this will NOT destructively modify any points_coords! *)
  (*								*)
  (* Note 2: points close to the boundary will feel no balancing force *)
  (* from the outside, hence be pushed with quite a large force into the *)
  (* boundary! *)

  (*   Note 3: We should anticipate the problem that a badly designed	*)
  (*   visualization subsystem may impose extra restrictions onto program *)
  (*   control flow. In brief, this means that relaxation should make its *)
  (*   internal recursion external, i.e. return a closure which has to be *)
  (*   fed with a command which either is Continue, or Readout, so that *)
  (*   one can get intermediate visualization of what is going on. We handle *)
  (*   this problem via a special "driver" function. *)


type meshgen_engine_command = Mesh_Engine_Do_Extract | Mesh_Engine_Do_Step
(**
   The driver will provide commands to the meshing engine. For now,
   there are only two: Extract an intermediate mesh, and do another
   iteration step. This should be of relevance only to users who want
   to implement an own driver.
 *)

type meshgen_engine_output =
  | Mesh_Engine_Finished_Step_Limit_Reached of (mesh * int)
  | Mesh_Engine_Finished_Force_Equilibrium_Reached of (mesh * float)
  | Mesh_Engine_Can_Continue of (meshgen_engine_command -> meshgen_engine_output)
  | Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue of
      (mesh * (meshgen_engine_command -> meshgen_engine_output))
;;
(**
   A meshing engine will, in some way, contain a "continuation" that
   will take a command and do the next step. The answer produced by
   this continuation is then again a [meshgen_engine].

   Note that in some cases, a [meshgen_engine] will want to provide
   more than just the continuation. For example, if it wants to
   announce to be finished.

   TODO: Note: the type is not appropriate yet. Will have to take care
   of this later on. The thing is not as general as we specify it here!
 *)

type meshgen_engine = (meshgen_engine_command -> meshgen_engine_output)
;;

(* @@@ *)
let make_meshgen_engine
    ?(rng = Mt19937.make 97)
    ?(rod_length = 1.0)
    mdefaults
    immobile_points_coords
    moving_points_coords
    fem_geometry
    =
  let () = loginfo (Printf.sprintf "Making engine - initially, have %d moving points; step max = %d%!" (Array.length moving_points_coords) mdefaults.mdefault_controller_step_limit_max) in (* DDD *)
  let dim = fem_geometry.fem_geo_dim in
  let nearest_neighbours_dim = nearest_neighbours dim in

  let density = fem_geometry.fem_geo_density in
  let boundary_conditions = Array.map snd fem_geometry.fem_geo_boundaries in

  let outer_region_constrained_density x =                                                                         (* the point x is in the outer space *)
    array_all_satisfy (fun bc -> bc x < mdefaults.mdefault_boundary_condition_acceptable_fuzz) boundary_conditions
  in

  let region_constrained_density x =                                                                               (* it returns the density function associated
														      to the object if the point is inside it,
														      0 otherwise *)
    if (array_all_satisfy (fun bc -> bc x > mdefaults.mdefault_boundary_condition_acceptable_fuzz) boundary_conditions)
    then density x
    else 0.0
  in
  let fun_rng = (fun x -> Mt19937.float rng x) in
  let force_fun=mdefaults.mdefault_relaxation_force_fun in                                                         (* function which defines the force between
														      two mobile points within the object *)
  let bforce_fun=mdefaults.mdefault_boundary_node_force_fun in                                                     (* function which defines the force between
														      a  point (also fixed) and a fixed point *)

  let shape_force_scale = mdefaults.mdefault_controller_shape_force_scale in                                       (* parameters of the mesher *)
  let volume_force_scale = mdefaults.mdefault_controller_volume_force_scale in
  let neigh_force_scale = mdefaults.mdefault_controller_neigh_force_scale in
  let irrel_elem_force_scale = mdefaults.mdefault_controller_irrel_elem_force_scale in
  let sliver_correction = mdefaults.mdefault_controller_sliver_correction   in
  let smallest_allowed_ratio = mdefaults.mdefault_controller_smallest_allowed_volume_ratio in
  let controller=mdefaults.mdefault_meshgen_controller in                                                          (* parameters of the controller *)
  let controller_initial_state = mdefaults.mdefault_meshgen_controller_initial in

  let ref_our_points_coords =
    ref (Array.map Array.copy (Array.append immobile_points_coords moving_points_coords)) in
  let inv_rod_length = 1.0 /. rod_length
  and controller_input =                      (* mci: mesher controller input *)
    {mci_bound_pt_between_immobiles = false;  (* boundary point between immobiles *)
     mci_dimension = dim;                     (* dimension of the space *)
     mci_size_last_time_step=0.0;             (* time step *)
     mci_max_node_rel_movement_since_last_triangulation=0.0; (* max ratio [distFromLastTriang/a0 *density^(1/dim)]; controlled by topology_threshold *)
     mci_largest_rel_movement_in_mesh=0.0;                   (* max ratio [stepDist/a0 *density^(1/dim)]; controlled by tolerated_relative_movement *)
     mci_largest_effective_force_in_mesh=0.0;                (* max ratio [|forceVector|/a0 *avgDensity^(1/dim)]; controlled by largest_effective_force *)
     mci_node_avg_density=Array.map (fun _ -> (0.0,0.0)) !ref_our_points_coords; (* (densityRatio, avgForce); controlled by dens<100 and ??? *)
    }

  and scratchpad_vec = Array.make dim 0.0                                                                           (* functions used to overwrite vectors
														     keeping memory of their initial value *)
  and scratchpad_old_coords = Array.make dim 0.0
  (* The two scratchpad values below are used in the local function determine_forces(): *)
  and scratchpad_vec_force_gradient = Array.make dim 0.0
  and scratchpad_vec_force =  Array.make dim 0.0

  in

  let nr_immobile_points = Array.length immobile_points_coords in
  let dim_root_exponent = 1.0 /. (float_of_int dim)
  and grad_dim = symm_grad_storing_result dim in
  let make_points points_coords =                                                        (* initialization of the points *)
    Array.mapi
      (fun ix c -> {coords_start = Array.copy c;
		    coords_here = c;
		    (* NOTE: This is identiclly the same as the
		       corresponding ref_our_points_coords entry! *)
		    force_here =  Array.copy c;
		    sum_abs_forces =0.0;
		    nr_forces=0;
		    density_here = density c;
		    state = (if ix<nr_immobile_points then Immobile else Mobile);        (* the immobile points are stored at the beginning of the list *)
		  })
      points_coords
  in

  let correct_state_boundary_points boundary_points_indices blank_points =               (* function used to set the right state of  boundary points after
											    each retriangulation *)
    Array.iter
      (fun bp_ix -> blank_points.(bp_ix).state <- Boundary)
      boundary_points_indices
  in

  let ref_points = ref (make_points !ref_our_points_coords) in                           (* reference to the initialized vectors - so that we can change them *)

  let classificator =                                                                    (* classificator used to remove bad simplices after each retriangulation *)
    mdefaults.mdefault_make_boundary_condition_simplex_classificator dim  boundary_conditions
  in

  let calculate_topology () =                                                            (* function used to calculate the topology of the triangulation:
											    it consists of two arrays of simplices, the good and the bad ones *)
    nearest_neighbours_dim
      ~simplex_classificator:classificator
      smallest_allowed_ratio
      !ref_our_points_coords  (Array.map (fun p -> p.state) !ref_points)
  in

  let calculate_nn_topology simplices =                                                  (* function used to calculate the topology of the triangulation:
											    a list of arrays where each array represents
											    the connection between the point associated to the index of the
											    array in the list and its neighbours: it is returned in two forms,
											    one way and both ways *)
     extract_topology_from_simplices
       simplices
       !ref_our_points_coords
  in
  let current_simplices, current_irrelevant_simplices = calculate_topology() in          (* calculate topology considering only the relevant simplices
											 based on the decision of the classificator *)
  let current_topology, current_topology_both_ways =
    calculate_nn_topology
      current_simplices in

  let ref_current_topology = ref current_topology in                                     (* references to the current topology and list of simplices *)
  let ref_current_topology_both_ways = ref current_topology_both_ways in
  let ref_current_simplices = ref current_simplices in
  let ref_current_irrelevant_simplices = ref current_irrelevant_simplices in

  let current_topology_elements_total_length =                                           (* number of connections in the mesh *)
    Array.fold_left (fun sum_so_far current_elem -> sum_so_far + Array.length current_elem) 0 !ref_current_topology
  in

  let mirror_on_surfaces_htb = Hashtbl.create current_topology_elements_total_length     (* hashtbl to keep track of the boundary surfaces used
											    as mirroring planes or lines *)
  in
  let ref_sx_vols = ref (Array.init (Array.length (!ref_current_simplices)) (fun i -> 0.0))  (* reference to the array of volumes of the simplices in
											     the mesh: they are all initialized at 0 *)
  in

  (* ----
     Now, first implement all the controller actions
     ---- *)
  let visual_mesh () =                                                                   (* Used for Force_Equilibrium_Reached and Step_Limit_Reached: *)
    let the_mesh =
      mesh_from_points dim
	boundary_conditions
        ~simplex_classificator:classificator
	smallest_allowed_ratio
	(Array.map (fun p -> p.state) !ref_points)
	!ref_our_points_coords
    in
    let () = the_mesh.mm_fem_geometry <- Some fem_geometry in
      the_mesh
  in

  let rec extract_mesh () =
    let nr_points = Array.length (!ref_our_points_coords ) in

    let () = logdebug (Printf.sprintf "Extract mesh - %d points %!" nr_points) in

    let filt = array_filter
      (fun p ->                                                                                        (* only the points with "low" density *)
	let (dens, force) = controller_input.mci_node_avg_density.(p) in dens < 100.                   (* are passed to the final triangulation process *)
      ) (Array.init nr_points (fun i -> i))
    in
    let new_our_points_coords = Array.map (fun i -> !ref_our_points_coords.(i)) filt in
    let () = ref_our_points_coords := new_our_points_coords in

    let new_simplices, new_irrelevant_simplices = calculate_topology() in                              (* new topology *)
    let new_topology, new_topology_both_ways = calculate_nn_topology new_simplices in
    let () = ref_current_topology_both_ways := new_topology_both_ways in
    let () = ref_points :=  make_points !ref_our_points_coords in
    let () = ref_current_simplices := new_simplices in

    let coords_points_to_boundary  = Array.map (fun p -> Array.copy p.coords_here ) !ref_points in

    let sum_immoboundary_neighbours = ref 0 in                                                         (* move all the points with boundary or *)
    let () = for pt_ix = 0 to (Array.length !ref_points)-1 do                                          (* immobile neighbours on the boundary *)
	let () = sum_immoboundary_neighbours := 0 in                                                   (* if the difference between the initial *)
	                                                                                               (* and the final position is small, they are *)
	let move_p = coords_points_to_boundary.(pt_ix) in                                              (* kept in the new position on the boundary *)

	let neighbours_indices = !ref_current_topology_both_ways.(pt_ix) in
	let nr_neighbours = Array.length neighbours_indices in
	let () = for nn = 0 to nr_neighbours-1
	  do
	    let n_ix = neighbours_indices.(nn) in
	    let neighbour = !ref_points.(n_ix) in

	      if neighbour.state = Immobile || neighbour.state = Boundary
	      then
		sum_immoboundary_neighbours := !sum_immoboundary_neighbours + 1
	      else ()
	  done
	in
	  if !sum_immoboundary_neighbours > 0
	  then
	    let () = _enforce_boundary_conditions_reversed                                              (* enforce boundary condition reversed *)
	    mdefaults                                                                                   (* pushes the point on the boundary itself *)
	    scratchpad_vec grad_dim
	    boundary_conditions move_p
	    in
	      ()
	  else
	    ()
      done
    in
    let () = for pt_ix = 0 to (Array.length coords_points_to_boundary)-1 do                             (* keep the new position for the points which *)
	let orig_coords = !ref_points.(pt_ix).coords_here in                                            (* new position is within 1/5 of the local rod lenght *)
	let bound_coords = coords_points_to_boundary.(pt_ix) in
	let orig_point_density = density orig_coords in
	let local_rod =  rod_length /. (orig_point_density ** dim_root_exponent) in
	let distance_from_bound = sqrt (euclidean_len_sq (array_pointwise (-.) orig_coords bound_coords)) in
	  if distance_from_bound < 0.2 *. local_rod
	  then
	    let () = for c = 0 to dim-1 do
	      !ref_points.(pt_ix).coords_here.(c) <- bound_coords.(c);
	      done in
              !ref_points.(pt_ix).state <- Boundary                                                     (* set the "kept" points as boundary points *)
	  else ()
      done
    in


    (* correct the index order for each simplex in order to have positive volume *)
    let scratch_points_coords = Array.make_matrix (dim+1) (dim+1) 0.0 in
    let swap a i j =
      let t = a.(i) in
	begin
	  a.(i) <- a.(j);
	  a.(j) <- t ;
	end
    in
    let () = Array.iter
	( fun sx ->
	  let () =
	    for i=0 to dim do
	      for c=0 to dim-1 do
		scratch_points_coords.(i).(c) <- !ref_points.(sx.(i)).coords_here.(c)
	      done;
	      scratch_points_coords.(i).(dim) <- 1.0
	    done
	  in
	  let det = determinant (dim+1) scratch_points_coords in
	  if det < 0.
	  then
	    swap sx 0 1
	  else ()
	 ) !ref_current_simplices
    in
    let points_coords = Array.map (fun p -> p.coords_here) !ref_points in
    let the_mesh =                                                                                      (* call the final triangulator, which removes the *)
      final_mesh_from_points                                                                            (* slivers on the surface and all the simplices which *)
	dim                                                                                               (* are in the outside of largely concave  objects *)
	boundary_conditions                                                                               (* for instance at the outer connections of the elements *)
	mdefaults.mdefault_controller_smallest_allowed_volume_ratio
	(Array.map (fun p -> p.state)!ref_points)                                                         (* in the 9 rings example *)
	points_coords (!ref_current_simplices) in
    let () = the_mesh.mm_fem_geometry <- Some fem_geometry in
    let () = logdebug (Printf.sprintf "... extracted!%!") in
      the_mesh                                                                                          (* return the final mesh *)

  and retriangulate () =
                                                   (* *)
    let new_simplices, new_irrelevant_simplices = calculate_topology()
    in

    begin

      let center_mass = Array.init dim (fun i -> 0.0) in                                                   (* initialize the center of mass *)

      let count = ref 0 in                                                                                 (* recall bad simplices in the interior *)
      let (new_relevants, irrelevants) =                                                                   (* of the mesh (to avoid holes in the mesh) *)
	array_filter_double
	  (fun sx ->
	    let points_coords_arr = Array.map (fun pt_ix -> !ref_points.(pt_ix).coords_here) sx in
	    let points_states_arr = Array.map (fun pt_ix -> !ref_points.(pt_ix).state) sx in
	    let condition =
	      let () = for j=0 to dim-1 do
		  center_mass.(j) <- center_mass_component j points_coords_arr;
		done
	      in
	      let count_mob_pts = ref 0 in
	      let () = for i=0 to dim do
		  if  points_states_arr.(i) = Mobile
		  then count_mob_pts := 1 + !count_mob_pts
		else ()
		done
	      in
		!count_mob_pts >= (dim-1) || region_constrained_density center_mass > 0.0                   (* if the number of mobile points in *)
	    in                                                                                              (* the simplex or the center of mass is *)
	    let () =                                                                                        (* outside the region defining the object *)
	      if condition                                                                                  (* (within the due error), then the simplex *)
	      then                                                                                          (* is a real irrelevant simplex *)
		let () = count := !count + 1 in
		  ()
	      else ()
	    in
	      condition
	  )
	  new_irrelevant_simplices
      in
      ref_current_simplices := Array.append new_simplices new_relevants;                                     (* append the recovered simplices to the *)
      ref_current_irrelevant_simplices := irrelevants;                                                       (* list of relevant simplices *)

      let new_topology, new_topology_both_ways = calculate_nn_topology !ref_current_simplices in             (* calculate topology *)
      ref_current_topology := new_topology;
      ref_current_topology_both_ways := new_topology_both_ways;

      controller_input.mci_max_node_rel_movement_since_last_triangulation<-0.0;
      for i = 0 to (Array.length !ref_points)-1 do
	let pi = !ref_points.(i) in
	for c = 0 to dim-1 do
	  pi.coords_start.(c) <- pi.coords_here.(c);                                                         (* pi.coords_start: this variable is not used !! *)
	done;
      done;
      ref_sx_vols := Array.init (Array.length (!ref_current_simplices)) (fun i -> 0.0);                      (* reference to the list of simplices *)

    end;

  and mirror_simplices fates =
    let nr_simplices = (Array.length !ref_current_simplices) in
    let () = logdebug (Printf.sprintf "mirror_simplices - nr simplices: %d" nr_simplices) in
                                                                                                                    (* on the boundary of an object when *)
                                                                                                                    (* the internal angles are somewhat *)
                                                                                                                    (* too large (which indicates that *)
                                                                                                                    (* the quality is rather poor) *)

    let filter_points sx_ix vertices =                                                                              (* separate points on the boundary from *)
      let states_arr = Array.map ( fun pt_ix -> !ref_points.(pt_ix).state) !ref_current_simplices.(sx_ix)  in       (* those in the interior of the body *)
      let rec filter_pt pt_ix_so_far bound intern immobiles =
	if pt_ix_so_far = (dim+1) then
	  (bound,intern,immobiles)
	else
	  if states_arr.(pt_ix_so_far) = Boundary                                      (* fill boundary points list *)
	  then
	    filter_pt
	      (pt_ix_so_far+1)
	      (vertices.(pt_ix_so_far)::bound)
	      intern immobiles
	  else if states_arr.(pt_ix_so_far) = Immobile                                 (* fill boundary points list and increase immobile points counter *)
          then
	    filter_pt
	      (pt_ix_so_far+1)
	      (vertices.(pt_ix_so_far)::bound)
	      intern (immobiles+1)                                                     (* fill internal points list *)
	  else if states_arr.(pt_ix_so_far) = Mobile
	  then
	    filter_pt
	      (pt_ix_so_far+1)
	      bound
	      (vertices.(pt_ix_so_far)::intern)
	      immobiles
	  else
	    filter_pt (pt_ix_so_far+1) bound intern immobiles
      in
      filter_pt 0 [] [] 0
    in

    let mirror_point surface_points point dim =                                                   (* mirror the interior point of a bad simplex *)
      let dim_mirror = (Array.length surface_points) in                                           (* with re-pect to the geometrical object (plane, line) *)
	                                                                                          (* defined by the surface points wrt the dimension of the space *)
	if dim_mirror = dim                                                                       (* if the number of points is equal to the *)
	then (* 3D plane *)                                                                       (* dimension of the space, we have a plane as a mirror *)
	  let plane = hyperplane_eqn dim surface_points in
	  let normal = array_one_shorter plane dim in
	  let factor = (Array.fold_left ( +. )
			  plane.(dim)
			  (array_pointwise ( *. )
			     point
			     normal) )
	    /.
			  (euclidean_len_sq normal) in
	  let proj_point = Array.mapi (fun ix ni -> point.(ix) -. factor *. ni) normal  in        (* find the projection of the internal point on the *)
	    array_pointwise (fun po pr -> 2.*.pr -. po) point proj_point                          (* mirror, take the vector between the original point *)
                                                                                                  (* and the projection and copy the vector on the "other" *)
	else if                                                                                   (* side of the mirroring face *)

	    dim_mirror = dim-1 (* only line in 3D space! *)                                       (* we have a line since the number of boundary points *)
	then                                                                                      (* is one less than the dimension of the space *)
	  let p0 = surface_points.(0) in
	  let p1 = surface_points.(1) in
	  let b =                                                                                 (* projection of point on p1-p0 is (p0 + b * (p1-p0)) *)
	    let w = array_pointwise (-.) point p0 in
	    let v = array_pointwise (-.) p1 p0 in
	      (scalar_prod w v) /. (euclidean_len_sq v)
	  in
	  let proj_point = array_pointwise (fun p_1 p_0 -> p_0 +. b *. (p_1-.p_0)) p1 p0 in
	    array_pointwise (fun po pr -> 2.*.pr -. po) point proj_point
	else point
    in

    let () = Hashtbl.clear mirror_on_surfaces_htb in                                              (* empty the hashtable *)

    let rec work sx_ix points_to_add =                                                            (* function to add the mirrored points to the list *)
      if sx_ix = nr_simplices                                                                     (* of mesh points *)

	  (* -------- *)
      then
	if List.length points_to_add > 0
	then
	  begin                                                                                   (* update mesh with new points *)
	    let new_points_total =
	      Array.append (!ref_our_points_coords) (Array.of_list points_to_add) in
	    ref_our_points_coords:= new_points_total;

	    Array.append fates                                                                    (* fates for the new points *)
	      (Array.init (List.length points_to_add)
		 (fun i -> (*let () = logdebug (Printf.sprintf "Bad surface simplex - mirroring internal point ") in (*  GB *) *)
			     Do_Nothing_With_Point));
	  end
	else fates
	  (* -------- *)

      else                                                                                        (* *)
	let vertices = Array.map (fun pt_ix -> !ref_our_points_coords.(pt_ix)) !ref_current_simplices.(sx_ix) in   (* vertices of the current simplex *)
	let center_mass = Array.init dim (fun i ->  0.0 )  in
	let volume = !ref_sx_vols.(sx_ix) in
	let (bound_vcs,intern_vcs,immobiles) = filter_points sx_ix vertices in                                     (* points splitted by type *)

	let boundary_in_htb surf_points =                                                                          (* register set of surface points *)
	  let () = Array.sort compare surf_points in                                                               (* used as mirror in a hashtable *)
	  let already_in = Hashtbl.mem mirror_on_surfaces_htb surf_points in
	  let () =
	    if not already_in
	    then
	      Hashtbl.add mirror_on_surfaces_htb surf_points 1
	    else
	      ()
	  in
	    already_in

	in

	let solid_angle_from_internal_point surface_points vertex_point dim =                                      (* compute solid angle spanned by *)
	  if dim = 2 then                                                                                          (* the surface points as seen by the internal point *)
	    acos ( scalar_prod ( unit_vector (array_pointwise (-.) surface_points.(0) vertex_point)) (unit_vector (array_pointwise (-.)  surface_points.(1) vertex_point)))
	  else if dim = 3 then
	    abs_float(triangle_space_angle_3d vertex_point surface_points.(0) surface_points.(1) surface_points.(2) )
	  else 0.0
	in

	let volume_ratio vertices dim =                                                                             (* idealVol/realVol ratio *)
	  let () = for j=0 to dim-1 do
	      center_mass.(j) <- center_mass_component j vertices;
	    done
	  in
	  let cm_density = max 1.0e-6 (density center_mass) in
	  let rod_scaled = rod_length /. (cm_density ** dim_root_exponent) in
	  let ideal_vol = ideal_vol_sx rod_scaled dim in

	    abs_float(volume /. ideal_vol)
	in

	let node_action_2D surface_pts vertex_point  =                                                              (* action on the nodes to mirror in 2D *)

	  let angle = solid_angle_from_internal_point surface_pts vertex_point 2 in                                 (* if the spanned angle is >0.55 pi *)
          let vertices = Array.append surface_pts [|vertex_point|] in                                               (* and volume ratio > 3e-2 then *)
	    if angle > 0.55*.pi && (volume_ratio vertices 2) > 3e-2                                                 (* mirror internal point *)
	    then let new_point = mirror_point surface_pts vertex_point 2 in
		   [new_point]
	    else
	      []
	in


	let node_action_3D_all surf_points  =                                                                        (* action on the nodes to mirror in 3D *)

          let dist = sqrt (euclidean_distance_sq surf_points.(0) surf_points.(1) ) in                                (* the operation is similar to the 2D case: *)
	  let middle_point = array_pointwise (fun x0 x1 -> 0.5*.(x0+.x1)) surf_points.(0) surf_points.(1) in         (* we consider nrPoints-1 and treat them as *)
	  let middle_pt_density = max 1.0e-6 (density middle_point) in                                               (* it were 3 points in a 2D space *)
	  let middle_pt_rod_length = rod_length /. (middle_pt_density ** dim_root_exponent) in                       (* the condition now is that the distance *)
	    if (dist > 1.2*. middle_pt_rod_length) && (volume_ratio vertices 3) > 3e-2                               (* between the two mirroring points is *)
	      && (outer_region_constrained_density middle_point)                                                     (* larger than 1.2 the rod lenght at their *)
	    then [middle_point]                                                                                      (* middle point and volume ratio > 3e-2*)
	    else []
	in


	(*let node_action_3D surface_pts vertex_point dim  =                                                           (* action on the nodes to mirror in 3D *)

	  let angle = solid_angle_from_internal_point surface_pts vertex_point dim in                                (* all the points on the mirroring surface *)
	  let vertices = Array.append surface_pts [|vertex_point|] in                                                (* are considered and the we use a plane as *)
	    if angle > 0.7*.pi && (volume_ratio vertices 3) > 3e-2                                                   (* the mirror *)
	    then let new_point = mirror_point surface_pts vertex_point 3 in
		   [new_point]
	    else
	      []
	in*)

	let all_couples list =                                                                                       (* function that returns all the couples *)
	  let len = List.length list in                                                                              (* out of the elements of a list *)
	  let rec add_element_j i j elems_so_far =
	      if j = len
	      then elems_so_far
	      else add_element_j i (j+1) ([(List.nth list i);(List.nth list j)]::elems_so_far)
	  in
	  let rec add_element_i i elems_so_far =
	      if i = len
	      then elems_so_far
	      else add_element_i (1+i) (add_element_j i (i+1) elems_so_far)
	  in
	    add_element_i 0 []

	in

	  if dim = 2                                                                               (* 3 nodes to deal with *)
	  then
	    if ( List.length bound_vcs = dim+1)                                                    (* 3 nodes to run on *)
	    then
	      let nodes_to_shift = ref bound_vcs in
	      let rec add_elements ix elems_so_far =
		if ix = dim+1
		then elems_so_far
		else
		  let surf_points = Array.of_list (List.tl !nodes_to_shift) in                     (* apply node_action_2D to all the points in turn *)
		  let () = Array.sort compare surf_points in                                       (* cycling on them; if the point is inserted and the *)
		  let vert_point = List.hd !nodes_to_shift in                                      (* boundary appears for the first time, then add point *)
		  let add_elem =  node_action_2D surf_points vert_point in                         (* to the mesh *)
                  let () = nodes_to_shift := shift_list_on_the_left (!nodes_to_shift) in
		    if (List.length add_elem) > 0 && not (boundary_in_htb surf_points)
		    then
		      add_elements (1+ix) (add_elem@elems_so_far)
		    else add_elements (1+ix) elems_so_far
	      in
		work (sx_ix+1) ((add_elements 0 [])@points_to_add )

	    else if ( List.length bound_vcs = dim) && (List.length intern_vcs = 1)                 (* there is only a boundary to consider, so that  *)
	    then                                                                                   (* we run the node_action_2D only once *)
	      let surf_points = Array.of_list bound_vcs in
	      let () = Array.sort compare surf_points in
	      let vert_point = 	List.hd intern_vcs in
	      let add_elem =  node_action_2D surf_points vert_point in
		if (List.length add_elem) > 0 && not (boundary_in_htb surf_points)
		then
		    work (sx_ix+1) (add_elem@points_to_add )
		else work (sx_ix+1) points_to_add
	    else                                                                                    (* no boundary points: go to the next simplex *)
	      work (sx_ix+1) points_to_add

(* ---------------------------------------------------------------- *)

	  else if dim = 3
	  then
	    if ( List.length bound_vcs >=2)                                                          (* add the middle point between all the couples of boundary *)
	    then                                                                                     (* points with the properties of node_action_3D_all *)
	      let all_possible_lines = all_couples bound_vcs in
	      let len = List.length all_possible_lines in
	      let rec list_loop ix elems_so_far =
		if ix = len
		then elems_so_far
		else
		  let surf_points = Array.of_list (List.nth all_possible_lines ix) in
		  let add_elem = node_action_3D_all surf_points in
		    if not (boundary_in_htb surf_points)
		    then list_loop (1+ix) (add_elem@elems_so_far)
		    else list_loop (1+ix) elems_so_far
	      in
(*            **this part of the function introduces to many points so far!!**
              let further_point =
		if ( List.length bound_vcs =3) && ( List.length intern_vcs =1)
		then
		  node_action_3D (Array.of_list bound_vcs) (List.hd intern_vcs) 3
		else []
	      in
		work (sx_ix+1) ((list_loop 0 [])@further_point@points_to_add)
*)
		work (sx_ix+1) ((list_loop 0 [])@points_to_add)
	    else
	      work (sx_ix+1) points_to_add

	  else (* the conditions to mirror are not met *)
	    work (sx_ix+1) points_to_add
    in
      work 0 [] ;

(* ------------------------------------------------------------ *)

    (* We will only consider the part of point_fates corresponding to non-immobile points.
       XXX Note: this will also make all pinned nodes unpinned. I think this is okay,
       but we should document that.
     *)
  and change_points point_fates =

    let add_random_point_close_to ix_src moving_points_so_far =
      let original_point_coords = !ref_our_points_coords.(ix_src) in
      let effective_rod_length_here =                                             (* effective rod lenght in the current position of the point *)
	rod_length*.
	  ((density original_point_coords)**(0.0-.dim_root_exponent))
      in
      let newly_added_point =                                                     (* a new point is added close to the current point *)
	gauss_random_n_dim                                                        (* using a N-dim gaussian distribution where the sigma *)
	  ~fun_rng:fun_rng original_point_coords effective_rod_length_here        (* is given by the effective rod lenght and the mean is *)
      in newly_added_point::original_point_coords::moving_points_so_far           (* given by the current position of the point *)
    in                                                                            (* the point is then added to the list of mesh points *)

    let nr_moving_points = (Array.length !ref_our_points_coords)-nr_immobile_points in  (* the introduction or deletion or points occurs *)
                                                                                        (* only using the moving points  *)

    let boundary_points_indices = Hashtbl.create (2*(Array.length !ref_points))  in  (* hashtable to store the boundary points and recover *)
    let nr_ref_points = Array.length !ref_points in                                  (* them after the deletion or insertion of points, when *)
                                                                                     (* the indices will be changed *)
    let rec work ix points_so_far index_corr =
      if ix = nr_moving_points
      then
	let new_points_total =                                                       (* append the new points to the list of current points *)
	  Array.append immobile_points_coords
	    (Array.of_list (List.rev points_so_far))
	in
	  (begin                                                                     (* update the references to the new list of points (to be) *)
	       ref_our_points_coords:=new_points_total;                              (* used by the determine_forces function, etc.) correct the *)
	       ref_points := make_points new_points_total;                           (* state of boundary points (all the points are set as *)
	       correct_state_boundary_points (hashtbl_keys boundary_points_indices) !ref_points; (* mobile after each retriangulation ) and reset *)
	       controller_input.mci_node_avg_density                                             (* the density of each point to be passed to the *)
	       <- Array.map (fun _ -> (0.0,0.0)) !ref_our_points_coords;                         (* controller *)
	   end)
      else
	match point_fates.(ix) with                                                   (* depending on the point_fates flag (given by the *)
	  | Do_Nothing_With_Point ->                                                  (* mdefault_controller_handle_point_density_fun function *)
	      let () =                                                                (* when called on the input.mci_node_avg_density) the point *)
		if ix < (nr_ref_points-nr_immobile_points) && !ref_points.(nr_immobile_points+ix).state = Boundary (* can be removed, kept or a new *)
		then Hashtbl.add boundary_points_indices (ix+index_corr+nr_immobile_points) 1                      (* point can be inserted nearby *)
		else ()
	      in
		work (1+ix) (!ref_our_points_coords.(nr_immobile_points+ix)::points_so_far) index_corr          (* the index_corr sums 1 to the *)
                                                                                                                (* point index whenever a new point *)
	  | Add_Another_Point ->                                                                                (* is inserted and subtracts 1 when *)
	      let () =                                                                                          (* a point is removed, updating the *)
		if ix < (nr_ref_points-nr_immobile_points) && !ref_points.(nr_immobile_points+ix).state = Boundary (* new position of boundary *)
		then Hashtbl.add boundary_points_indices (ix+index_corr+nr_immobile_points) 1                      (* points *)
		else ()
	      in
		work (1+ix) (add_random_point_close_to (nr_immobile_points+ix) points_so_far) (index_corr+1)

	  | Delete_Point ->
	      work (1+ix) points_so_far (index_corr-1)

    in
    let () = work 0 [] 0 in

    let () = loginfo (Printf.sprintf "Adjusted mobile points nr from %d to %d." nr_moving_points ((Array.length !ref_our_points_coords)-nr_immobile_points)) in
      retriangulate()                                                                  (* retriangulate to have the topology with the new set of nodes*)
  and pin_unpin_nodes _ =
    not_implemented_yet "TODO: implement dynamical node pinning"
      (* @@@ *)
  and determine_forces step_so_far =
    (* We need a helper function here that projects forces on boundary nodes to
       the component parallel to the boundary.
     *)

    let do_project_away_boundary_force force point boundary_condition =
    (* Note: this returns () and modifies scratchpad_vec_force.
       Note that the input force may as well just be scratchpad_vec_force.
    *)
      let val_bc = boundary_condition point in                                                                 (* copy the force on a temporary array
													       if the point is within the object region *)
      if abs_float(val_bc) >= mdefaults.mdefault_boundary_condition_acceptable_fuzz
      then (* No boundary - we are fine. *)
	for i =0 to dim-1 do
	  scratchpad_vec_force.(i) <- force.(i);
	done
      else                                                                                                     (* if the force pushes the point beyond the
													       limit defined by the boundary condition, the
													       force is scaled so that its value is only
													       sufficient to push the point on the boundary,
													       not beyond; such new force is then copied in
													       the temporary array *)
	let () = grad_dim scratchpad_vec_force_gradient boundary_condition point in
	let grad_bc = scratchpad_vec in
	let len_sq_grad = euclidean_len_sq grad_bc in
	let sprod_grad_force = scalar_product grad_bc force in
	let subtract_factor = (0.0-.sprod_grad_force)/.len_sq_grad in
	  for i =0 to dim-1 do
	    scratchpad_vec_force.(i) <- force.(i)-.subtract_factor*.grad_bc.(i);
	  done
    in
      begin
     controller_input.mci_bound_pt_between_immobiles <- false;                                                  (* mci stands for mesher controller input! *)
	                                                                                                        (* set the largest effective force to zero,
														the flag for bounding point between immobile
														points to false *)
     controller_input.mci_largest_effective_force_in_mesh <- 0.0;
      for ix = 0 to (Array.length !ref_points)-1 do
	let p = !ref_points.(ix) in                                                                             (* Clear all force vectors.
														   XXX Note for future optimization:
														   need not do so for fixed or pinned points. *)
	for c = 0 to dim-1 do p.force_here.(c) <- 0.0 done;
	p.nr_forces <-0;
	p.sum_abs_forces<-0.0;
      done;
      for ix = 0 to (Array.length !ref_points)-1 do                                                             (* loop over all the points *)
	let p = !ref_points.(ix) in
	let neighbours_indices = !ref_current_topology.(ix) in                                                  (* get the indices of the neighbours *)
	let nr_neighbours = Array.length neighbours_indices in
	for nn = 0 to nr_neighbours-1 do
	  let n_ix = neighbours_indices.(nn) in
	  let neighbour = !ref_points.(n_ix) in
	  if p.state = Immobile                                                      (* Force between Immobile points *)
	                                                                             (* if the neighbour is immobile no
											need to calculate the repelling force *)
	      && neighbour.state = Immobile
	  then ()
	  else
	    let avg_density =
	      ((p.density_here +. neighbour.density_here))*. 0.5 in
	                                                                            (* Note that it is less work to calculate this than the
										       density at the midpoint. Note also that zero densities
										       here would eventually produce NaN point coordinates! *)
	    let inv_length_scale = inv_rod_length *. (avg_density ** dim_root_exponent)                          (* value used for the calculation
														    of the largest effective force *)
	    in
	    begin
                                                                                    (* Initialize our scratchpad vector to the distance vector *)
	      for i = 0 to dim-1 do
		scratchpad_vec.(i)
		<- neighbour.coords_here.(i) -. p.coords_here.(i)
	      done;
	      let true_dist = sqrt(euclidean_len_sq scratchpad_vec) in
	      let rel_force =	                                                    (* the reduced distance; the force function is applied to this quantity *)
		true_dist *.inv_length_scale
	      in
	      let force =
		(if neighbour.state = Immobile || p.state = Immobile || neighbour.state = Boundary || p.state = Boundary
		  then                                                              (* the force between a mobile point and an immobile or a boundary point
										    is different from the one between two mobile points; the immobile points
										    have the same treatment, because they can move along the boundary *)
		    bforce_fun
		  else force_fun                                                    (* force between two mobile points *)
		) rel_force in

	      let () =                                 		                    (* Update statistics on forces... *)
		begin
		  p.sum_abs_forces <- p.sum_abs_forces +. neigh_force_scale *. force;
		  p.nr_forces <- 1+ p.nr_forces;
		  neighbour.sum_abs_forces <- neighbour.sum_abs_forces +. neigh_force_scale *. force;
		  neighbour.nr_forces <- 1+ neighbour.nr_forces;
		end
	      in
	      if force = 0.0
	      then ()	(* Nothing to do *)
	      else
		(* We have to add a force contribution.

		   We already have a vector with the right direction in
		   scratchpad_vec. If we scale it by 1/true_dist,
		   it will be a unit vector pointing towards our neighbour.

		   What we then have is a relative force. If we re-scale this
                   by length_scale (= 1/inv_length_scale), we get the proper
		   force for a long rod.

		   But of course, it has to point away from our neighbour.
		 *)
		begin
		  let force_factor = (0.0-.force)                                   (* -(true_dist*.inv_length_scale) *)
		  in
		  for i = 0 to dim-1 do
		    let force_contrib_coord_i =                                     (* add the force scaled with the neighbour force scale
										    to the list of forces acting on this node as well as
										    its neighbour (inverted in direction ???) *)
		      neigh_force_scale *.force_factor *. scratchpad_vec.(i)
		    in
		      p.force_here.(i)
		      <- p.force_here.(i) +. force_contrib_coord_i;
		      neighbour.force_here.(i)
		      <- neighbour.force_here.(i) -. force_contrib_coord_i;
		  done;
		end
	    end
	done; (* with neighbours *)
      done; (* with points *)


(* ######################### AUXILIARY FUNCTIONS ############################ *)

      let initial_relaxation_weight =                                                (*   weight used to limit the contribution from shape and
											  volume forces at the beginning of the mesh relaxation *)
	mdefaults.mdefault_initial_relaxation_weight
	  step_so_far
	  mdefaults.mdefault_controller_initial_settling_steps
	  0.
	  1.
      in
      let pct mat =                                                                  (* principal component transformation -> eigen vectors + eigen values *)
        let gsl_s = Gsl_vector.create dim in
        let gsl_work = Gsl_vector.copy gsl_s in
        let s = `V gsl_s in
        let work = `V gsl_work in

	let gsl_a = Gsl_matrix.of_arrays mat in
	let gsl_v = Gsl_matrix.create dim dim in
	let a = `M gsl_a in
	let v = `M gsl_v in

	let () = Gsl_linalg._SV_decomp a v s work in

	let eigen_vects_mat = (Gsl_matrix.to_arrays ((fun (`M x) -> x) a )) in

(*	let mat_u = Gsl_matrix.to_arrays ((fun (`M x) -> x) a ) in
	let mat_v = Gsl_matrix.to_arrays ((fun (`M x) -> x) v ) in
	RRR
*)
	let eigen_vals_vec = Gsl_vector.to_array ((fun (`V x) -> x) s ) in

	(eigen_vects_mat, eigen_vals_vec)
      in


(* ############################ SOLID ANGLES ############################## *)

      (* for each node a hash table will contain the
	 index of simplices connected to the node *)

      let pt_belongs_to =                                                                             (* for each node a hash table will contain the
													 index of simplices connected to the node *)
	Array.init (Array.length !ref_points) (fun _ -> Hashtbl.create
	  (2 * int_of_float (float_factorial (dim+1))))  in

      let solid_angle =                                                                               (* in 2D and 3D for each node a hash table will
													 contain the solid angle covered by each of the
													 simplices it belongs to; it will be used for
													 the computation of the equivalent voronoi volume
													 associated to the node; in 1D or higher dimension
													 such a volume uses a different kind of heuristic
													 estimation, considering the number of neighbours
													 instead of their angular extension. *)
	Array.init (Array.length !ref_points) (fun _ -> Hashtbl.create
	  (2 * int_of_float (float_factorial (dim+1)))) in

      let solid_angle_per_pt = Array.init (Array.length !ref_points) (fun i -> 0.0) in                (* total solid angle covered by each node *)

      let nr_vertices = dim+1 in                                                                       (* initialise the volume matrix *)
      let vol_mat = Array.make_matrix nr_vertices nr_vertices 0.0 in

      let vertices = Array.make_matrix nr_vertices dim 0.0 in                                          (* initialise the various arrays
													 (to be used in each simplex) *)
      let nodes = Array.make_matrix nr_vertices dim 0.0 in

      let all_raw_simplices = !ref_current_simplices in                                               (* references to the current good and bad simplices *)
      let all_raw_irrelevant_simplices = !ref_current_irrelevant_simplices in

      let shape_force_transf = Array.make_matrix dim dim 0.0 in                                       (* initialization of the arrays used to store
												      the shape and volume forces *)
      let shape_forces = Array.make_matrix nr_vertices dim 0.0 in
      let irrelev_sx_shape_forces = Array.make_matrix nr_vertices dim 0.0 in

      let vol_forces = Array.make_matrix nr_vertices dim 0.0 in

      let center_mass = Array.init dim (fun i ->  0.0 )   in                                          (* initialization of the center of mass and normal ??? *)
      let normal = Array.init dim (fun i ->  0.0 ) in
      let sx_coords = Array.make_matrix nr_vertices dim 0.0 in                                         (* initialization of the simplex coordinates *)

      let () = for sx_ix=0 to (Array.length (!ref_current_simplices))-1 do                            (* loop over all the simplices *)
	let pts_ix = (!ref_current_simplices).(sx_ix) in                                              (* indices of nodes for the current simplex *)

	let () = for i=0 to nr_vertices-1 do                                                           (* add the current simplex index in the
													 hash table of each of its nodes *)
	    Hashtbl.add pt_belongs_to.( pts_ix.(i)) sx_ix 1;
	  for j=0 to dim-1 do                                                                         (* store the coordinates of the points in
													 the vertices array *)
            vertices.(i).(j) <- (!ref_points).(pts_ix.(i)).coords_here.(j)
	  done
	done
	in

	let () = for j=0 to dim-1 do                                                                  (* compute center of mass of the simplex *)
	    center_mass.(j) <- center_mass_component j vertices;
	  done
	in

	let cm_density = max 1.0e-6 (density center_mass) in                                          (* density at the center of mass *)
	let rod_scaled = rod_length /. (cm_density ** dim_root_exponent) in                           (* rod lenght scaled wrt the density at CM *)
	let ideal_vol = ideal_vol_sx rod_scaled dim in                                                (* ideal volume of the simplex with the
												      edges equal to the ideal rod lenght at the CM *)

	let nodes_to_shift = ref vertices in                                                          (* reference to the nodes: it will be used to
												      compute the solid angle spanned by the simplex
												      looking from each of its nodes *)

	let () = for i=0 to (nr_vertices-1) do                                                         (* build the volume matrix *)
	  let () = for j=0 to (dim-1) do
	    vol_mat.(i).(j) <- vertices.(i).(j)
	  done in
	  vol_mat.(i).(dim) <- 1.0
	done
	in

	let fact_dim = float_factorial dim in                                                         (* save the volume of the simplex
													 in the !ref_sx_vols array *)
	let volume = abs_float ((determinant (dim+1) vol_mat)/.(fact_dim)) in

	if is_obj_nan volume  || (volume/.ideal_vol < 1e-2)                                           (* hack to avoid volumes calculated with a
													 division by zero in the computation
													 of the determinant *)
	then
	  !ref_sx_vols.(sx_ix) <- 0.0                                                                 (* the angles are not computed in this case *)
	else

	  let () = !ref_sx_vols.(sx_ix) <- volume in

	  for i=0 to (nr_vertices-1) do                                                                (* compute the solid angle spanned by the
													 current simplex from each node of the simplex:
													 ND -> ang = 1.0 ( to be careful!! )
													 1D -> ang = pi,
													 2D -> ang = arccos(r1 dot r2),
													 3D => ang = triangle_space_angle_3d in snippets
													 where ri is a vector connecting the node under
													 examination to one of the others of the same simplex *)

	    let alpha =
	      if dim = 2 then
		acos ( scalar_prod ( unit_vector (array_pointwise (-.) !nodes_to_shift.(1) !nodes_to_shift.(0))) (unit_vector (array_pointwise (-.) !nodes_to_shift.(2) !nodes_to_shift.(0))))
	      else if dim = 3 then
		abs_float (triangle_space_angle_3d !nodes_to_shift.(0) !nodes_to_shift.(1) !nodes_to_shift.(2) !nodes_to_shift.(3))
	      else if dim = 1 then pi
 	      else 1.0
	    in
	    let () = nodes_to_shift := shift_array_on_the_left (!nodes_to_shift) in                    (* loop over the cycling of the nodes in the array *)
	    if is_obj_nan alpha                                                                        (* the angle could be (???) a not_a_number quantity *)
	    then
	      ()
	    else                                                                            	       (* update information for solid_angle hashtable *)
	      Hashtbl.add solid_angle.(pts_ix.(i)) alpha 1
	  done;

      done (* for sx_ix=0 to *)
      in

(* ################# DENSITY FROM VORONOI ############################## *)                            (* Update of the point density *)

      let () = for pt_ix = nr_immobile_points to (Array.length !ref_points)-1 do                       (* loop over the mobile points *)
	let p = !ref_points.(pt_ix) in

	let () = match p.state                                                                         (* print state, coordinates and total
												       force acting on each node *)
	  with
	    | Mobile -> logdebug (Printf.sprintf "%d Mobile: %s -- neigh forces:%s %!"  pt_ix
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)  )
	    | Immobile -> logdebug (Printf.sprintf "%d Immobile: %s -- neigh forces:%s %!"  pt_ix
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)  )
	    | Boundary -> logdebug (Printf.sprintf "%d Boundary: %s -- neigh forces:%s %!"  pt_ix
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)  )
	    | Pinned -> logdebug (Printf.sprintf "%d Pinned: %s -- neigh forces:%s %!"  pt_ix
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)  )
	in
	let sx_ixs = hashtbl_keys pt_belongs_to.(pt_ix) in                                              (* for the current node extract the
													list of simplices it belongs to *)
	let () = logdebug (Printf.sprintf "\n solid angles: %s %!"
			      (float_array_to_string (hashtbl_keys solid_angle.(pt_ix)) )) in
	let angles_from_ht = hashtbl_keys solid_angle.(pt_ix) in                                        (* for the current node extract the
													   list of solid angles spanned by the simplices
													   it belongs to *)
	let nr_angles = Array.length angles_from_ht in
	let angle =                                                                                     (* total angle spanned by the simplices
													   containing the current node *)
	  let rec walk ix sum_up_to_now =
	    if ix = nr_angles
	    then sum_up_to_now
	    else walk (ix+1) (sum_up_to_now+.angles_from_ht.(ix))
          in
	  walk 0 0.0
	in

	let () = solid_angle_per_pt.(pt_ix) <- angle in 	                                        (* store total angle spanned by the point *)

	let () = logdebug (Printf.sprintf "corrected angle: %f %!" angle) in

	let nr_sx = Array.length sx_ixs in                                                              (* number of simplices the node belongs to *)

	let voronoi_vol = 	                                                                        (* voronoi volume associated to the node pt_ix
													   - in the case of a corner point we have nr = 1
													   and we set the voronoi volume to sxVol/dim,
													   a bit larger than the other ones, which are
													   sumSxVols/(dim+1) from having 4 nodes in a
													   3D simplex, so that the node has less probability
													   to be removed because its density is a bit
													   smaller than the real one
													*)
	  if nr_sx = 1
          then
            !ref_sx_vols.(sx_ixs.(0))/.(float_of_int (dim))
          else
	    let rec walk ix sum_up_to_now =
	      if ix =  nr_sx
	      then sum_up_to_now
	      else walk (ix+1) (sum_up_to_now+.(!ref_sx_vols.(sx_ixs.(ix)))/.(float_of_int (dim+1)) )
            in
	    walk 0 0.0
	in

	let sum_immobile_neighbours = ref 0 in	                                                       (* if the point is a boundary point,
													  count number of neighbours whose state
													  is Immobile; if they are two or more,
													  remove the point because it could
													  affect the integrity of the mesh
													  (it could be inserted at the interface
													  with a previous meshed object whose
													  connectivity doesn't include this point)
												       *)
	let () =
	  if p.state = Boundary
	  then
	    let neighbours_indices = !ref_current_topology_both_ways.(pt_ix) in

	    let nr_neighbours = Array.length neighbours_indices in
	    for nn = 0 to nr_neighbours-1
	    do
	      let n_ix = neighbours_indices.(nn) in
	      let neighbour = !ref_points.(n_ix) in
	      if neighbour.state = Immobile
	      then sum_immobile_neighbours := !sum_immobile_neighbours + 1
	      else ()
	    done
	  else ()
	in

	let eff_rod_length = rod_length *. ((density  p.coords_here)**(0.0-.dim_root_exponent)) in  	(* we use the volume of a sphere
													   with radius = 1/2*effective_rod_length
													   (where the effective rod lenght is
													   the rod lenght scaled with the density
													   on the point)  as reference volume *)
	let ideal_local_vol = (volume_d_sphere dim)*.(eff_rod_length*.0.5)**(float_of_int dim) in

	let voronoi_equivalent  =                                                                  	(* introduces the correction to the voronoi volume
													   from the angle spanned by the simplices it belongs
													   to: this correction is necessary to assign a correct
													   volume to the nodes on the boundaries *)

	  if !sum_immobile_neighbours > 1                                                               (* if the point is a boundary point and has
													   two or more neighbours which are
													   immobile points, set its volume to a
													   very small value so that the density is very high
													   and it is very likely to be removed *)
	  then
	    let () = controller_input.mci_bound_pt_between_immobiles <- true in
	    1e-4 *. ideal_local_vol (* set a tiny voronoi volume (high density)
					  so that the node will be deleted *)

	  else if angle = 0.0                                                                           (* the classificator killed or set as belonging
													   to Body_Nr 0 (???) all the simplices sharing
													   this node: let's kill also the node then! *)
	  then (* set a tiny voronoi volume (high density) so that the node will be deleted *)
	    1e-4 *. ideal_local_vol
	  else
	    let corr_factor =                                                                           (* the correction factor depends on the dimension  *)
	      if dim = 2                                                                                (* of the space:				   *)
	      then 2.*.pi /. angle                                                                      (*   - 1D: 1.5 (all the points are on the surface) *)
	      else if dim = 3                                                                           (*   - 2D: 2pi/angle				   *)
	      then 4.*.pi /. angle                                                                      (*   - 3D: 4pi/angle				   *)
	      else if dim =1 then 1.5                                                                   (*   - ND: (dim+1)!/(nr of simplices)		   *)
	      else  (float_factorial (dim+1)) /. (float_of_int nr_sx) (* dim > 3 *)                     (*         (the way a sphere volume scales with	   *)
	    in                                                                                          (*          the dimension *)
	    if p.state = Boundary then
	      1.2 *. voronoi_vol *. corr_factor                                                         (* allow higher density at boundaries *)
	    else
	      voronoi_vol *. corr_factor
	  in

	let density_ratio =                                                                             (* density ratio to be used by the *)
	  ideal_local_vol /. voronoi_equivalent                                                         (* node adding-removing algorithm *)
	in

	let average_force =                                                                             (* average sum of *neighbour* forces *)
	  if angle = 0.0                                                                                (* to be passed to the controller *)
	  then 1e4 (* weird case: let's kill the node! *)
	  else if p.nr_forces = 0
	  then 0.0
	  else p.sum_abs_forces /. (float_of_int (p.nr_forces))
	in

	controller_input.mci_node_avg_density.(pt_ix) <- (density_ratio,average_force);
      done
      in


(* ############################ SIMPLEX EVOLVE ############################## *)

      let simplex_evolve sx_ix sx_points points_indices =                                              (* loop over all the simplices *)

	let () = for i=0 to nr_vertices-1 do                                                            (* initialize shape and volume forces *)
	  for j=0 to dim-1 do
	    shape_forces.(i).(j) <- 0.0;
	    vol_forces.(i).(j) <- 0.0;
	  done
	done
	in

	let () = for i=0 to nr_vertices-1 do  	                                                       (* copy vertices to avoid changes of points *)
	  for j=0 to dim-1 do                                                                          (* coordinates *)
	    nodes.(i).(j) <- sx_points.(i).(j);
	  done
	done
	in

	let () = for j=0 to dim-1 do                                                                   (* re-write the coordinates wrt the centre of mass *)
	    center_mass.(j) <- center_mass_component j nodes;
	  done
	in

	let () = for i=0 to nr_vertices-1 do
	  for j=0 to dim-1 do
	    vertices.(i).(j) <- nodes.(i).(j) -. center_mass.(j);
	  done
	done
	in

	let cm_density = max 1.0e-6 (density center_mass) in                     	                (* rod length scaled with the local density *)
	let rod_scaled = rod_length /. (cm_density ** dim_root_exponent) in

        (* build the volume matrix *)
	let () = for i=0 to (nr_vertices-1) do
	  let () = for j=0 to (nr_vertices-2) do
	    vol_mat.(i).(j) <- vertices.(i).(j)
	  done in
	  vol_mat.(i).(nr_vertices-1) <- 1.0
	done
	in


(* ############################ VOLUME-DEP FORCE ############################# *)

	let vol = !ref_sx_vols.(sx_ix) in                                                             	(* volume of the given simplex *)

        let ideal_vol = 1.0 *. ideal_vol_sx rod_scaled dim in                                          	(* ideal volume slightly larger than the correct one,
													   in order to have some pressure on the nodes
													   - set to the volume itself until we have a
													   proper implementation of the force (now is
													   sort of destructive force....)*)

	let vol_force_fact =                                                                  	        (* the volume force_factor is computed taking the *)
	  if vol/.ideal_vol = 0.0                                                                       (* logarithm of the ratio between the simplex volume *)
	  then 0.0                                                                                      (* and the volume of an ideal simplex with the edge *)
	  else                                                                                          (* slightly larger than the effective rod_length *)
 	    let log_vol_ratio = log (ideal_vol/.vol) in                                                 (* on the node *)
	    volume_force_scale *. log_vol_ratio
	in

	let () = for i=0 to nr_vertices-1 do                                                            	(* the volume force is applied along the lines of *)
	  for j=0 to dim-1 do                                                                           (* gravity with a magnitude which is scaled by *)
	    vol_forces.(i).(j) <-                                                                       (* the volume force_factor and the *)
	      initial_relaxation_weight *. vol_force_fact  *. vertices.(i).(j)                          (* initial_relaxation_weight *)
	  done
	done
	in


(* ############################ SHAPE-DEP FORCE ############################# *)

	let cov_mat = outerproduct vertices in                             	                        (* build the matrix a in the definition of  *)
													(* singular value decomposition (SVD)       *)


        (* all the elements should be divided by 1/(N-1) *)
	let division_fact = float_of_int (nr_vertices-1) in                                              (* all the elements should be divided by 1/(N-1) *)
	let () =                                                                                        (* for the definition of variance *)
	  for i=0 to dim-1 do
	    for j=0 to dim-1 do
	      cov_mat.(i).(j) <- cov_mat.(i).(j)/. division_fact
	    done
	  done
	in

	let (m, eigenvals) = pct cov_mat in                                               	        (* eigenvetors matrix and eigenvalues of *)
													(* the rotated simplex *)
	let inv_m = snd (det_and_inv dim m) in

	let product_eigenvals =                                                                 (* compute the shape force matrix in the transformed space; *)
	  Array.fold_left ( *. ) 1.0 eigenvals                                                  (* scale the eigenvalues wrt the dimension of the space *)
	in                                                                                      (* and their product (which is 1 for the ideal case of a *)
	                                                                                        (* sphere) so that the volume of the ellipsoid remains *)
	                                                                                        (* constant *)

	let () =
	  if product_eigenvals = 0.0
	  then ()
	  else
	    for i=0 to dim-1 do
	      let scaled_ev = eigenvals.(i) *. ((1./.product_eigenvals)**(1./. (float_of_int dim)) ) in
	      let log_evals_i = log  scaled_ev in
	      shape_force_transf.(i).(i) <- (0.0-.log_evals_i)                                  (* fill the shape force matrix in the transformed space *)
	    done
	in

	(* the force matrix is applied to each node and weighted
	   with the shape_force-scale and the initial_relaxation_weight
	   only forces perpendicular to the lines of gravity are considered
	 *)
	let shape_force_orig_basis = mx_mult m (mx_mult shape_force_transf inv_m) in            (* the shape force matrix is tranformed back in the
												out-of-axis (initial) *)

	  let corr_for_vol offset =                                                             (* correction to deal with sliver-like simplices: *)
	    if vol/.ideal_vol = 0.0	                                                        (* the force increases with the log(idealVol/realVol) *)
	    then 0.0                                                                            (* in that case *)
	    else
 	      max 1.0 ( offset +.(log (ideal_vol/.vol)))
	  in
	  let vol_corr =
	    if dim <= 2 then corr_for_vol 0.
	    else corr_for_vol 1.
	  in
	let () = for i=0 to nr_vertices-1 do                                                     (* the shape force is applied to each node; only *)
	  let orig_shape_force_on_node_i = mx_x_vec shape_force_orig_basis vertices.(i) in      (* its perpendicular component wrt the line of *)
	  let norm_i = norm vertices.(i) in                                                     (* gravity is used because the volume of the simplex *)
	  let () = for j =0 to dim-1 do                                                         (* would be changed if all the force is considered *)
	    normal.(j) <- vertices.(i).(j)/.norm_i
	  done in
	  let proj = scalar_prod orig_shape_force_on_node_i normal in

	  let angular_force_on_node_i =
	    array_pointwise (fun no fo -> fo -. no *. proj) normal orig_shape_force_on_node_i in

	  let () = for j=0 to dim-1 do
	      shape_forces.(i).(j) <-
		initial_relaxation_weight *.  vol_corr *.  shape_force_scale*.angular_force_on_node_i.(j); (* the force is scaled with the initial *)
	    done                                                                                           (* relaxation weight and the shape_force_scale *)
	  in                                                                                               (* the vol_corr increase the strenght in slivers *)

	    if proj > 0.0 && sliver_correction > 0.0                                                       (* if the sliver_correction parameter is > 0 *)
	    then                                                                                           (* also  a longitudinal (wrt the lines of gravity) *)
	      let longit_force_on_node_i = array_pointwise (-.) orig_shape_force_on_node_i  angular_force_on_node_i in (* component of the shape force is used *)
		for j=0 to dim-1 do
		  shape_forces.(i).(j) <- shape_forces.(i).(j) +. shape_force_scale*.sliver_correction *. initial_relaxation_weight *. (vol_corr-.1.) *. longit_force_on_node_i.(j)
		done
	    else ()
	  done
	in
	  (* return the forces *)
	  (shape_forces, vol_forces)
      in

(* ############################## FORCES ON IRRELEVANT SIMPLICES ################################ *)
                                                                (* *)
      let irrelevant_simplex_evolve sx_ix sx_points points_ix =                                             (* this forces apply to simplices which *)
                                                                                                            (* are classified as belonging to the outer *)
                                                                                                            (* space by the classificator *)
	let () =
	  for i=0 to nr_vertices-1 do
	    for j=0 to dim-1 do                                                                             (* initialize the force *)
	      irrelev_sx_shape_forces.(i).(j) <- 0.0;
	    done
	    done
	in
   	let points_to_consider =                                                                            (* the points we are interested in are *)
	  let rec find_points so_far pos =                                                                  (* only those which are mobile, the application *)
	    if so_far = nr_vertices                                                                          (* of this force on the others is useless *)
	    then
	      Array.of_list pos
	    else if (!ref_points.(points_ix.(so_far)).state) = Mobile
	    then find_points (1+so_far) (so_far::pos)
	    else find_points (1+so_far) pos
	  in
	    find_points 0 []
	in
	let len_ptc = Array.length points_to_consider in
	  if len_ptc = 0
	  then  irrelev_sx_shape_forces
	  else
	    let () = for i=0 to nr_vertices-1 do                                                           (* copy vertices to avoid changes of points *)
		for j=0 to dim-1 do                                                                       (* to avoid changes of coordinates *)
		  nodes.(i).(j) <- sx_points.(i).(j);

		done
		done
	    in

	    let () = for j=0 to dim-1 do                                                                  (* re-write the coordinates wrt the centre of mass *)
		center_mass.(j) <- center_mass_component j nodes;
	      done
	    in

	    let () = for i=0 to len_ptc-1 do                                                              (* identify the bad nodes located inside an object *)
		for j=0 to dim-1 do                                                                       (* exert a contracting force between the node and *)
		  irrelev_sx_shape_forces.(points_to_consider.(i)).(j) <-                                 (* the centre of mass *)
		    irrel_elem_force_scale *. (center_mass.(j)-. nodes.(points_to_consider.(i)).(j))
		done
		done
	    in
	      irrelev_sx_shape_forces
      in

(* ############################## ADD FORCES ################################ *)

      let force_total = Array.init (Array.length !ref_points) (fun i -> Array.make dim 0.0)               (* create a vector for the collection of total*)
      in                                                                                                  (* forces *)

      for sx_ix=0 to (Array.length all_raw_simplices)-1 do                                                (* fill the force vector with force from shape *)
                                                                                                          (* and volume *)
      	let sx_vx_ix = all_raw_simplices.(sx_ix) in                                          (* list of point indices of the current simplex' nodes *)

	let () = for i=0 to nr_vertices-1 do                                                  (* list of coordinates of the nodes *)
          for j=0 to dim-1 do
            sx_coords.(i).(j) <- (!ref_points).(sx_vx_ix.(i)).coords_here.(j)
          done
	done
	in

	  if shape_force_scale > 0.0 then                                                    (* forces on the nodes (volume one unused) *)
	    let (shape_force, vol_force) =
	      simplex_evolve sx_ix sx_coords  sx_vx_ix
	    in

	      for vx_ix=0 to (Array.length sx_vx_ix)-1 do                                    (* update forces on the nodes *)
		let pt_ix = sx_vx_ix.(vx_ix) in
		  for c=0 to dim-1 do                                                        (* add the shape and the volume forces to the *)
		    force_total.(pt_ix).(c) <-                                               (* corresponding node in the mesh *)
		      force_total.(pt_ix).(c) +. shape_force.(vx_ix).(c) +. vol_force.(vx_ix).(c)
		  done
		  done
	  else ()
      done;

      if irrel_elem_force_scale > 0.0                                                        (* use of irrelevant forces to remove bad simplices *)
      then                                                                                   (* on the boundary of meshes characterized by concave objects *)
	for sx_ix=0 to (Array.length all_raw_irrelevant_simplices)-1 do

      	let sx_vx_ix = all_raw_irrelevant_simplices.(sx_ix) in                               (* list of point simplices of the current simplex' nodes *)
	let () = for i=0 to nr_vertices-1 do                                                  (* list of the coordinates of the nodes *)
          for j=0 to dim-1 do
            sx_coords.(i).(j) <- (!ref_points).(sx_vx_ix.(i)).coords_here.(j)
          done
	done
	in
	let force_from_irr = irrelevant_simplex_evolve sx_ix sx_coords sx_vx_ix  in
	  for vx_ix=0 to (Array.length sx_vx_ix)-1 do

	    let pt_ix = sx_vx_ix.(vx_ix) in
	      for c=0 to dim-1 do                                                            (* add the forces from irrelevant simplices to the *)
		force_total.(pt_ix).(c) <-                                                   (* corresponding node in the mesh *)
		  force_total.(pt_ix).(c) +. force_from_irr.(vx_ix).(c)
	      done
	      done;
	      done
      else
      () ;

      for ix = 0 to (Array.length !ref_points)-1 do                                          (* scale the node contribution from the neighbour and *)
	let p = !ref_points.(ix) in                                                          (* add the other forces *)
	  if dim = 2 && solid_angle_per_pt.(ix) < 0.75*.pi                                   (* near the corners the only force comes from the neighbours *)
	  then
	    ()
	  else if dim = 3 && solid_angle_per_pt.(ix) < 0.85*.pi                              (* near the corners the only force comes from the neighbours *)
	  then
	    ()
	  else
	for c = 0 to dim-1 do                                                                (* add forces to the nodes *)
	  p.force_here.(c) <- p.force_here.(c) +.  force_total.(ix).(c)
	done;
      done;

      let () = logdebug (Printf.sprintf "NUMBER OF POINTS: %d" (Array.length !ref_points)) in

(* ################# LARGEST EFFECTIVE FORCE ############################## *)

      for i = 0 to (Array.length !ref_points)-1 do                                          (* now we still have to determine the *)
	let p = !ref_points.(i) in                                                          (* largest effective force in the mesh *)
(*	let angle = solid_angle_per_pt.(i) in

	let () = match p.state
	  with
	    | Mobile -> logdebug (Printf.sprintf "%d Mobile: %s -- total forces:%s  angle:%f %!"  i
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)   angle)
	    | Immobile -> logdebug (Printf.sprintf "%d Immobile: %s -- total forces:%s  angle:%f %!"  i
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)   angle)
	    | Boundary -> logdebug (Printf.sprintf "%d Boundary: %s -- total forces:%s  angle:%f %!"  i
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)   angle)
	    | Pinned -> logdebug (Printf.sprintf "%d Pinned: %s -- total forces:%s  angle:%f %!"  i
		(float_array_to_string p.coords_here)  (float_array_to_string p.force_here)   angle)
	in


*)
	let inv_length_scale = inv_rod_length *. ((density p.coords_here)** dim_root_exponent) in
	begin
	  for k=0 to dim-1 do
	    scratchpad_vec_force.(k) <- p.force_here.(k);
	  done;
	  for k=0 to (Array.length boundary_conditions)-1 do
	    do_project_away_boundary_force                                             (* bring back on the object surface the nodes which moved outside *)
	      scratchpad_vec_force p.coords_here boundary_conditions.(k);
	  done;
	  let effective_force=sqrt(euclidean_len_sq scratchpad_vec_force) *.           (* compute the maximum effective force to be used by the controller *)
	  (* HHH *) inv_length_scale
	  in
	  if effective_force > controller_input.mci_largest_effective_force_in_mesh    (* the force is the sum of all of the forces introduced above *)
	  then controller_input.mci_largest_effective_force_in_mesh <- effective_force
	  else ()
	end
      done;
    end

  and time_step dt =
    begin
      controller_input.mci_size_last_time_step<-dt;                                               (* initializes the controller parameters *)
      controller_input.mci_max_node_rel_movement_since_last_triangulation <- 0.0;
      controller_input.mci_largest_rel_movement_in_mesh <- 0.0;
      (* Have to update all point coordinates, local density information,
	 and iterate.
       *)
      let new_coords = Array.init dim (fun i -> 0.0) in                                           (* initializes the new coordinates *)
      for i = 0 to (Array.length !ref_points)-1 do
	let p = !ref_points.(i) in
	if p.state = Immobile                                                                     (* don't relax the Immobile points *)
	then ()
	else                                                                                      (* update node coordinates if it is Mobile *)
	  let coords = p.coords_here                                                              (* or Boundary (can move along the boundary) *)
	  and force = p.force_here in
	    begin
	      let () =
		for j=0 to dim-1 do                                                               (* copy the current coordinates in the *)
		  scratchpad_old_coords.(j) <- coords.(j);                                        (* scratchpad and set the new coordinates *)
		  new_coords.(j) <- coords.(j) +. force.(j) *. dt                                 (* summing the force*dt to the current ones *)
		done
	      in
	      let keep_boundary_points_from_going_back_inside =                                   (* if the new position of boundary points is *)
		p.state = Boundary &&                                                             (* farther from the boundary, keep the current *)
		array_all_satisfy (fun bc -> bc new_coords <= 1. *. (bc coords) ) boundary_conditions (* position of the node, update it viceversa *)
	      in
		(* GB *)
		if p.state <> Boundary || keep_boundary_points_from_going_back_inside
		then
		  for j=0 to dim-1 do
		    coords.(j) <- new_coords.(j) ;
		  done
		else
		  ();

	    _enforce_boundary_conditions                                                           (* push back on the boundary the nodes *)
	      mdefaults                                                                            (* which go beyond them *)
	      scratchpad_vec grad_dim
	      boundary_conditions p;
	    p.density_here <- density coords;
	    (
	     let displacement = sqrt(euclidean_distance_sq coords scratchpad_old_coords) in        (* compute the displacement of the node *)
	     let rel_displacement = displacement/.rod_length *.(p.density_here**(dim_root_exponent)) in (* and compute the relative displacement *)
                                                                                                   (* to be given to the controller for the next *)
	     if rel_displacement > controller_input.mci_largest_rel_movement_in_mesh               (* command to perform *)
	     then controller_input.mci_largest_rel_movement_in_mesh <- rel_displacement
	     else ()
	    );
	  end
      done;
      for ix = 0 to (Array.length !ref_points)-1 do                                                (* Update information about largest movement *)
	let p = !ref_points.(ix) in
	let rel_dist = sqrt(euclidean_distance_sq p.coords_here p.coords_start) *.
	    (p.density_here ** (dim_root_exponent)) /. rod_length in
	  if rel_dist <=
	    controller_input.mci_max_node_rel_movement_since_last_triangulation
	  then ()
	  else
	    controller_input.mci_max_node_rel_movement_since_last_triangulation <- rel_dist;
      done;
    end
  in

(* Okay, now we have all the actions we want. *)

  let do_command cmd =
    match cmd with
    | Force_Equilibrium_Reached f ->                                                   (* the time_step 0.0 call is needed to enforce boundary points *)
	let () = logdebug (Printf.sprintf "Force_Equilibrium_Reached f - %d mobile points %!" (Array.length !ref_our_points_coords - nr_immobile_points) ) in
	let () = time_step 0.0 in
	Some (cmd,extract_mesh())
    | Step_Limit_Reached iteration_nr ->
	let () = logdebug (Printf.sprintf "Step_Limit_Reached iteration_nr - %d mobile points %!" (Array.length !ref_our_points_coords - nr_immobile_points) ) in
	let () = time_step 0.0 in
	  Some (cmd,extract_mesh())
    | Retriangulate iteration_nr ->
	begin
	  let () = logdebug (Printf.sprintf "Retriangulate - %d mobile points %!" (Array.length !ref_our_points_coords - nr_immobile_points)) in
	    retriangulate();
	    determine_forces iteration_nr;
	    None;
	end
    | Pin_Unpin_Nodes states ->
	let _ = pin_unpin_nodes states in
	None
    | Determine_Forces iteration_nr ->
	let () = logdebug (Printf.sprintf "Determine_Forces iteration_nr - %d mobile points %!" (Array.length !ref_our_points_coords - nr_immobile_points)) in
	let () = determine_forces iteration_nr in
	  None
    | Change_points_and_retriangulate (fates,iteration_nr) ->
	begin
	  let () = logdebug (Printf.sprintf "Change points and retriangulate - %d mobile points %!" (Array.length !ref_our_points_coords - nr_immobile_points) ) in
	  let new_fates = mirror_simplices fates in
	  let () = logdebug (Printf.sprintf "New fates nr: %d" (Array.length new_fates)) in
	    change_points (Array.sub new_fates nr_immobile_points
			      (Array.length !ref_our_points_coords - nr_immobile_points));

            determine_forces iteration_nr;
	    None;
	end
    | Time_step (dt,iteration_nr) ->
	let () = logdebug (Printf.sprintf "Time_step - %d mobile points %!" (Array.length !ref_our_points_coords - nr_immobile_points) ) in
	begin
	  time_step dt;
	  determine_forces iteration_nr;
	  None;
	end
  in
  let () = determine_forces 0 in (* It's good to have guartantees these are set up beforehand. *)          (* operations performed on the base of the *)
  let rec engine controller_state driver_command =                                                         (* controller commands *)
    match driver_command with
    | Mesh_Engine_Do_Step ->
	let (new_controller_state,controller_cmd) =
	  controller mdefaults rng controller_state controller_input in
	let executed_command = do_command controller_cmd in
	(
	 match executed_command with
	 | Some (stop_reason,mesh) ->
	     (
	      match stop_reason with
	      | Step_Limit_Reached n ->
		  Mesh_Engine_Finished_Step_Limit_Reached (mesh,n)
	      | Force_Equilibrium_Reached force ->
		  Mesh_Engine_Finished_Force_Equilibrium_Reached (mesh,force)
	      | _ -> impossible()
              (* NOTE: enclosing parens necessary - "dangling match" problem. *)
	     )
	 | None ->
	     Mesh_Engine_Can_Continue (engine new_controller_state)
	)
    | Mesh_Engine_Do_Extract ->
	Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue
	  (visual_mesh(), engine controller_state)
          (* Note that we give the engine back the original controller state. *)
  in
  engine controller_initial_state
;;

let default_driver mge =
  let rec work mge_out_now =
    match mge_out_now with
    | Mesh_Engine_Finished_Step_Limit_Reached _ -> mge_out_now
    | Mesh_Engine_Finished_Force_Equilibrium_Reached _ -> mge_out_now
    | Mesh_Engine_Can_Continue mge_next -> work (mge_next Mesh_Engine_Do_Step)
    | Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue (_,mge_next) ->
	work (mge_next Mesh_Engine_Do_Step)
  in work (mge Mesh_Engine_Do_Step)
;;

let default_gendriver _ = default_driver
;;

(* The bare-bones stepping driver presented here is not really that useful.
   XXX NOTE: I think it should be changed anyway!
 *)
let stepping_driver nr_steps_per_bunch mge =
  let rec work nr_step mge_out_now =
    match mge_out_now with
    | Mesh_Engine_Finished_Step_Limit_Reached _ -> mge_out_now
    | Mesh_Engine_Finished_Force_Equilibrium_Reached _ -> mge_out_now
    | Mesh_Engine_Can_Continue cont ->
	if (nr_step mod nr_steps_per_bunch<>0)|| nr_step = 0
	then (* Just go on *)
	  (work (1+ nr_step) (cont Mesh_Engine_Do_Step))
	else (* Schedule Extraction - do not increase the step number! *)
	  (work nr_step (cont Mesh_Engine_Do_Extract))
    | Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue (_,cont) ->
	(work (1+ nr_step) (cont Mesh_Engine_Do_Step))
  in
  work 0 (mge Mesh_Engine_Do_Step)
;;


let do_every_n_steps_driver nr_steps_per_bunch f mge =
  let rec work nr_step mge_out_now =
    let () = loginfo (Printf.sprintf "do_every_n_steps_driver [%d]" nr_step) in
    match mge_out_now with
    | Mesh_Engine_Finished_Step_Limit_Reached _ -> mge_out_now
    | Mesh_Engine_Finished_Force_Equilibrium_Reached _ -> mge_out_now
    | Mesh_Engine_Can_Continue cont ->
	if (nr_step mod nr_steps_per_bunch<>0) || nr_step = 0
	then (* Just go on *)
	  (work (1+ nr_step) (cont Mesh_Engine_Do_Step))
	else (* Schedule Extraction - do not increase the step number! *)
	  let () = logdebug (Printf.sprintf "Scheduling Mesh Extraction!%!") in
	  (work nr_step (cont Mesh_Engine_Do_Extract))
    | Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue (mesh,cont) ->
	let () = logdebug (Printf.sprintf "Extracted Mesh!%!") in
	if nr_step = 0
	then (work (1+ nr_step) (cont Mesh_Engine_Do_Step))
	else
	  let () = f nr_step mesh in
	  (work (1+ nr_step) (cont Mesh_Engine_Do_Step))
  in
  work 0 (mge Mesh_Engine_Do_Step)
;;

(* Note: it will be sufficient for our purposes to mesh pieces and provide means to
   fuse meshes of pieces to larger meshes later on.

   Synthesis is relevant for:

   - distributing meshing over multiple processors
   - combining individually meshed pieces to a larger entity.
     (Problem still to be resolved: how to deal with faces?
      In a certain sense, their meshing has to be known beforehand -
      it will not always work to let one meshed thingy provide
      the face mesh for the others, should be possible to treat
      all of them on an equal footing.)

   Note that this should be our main - and maybe even only - interface
   to the mesh generator.

   This way, we also do not have to worry about what data structure
   the relaxate function operates on, as this will be purely internal,
   and furthermore, we will eventually hide all the bells, gongs, and
   whistles, in the mesher_defaults data structure...


   IMPORTANT NOTE: the fem_geometry passed to mesh_it should contain
   just one single boundary condition. (XXX Note: code should be
   re-structured in such a way that it indeed does not give the
   impression any longer that it would care about multiple boundary
   conditions!)

   Furthermore, we explicitly give the guarantee that the first
   (Array.length immobile_points) points in the result are immobile
   points that have been passed in.

*)

let mesh_a_piece
    ?(driver=default_driver)
    ?(rng= Mt19937.make 97)
    ?(immobile_points=[||]) (* fixed points: the mesher will not move them *)
    ?(mobile_points= [||]) (* - if it is [||], no mobile points are defined
			      by the user and the mesher after sampling the space uses
			      an initial set of points randomly distributed (+ fixed points )
			      - if it is [|a,b,...|] the mesher uses the set of
			      mobile points as the initial distribution of points
			   *)
    ?(simply_points= [||]) (* if given, the user wants to mesh simply this points:
			      mind that an object should be still given (for the fem geometry)!!! gb *)

    fem_geometry
    mdefaults
    (* mdefaults catches "things we usually do not want to worry about, but are glad
       to know we can change them easily if we really should have to."
     *)
    length_scale
    (* The desired next-neighbour distance.
       The number of nodes required will then be determined automatically.

       XXX Note: this should be changed in such a way that we also can specify
       a fixed number of nodes. This would be useful e.g. for abusing the mesher
       as a component in a "graphviz" like program.
     *)
    =
  let (corner1,corner2) = fem_geometry.fem_geo_bounding_box in                   (* extract the coordinates of the bounding box in
										 order to get the volume of the space to sample for
										 the initial set of points *)

  let boundary_condition_funs = Array.map snd fem_geometry.fem_geo_boundaries in (* fem_geo_boundaries is (simplex region, boundary conditions) *)

  let () = loginfo (Printf.sprintf "mesh_a_piece: %d boundaries." (Array.length boundary_condition_funs)) in

  let relative_density_fun = fem_geometry.fem_geo_density in                     (* density function used to set the initial distribution
										    of points *)

  let clamped_relative_density_fun pos =                                         (* function that sets the density function to zero
										 if some of the boundary conditions are not satisfied
										 - to avoid some of the initial set of points outside the object *)
    (if (-1 <> (array_position_if (fun bc -> bc pos < 0.0) boundary_condition_funs 0)) then 0.0 else relative_density_fun pos)
  in
  let nr_volume_probes = mdefaults.mdefault_nr_probes_for_determining_volume in  (* the sampling of the space is made by N probes *)
  let dim = Array.length corner1 in
  let () = (if Array.length corner2 <> dim then failwith "Dimensions do not match" else ()) in  (* check for correct dimensions of the
												 arrays defining the bounding box *)
  let corner_min = Array.init dim (fun n -> min corner1.(n) corner2.(n))
  and corner_max = Array.init dim (fun n -> max corner1.(n) corner2.(n))
  in
  let box_volume =                                                                (* compute the volume of the bounding box *)
    (let rec walk d v =
      if d = dim then v else walk (1+d) (v*.(corner_max.(d)-.corner_min.(d)))
    in walk 0 1.0)
  in
  let fun_random_point () =
    Array.init
      dim
      (fun j ->
	let dj = corner_max.(j)-.corner_min.(j) in
	corner_min.(j)+.(Mt19937.float rng dj))
  in
    if (Array.length simply_points) > 0                 (* the user provided an initial set of fixed points and wants to mesh only those *)
    then
      let () = loginfo (Printf.sprintf "Using only the fixed points given by the user!%!") in (* DDD *)
      let new_mdefaults =
	copy_mesher_defaults mdefaults
	  mdefaults.mdefault_meshgen_controller
	  mdefaults.mdefault_meshgen_controller_initial
      in
	                                      (* We use a quite bad hack to incorporate the hint flags. This is not nice. *)
	                                      (* We include the hint feature one step earlier (in add_mesh_data) and here we *)
	begin                                 (* mesh only the set of fixed points given by the user *)
	  new_mdefaults.mdefault_controller_step_limit_max <- 1;
	  new_mdefaults.mdefault_controller_handle_point_density_fun <-  (fun _ _ _ _ -> Do_Nothing_With_Point);
	  let engine =
	    make_meshgen_engine
	      ~rng:rng
	      ~rod_length:length_scale
	      new_mdefaults
	      simply_points  (* this is the list of immobile points to be used *)
	      [||]           (* no initial set of mobile points *)
	      fem_geometry
	  in
	    driver engine
	end
    else                                            (* the user wants to start from a set of mobile points (plus fixed one possibly) *)
      let points =
	if Array.length mobile_points = 0
	then                                        (* generate a set of random points from the boundary conditions defining the object *)
	  let (est_max, est_avg) =
	    estimate_max_and_average
	      nr_volume_probes clamped_relative_density_fun fun_random_point
	  in
	  let () = loginfo (Printf.sprintf "est_max: %f est_avg: %f!%!" est_max est_avg) in
	  let () = loginfo (Printf.sprintf "nr_volume_probes: %d %!" nr_volume_probes) in
	  let est_integrated_density = est_avg *. box_volume in
	    (* Now, we face the problem: what is a reasonable volume
	       we should attribute to a single node?
	       Answer: assume the node is at the center of a ball
	       of a reasonably dense D-dimensional lattice sphere packing,
	       take the ball's volume and multiply with the inverse sphere
	       packing efficiency.
	       We use D-type lattices here. Note that those may provide
	       a quite crude estimate in some dimensions.

	       However, our estimates are bound to be crude anyway, as we do not
	       have any control over the amount of surface and its structure,
	       and the structure of density variations. It is reasonable
	       to assume that our packing will be worse than the lattice packing,
	       and so, our rods will feel a little bit of strain, but that is
	       indeed a good thing.
	    *)
	  let est_node_volume =
	    (volume_d_sphere dim)*.((length_scale*.0.5*.0.7)**(float_of_int dim))
	    /.(sphere_packing_ratio_lattice_type_d dim) in
	    (* Note: length_scale/2 comes from every rod being 2r in length -
	       the other factor 0.5 comes from setting the mesh under some
	       initial strain. *)
	  let est_nr_nodes = est_integrated_density/.est_node_volume in
	  let () = loginfo (Printf.sprintf "Est. avg=%f vol=%f node_vol=%f nr_nodes=%f%!" est_avg est_integrated_density est_node_volume est_nr_nodes) in
	  let new_points_required=
	    min 10000
	      (* We never start out with more than that many points *)
	      (max (dim+1+5)
		  (* make sure we at least have a few points -
		     dim+1 should suffice in theory,
		     but qhull thinks otherwise! *)
		  (int_of_float
		      (est_nr_nodes -. (float_of_int (Array.length immobile_points))*.0.0)))
	  in
	    (* XXX Note: actually, the original way of estimating the number
	       of points required turned out to be far too crude:

	       let new_points_required=max (dim+1) ((int_of_float est_nr_nodes)
	       - Array.length immobile_points) in

	       Reason: as points on the surface count as full points, but take
	       up much less space, as they do not contribute to a full volume,
	       we place far too few points on the inside.

	       Note that it is anyway better to slightly over-estimate and axe points
	       than to under-estimate.

	       Further note: there is even a serious bug in the original approach:
	       We provided all the other boundary points which we found so far - even ones which
	       correspond to very different boundaries! Hence, this generally gives far more points
	       than we are interested in!
	    *)
	  let (rand_points,_) =
	    distribute_points_randomly
	      ~fun_rng:(fun x -> Mt19937.float rng x)
	      ~max_density_and_average:(est_max,est_avg)
	      fun_random_point
	      clamped_relative_density_fun
	      new_points_required
	  in rand_points
	else
	  mobile_points                                        (* uses the set of initial mobile points given by the user *)
      in
	(* the mesher will now use the mobile points from the user or from the random distribution as initial points *)
      let nr_new_points = Array.length points in
      let nr_immobile_points = Array.length immobile_points in
      let () = loginfo (Printf.sprintf " mesh_a_piece nr_new_points=%d nr_immobile_points=%d" nr_new_points nr_immobile_points) in

      let engine =
	make_meshgen_engine
	  ~rng:rng
	  ~rod_length:length_scale
	  mdefaults
	  immobile_points
	  points
	  fem_geometry
      in
	driver engine
	  (* XXX PROBLEM: if fem_geo was given, we should see that
	     any mesh we get from this also does contain that fem
	     geo. (Right now, we add that info manually further down!

	     -> The boundary between mesh_it and mesh_a_piece must
	     be shifted: mesh_a_piece should return a full mesh,
	     not a mgo!  *)
;;

(* When we have individually meshed pieces that share boundary points,
   it would be unwise to just throw all points into one large sack and
   Delaunay triangulate the result, as this would presumably result in
   simplices crossing boundaries.

   In other words: our strategy of piecewise meshing potentially
   induces small violations of the Delaunay property close to piece
   boundaries. Not a problem by itself, though.

   So, how do we combine pieces? Answer:

   * Take the proper full geometry information

   * For every simplex, find the region it belongs to.
   NOTE: every simplex has to belong to one and only one region
   (which may even be "non-meshed space" (id = -1), though.)

   * For every piece, throw away all simplices that do not belong to its region.

   * Just combine all the rests, and use mesh_from_known_delaunay on it.

   Note: here, we may be tempted to hash floatingpoint arrays.
   This is indeed somewhat okay, as we have guarantees that fixed points really
   do stay fixed.

   Note: while we are at it, we also can address the issue that
   simplices should indeed know about the regions they belong to
   directly (i.e. know "the material they are made of"). This does
   indeed offer a score of new possibilities, as we may want to
   attribute different degrees of freedom to different materials!
*)

let mesh_fuse_pieces fem_geometry pieces =
  failwith "XXX write me!"
;;

(* XXX Note: we might want to have a variant of this that allows us to "look into"
  the meshing process. However, it is presumably wiser to just provide means for a twin
   implementation of this in a high-level scripting language...

   No. For now, we do it by allowing drivers that do custom callbacks.
 *)


let mesh_it_work_forward                                                            (* function which creates the triangulation simply using the outcome of qhull *)
    dim
    ?(triangulator=Qhull.delaunay)
    rod_length fem_geometry mdefaults pts_states points_coords
    =
  let simplices = triangulator points_coords in

  let bnr_1 = Body_Nr 1 in
  let the_mesh =
    mesh_from_known_delaunay                                                         (* function to tell the points about the simplices they belong to and viceversa *)
      	points_coords
	(Array.map (fun sx_indices -> (bnr_1,sx_indices)) simplices)
  in
  let () = the_mesh.mm_fem_geometry <- Some fem_geometry in

  let () = mesh_grow_bookkeeping_data                                                (* tell each simplex which are its neighbours, compute in (circum) circles and *)
    ~do_connectivity:true                                                            (* tell each simplex about the region it belongs to *)
    ~do_incircle_circumcircle:true
    ~do_regions:true the_mesh
  in

  let () = report_mesh_quality 10 the_mesh

  in
    the_mesh
;;

let mesh_it_work
    ~gendriver      (* driver *)
    ~rng            (* random generator *)
    ~fixed_points   (* fixed points - included the possible periodic ones *)
    ~mobile_points  (* mobile points - they are given initially and can be (re)moved by the mesher *)
    ~simply_points  (* fixed points - the mesher uses only these ones for the generation of the mesh *)
    ht_periodic_pts_to_save (* hashtable where each entry is an array of "same" periodic points *)
    fem_geometry    (* data structure storing info about the objects to mesh and their transformations *)
    mdefaults       (* default values of the mesh parameters *)
    length_scale    (* desired rod lenght of the mesh *)
    =

  let local_mobile_points = ref mobile_points in                         (* reference to the mobile points array
                                                                            - used in order to add or remove points *)
  let pieces = fem_geometry.fem_geo_boundaries in                        (* objects to be meshed *)
  let nr_pieces = Array.length pieces in                                 (* number of objects to be meshed *)
  let forall_simplices mesh f = Array.iter f mesh.mm_simplices in        (* functions to loop over all the points and simplices *)
  let forall_points mesh f = Array.iter f mesh.mm_points in

  (*let register_point_ix ht coords region_nr=                             (* function to register points in a hashtable associated
                                                                            to the region they belong to *)
    try
      Hashtbl.find ht coords
    with
    | Not_found ->
	let () = Hashtbl.add ht coords region_nr
	in region_nr
  in*)


  let add_meshdata_of_piece_nr                                                    (* dht_immobile_points is a hash table mapping coords-vector => index *)
      n                                                                           (* which will be modified destructively by putting in all the new	*)
      li_simplices_pcoords_sf                                                     (* boundary points which we discover as well.				*)
      li_surfaces_pcoords_sf                                                      (*  We use the value to index-count points.                           *)
      dht_immobile_points
      points_preventing_surface_pb
      ?(simply_points=[||])
      =

    let fem_geo_piece =                                                           (* copy the fem_geometry data structure related to the n-th object
                                                                                      in a local fem_geometry data structure *)
      {fem_geo_dim = fem_geometry.fem_geo_dim;
       fem_geo_bounding_box = fem_geometry.fem_geo_bounding_box;
       (* XXX Note that we have a problem here if some part of the mesh has FAR smaller volume
	  than the bounding box!
	  Maybe this hints at the issue that every piece should have its own fem_geometry?
	*)
       fem_geo_density = fem_geometry.fem_geo_density;
       fem_geo_boundaries = Array.sub fem_geometry.fem_geo_boundaries n 1;
       fem_geo_piece_hints = Array.sub fem_geometry.fem_geo_piece_hints n 1;
       fem_geo_simplex_classificator = fem_geometry.fem_geo_simplex_classificator;
     }
    in

    let bcs = Array.map (fun (x,y) -> y) fem_geo_piece.fem_geo_boundaries in

    let create_mesh given_mobile_points given_immobile_points fem_geometry boundary_cs rod_length =


      let (good_mobile_pts, bad_mobile_pts) =                                         (* separate the mobile points in good or bad
											 a point is classified as good if it satisfies
											     all the boundary conditions of the given object *)
	array_filter_double
	  ( fun p ->
	      array_all_satisfy
		( fun bc -> (bc p) >= (0.0 -. mdefaults.mdefault_boundary_condition_acceptable_fuzz))
		boundary_cs
	  )
	  given_mobile_points  (* reference to the mobile points array *)
      in

      let (good_immobile_pts, bad_immobile_pts) =                                         (* separate the immobile points in good or bad
											     a point is classified as good if it satisfies
											     all the boundary conditions of the given object *)
	array_filter_double
	  ( fun p ->
	      array_all_satisfy
		( fun bc -> (bc p) >= (0.0 -. mdefaults.mdefault_boundary_condition_acceptable_fuzz))
		bcs
	  )
	  given_immobile_points
      in

      (* if some mobile points are coincident with some immobile points, use only the latter ones *)
      let filt_mobile_points = array_filter (fun mob -> array_all_satisfy (fun imm -> imm <> mob) good_immobile_pts) good_mobile_pts in

      let mgo =                                               (* mesh generated object *)
	mesh_a_piece ~driver:(gendriver n) ~rng:rng
	  ~immobile_points:good_immobile_pts
	  (* DDD - BE VERY CAREFUL WITH THIS CHOICE!!!
	     (hashtbl_keys dht_immobile_points) (* should we use "good_immobile_pts" ??? - in that case the mesher doesn't know
	     about the neighbour of the object, and it is more likely to generate broken meshes *)
	  *)
	  ~mobile_points:filt_mobile_points                           (* only the filtered good points are used to generate the mesh *)
	  ~simply_points:simply_points                        (* if given, the mesher uses only these ones for the generation of the mesh *)
	  fem_geometry mdefaults rod_length
      in
      let partial_mesh =
	match mgo with                                        (* the object returned by the mesh_a_piece function contains the mesh
								 plus the information about what stopped the mesher to evolve, hence
								 we need to extract the mesh from this object *)
	  | Mesh_Engine_Finished_Step_Limit_Reached(m,nr) ->
	      let () = loginfo (Printf.sprintf "Piece #%d reached meshing step limit. points=%d" n (Array.length m.mm_points)) in
		(* Maybe should do more about this...? *)
		m
	  | Mesh_Engine_Finished_Force_Equilibrium_Reached (m,f) ->
	      (* XXX Hey, force equilibrium was reached anyway. I am as good as you wanted me to be.
		 Hence, this should not be reported!
	      *)
	      let () = loginfo (Printf.sprintf "Piece #%d reached equilibrium forces. points=%d" n (Array.length m.mm_points)) in
		m
	  | _ -> impossible()  (* This should never happen! *)

      in
      let () = partial_mesh.mm_fem_geometry <- Some fem_geo_piece in                  (* we attach the fem geometry which generated the mesh to the mesh final structure *)

      let () = loginfo (Printf.sprintf "Mesh piece: points=%d simplices=%d%!" (Array.length partial_mesh.mm_points) (Array.length partial_mesh.mm_simplices)) in

      (* Ensure that we do know for every point which region it belongs to.
	 Note that this uses its own, and pretty strong, heuristics based on
	 the boundary conditions.
      *)
      let () = mesh_grow_bookkeeping_data ~do_regions:true ~do_connectivity:true partial_mesh in
	(* Okay, the partial mesh we have right now still has to be pruned down to the one component
	 we are interested in. Note that for this mesh, we only know about Body_Nr 0 (= outside) and
	   Body_Nr 1 (= inside), and retain just those simplices which are "inside". We furthermore
	   keep track of boundary points.

	   Eventually, we will have to fuse topology information from mesh parts. How to do this?

	   * Every simplex is represented as a vector of points, where every point is given
           just by its coordinates.

	   * We have guarantees that points which we consider fixed really do not move!

	   * Eventually, we will use a hash that maps a point-coordinates vector to a point number
         and use this to label points and give simplices as vectors of such integer labels.
	*)
	partial_mesh
    in

    let this_mesh =
      let hint_points, hint_simplices = fem_geo_piece.fem_geo_piece_hints.(0) in
	if  Array.length hint_points > 0 then
	  let () = loginfo (Printf.sprintf "Meshing object from hints") in
	  let ht_good_points = Hashtbl.create 1000 in                       (* hashtable to store the points within the object specified by the bcs *)

	  let good_simplices =                                              (* take only the simplices whose points are within the hint object function; for these  *)
	    array_filter                                                    (* simplices, register the point in the hashtable of good points *)
	      ( fun sx_indices ->
		let point_in_obj p =
		  array_all_satisfy
		    ( fun bc -> (bc p) >= (0.0 -. mdefaults.mdefault_boundary_condition_acceptable_fuzz))
		    bcs
		in
                let condition = array_all_satisfy (fun ix -> point_in_obj hint_points.(ix)) sx_indices in
		let () =
		  if condition then
		    Array.iter (fun ix -> let (*num*) _ = register_point ht_good_points hint_points.(ix) in ()) sx_indices
		  else  ()
		in
		  condition
	      )
	      hint_simplices
	  in

	  let simplices_with_corrected_indices =                  (* we need to change the indices in the good simplices according to the new list of (good) points *)
	    Array.map
	      (fun sx ->
		Array.map
		  (fun old_index -> Hashtbl.find ht_good_points hint_points.(old_index) )
		  sx)
	      good_simplices
	  in

	  let ordered_points = array_sorted (fun a b -> (Hashtbl.find ht_good_points a) - (Hashtbl.find ht_good_points b) ) (hashtbl_keys ht_good_points) in
	                                                                    (* mesh the hint object as it is, giving only a temporary value *)
									    (* to the simplex region to match the function argument type *)

	  let hint_m =  mesh_from_known_delaunay ordered_points (Array.map (fun sx_ix -> (Body_Nr 0, sx_ix)) simplices_with_corrected_indices )
	  in
          let () = hint_m.mm_fem_geometry <- Some fem_geo_piece in        (* we attach the fem geometry which generated the mesh to the mesh final structure *)

	  let () = loginfo (Printf.sprintf "Mesh hint: points=%d simplices=%d%!" (Array.length hint_m.mm_points) (Array.length hint_m.mm_simplices)) in

	  let () = mesh_grow_bookkeeping_data ~do_regions:true ~do_connectivity:true hint_m in
	    hint_m
	else
	  (* the boundary conditions for the filter are weakened *)
	  create_mesh !local_mobile_points (hashtbl_keys dht_immobile_points) fem_geo_piece bcs length_scale
    in
    (* not very nice way to define this reference, but it matches the definition of the same quantity for the current mesh *)
    let simplices_in_previous_mesh = ref (Array.to_list (Array.copy (Array.of_list li_simplices_pcoords_sf))) in
    let r_inner_simplices = ref li_simplices_pcoords_sf in
    let (boundary_body_nr,_) = fem_geometry.fem_geo_boundaries.(n) in  (* extract the simplex region from
									  the tuple (region, boundary condition to meet) *)
    let () = loginfo (Printf.sprintf "forall_simplices!") in
    let simplices_in_current_mesh = ref [] in
    let () =
      forall_simplices this_mesh
	(fun sx ->
	  if sx.ms_in_body = boundary_body_nr                                 (* the region of the object and that of the simplex match *)
	      (* XXX Note: this is subtle (call it a "glitch"):
		 Here, we used a classifier which does try to spit out
		 the correct body nr, by looking at boundaries.
	       *)
	  then                                                                (* add simplex to the list of inner and partial simplices *)
	    let () = simplices_in_current_mesh := (Array.map (fun p -> p.mp_coords) sx.ms_points):: !simplices_in_current_mesh in
	    r_inner_simplices := (Array.map (fun p -> p.mp_coords) sx.ms_points):: !r_inner_simplices
	  else
	    ())
    in
    let () = loginfo (Printf.sprintf "forall_points!") in
    let () =
      forall_points this_mesh
	(fun pt ->
	  match pt.mp_in_body with
	  | _::_::_ -> (* Length >= 2 *)                                      (* the point belong to at least two regions, so it
									         must be on the boundary and therefore it becomes
									         an immobile point *)
	      ignore(register_point dht_immobile_points pt.mp_coords)
	  | _ -> ())
    in

    let r_surface_simplices = ref li_surfaces_pcoords_sf in                   (* reference to the list of surface simplices, so far *)
    let current_surface_simplices = ref [] in                                 (* initialize the list of surface simplices in this object *)

    let () = loginfo (Printf.sprintf "forall_simplices 2!") in
    let () =
      forall_simplices this_mesh
	(fun sx ->
	  let region = sx.ms_in_body in
	  let backref = sx.ms_neighbour_backrefs in   (* int array *)
	  let neigh = sx.ms_neighbours in             (* simplex option array *)
	  let nr_neigh = Array.length neigh in
	  let check_outer_elem =                                               (* true if the simplex has all proper neighbours, none degenerated,
									          that is the simplex is not on a body-outerSpace boundary *)
	    array_all_satisfy (fun x -> x >= 0) backref
	  in
	  let check_surface_elem =                                             (* true if all the neighbours are in the same body,
									          that is the simplex is not on a body-body surface *)
	    array_all_satisfy
	      (fun x ->
		match x with
		  | Some y -> y.ms_in_body = sx.ms_in_body
		  | None -> 1 = 1)
	      neigh
	  in
	  let surface_simplices =
	    if check_outer_elem
	    then
	      if check_surface_elem then
		[]
	      else
		let rec find_interior_surface_nodes ix surface_sf =          (* find the nodes laying on the surface *)
		  if ix = nr_neigh then surface_sf
		  else
		    let pos =
		      (array_position_if                                       (* index of the neighbour which belongs to a different region *)
			  (fun n ->
			    match n with
			      | Some m -> m.ms_in_body <> region
			      | None -> 1 = 1
			  )
			  neigh ix)
		    in
		      if pos > (-1) then                                       (* there is a neighbour which belongs to a different region *)
			let arr = (Array.map (fun p -> p.mp_coords) (array_one_shorter sx.ms_points pos)) in  (* create an array of coords with all the points but *)
			                                                                                      (* the one in the interior of the object: surface of the object *)
			let () = (Array.sort compare arr) in
			  find_interior_surface_nodes (pos+1) (arr:: surface_sf)                              (* add this new surface to the list *)
			                                                                                      (* - could the same surface be saved *)
			                                                                                      (* more than once ?? gb *)
		      else
			find_interior_surface_nodes (ix+1) surface_sf
		in find_interior_surface_nodes 0 []
	    else
	      let rec find_surface_nodes ix surface_sf =                            (* find the surface with the outer space and add it to the list*)
		if ix = nr_neigh then surface_sf
		else
		  let pos = (array_position (-1) backref ix) in
		    if pos > (-1) then
		      let arr = (Array.map (fun p -> p.mp_coords) (array_one_shorter sx.ms_points pos)) in
		      let () = (Array.sort compare arr) in
			find_surface_nodes (pos+1) (arr::surface_sf)
		    else
		      find_surface_nodes (ix+1) surface_sf
	      in
		find_surface_nodes 0 []

	  in
	  let len = List.length  surface_simplices in
	    current_surface_simplices :=
	      let rec add_item list_sf ix =
		if ix = len
		then list_sf
		else add_item ((List.nth surface_simplices ix)::list_sf) (1+ix)
	      in
		add_item !current_surface_simplices 0
	)
    in
    let () = loginfo (Printf.sprintf "end forall_simplices 2!") in
    let v_bodies_surfaces = [|Array.of_list (!r_surface_simplices); Array.of_list (!current_surface_simplices) |]
    in
      (* verify bodies surface:
	 check if there is any problem between
	 the current surface simplices and each
	 of the surface simplices found so far *)

      (* new points as array of (edge,(regions separated by the broken faces)); the region info is useless in this implementation *)
    let new_points = find_interface_points_to_add_between_mesh_pieces v_bodies_surfaces in  (* possibly add new points to prevent broken meshes *)

    let ref_current_points = ref (Array.map (fun p -> p.mp_coords) this_mesh.mm_points) in

    let () = for i=0 to (Array.length new_points)-1 do

      (* simplices sharing broken edge in current mesh *)
      let (edge_j,edge_i) = (fun (edge,(other_edge,(region1,region2))) -> (edge, other_edge)) new_points.(i) in
      let middle_pt = (float_arrays_avg edge_i) in

      (* first we update the information in the current mesh .... *)

      let (mob_pts, imm_pts) = array_filter_double ( fun p -> array_all_satisfy (fun fixp -> p <> fixp) fixed_points) !ref_current_points in
      let () = ref_current_points := Array.append [|middle_pt|] !ref_current_points in

      let corrected_current_mesh = create_mesh mob_pts (Array.append [|middle_pt|] imm_pts) fem_geo_piece  [|(fun x -> 1.0)|] (0.01*.length_scale) in

      let new_sxs_current_mesh = Array.map (fun sx -> Array.map (fun p -> p.mp_coords) sx.ms_points) corrected_current_mesh.mm_simplices in

      (* add new simplices to the list of simplices *)
      let () = simplices_in_current_mesh := Array.to_list new_sxs_current_mesh in

	(* remove the broken surfaces from the mesh *)
      let () = current_surface_simplices := remove_surfaces_with_edge edge_i(Array.of_list (!current_surface_simplices)) in

	(* add the new surfaces (the ones from the retriangulation) *)
      let broken_free_surfaces_current = extract_surfaces_with_edge edge_i new_sxs_current_mesh  in
      let () = Array.iter (fun new_surf -> current_surface_simplices := new_surf :: (!current_surface_simplices)) broken_free_surfaces_current in





      (* ... and then in the mesh previously created ... *)

      let (new_sxs_previous_mesh,pre_obj_ix) =

	let rec find_previous_obj obj_ix simplices_sf =
	  if (Array.length simplices_sf) > 0
	  then
	    (simplices_sf,obj_ix-1)
	  else
	    let simplices =
	      try
		let new_fem_geometry =
		  {fem_geo_dim = fem_geometry.fem_geo_dim;
		   fem_geo_bounding_box = fem_geometry.fem_geo_bounding_box;
		   (* XXX Note that we have a problem here if some part of the mesh has FAR smaller volume
		      than the bounding box!
		      Maybe this hints at the issue that every piece should have its own fem_geometry?
		    *)
		   fem_geo_density = fem_geometry.fem_geo_density;
		   fem_geo_boundaries = Array.sub fem_geometry.fem_geo_boundaries obj_ix 1;
		   fem_geo_piece_hints = Array.sub fem_geometry.fem_geo_piece_hints obj_ix 1;
		   fem_geo_simplex_classificator = fem_geometry.fem_geo_simplex_classificator;
		 }
		in

		let point_in_obj_condition = (fun ((Body_Nr x),bc) p -> bc p > 0.0-.1.0e-6) new_fem_geometry.fem_geo_boundaries.(0) middle_pt in
		if point_in_obj_condition
		then
		  (* the boundary conditions for the filter are weakened *)
		  let new_mobile_points = array_filter (fun p -> array_all_satisfy (fun ((Body_Nr x),bc) -> bc p > 0.0-.1.0e-6) new_fem_geometry.fem_geo_boundaries ) (hashtbl_keys dht_immobile_points) in

		  let (mob_pts, imm_pts) = array_filter_double ( fun p -> array_all_satisfy (fun fixp -> p <> fixp) fixed_points) new_mobile_points in

		  let corrected_previous_mesh = create_mesh mob_pts (Array.append [|middle_pt|] imm_pts) new_fem_geometry  [|(fun x -> 1.0)|] (0.01*.length_scale) in
		  Array.map (fun s -> Array.map (fun p -> p.mp_coords) s.ms_points ) corrected_previous_mesh.mm_simplices

		else
		  [||]
	      with
	      | _ -> [||]
	    in
	    find_previous_obj (obj_ix+1) simplices
	in
	find_previous_obj (n+1) [||]
      in

      (* remove simplices associated to the retriangulated obj from previous mesh *)

      (* for each point there is only one previous object to update, therefore we need the index for only one entry of the fem_geometry array *)
      let bc = (fun ((Body_Nr x),b) -> b ) (Array.sub fem_geometry.fem_geo_boundaries pre_obj_ix 1).(0) in
      let simplices_in_previous_mesh_to_remove = array_filter (fun sx -> array_all_satisfy ( fun p -> (bc p) > 0.0-.1.0e-6 ) sx) (Array.of_list !simplices_in_previous_mesh) in

      let clean_previous_simplices =
	List.filter (fun sx ->
	  array_all_satisfy (fun sx_to_delete -> sx_to_delete <> sx) simplices_in_previous_mesh_to_remove
		    ) !simplices_in_previous_mesh
      in

      (* add new simplices to the list of simplices *)
      let () = simplices_in_previous_mesh := List.append (Array.to_list new_sxs_previous_mesh) clean_previous_simplices in

	(* remove the broken surfaces from the mesh *)
      let () = r_surface_simplices := remove_surfaces_with_edge edge_j (Array.of_list (!r_surface_simplices)) in

	(* add the new surfaces (the ones from the retriangulation) *)
      let broken_free_surfaces_previous = extract_surfaces_with_edge edge_j new_sxs_previous_mesh  in
      let () = Array.iter (fun new_surf -> r_surface_simplices := new_surf :: (!r_surface_simplices)) broken_free_surfaces_previous in

      (* and eventually store the new point to prevent a broken mesh *)
      ignore(register_point dht_immobile_points middle_pt)

    done

    in

    let () = r_inner_simplices := List.append !simplices_in_current_mesh !simplices_in_previous_mesh in
    let () = r_surface_simplices :=
      List.append (!current_surface_simplices) (!r_surface_simplices)
    in

      (!r_inner_simplices,!r_surface_simplices,dht_immobile_points, points_preventing_surface_pb)
  in

  let mesh_from_pieces_meshdata
      li_simplices_pcoords      (* list of simplices where for each simplex we have the list of coordinates of its points *)
      dht_immobile_points       (* list of immobile points *)
      =
                                                                                    (* We will just abuse dht_immobile_points to get all the other points numbered as well... *)

    let result_simplices =
      Array.map (Array.map (register_point dht_immobile_points)) (Array.of_list li_simplices_pcoords)   (* NOT CLEAR!!! gb  *)
    in

    let nr_points = Hashtbl.length dht_immobile_points in
    let result_points = Array.make nr_points [||] in
    let () =
      Hashtbl.iter
	(fun coords ix ->
          result_points.(ix) <- coords)
	dht_immobile_points
    in
    let bnr_1 = Body_Nr 1 in

   let () = loginfo (Printf.sprintf "MESH POINTS:\n%s\nMESH SIMPLICES:\n%s\n"
			(String.concat "\n" (Array.to_list (Array.map float_array_to_string result_points)))
			(String.concat "\n" (Array.to_list (Array.map int_array_to_string result_simplices))))
   in

    let fused_mesh =
      mesh_from_known_delaunay                                                            (* tell the points about the simplices they belong to and vice-versa *)
	result_points                                                                     (* this is the point to hack if we want to keep meshing a previous generated mesh *)
	(Array.map (fun sx_indices -> (bnr_1,sx_indices)) result_simplices)
    in
      (* Note that we cheated above with region information: we claimed that all simplices
	 belong to body #1. We still have to mend that:
      *)
    let () = fused_mesh.mm_fem_geometry <- Some fem_geometry in
    let () = fused_mesh.mm_have_regions <- false in
    let () = mesh_grow_bookkeeping_data ~do_regions:true fused_mesh in
      fused_mesh
  in

(*
###############
# WALK_PIECES
###############
*)

  let () = loginfo (Printf.sprintf "\nenter walk_pieces") in

  (* Everything together now *)
  let rec walk_pieces
      nr_piece
      li_simplices_pcoords_sf
      li_surfaces_pcoords_sf
      dht_immobile_points
      dht_points_preventing_surface_pb
      ?(simply_points=[||])
      =
    (* We work with decreasing nr_piece, with piece #0 being "outer space"
       (if requested to be meshed as well). Rationale: our method deals better
       with convex corners than with concave ones. The "outside" will usually
       have quite many concave corners.
     *)
    if nr_piece < 0                    (* return the list of simplices, the list of surfaces,
					  the list of fixed points and that of new points introduced
				          to prevent a broken mesh
				       *)
    then
      (li_simplices_pcoords_sf, li_surfaces_pcoords_sf, dht_immobile_points, dht_points_preventing_surface_pb)

    else
      let (new_li_simplices_pcoords_sf, new_li_surfaces_pcoords_sf, new_dht_immobile_points, new_dht_points_preventing_surface_pb) =
	add_meshdata_of_piece_nr
	  nr_piece                                 (* piece number - 1 *)
	  li_simplices_pcoords_sf                  (* list of simplices where for each simplex we have the
                                                      list of coordinates of its points, so far *)
	  li_surfaces_pcoords_sf                   (* list of surface simplices where for each simplex we
                                                      have the list of coordinates of its points, so far *)
	  dht_immobile_points                      (* hashtable with all the immobile points, so far *)
	  dht_points_preventing_surface_pb         (* hashtable with all the points preventing broken mesh at surfaces, so far *)
          ~simply_points:simply_points             (* used when we know the points to be added to prevent a broken mesh and we *)
      in                                           (* call the function walk_pieces the second time *)
      walk_pieces
	(nr_piece-1)
	new_li_simplices_pcoords_sf
	new_li_surfaces_pcoords_sf
	new_dht_immobile_points
	new_dht_points_preventing_surface_pb
	~simply_points:simply_points
  in


  let dht_immobile_points_initial = Hashtbl.create 100 in
  let dht_points_preventing_surface_pb = Hashtbl.create 100 in

  let () = Array.iter (fun p -> ignore(register_point dht_immobile_points_initial p)) fixed_points in

  (* the function walk_pieces is called twice: the first time to obtain the list of points to introduce
     in order to prevent a broken mesh (and the fixed points are only those fixed at the python interface),
     the second time to retriangulate the object adding the aforementioned ones to the list of fixed points,
     as well as the mobile ones, and setting all of them as simply_points, so that there is no relaxation of the mesh
  *)

  let (li_simplices_pcoords_sf, li_surfaces_pcoords_sf,
      dht_immobile_points, dht_points_preventing_surface_pb) =
    walk_pieces
      (nr_pieces-1)
      []
      []
      dht_immobile_points_initial
      dht_points_preventing_surface_pb
      ~simply_points:simply_points
  in

  let () = logdebug (
      let outcome_str =
	if Array.length (hashtbl_keys dht_points_preventing_surface_pb) = 0
	then "none."
	else (String.concat "\n" (Array.to_list (Array.map float_array_to_string (hashtbl_keys dht_points_preventing_surface_pb))))
      in
	Printf.sprintf "New points to be added for prevention of a broken mesh: %s%!" outcome_str)
  in

  let dht_simply_points = Hashtbl.create 1000 in                (* hashtable to store all the points
								   obtained from the first run of the mesher *)

  let () = List.iter                                            (* add the points from the simplices to the list of simply_points *)
    (fun sx ->
      Array.iter
	( fun p -> ignore(register_point dht_simply_points p) )
	sx)
    li_simplices_pcoords_sf
  in
                                                                (* no need to set the mesher parameters in order to have no relaxation *)
                                                                (* because we use the simply_points option to call the mesher *)

  let () = Array.iter                                           (* add the "preventing" points to the set of simply_points *)
    ( fun p -> ignore(register_point dht_simply_points p) )
    (hashtbl_keys dht_points_preventing_surface_pb)
  in

(*
  let (final_li_simplices_pcoords_sf, final_li_surfaces_pcoords_sf, final_dht_immobile_points, final_dht_points_preventing_surface_pb) =
    (li_simplices_pcoords_sf, li_surfaces_pcoords_sf,
      dht_immobile_points, dht_points_preventing_surface_pb)
      in

    walk_pieces
      (nr_pieces-1)
      []
      []
      (Hashtbl.create 0)                                  (* we create these hashtables in order to match the type of the argument but we don't *)
      (Hashtbl.create 0)                                  (* use them because we call the function mesh_a_piece with the simply_points option *)
      ~simply_points: (hashtbl_keys dht_simply_points)
  in
  let broken_pts_len = Array.length (hashtbl_keys final_dht_points_preventing_surface_pb) in
  let () = loginfo (
      let outcome_str =
	if broken_pts_len = 0
	then "none."
	else (String.concat "\n" (Array.to_list (Array.map float_array_to_string (hashtbl_keys dht_points_preventing_surface_pb))))
      in
	Printf.sprintf "New points to be added for prevention of a broken mesh after their first insertion: %s%!" outcome_str)
  in
  let () =
    if broken_pts_len > 0
    then let () = loginfo ("Mesh is broken, no way.") in failwith ""
    else ()
  in
*)
  (* at this point all the necessary points to prevent a broken mesh should be in place and their
     insertion should not affect the consistency of the triangulation any further -- in theory! gb *)

  (* DDD not using the final version with the points added to prevent a broken mesh *)
  let result_mesh = mesh_from_pieces_meshdata li_simplices_pcoords_sf dht_immobile_points
    (* Actually, meshing takes such a long time that the effort for eventually
       computing all the extra bookkeeping data is negligible -
       so we do this by default!
    *)
  in
  let pts_coords = Array.map (fun p -> p.mp_coords) result_mesh.mm_points in
  let arr_periodic_pts =
    Array.map
      ( fun key ->
	  let periodic_points = Hashtbl.find ht_periodic_pts_to_save key in
          Array.map (fun p -> array_position p pts_coords 0 ) periodic_points
      )
      (hashtbl_keys ht_periodic_pts_to_save) in

  let () = result_mesh.mm_periodic_points <- arr_periodic_pts in

  let () = mesh_grow_bookkeeping_data
    ~do_connectivity:true
    ~do_incircle_circumcircle:true
    ~do_regions:true result_mesh
  in

  let () = report_mesh_quality 10 result_mesh

  in
    result_mesh
;;

(************************************************************************)
(*                 PERIODIC BOUNDARY CONDITIONS                         *)
(************************************************************************)

let mesh_periodic_outer_box
    ?(gendriver=(fun _ -> default_driver))
    ?(rng= Mt19937.make 97)
    fixed_points
    fem_geometry
    mdefaults
    length_scale
    filter
    =
  let () = Printf.printf "periodic fem_geometry dimension: %d\n%!" fem_geometry.fem_geo_dim in
  let ht_periodic_pts_to_save = Hashtbl.create 100 in            (* hashtable to store the periodic points; each entry is a lists of "same" *)
                                                                 (* (in the periodic sense) points *)
  let forall_points mesh f = Array.iter f mesh.mm_points in

  let add_meshdata_of_outer_box iter_nr mask bbox dht_immobile_points =

    let () = Printf.printf "MASK = %s \n%!" (bool_array_to_string mask) in
    let len = Array.length mask in
    let unmasked_components =                                                 (* extract the components which won't be masked *)
      let rec find_comp ix comp_sf =
	if ix = len
	then (Array.of_list comp_sf)
	else
	  if mask.(ix)
	  then find_comp (1+ix) (ix::comp_sf)
	  else find_comp (1+ix) comp_sf
      in
	find_comp 0 []
    in
    let unmasked_len = Array.length unmasked_components in
    let corrected_density =                                                    (* set the coordinates of the unmasked components as those of *)
      (fun x ->                                                                (* the bounding box *)
	 fem_geometry.fem_geo_density
	   (let copy_bbox = Array.copy (fst bbox) in
	    let () = for i=0 to unmasked_len-1 do
	      copy_bbox.(unmasked_components.(i)) <- x.(i)
	    done
	    in copy_bbox
	   )
      )
    in

    let new_corner_nw = Periodic.mask_coords mask (fst bbox) (snd bbox) (fst bbox) in
    let new_corner_se = Periodic.mask_coords mask (fst bbox) (snd bbox) (snd bbox) in

    let boundary_condition =
      (fun x -> Periodic.bc_box new_corner_nw new_corner_se x)
    in
    let () = Array.iter (fun arr -> Printf.printf "starting immobile %s\n%!" (float_array_to_string arr)) (hashtbl_keys dht_immobile_points) in
    let immobiles =
      let table = (Hashtbl.create 100) in
      let mask_arr = Periodic.maskarr mask (fst bbox) (snd bbox) (hashtbl_keys dht_immobile_points) in
      let () = Array.iter (fun arr -> Printf.printf "immobile %s\n%!" (float_array_to_string arr)) mask_arr in
      let () =
	for i = 0 to (Array.length mask_arr)-1 do
	  ignore(register_point table mask_arr.(i) )
	done
      in table
    in
    let () = loginfo (Printf.sprintf "--------------------- fixed -----------------------%!") in
    let () = Array.iter ( fun x -> loginfo (Printf.sprintf "fixed point : %s%!" (float_array_to_string x) )) (hashtbl_keys immobiles) in
    let mgo =
      mesh_a_piece ~driver:(gendriver 0) ~rng:rng
	~immobile_points: (hashtbl_keys immobiles)
	(make_fem_geometry
	    ~bounding_box:(new_corner_nw,new_corner_se)
	    ~boundaries:[|(Body_Nr 0,boundary_condition)|] ()
	    ~density: corrected_density
	)
	mdefaults length_scale
    in
    let () = loginfo (Printf.sprintf "Finished mesh%!") in
    let this_mesh =
      match mgo with
      | Mesh_Engine_Finished_Step_Limit_Reached(m,nr) ->
	  let () = loginfo (Printf.sprintf "Piece #%d reached meshing step limit. points=%d" 0 (Array.length m.mm_points)) in
	  (* Maybe should do more about this...? *)
	  m
      | Mesh_Engine_Finished_Force_Equilibrium_Reached (m,f) ->
	  let () = loginfo (Printf.sprintf "Equilibrium Reached%!") in
          (* XXX Hey, force equilibrium was reached anyway. I am as good as you wanted me to be.
	     Hence, this should not be reported!
	   *)
	  m
      | _ ->
	  let () = loginfo (Printf.sprintf "IMPOSSIBLE%!") in
	  impossible()  (* This should never happen! *)
    in
    let () =
      forall_points this_mesh
	(fun pt ->
	   (* all the copies of the mesh generated along a periodic entity (edge or face) *)
	  let parallel_directions = Periodic.unmask_coords mask pt.mp_coords (fst bbox) (snd bbox) in
(*	  let () = Array.iter (fun p -> Printf.printf "parallel %s\n%!" (float_array_to_string p)) parallel_directions in
*)	  let () =
	    if Periodic.nr_true mask = fem_geometry.fem_geo_dim-1
	    then                                                                          (* store the periodic points (only when a direction is
											     completed) to be later saved in the nmesh file *)
(*              let () = Printf.printf "periodic_pts_to_save: %s\n%!"
		(String.concat " - "(Array.to_list (Array.map float_array_to_string parallel_directions)))
	      in
*)	      Hashtbl.add ht_periodic_pts_to_save (Hashtbl.length ht_periodic_pts_to_save) parallel_directions
	  in
	    (* the points of a mesh generated along a periodic entity are always stored as immobile *)
	  Array.iter
	    ( fun p ->
	      ignore(register_point dht_immobile_points p )
	     ) parallel_directions
	)
    in
    dht_immobile_points
  in

    (* when we mesh a periodic face (associated to the required periodic direction)
       we mesh the edges first, and then the interior of the face. This approach is
       strichter than the simple aprroach where we mesh of the periodic face (which is then extended with
       the missing components of the points for the two sides of each bbox direction)
       but is used to avoid possible problems when the periodic directions are more
       than one (the edges could not be set as periodic if we use the simple approach)
    *)


  let sub_face_directions filt =                                      (* all the directions needed for the required periodic mesh *)
    Periodic.periodic_directions (Array.map (fun x -> x = 1.0) filt)
  in

  (* array with all the masks needed to generate the required periodic mesh *)
  let mask_array = sub_face_directions filter in

  let corner_points = Periodic.corner_points fem_geometry.fem_geo_bounding_box in

  let ht_fixed_points = Hashtbl.create 100 in
    (* we store the corner points as well as the fixed points as immobile points for the generation of the next periodic mesh *)
  let () = Array.iter (fun p -> Hashtbl.add ht_fixed_points p (Hashtbl.length ht_fixed_points)) (reduce_array (Array.append fixed_points corner_points)) in

  let dht_immobile_points =
    let rec walk so_far immobile_points =
      if so_far = (Array.length mask_array) then
	hashtbl_keys immobile_points
      else
	let immobile_points = add_meshdata_of_outer_box so_far mask_array.(so_far) fem_geometry.fem_geo_bounding_box immobile_points in
	walk (so_far+1) immobile_points
    in
    walk 0 ht_fixed_points
  in
  let () = Printf.printf "Finished periodic points\n%!" in

  (dht_immobile_points, Periodic.merge_periodic_pts ht_periodic_pts_to_save)
;;

(************************************************************************)
(*                    END PERIODIC BOUNDARY CONDITIONS                  *)
(************************************************************************)

(* We need a collection of functions that help us extracting useful
   information on meshes...
 *)

(* XXX this should perhaps also allow region filtering and such... *)




let mesh_extract_surface mesh =
  let () = mesh_grow_bookkeeping_data ~do_connectivity:true mesh in
  let ht_surfaces = Hashtbl.create 100 in
  let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let () =
    forall_simplices
      (fun sx ->
	let region = sx.ms_in_body in
	let neigh = sx.ms_neighbours in
	let points = sx.ms_points in

	Array.iteri (
	fun ix_ngb neighbour ->
	  let surf_elem =
	    match neighbour with
	    | Some ngb ->

		(* some neighbour: take all the
		   points but the one corresponding to
		   the neighbour index *)

		if ngb.ms_in_body <> region
		then
		  array_one_shorter points ix_ngb
		else [||]
	    | None ->

		(* no neighbours: take all the
		   points but the one corresponding to
		   the neighbour index *)
		array_one_shorter points ix_ngb
	  in
	  let nodes_arr = surf_elem in
	  let dim = (Array.length nodes_arr) in
	  if dim = 2
	  then
	    let midpoint = array_map2 (fun x y -> 0.5 *. (x +. y)) nodes_arr.(0).mp_coords nodes_arr.(1).mp_coords in
	    let half_dist = 0.5 *. sqrt( Array.fold_left (+.) 0.0 (array_map2 (fun x y -> (x-.y)*.(x-.y)) nodes_arr.(0).mp_coords nodes_arr.(1).mp_coords)) in
	    let () =
	      Hashtbl.replace ht_surfaces (Array.map (fun pt -> pt.mp_id) nodes_arr)
		((midpoint,half_dist),
		 (midpoint,half_dist),
		 (fun (Body_Nr x) -> x) region
		)
	    in ()
	  else if dim > 2
	  then
	    let () =
	      Hashtbl.replace ht_surfaces (Array.map (fun pt -> pt.mp_id) nodes_arr)
		((Array.init dim (fun x -> 1.0), 1.0),
		 (Array.init dim (fun x -> 1.0),1.0),
		 (fun (Body_Nr x) -> x) region
		)
	    in ()
	  else (* XXX Why do we exclude the 1d case here? Hans, 18 Nov 2006 *)
	    ()
       ) neigh
      )
  in
  (points_coords,
   hashtbl_to_array ht_surfaces
     (* XXX Check: Do the data in points_coords and 'hashtbl_to_array ht_surfaces' refer to surface
	simplices in the same order? Seems unlikely but shoudl be in the same order. Hans, 19 Nov 2006*)
  )
;;


(* Works out (n-1)d-surface simplices, and their regions. Nothing
   else. This is done together because it would be very inefficient to do
   the loop over the simplices twice. See mesh_extract_surfaces to get
   more data about the surface elements. *)

let mesh_plotinfo_surfaces_and_surfacesregions mesh =
  let () = mesh_grow_bookkeeping_data ~do_connectivity:true mesh in
  let ht_surfaces = Hashtbl.create 100 in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let () =
    forall_simplices
      (fun sx ->
	let region = sx.ms_in_body in
	let neigh = sx.ms_neighbours in
	let points = sx.ms_points in
	  Array.iteri (
	      fun ix_ngb neighbour ->
		let surf_elem =
		  match neighbour with
		    | Some ngb ->
			(* some neighbour: take all the
			   points but the one corresponding to
			   the neighbour index *)
		if ngb.ms_in_body <> region
		then
		  array_one_shorter points ix_ngb
		else [||]
	    | None ->
		(* no neighbours: take all the
		   points but the one corresponding to
		   the neighbour index *)
		array_one_shorter points ix_ngb
	  in
	  let nodes_arr = surf_elem in
	  let dim = (Array.length nodes_arr) in
	  if dim >= 2
	  then
	    let () =
	      Hashtbl.replace ht_surfaces
		(Array.map (fun pt -> pt.mp_id) nodes_arr)
		((fun (Body_Nr x) -> x) region)
	    in ()
	  else (* XXX Why do we exclude the 1d case here? Hans, 18 Nov 2006 *)
	    ()
       ) neigh
      )
  in (* Now convert dictionarry into two arrays (with data in same order): YYY*)
  let dataarray = hashtbl_to_array ht_surfaces in
  let surfsimplices = Array.make (Array.length dataarray) [||] in
  let regionids = Array.make (Array.length dataarray) (-3) in
  let ()= Array.iteri (
      fun i data -> let (ssimp,region) = data in
      let () = surfsimplices.(i) <- ssimp in
      let () = regionids.(i) <- region in ()
    ) dataarray
in (surfsimplices,regionids);;


let mesh_plotinfo_links mesh =
  let ht_links = Hashtbl.create 1000 in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let () = forall_simplices
    (fun sx -> do_for_any_two_of sx.ms_points (
	fun p q -> Hashtbl.replace ht_links (p.mp_id,q.mp_id) true)
    )
  in
    map_hashtbl_to_array (fun k v -> k) ht_links;;


(* This (mesh_plotinfo_links_faster mesh)is an attempt to make
   mesh_plotinfo_links faster by not allocating memory for the link
   tuples every time we test whether the tuple is in the dictionary
   already. For a mesh with approx 5000 points, 200000 simplices and
   40000 links, this changed the execution time from 0.23 seconds to 0.22
   seconds, i.e. approx 5%. If we desperately need speed, we can re-asses
   other plot_meshinfo functions for the same problem and use the
   mesh_plotinfo_links_faster. For now, I leave the code in but we wont
   use it because the old code is clearer and not significantly
   slower. Hans, 21/11/2006 *)

let mesh_plotinfo_links_faster mesh =
  let () = loginfo (Printf.sprintf("in new links")) in
  let ht_links = Hashtbl.create 1000 in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let pre_key = Array.make 2 0 in  (* Use mutable array to avoid repeated allocation of memory *)
  let () = forall_simplices
    (fun sx -> do_for_any_two_of sx.ms_points (
	fun p q ->
	  let () = pre_key.(0) <- p.mp_id in
	  let () = pre_key.(1) <- q.mp_id in
	  let _ = try Hashtbl.find ht_links pre_key with
	    | Not_found -> let () = Hashtbl.add ht_links (Array.copy pre_key) true in true (* return true to get boolean *)
	  in ()
      )
    )
  in
    map_hashtbl_to_array (fun k v -> (k.(0),k.(1))) ht_links;;

let mesh_plotinfo_points mesh =
  Array.map (fun p -> p.mp_coords) mesh.mm_points;;

let mesh_plotinfo_pointsregions mesh =
  let npoints = Array.length mesh.mm_points in
  let pointregions = Array.make npoints [||] in
  let () =
    for i=0 to (npoints-1) do
      pointregions.(i) <- Array.of_list (List.map (fun (Body_Nr x) -> x) mesh.mm_points.(i).mp_in_body);
    done
  in pointregions;;


let mesh_plotinfo_simplices mesh =
  let nsimplices = Array.length mesh.mm_simplices in
  let simplices = Array.make nsimplices [||] in
  let () =
    for i=0 to (nsimplices-1) do
      simplices.(i) <- Array.map (fun point -> point.mp_id) mesh.mm_simplices.(i).ms_points;
    done
  in simplices;;

let mesh_plotinfo_simplicesregions mesh =
  let nsimplices = Array.length mesh.mm_simplices in
  let simplicesregions = Array.make nsimplices (-3) in
  let () =
    for i=0 to (nsimplices-1) do
      simplicesregions.(i) <-
	let Body_Nr x = mesh.mm_simplices.(i).ms_in_body in x;
    done
  in simplicesregions;;


let mesh_plotinfo_periodic_points_indices mesh =
  mesh.mm_periodic_points





(* Note: we may eventually want to get rid of this! *)
let mesh_plotinfo mesh =
  let ht_links = Hashtbl.create 100 in
  let ht_simplices = Hashtbl.create 100 in
  let points_coords = mesh_plotinfo_points mesh in
  let get_ic_mp = Simplex.get_incircle_midpoint mesh.mm_simplex_data in
  let get_cc_mp = Simplex.get_circumcircle_midpoint mesh.mm_simplex_data in
  let get_ic_r = Simplex.get_incircle_radius mesh.mm_simplex_data in
  let get_cc_r = Simplex.get_circumcircle_radius mesh.mm_simplex_data in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let () =
    forall_simplices
      (fun sx ->
         let sx_nr = sx.ms_id in
         let () =
           do_for_any_two_of sx.ms_points
             (fun p q -> Hashtbl.replace ht_links (p.mp_id,q.mp_id) true)
         in
         let () =
           Hashtbl.replace ht_simplices
             (Array.map (fun p -> p.mp_id) sx.ms_points)
             ((F.to_ml1 (get_cc_mp sx_nr), get_cc_r sx_nr),
              (F.to_ml1 (get_ic_mp sx_nr), get_ic_r sx_nr),
              (let Body_Nr x = sx.ms_in_body in x))
         in ())
  in
  (points_coords,
   map_hashtbl_to_array (fun k v -> k) ht_links,
   hashtbl_to_array ht_simplices,
   Array.map (fun p -> List.map (fun (Body_Nr x) -> x) p.mp_in_body) mesh.mm_points
  )
;;



(* === Bodies === *)

(*
   The idea is simple: provide a few elementary functions that implement certain
   body boundary conditions, as well as standard operations such as union
   and intersection, and transformations. From those, we then construct a mesh.

   Note: we do not support overlapping bodies. We do, however, try to
   support touching bodies. Our strategy will be to mesh

   (1) the exterior,

   (2) the first body,

   (3) the next body,

   (...) ...

   where in every step, we add new boundary points that show up to the
   set of fixed points that have to be respected.

   Note that one is bound to get very crazy meshes if one tried to
   just mesh overlapping bodies. I suppose this is okay.

   Note: should composing transformations and rotations be expressed
   wrt. body coordinates, or wrt. space coordinates? Presumably, the
   former is more useful for computer graphics. However, things may be
   completely different for meshing. Here, I'd suppose that the latter
   is more reasonable and will implement this for now.

 *)

(* Maybe we should really try to do projective geometry instead?!? *)

type body_trafo = Affine_Trafo of float array2 * float array;;

(* A body transformation and a boundary condition make a Mesh.body: *)

type body = Body of body_trafo * (float array -> float);;

type mesh_with_body = mesh * body;;

let body_trafo_dim (Affine_Trafo(_,disp)) = Array.length disp;;

let combine_body_transformations
    (Affine_Trafo (mx1,displacement1))
    (Affine_Trafo (mx2,displacement2))
    =
  Affine_Trafo (mx_mult mx1 mx2,
		array_pointwise (+.) displacement1 (mx_x_vec mx1 displacement2))
;;

let body_trafo_shift displacement =
  let dim = Array.length displacement in
  let mx = Array.init dim (fun row -> Array.init dim (fun col -> if row=col then 1.0 else 0.0))
  in
  Affine_Trafo (mx,displacement)
;;

let body_trafo_id dim = body_trafo_shift (Array.make dim 0.0);;

let body_trafo_scale scaling_factors =
  let dim = Array.length scaling_factors in
  let mx =
    Array.init dim
      (fun row ->
	Array.init dim
	  (fun col ->
	    if row=col then scaling_factors.(row) else 0.0))
  in
  Affine_Trafo (mx,Array.make dim 0.0)
;;

let body_trafo_rotate dim nr_axis1 nr_axis2 rad_angle =
  let co = cos rad_angle in
  let si = sin rad_angle in
  let mx = Array.init dim (fun row -> Array.init dim (fun col -> if row=col then 1.0 else 0.0)) in
  begin
    mx.(nr_axis1).(nr_axis1)<-co;
    mx.(nr_axis2).(nr_axis2)<-co;
    mx.(nr_axis2).(nr_axis1)<-si;
    mx.(nr_axis1).(nr_axis2)<-(0.0-.si);
    Affine_Trafo (mx, Array.make dim 0.0);
  end
;;

let body_trafo_rotate_axis dim axis rad_angle =
(*  Graphics Gems (Glassner, Academic Press, 1990) *)
  if dim <> 3 then failwith "This function can be used only in 3D space."
  else
    begin
      let co = cos rad_angle in
      let si = sin rad_angle in
      let t = 1. -. co in
      let d = ref 0.0 in
      let () =
	for i=0 to 2 do
	  d := axis.(i)**2.0 +. !d
	done
      in
      if !d = 0.0 then
	failwith "Specify a non-zero amplitude for the axis!!%!"
      else
	let ax = axis.(0)/.sqrt(!d) in
	let ay = axis.(1)/.sqrt(!d) in
	let az = axis.(2)/.sqrt(!d) in
	let mx = Array.init dim (fun row -> Array.init dim (fun col -> 0.0)) in
	begin
	  mx.(0).(0)<-(co+.t*.ax**2.0);
	  mx.(0).(1)<-(t*.ax*.ay-.az*.si);
	  mx.(0).(2)<-(t*.ax*.az+.ay*.si);
	  mx.(1).(0)<-(t*.ax*.ay+.az*.si);
	  mx.(1).(1)<-(co+.t*.ay**2.0);
	  mx.(1).(2)<-(t*.ay*.az-.ax*.si);
	  mx.(2).(0)<-(t*.ax*.az-.ay*.si);
	  mx.(2).(1)<-(t*.ay*.az+.ax*.si);
	  mx.(2).(2)<-(co+.t*.az**2.0);
	  Affine_Trafo (mx, Array.make dim 0.0);
	end
    end
;;
(*



let body_trafo_rotate_axis dim axis rad_angle =
(*  Graphics Gems (Glassner, Academic Press, 1990) *)
  if dim <> 3 then failwith "This function can be used only in 3D space."
  else
    begin
      let co = cos rad_angle in
      let si = sin rad_angle in
      let t = 1. -. co in
      let rec sum2 ix sum =
	if ix = dim
	then sum
	else
	  sum2 (ix+1) (axis.(ix)**2.0)
      in
      let d = sqrt( sum2 0 0.0) in
      if d = 0.0 then
	failwith "Specify a non-zero amplitude for the axis!!%!"
      else
	let u = axis.(0)/.d in
	let v = axis.(1)/.d in
	let w = axis.(2)/.d in
	let mx = Array.init dim (fun row -> Array.init dim (fun col -> 0.0)) in
	begin
	  mx.(0).(0)<-(     u**2.0 +. (v**2.0 +. w**2.0)*.co  );
	  mx.(0).(1)<-(     u*.v*.t -. w*.si                  );
	  mx.(0).(2)<-(     u*.w*.t +. v*.si                  );
	  mx.(1).(0)<-(     u*.v*.t +. w*.si                  );
	  mx.(1).(1)<-(     v**2.0 +. (u**2.0 +. w**2.0)*.co  );
	  mx.(1).(2)<-(     v*.w*.t -. u*.si                  );
	  mx.(2).(0)<-(     u*.w*.t -. v*.si                  );
	  mx.(2).(1)<-(     v*.w*.t +. u*.si                  );
	  mx.(2).(2)<-(     w**2.0 +. (u**2.0 +. v**2.0)*.co  );
	  Affine_Trafo (mx, Array.make dim 0.0);
	end
    end
;;


*)










(* We have to eventually apply those body transformations to boundary condition functions. *)

let body_trafo_apply_to_bc (Affine_Trafo (mx,displacement)) =
  let dim = Array.length displacement in
  let scratch = Array.make dim 0.0 in
  (fun bc ->
    (fun pos ->
      let pos_new = mx_x_vec ~store_result:scratch mx pos in
      begin
	for i=0 to dim-1 do
	  pos_new.(i) <- pos_new.(i) +. displacement.(i);
	done;
	bc pos_new
      end
       ))
;;

(* Let's do some simple BCs then... *)

(*
   Note that we put all our boundary conditions under a tanh.
   Rationale: this does not modify the gradient at the zero,
   and it helps to keep function values under control
   away from boundaries, which is somewhat important for CSG.

   XXX Note: the tanh was a bad idea: it confuses the hell out of our
   boundary condition enforcer!
 *)

(*
   (x0/r0)^2 + (x1/r1)^2 + ... -1
*)

let bc_ellipsoid radii =
  let dim = Array.length radii in
  let inv_radii = Array.map (fun x -> 1.0/.x) radii in
  (fun pos ->
    let rec walk n so_far =
      if n=dim then so_far
      else walk (n+1) (so_far +. (let d = pos.(n)*.inv_radii.(n) in d*.d))
    in
    let v0 = (0.0-.walk 0 (-1.0)) in
    (* tanh v0 *)
    v0
  )
;;

(* Box *)

let bc_box corner_nw corner_se =
  let dim = Array.length corner_nw in
  let midpoint = array_pointwise (fun x y -> (x+.y)*.0.5) corner_nw corner_se in
  let edge_lengths = array_pointwise (-.) corner_nw corner_se in
  let inv_half_edge_lengths = Array.map (fun x -> 2.0/.x) edge_lengths in
  (fun pos ->
    let rec walk n so_far =
      if n = dim then so_far
      else
	let x_n = abs_float ((pos.(n) -. midpoint.(n)) *. inv_half_edge_lengths.(n)) in
	walk (n+1) (max x_n so_far)
    in
    let max_rel_dist_center = walk 0 0.0 in
    let v0 = 1.0 -. max_rel_dist_center in
    (* tanh v0 *)
    v0)
;;


let bc_helix (center1,radius1) (center2,radius2) =
  let r_spiral = radius1 in
  let r_circle = radius2 in
  let dim = Array.length center1 in
    if r_circle <= 0.0 || r_spiral <= 0.0 then failwith "Value error for radii"
    else if dim <> 3 then failwith "The spiral is defined only in 3D"
    else
      let axis = array_pointwise (-.) center2 center1 in
      let projection_to_axis v =
	let rec walk index sum_now =
	  if index=dim then sum_now
	  else
            walk (1+index) (sum_now+.axis.(index)*.v.(index))
	in walk 0 0.0
      in
	(*  let axis_length_sq = projection_to_axis axis in
	    RRR
	*)
      let axis_projection_center1 = projection_to_axis center1 in
      let axis_projection_center2 = projection_to_axis center2 in
      let ap12 = axis_projection_center2 -. axis_projection_center1 in
      let min_ap = min axis_projection_center1 axis_projection_center2 in
      let max_ap = max axis_projection_center1 axis_projection_center2 in
      let centre12 = 0.5*.(axis_projection_center2 +. axis_projection_center1) in
      let scratch_axis_projection = Array.make dim 0.0 in
	(fun pos ->
	   let ap = projection_to_axis pos in
	   let axis_factor = (ap-.axis_projection_center1)/.ap12 in
	   let out_top_bottom =
	     if (ap > centre12)
	     then
	       begin
		 (ap-.max_ap)/.(centre12-.max_ap) (* outside & above *)
	       end
	     else if (ap < centre12)
	     then
	       begin
		 (min_ap-.ap)/.(min_ap-.centre12) (* outside & below *)
	       end
	     else 1.0
	   in
	   let alpha = 8.*.pi*. axis_factor in
	   let angle = [|cos alpha; sin alpha; 0.0 |] in
	   let r_h_circle = r_circle*.(1. -. axis_factor) in
	   let r_h_spiral = r_spiral*.(1. -. axis_factor) in
	   let () =
	     for i=0 to dim-1 do
               scratch_axis_projection.(i)
               <- center1.(i) +. axis_factor *. axis.(i)
	     done
	   in
           let hlx = r_h_circle**2. -. (euclidean_len_sq (Array.mapi (fun ix p -> p -. (scratch_axis_projection.(ix) +. r_h_spiral *. angle.(ix))) pos) ) in
(*           let () = Printf.printf "pos:%s  ap:%f axis_factor:%f alpha:%f r_h_circle:%f r_h_spiral:%f hlx:%f%!" (float_array_to_string pos) ap axis_factor alpha r_h_circle r_h_spiral hlx
	   in
*)	     min out_top_bottom  hlx
	)
;;



(* This will also specialize to cone and cylinder. *)

let bc_frustum (center1,radius1) (center2,radius2) =
  if (min radius1 radius2) < 0.0 || (max radius1 radius2) = 0.0
  then failwith "Value error for radii";
  let dim = Array.length center1 in
  let axis = array_pointwise (-.) center2 center1 in
  let projection_to_axis v =
    let rec walk index sum_now =
      if index=dim then sum_now
      else
        walk (1+index) (sum_now+.axis.(index)*.v.(index))
    in walk 0 0.0
  in
(*  let axis_length_sq = projection_to_axis axis in
    RRR
*)
  let axis_projection_center1 = projection_to_axis center1 in
  let axis_projection_center2 = projection_to_axis center2 in
  let min_ap = min axis_projection_center1 axis_projection_center2 in
  let max_ap = max axis_projection_center1 axis_projection_center2 in
  let ap12 = axis_projection_center2 -. axis_projection_center1 in
  let centre12 = 0.5*.(axis_projection_center2 +. axis_projection_center1) in
  let r12 = radius2 -. radius1 in
  let scratch_axis_projection = Array.make dim 0.0 in
  (fun pos ->
    let ap = projection_to_axis pos in
    let out_top_bottom =
      if (ap > centre12)
      then
	begin
	  (ap-.max_ap)/.(centre12-.max_ap) (* outside & above *)
	end
      else if (ap < centre12)
      then
	begin
	  (min_ap-.ap)/.(min_ap-.centre12) (* outside & below *)
	end
      else 1.0
    in
    let axis_factor = (ap-.axis_projection_center1)/.ap12 in
    let radius_here = radius1+.r12*.axis_factor in
    begin
      for i=0 to dim-1 do
        scratch_axis_projection.(i)
        <- center1.(i) +. axis_factor *. axis.(i)
      done;
      let axis_distance =
        sqrt (euclidean_distance_sq pos scratch_axis_projection)
      in
      min out_top_bottom  (radius_here -. axis_distance)
    end
  )
;;

let bc_convex_hull (points:float array array) =
  failwith "XXX write me: convex_hull"
;;

(* XXX Maybe, we'd like to have an extra arg specifying whether this is given in body coords,
   or space coords - or we may want to represent movement in such a way that the vector "knows"
   what coordinate system it refers to.
 *)

let body_apply_trafo (Body (trafo,bc)) next_trafo =
  Body (combine_body_transformations next_trafo trafo, bc)
;;

(* Note: could be optimized somewhat. I do not care about that for now. *)

let body_csg_11 operation (Body (t1,bc1)) (Body (t2,bc2)) =
  let bc1t = body_trafo_apply_to_bc t1 bc1
  and bc2t = body_trafo_apply_to_bc t2 bc2 in
  let Affine_Trafo (_,disp) = t1 in
  let dim = Array.length disp in
  let id = body_trafo_shift (Array.make dim 0.0) in
  Body (id,(pointwise operation bc1t bc2t))
;;

(* One-body-to-n-bodies *)

let body_csg_1n to_fold (Body (t1,bc1)) other_bodies =
  let bc1t = body_trafo_apply_to_bc t1 bc1
  and bcts = Array.map (fun (Body (t,bc)) -> body_trafo_apply_to_bc t bc)
      other_bodies in
  let Affine_Trafo (_,disp) = t1 in
  let dim = Array.length disp in
  let id = body_trafo_shift (Array.make dim 0.0) in
  Body(id,(fun pos -> Array.fold_left (fun sf bc -> to_fold sf (bc pos)) (bc1t pos) bcts))
;;

let body_csg_n to_fold bodies =
  let () = loginfo (Printf.sprintf "body union n %d %!" (Array.length bodies)) in
  let nr_bodies = Array.length bodies in
  if nr_bodies=0 then failwith "No-Body CSG encountered!"
  else if nr_bodies=1 then
    bodies.(0)
      (* We just assume that Op(1 body) is just this body again. *)
  else
    (* I am a lazy person. For now, let's just reduce this to the _1n case.
       Could be improved, though.
     *)
    body_csg_1n to_fold bodies.(0) (Array.sub bodies 1 (nr_bodies-1))
;;


let body_union_11 = body_csg_11 max;;
let body_union_1n = body_csg_1n max;;
let body_union_n  =
  body_csg_n max;;

let body_intersection_11 = body_csg_11 min;;
let body_intersection_1n = body_csg_1n min;;
let body_intersection_n  = body_csg_n min;;

let (body_difference_11,body_difference_1n) =
  let sub = (fun a b -> min a (0.0-.b)) in
  (body_csg_11 sub, body_csg_1n sub)
;;



let extract_structures_from_hints hints =                                           (* extract the mesh associated to each hint; all the mesh is extracted, *)
  Array.map                                                                         (* so we need another function which filters only the part within the hint body *)
    (fun (mesh, body) ->
      (mesh_plotinfo_points mesh, mesh_plotinfo_simplices mesh)
    )
    hints



let fem_geometry_from_bodies                                                         (* this function is called by pyfem to build the fem geometry structure *)
    ?(mesh_exterior=false)
    ?(density=(fun _ -> 1.0))
    ((corner_nw,corner_se) as bounding_box) bodies
    hints
    =
  let dim = Array.length corner_nw in

  let body_bcs =                                                                     (* boundary conditions from the objects defined by the user *)
    Array.map (fun (Body (t,bc)) -> body_trafo_apply_to_bc t bc) bodies
  in

  let hints_bcs =                                                                    (* boundary conditions from objects extracted from loaded meshes *)
    Array.map
      (fun (mesh, body) ->
        let (Body (t,bc)) = body in
	  body_trafo_apply_to_bc t bc)
      hints
  in

  let () = Printf.printf "Dimension of hints_bcs array: %d\n%!" (Array.length hints_bcs) in

  let body_bcs = Array.append  body_bcs hints_bcs in                                 (* the total boundary conditions include also the hints ones, which come at last *)
  let body_bcs =                                                                     (* and therefore will be "meshed" first *)
    ( let bc_outer = bc_box corner_nw corner_se in
	if mesh_exterior                                                             (* include the outer box in the list of boundary conditions if the user wants to mesh it *)
	then Array.append [|fun pos -> Array.fold_left (fun sf bc -> min sf (0.0-.(bc pos))) (bc_outer pos) body_bcs|] body_bcs

        else Array.map (fun bc -> fun pos -> Array.fold_left (fun sf bn -> min sf (bn pos)) (bc_outer pos) [|bc|]) body_bcs)
  in
    (* The simplex classificator we are using here just looks in which body the center of gravity
       is located. This is a very simplistic, but usually quite effective heuristics.
    *)
  let body_finder pos = array_position_if (fun bc -> bc pos > 0.0) body_bcs 0 in
  let (*classificator*) _ =
    make_simplex_lines_of_gravity_applicator dim
      (fun center_of_gravity lines_of_gravity log_lens vol ->
	Body_Nr (1+body_finder center_of_gravity))
  in
  let geo_piece_hints =                                                               (* add empty tuples for all the bcs which don't correspond to an hint object *)
    let bcs_len = Array.length body_bcs in
    let hints_obj = extract_structures_from_hints hints in
    let hints_len = Array.length hints_obj in
      Array.append (Array.make (bcs_len - hints_len) ([||],[||]))  hints_obj
  in
    {
      fem_geo_dim = dim;
      fem_geo_bounding_box = bounding_box;
      fem_geo_density = density;
      fem_geo_boundaries = (Array.mapi (fun n bc -> ((Body_Nr (if mesh_exterior then n else 1+n)),bc)) body_bcs);
      fem_geo_piece_hints = geo_piece_hints;
      fem_geo_simplex_classificator = (fun x -> Body_Nr 1);       (* the classificator is hacked: double check if this is the right behaviour *)
    }
;;

(*

Example:

# fem_geometry_from_bodies ~mesh_exterior:true ([|-5.0;-5.0;-5.0|],[|5.0;5.0;5.0|]) [|Body ((body_trafo_id 3),bc_ellipsoid [|1.0;1.0;1.0|])|];;
- : fem_geometry =
{fem_geo_dim = 3; fem_geo_bounding_box = ([|-5.; -5.; -5.|], [|5.; 5.; 5.|]);
 fem_geo_density = <fun>;
 fem_geo_boundaries = [|(Body_Nr 0, <fun>); (Body_Nr 1, <fun>)|];
 fem_geo_simplex_classificator = <fun>}

*)

(* Problem: we want to mesh composite objects subsequently.

   First question: do we always want to mesh the exterior
   as well, that is, include the bounding box?

   Perhaps, a cavalier way to resolve this is to offer a function
   which extends boundary conditions by an extra one for the
   "background".

   Maybe, we should just provide different meshing functions, or a
   "meshing mode" parameter, which can specify:

   * objects-only
   * with-box
   * pbc-box

   In any case, the problem with PBCs is that we need nodes of an
   extra class - multiply occurring boundary nodes.

   For now, let us just forget about PBC meshing and work this in
   later on - it can be done.
 *)



(* We have to classify points w.r.t. which objects they belong to.
   Note that a point in the mesh may belong to more than one object.

   Default heuristics:

   For every simplex, go through all points and pull them 1/100 closer
   to the centroid, and push them 1/100 further away. If the resulting
   points are classified as lying on different sides w.r.t. some
   boundary condition, then the point is considered to lie on that boundary.

   Note that we have to do this over and over again, and gather
   information about a point from all the simplices it belongs to.

   XXX maybe we should handle the scaling in a somewhat different way, so that we can interface nicer
   with other plotting functions...

*)

let mesh_boundaries_and_objects
    ?(gendriver=(fun _ -> default_driver))                                   (* driver *)
    ?(rng= Mt19937.make 97)                                                  (* random number generator *)
    raw_fixed_points                                                         (* fixed points *)
    raw_mobile_points                                                        (* mobile points - they are given initially and can be
                                                                                (re)moved by the mesher *)
    simply_points                                                            (* points without cleaning of surface (or interior) bad simplices *)
    fem_geometry                                                             (* kernel of the functions defining the objects + their transformations *)
    mdefaults                                                                (* mesher parameters *)
    length_scale                                                             (* rod lenght of the mesh *)
    filter                                                                   (* periodicity flags *)
    =

  let () = logdebug (Printf.sprintf "\nenter mesh_boundaries_and_objects") in

(*
    UNUSED: IT WAS INTENDED TO OBTAIN A MESH DIRECTLY FROM QHULL, WITH
    ALL ITS PROBLEMS ON THE SURFACES
if (Array.length simply_points > 0 )                                       (* mesh the points without "cleaning" the mesh *)
  then
    let pts_states = Array.map (fun p -> Boundary) simply_points in
    let dim = Array.length simply_points.(0) in
      mesh_it_work_forward
	dim
	length_scale
	fem_geometry
	mdefaults
	pts_states
	simply_points
  else                                                                       (* proper generation of the mesh from the given objects *)
*)
  let (corner_nw, corner_se) = fem_geometry.fem_geo_bounding_box in
  let bounding_box = bc_box corner_nw corner_se in                           (* function defining the bounding box *)

  let ht_no_repeated_points =                                                (* hashtable to avoid presence of double points *)
    Hashtbl.create
      ((Array.length raw_fixed_points)+(Array.length raw_mobile_points))
  in

  let point_in_ht point =                                                    (* check if a point is already in the hashtable *)
    let already_in = Hashtbl.mem ht_no_repeated_points point in
    let() =
      if already_in
      then Hashtbl.remove ht_no_repeated_points point
      else ()
    in
      already_in
  in

  let add_point_in_ht point =                                                (* add point to the hashtable *)
    let already_in = Hashtbl.mem ht_no_repeated_points point in
      if already_in
      then ()
      else Hashtbl.add  ht_no_repeated_points point 1
  in

  let () =                                                                   (* add all the given points to the hashtable *)
    for i=0 to (Array.length raw_fixed_points)-1 do
      add_point_in_ht raw_fixed_points.(i)
    done;
    for i=0 to (Array.length raw_mobile_points)-1 do
      add_point_in_ht raw_mobile_points.(i)
    done;
  in

  let fixed_points =                                                         (* filter out all the fixed points that are outside the bounding box
                                                                                or appear twice in the list of given points  *)
    array_filter (fun p ->
      (bounding_box p) > (0.0 -. mdefaults.mdefault_boundary_condition_acceptable_fuzz) && point_in_ht p)
      raw_fixed_points
  in
  let mobile_points =                                                        (* filter out all the mobile points that are outside the bounding box
                                                                                or appear twice in the list of given points  *)
    array_filter
      (fun p ->
	(bounding_box p) > (0.0 -. mdefaults.mdefault_boundary_condition_acceptable_fuzz) && point_in_ht p)
      raw_mobile_points
  in

  let (periodic_points, ht_periodic_pts_to_save) =                                                      (* if one or more of the flags in the periodicity vector is set to
                                                                                one, perform the associate "periodization" in the corresponding
                                                                                dimensions *)
    if List.exists (fun x -> x = 1.0) (Array.to_list filter)
    then
      let () = loginfo (Printf.sprintf "*************** PERIODIC **************") in
      mesh_periodic_outer_box
	~gendriver:gendriver ~rng:rng fixed_points fem_geometry mdefaults length_scale filter
    else
      [||],(Hashtbl.create 1)
  in
  let initial_points =                                                        (* set the initial points of the mesh; if a periodicity along one
                                                                                 or more axis is given, include the generated periodic points to
                                                                                 the set of fixed points *)
    reduce_array (Array.append fixed_points periodic_points)
  in
  let () = logdebug (Printf.sprintf "\ngo to mesh_it_work") in
  mesh_it_work                                                                (* eventually call the mesher! *)
    ~gendriver:gendriver
    ~rng:rng
    ~fixed_points:initial_points
    ~mobile_points:mobile_points
    ~simply_points:simply_points  (* all the conditions on the points within the boundaries are useless if we use
				     this feature, but it shouldn't be a problem. gb *)
    ht_periodic_pts_to_save
    fem_geometry
    mdefaults
    length_scale
    (* this is the result of mesh_from_pieces_meshdata*)
;;


let mesh2d_ps
    ?(scale=(fun pos -> [|100.0*.pos.(0)+.100.0;100.0*.pos.(1)+.100.0|]))
    ?further_ps	(* Other postscript stuff to put into the graph *)
    mesh filename =
  let ps_header =
"
/colors_by_id 7 array def
colors_by_id 0   1.00 1.00 1.00 3 array astore put
colors_by_id 1   0.00 1.00 0.00 3 array astore put
colors_by_id 2   0.00 0.00 1.00 3 array astore put
colors_by_id 3   1.00 0.00 0.00 3 array astore put
colors_by_id 4   1.00 1.00 0.00 3 array astore put
colors_by_id 5   0.00 1.00 1.00 3 array astore put
colors_by_id 6   1.00 0.00 1.00 3 array astore put

/debug {stack pop} def

/setcol
{dup -1 lt
  {pop 0.2 0.2 0.2}
  {dup -1 eq
    {pop 0.0 0.0 0.0}
    {dup 6 gt
      {pop 0.7 0.7 0.7}
      {colors_by_id exch get aload pop} ifelse } ifelse } ifelse
  setrgbcolor
} def

/points 0 def % changed later on

/bounded_triangle % x0 y0 x1 y1 x2 z2 color triangle
{
  setcol
  6 array astore
  dup dup 0 get exch 1 get moveto
  dup dup 2 get exch 3 get lineto
  dup dup 4 get exch 5 get lineto
  closepath fill stroke
  0 0 0 setrgbcolor
  dup dup 0 get exch 1 get moveto
  dup dup 2 get exch 3 get lineto
  dup dup 4 get exch 5 get lineto
  closepath stroke
  pop
}
def

/tri % p0 p1 p2 col tri
{
 4 1 roll % col p0 p1 p2
 3 2 roll % col p1 p2 p0
 2 mul dup points exch get exch 1 add points exch get % col p1 p2 p0x p0y
 4 3 roll
 2 mul dup points exch get exch 1 add points exch get % col p2 p0x p0y p1x p1y
 5 4 roll
 2 mul dup points exch get exch 1 add points exch get % col p0x p0y p1x p1y p2x p2y
 7 6 roll bounded_triangle
} def

/arrow % x_origin y_origin x_vec y_vec
% Note: this definition is scale invariant!
{
  matrix currentmatrix % remember current transformation
  5 1 roll % bury matrix
  4 2 roll % mx x_vec y_vec x_origin y_origin
  translate
  dup 3 2 roll dup 3 1 roll % mx y_vec x_vec y_vec x_vec
  dup mul exch dup mul add sqrt % mx y_vec x_vec length
  dup 0 ne % null vector requires special treatment!
  {
    3 1 roll
    atan rotate
    0 0 moveto dup 0 lineto stroke % drawnig the line
    dup 0 moveto dup 0.7 mul dup 0.15 mul lineto dup 0.7 mul dup -0.15 mul lineto closepath fill stroke
    pop
  }
  {pop pop pop}
  ifelse
  setmatrix
} def

/ybar % x_origin y_origin height
{
  matrix currentmatrix % remember current transformation
  4 1 roll % bury matrix
  3 1 roll % mx height x_origin y_origin
  translate
  0 0 moveto 0 exch lineto stroke % drawing the bar
  setmatrix
} def
"
  in
  let forall_simplices f = Array.iter f mesh.mm_simplices in
  let print_points fh =
    Array.iteri
      (fun n pt ->
	let pos = scale pt.mp_coords in
	Printf.fprintf fh "%s%f %f" (if n mod 4 = 0 then "\n" else "  ") pos.(0) pos.(1))
      mesh.mm_points
  in
  let out_fh = open_out filename in
  begin
    Printf.fprintf out_fh "%s" ps_header;
    Printf.fprintf out_fh "\n/points";
    print_points out_fh;
    Printf.fprintf out_fh "\n%d array astore def\n" (2*Array.length mesh.mm_points);
    forall_simplices
      (fun sx ->
	let Body_Nr region = sx.ms_in_body in
	Array.iter (fun p -> Printf.fprintf out_fh " %d" p.mp_id) sx.ms_points;
	Printf.fprintf out_fh " %d tri\n" region);
    Printf.fprintf out_fh "\n";
    (match further_ps with
    | None -> ()
    | Some f -> f out_fh);
    close_out out_fh;
  end
;;

(*
   This actually does not use the mesher at all, but it is
   nevertheless helpful to have it:
*)

let mesh_connectivity mesh =
  let points = mesh.mm_points in
  let nr_points = Array.length points in
  let adjacency = Array.make nr_points [] in
  let () =
    let rec contains elem li =
      match li with
      | [] -> false
      | (hd::tl) -> if hd = elem then true else contains elem tl
    in
    Array.iter
      (fun sx ->
	let sx_point_ids = Array.map (fun p -> p.mp_id) sx.ms_points in
	do_for_any_two_of sx_point_ids
	  (fun ix1 ix2 ->
	    begin
	      (if contains ix1 adjacency.(ix2)
	      then ()
	      else adjacency.(ix2) <- ix1::adjacency.(ix2));
	      (if contains ix2 adjacency.(ix1)
	      then ()
	      else adjacency.(ix1) <- ix2::adjacency.(ix1));
	    end))
      mesh.mm_simplices
  in adjacency
;;

(* Permuting (re-labeling) the points in a mesh should be simple and straightforward,
   as we usually reference them directly, not via their ID.

   So, "all that has to be done is to permute the point array and adjust the mp_id fields".
 *)

let mesh_do_reorder_vertices mesh permutation =
  let points = mesh.mm_points in
  let nr_points = Array.length points in
  let reordered_points = Array.init nr_points (fun n -> points.(permutation n)) in
  begin
    Array.iteri (fun n p -> p.mp_id <- n) reordered_points;
    mesh.mm_points <- reordered_points;
    mesh.mm_mesh0 <- mesh0_from_mesh mesh.mm_points mesh.mm_simplices;
    (* That seems necessary to me, after the change I made
       even if the simulations I made did not require it, apparenlty.
       Should investigate this better.
       mf, 18 Aug 2010 *)
    ()
  end
;;


(* ** internal only ** *)
let find_simplex_exit_point_and_nr_face simplex target_point entry_point nr_entry_face =
  let nr_faces =
    let simplex_data, _ = simplex in Simplex.get_num_points simplex_data
  in
  let rec walk nr_face min_param exit_point nr_exit_face =
    if nr_face >= nr_faces
    then
      (exit_point,
       if min_param >= 1.0
       then (-1) (* It's within this very simplex! *)
       else nr_exit_face)
    else
      if nr_face = nr_entry_face
      then walk (1+nr_face) min_param exit_point nr_exit_face
      else
	let a = simplex_face_eqn simplex nr_face in
        let param = line_hyperplane_intersection_param entry_point target_point a
        in
          if param > 0.0 && param < min_param
          then walk (nr_face+1) param (line_point entry_point target_point param) nr_face
          else walk (nr_face+1) min_param exit_point nr_exit_face
  in
    walk 0 max_float entry_point (-1)
;;


(* ** internal only ** *)

let find_simplex mesh (origin_coords, origin_simplex) target_coords =
  let sd = mesh.mm_simplex_data in
  let get_inv_point_matrix = Simplex.get_inv_point_matrix sd in
  let this_is_roughly_okay sx_nr =
    let sx_inv_point_mx = get_inv_point_matrix sx_nr in
    let dim = Array.length target_coords in
    let ext_target_coords =
      Array.init (1+dim) (fun n -> if n < dim then target_coords.(n) else 1.0)
    in
    let local_coords = matrix_x_vec sx_inv_point_mx ext_target_coords in
      array_position_if (fun x -> x > 1.01 || x < (-0.01)) local_coords 0 = (-1)
  in
  let rec walk cur_sx coords_now nr_face_coming_from =
    let cur_sx_nr = cur_sx.ms_id in
    let (exit_point, nr_exit_face) =
      find_simplex_exit_point_and_nr_face
	(sd, cur_sx_nr) target_coords coords_now nr_face_coming_from
    in
      if nr_exit_face = (-1)
      then cur_sx
      else
	match cur_sx.ms_neighbours.(nr_exit_face) with
	  | None ->
	      if this_is_roughly_okay cur_sx_nr
	      then cur_sx
	      else raise Not_found
	  | Some neighbour_simplex ->
	      walk neighbour_simplex
		exit_point
		cur_sx.ms_neighbour_backrefs.(nr_exit_face)
  in
    walk origin_simplex origin_coords (-1)
;;



let mesh_locate_point mesh point =
  let origins = mesh.mm_origins in
  let nr_origins = Array.length origins in
  let get_ic_mp = Simplex.get_incircle_midpoint mesh.mm_simplex_data in
  let get_cc_mp = Simplex.get_circumcircle_midpoint mesh.mm_simplex_data in
  let get_cc_r = Simplex.get_circumcircle_radius mesh.mm_simplex_data in
  let get_inv_point_matrix =
    Simplex.get_inv_point_matrix mesh.mm_simplex_data in
    (* If all else fails, we have to do point location the hard way:
       walk all mesh simplices.

       Note: we can speed things up by only considering those simplices that
       have the point in question inside their circumcircle.
    *)
  let locate_point_in_simplex sx =
    let ext_coords =
      Array.init
	(1+mesh.mm_dim)
	(fun n -> if n < mesh.mm_dim then point.(n) else 1.0)
    in
    let sx_inv_point_mx = get_inv_point_matrix sx.ms_id
    in
      (sx, matrix_x_vec sx_inv_point_mx ext_coords)
  in
  let walk_simplices () =
    let simplices = mesh.mm_simplices in
    let nr_simplices = Array.length simplices in
    let rec walk n =
      if n = nr_simplices then raise Not_found
      else
	let sx = simplices.(n) in
	let ic_midpoint = F.to_ml1 (get_ic_mp n) in
	let cc_midpoint = F.to_ml1 (get_cc_mp n) in
	let cc_radius = get_cc_r n in
	let r2 = cc_radius*.cc_radius in
	  if euclidean_distance_sq cc_midpoint point > r2
	  then walk (1+n)
	  else
	    let sx_found =
	      try
		Some (find_simplex mesh (ic_midpoint, sx) point)
	      with | Not_found -> None
	    in
	      match sx_found with
		| None -> walk (1+n)
		| Some s ->
		    (* Register a new origin and return the simplex *)
		    let origins = mesh.mm_origins in
		    let nr_old_origins = Array.length origins in
		    let new_origins =
		      Array.init
			(1+nr_old_origins)
			(fun n -> if n=0 then (ic_midpoint, s) else origins.(n-1))
		    in
		    let () = mesh.mm_origins <- new_origins in
		      locate_point_in_simplex s
    in walk 0
  in
  let rec walk_origins nr_origin =
    if nr_origin = nr_origins then walk_simplices ()
    else
      try
	let sx = find_simplex mesh origins.(nr_origin) point in
	  (* We succeeded in locating the simplex.
	     Now, exchange the origin from which
	     we located the point successfully with the
	     very first origin. This will speed things
	     up if we move the "probe point" just
	     a little bit.
	  *)
	let origin0 = origins.(0) in
	  begin
	    origins.(0) <- origins.(nr_origin);
	    origins.(nr_origin) <- origin0;
	    locate_point_in_simplex sx
	  end
      with
	| Not_found -> walk_origins (1+nr_origin)
  in walk_origins 0
;;


(* this function scales the nodes of a mesh by a common scaling factor.
   We do it this way. We scale the coordinates and throw away all the computed
   simplex data. That is lazy, but in practice does not lead to performance
   penalties if the mesh is scaled just after being loaded, which is virtually
   always the case (cannot imagine why one may load the mesh, use it, scale it
   and then use it again). *)
let scale_node_positions mesh scaling_factor =
  let dim = Array.length mesh.mm_points.(0).mp_coords in
  let () =
    Array.iter
      (fun pt ->
         for i=0 to dim-1 do
           pt.mp_coords.(i) <- scaling_factor *. pt.mp_coords.(i)
         done)
      mesh.mm_points
  in
  let mesh0 = mesh0_from_mesh mesh.mm_points mesh.mm_simplices in
  let simplex_data = Simplex.init mesh0
  in
    begin
      mesh.mm_mesh0 <- mesh0;
      mesh.mm_simplex_data <- simplex_data;
      mesh.mm_region_volumes <-
        _region_volumes mesh.mm_simplex_data mesh.mm_simplices;
    end

;;

(* Important Note: a loading function MUST check the validity of the data file,
   and in particular ensure that it cannot be crashed by providing a broken data file!

 *)

(*#load "str.cma";;*)

(* function that returns a mesh structure from
   the file containing mesh data
*)


let write_mesh filename mesh =
  let nr_vertices = Array.length mesh.mm_simplices.(0).ms_points in
  let dim = Array.length mesh.mm_points.(0).mp_coords in
  let scratch_points_coords_for_sx = Array.make_matrix nr_vertices nr_vertices 0.0 in
  let scratch_points_coords_for_face = Array.make_matrix dim dim 0.0 in

  let () = mesh_grow_bookkeeping_data ~do_connectivity:true (*~do_regions:true*) mesh in
  let format_version = "1.0" in
  let output_file = open_out filename in
  let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in

  let forall_simplices f = Array.map f mesh.mm_simplices in

  let points_indices_region_of_simplices =
    forall_simplices
      (fun sx ->
	let () =
	  for i=0 to dim do
	    for c=0 to dim-1 do
	      scratch_points_coords_for_sx.(i).(c) <- sx.ms_points.(i).mp_coords.(c)
	    done;
	    scratch_points_coords_for_sx.(i).(dim) <- 1.0
	  done
	in
	let det = determinant (dim+1) scratch_points_coords_for_sx in             (* this re-orderingn should be *)
	let scratch_sx_indices = Array.map (fun p -> p.mp_id) sx.ms_points in     (* useless now because such operation *)
	let sx_indices =                                                          (* is performed when we extract the mesh *)
	  let () =                                                                (* but I keep it for the cases where *)
	    if det < 0.                                                           (* a given mesh is loaded and then re-saved *)
	    then
	      let aux = scratch_sx_indices.(0) in
	      let () = scratch_sx_indices.(0) <- scratch_sx_indices.(1) in
		scratch_sx_indices.(1) <- aux
	    else ()
	  in
	    scratch_sx_indices
	in
	let Body_Nr sx_region = sx.ms_in_body
	in (sx_indices, sx_region)
      ) in
  (* store the region which are separated by each surface *)
  let ht_surface_regions_info = Hashtbl.create ((Array.length mesh.mm_simplices)/5) in
  let register_surf_in_ht surf_ixs region =
    let sorted_surf_ixs = Array.copy surf_ixs  in  (* without this all the keys are updated, man!! *)
    let () = Array.sort compare sorted_surf_ixs in
    try    (* the comparison is done on the sorted list of face point indices *)
      let (regions_sf,det) = Hashtbl.find ht_surface_regions_info sorted_surf_ixs in
	Hashtbl.replace ht_surface_regions_info sorted_surf_ixs ((region::regions_sf),det)
    with
    | Not_found ->
	  let () =   (* the first time I compute&store the determinant of the unsorted indices, so later on I know which region to print first *)
	    for i=0 to dim-1 do
	      for c=0 to dim-1 do
		scratch_points_coords_for_face.(i).(c) <- points_coords.(surf_ixs.(i)).(c)
	      done;
	    done
	  in
	  let det = determinant (Array.length surf_ixs) scratch_points_coords_for_face in
	  Hashtbl.add ht_surface_regions_info sorted_surf_ixs ([region],det)
  in
  let () =
    Array.iter (fun sx ->
      let region = sx.ms_in_body in
      let backref = sx.ms_neighbour_backrefs in
      let neigh = sx.ms_neighbours in
      let nr_neigh = Array.length neigh in
      let check_outer_elem =
	array_all_satisfy (fun x -> x >= 0) backref
      in
	let () =
	  if check_outer_elem then
	    ()
	  else
	    let rec find_surface_nodes ix =
	      if ix = nr_neigh then ()
	      else
		let pos = (array_position (-1) backref ix) in
		if pos > (-1) then
		  let surf_ixs = (array_one_shorter (Array.map (fun p -> p.mp_id) sx.ms_points) pos) in
		  let () = register_surf_in_ht surf_ixs ((fun (Body_Nr r) -> r) region) in
		  let () = register_surf_in_ht surf_ixs (-1) in
		  find_surface_nodes (1+pos)
		else
		  ()
	    in
	    find_surface_nodes 0
	in
	let check_surface_elem =
	  array_all_satisfy (fun x ->
	    match x with
	    | Some y -> y.ms_in_body = sx.ms_in_body
	    | None -> 1 = 1
			    ) neigh
	in
	if check_surface_elem
	then
	  ()
	else
	  let rec find_interior_surface_nodes ix =
	    if ix = nr_neigh then ()
	    else
	      let pos = (array_position_if (fun n ->
		match n with
		| Some m -> m.ms_in_body <> region
		| None -> 1 = 1 )
			   neigh ix)
	      in
	      if pos > (-1) then
                let surf_ixs = (array_one_shorter (Array.map (fun p -> p.mp_id) sx.ms_points) pos) in
		let () = register_surf_in_ht surf_ixs ((fun (Body_Nr r) -> r) region) in
		find_interior_surface_nodes (1+pos)
	      else
		(* found all the neighbours belonging to a different region *)
		()
	  in find_interior_surface_nodes 0
	       )
      mesh.mm_simplices
  in
  let dim = Array.length points_coords.(0) in
  let nodes_nr = Array.length points_coords in
  let simplices_nr = Array.length points_indices_region_of_simplices in
  let periodic_nr = Array.length mesh.mm_periodic_points in
(*  let surface_sx_nr = ref 0 in
    RRR
*)
  let () =
    Printf.fprintf output_file "# PYFEM mesh file version %s\n" format_version
  in
  let () =
    Printf.fprintf output_file "# dim = %d \t nodes = %d \t simplices = %d \t surfaces = %d \t periodic = %d\n%!" dim nodes_nr simplices_nr (Array.length (hashtbl_keys ht_surface_regions_info)) periodic_nr
  in
  let write_coords coords =
    begin
      Array.iter
	(fun coord ->
	  Printf.fprintf output_file "\t%25.12f " coord) coords;
       Printf.fprintf output_file "\n"
    end
  in
  let write_simplex_points (points,region) =
    begin
      Printf.fprintf output_file "\t%d " region;
      Array.iter
	(fun point ->
	  Printf.fprintf output_file "%d\t" point) points;
      Printf.fprintf output_file "\n"
    end
  in
  let test_backrefs () =
    (* print the surfaces and the regions they belong to in the right sequence (the one with volume>0 first) *)
    (* actually, the two regions are printed first, then the indices of the points representing the surface *)
    Printf.fprintf output_file "%d\n" (Array.length (hashtbl_keys ht_surface_regions_info));
    Hashtbl.iter (fun face (regions,determ) ->
      let second_region::first_region::tail = regions in
      let () =
	if determ > 0.0
	then
	  Printf.fprintf output_file "\t%d %d\t" first_region second_region
	else
	  Printf.fprintf output_file "\t%d %d\t" second_region first_region
      in
      let () = Array.iter (fun x -> Printf.fprintf output_file "%d "  x) face in
      Printf.fprintf output_file "\n%!"
		 ) ht_surface_regions_info
  in
  let write_periodic () =
    let ht_mesh_points = Hashtbl.create (Array.length points_coords) in           (* hashtbl to support the storage of periodic points in
										     the mesh structure *)

    let () = Array.iteri (fun pt_ix p -> Hashtbl.add ht_mesh_points p pt_ix ) points_coords in
    Array.iteri
      (fun arr_i periodic_same ->
	let () =
	  let () = Printf.fprintf output_file "\t%d\t" arr_i in
	  Array.iter (fun pt_index -> Printf.fprintf output_file "%d " (Hashtbl.find ht_mesh_points points_coords.(pt_index))) periodic_same  in
	Printf.fprintf output_file "\n%!"
      ) mesh.mm_periodic_points

  in

  begin
    Printf.fprintf output_file "%d\n" nodes_nr;
    Array.iter write_coords points_coords;
    Printf.fprintf output_file "%d\n" simplices_nr;
    Array.iter write_simplex_points points_indices_region_of_simplices;
    test_backrefs ();
    Printf.fprintf output_file "%d\n" periodic_nr;
    write_periodic ();
    close_out output_file;
  end
;;


let read_mesh filename =
  let rx_nonnumber = Str.regexp "[\\ \t\r\n]+" in
  let parse_type = regexp_decompose 1 "# PYFEM mesh file version \\([0-9]+\\).\\([0-9]+\\)" in
  let parse_structure = regexp_decompose 5 "# dim[\\ \t]*=[\\ \t]*\\([0-9]+\\)[\\ \t]*nodes[\\ \t]*=[\\ \t]*\\([0-9]+\\)[\\ \t]*simplices[\\ \t]*=[\\ \t]*\\([0-9]+\\)[\\ \t]*surfaces[\\ \t]*=[\\ \t]*\\([0-9]+\\)[\\ \t]*periodic[\\ \t]*=[\\ \t]*\\([0-9]+\\)"
  in
  let lines = Array.of_list( read_file_as_lines filename) in
    if (Array.length lines < 2) then
      let () = Printf.printf "Mesh information not specified" in
	None
    else
      let parsed_type = parse_type lines.(0) in
      (fun f -> (* Functional hack to have the complicated case
		   at the end of the pattern matching *)
	match parsed_type with
	| ([|Some version_nr|]::empty) ->
	    if version_nr = "1" then f () else
	    let () = Printf.printf "Wrong version of the file" in
	    None
	| _ ->
	    let () =Printf.printf "Version of the file not given" in
	    None)
	(fun () ->
	  let parsed_structure = parse_structure lines.(1) in
	  (fun f ->
	    match parsed_structure with
	    | ([|Some dim; Some nodes_nr; Some simplices_nr; Some surfaces_nr; Some periodic_nr|]::empty) ->
		f (int_of_string dim) (int_of_string nodes_nr) (int_of_string simplices_nr) (int_of_string surfaces_nr) (int_of_string periodic_nr)
	    | _ ->
		let () = Printf.printf "Mesh summary not given" in
		None
	  )
	    (fun dim nodes_nr simplices_nr surfaces_nr periodic_nr ->
(*	      let points_coords = Array.make nodes_nr (Array.make dim 0.0) in
	      let points_indices_of_simplices = Array.make simplices_nr (Array.make (dim+1) 0) in
	      RRR
*)
	       let () = logdebug (Printf.sprintf "Going to the points section\n%!") in
	       let points_line_offset = 3 in
		let the_points =
		  Array.mapi
		    (fun line_nr line ->
		      let pieces = Str.split rx_nonnumber line
		      in
		      let coords = Array.of_list (List.map float_of_string pieces) in
		      if Array.length coords <> dim
		      then
			let failstring = filename^" - LINE "^(string_of_int (line_nr + points_line_offset + 1))^": Wrong number of point coordinates" in
			failwith failstring
		      else coords
		    )
		    (Array.sub lines points_line_offset nodes_nr)
		in
		let () = logdebug (Printf.sprintf "Going to the simplices section") in
		let simplices_line_offset = (4+nodes_nr) in
		let the_simplices =
		  Array.mapi
		    (fun line_nr line ->
		      let pieces = Str.split rx_nonnumber line in
		      if List.length pieces <> (dim+2)
		      then
			let failstring = filename^" - LINE "^(string_of_int (line_nr + simplices_line_offset + 1))^": Wrong number of simplex coordinates" in
			failwith failstring
		      else
			let sx_region = List.hd pieces in
			let sx_indices = List.tl pieces in
			let indices = Array.of_list (List.map int_of_string sx_indices ) in
			if (Array.length indices <> (dim+1))
			then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + simplices_line_offset + 1))^": Wrong number of points for a simplex" in
			  failwith failstring
			else let wrong_inx = (array_position_if (fun i -> i>=nodes_nr) indices 0) in
	                if wrong_inx > (-1)
			  then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + simplices_line_offset + 1))^": Index "^(string_of_int (wrong_inx+1))^" out of range" in
			  failwith failstring
			else let sorted_copy = array_sorted compare indices in
                        if (sorted_array_has_duplicate (fun a b -> a = b) sorted_copy) then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + simplices_line_offset + 1))^": Duplicate index" in
			  failwith failstring
			else
			  try
			    let region = (fun n -> Body_Nr n ) (int_of_string sx_region)
			    in
			    region, indices
			  with
			  | _ -> let failstring = filename^" - LINE "^(string_of_int (line_nr + simplices_line_offset + 1))^": Wrong number of simplex region" in
			      failwith failstring
		    )
		    ( Array.sub lines simplices_line_offset simplices_nr)
		in
		let () = logdebug (Printf.sprintf "Going to the surfaces section") in
		let surfaces_line_offset = (4+nodes_nr+1+simplices_nr) in
		let (*the_surfaces*) _ =
		  Array.mapi
		    (fun line_nr line ->
		      let pieces = Str.split rx_nonnumber line in
		      if List.length pieces <> (dim+2)
		      then
			let () = loginfo (Printf.sprintf  "Warning: surface information not complete (don't worry, I can deal with it)") in
			((Body_Nr (-3)), (Body_Nr (-3)), [||]) (* this value is simply to match the other condition of if statement *)
		      else
			let sx_region1::sx_region2::list_tail = pieces in
			let sx_indices = list_tail in
			let indices = Array.of_list (List.map int_of_string sx_indices ) in
			if Array.length indices <> dim
			then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + surfaces_line_offset + 1))^": Wrong number of points for a surface" in
			  failwith failstring
			else let wrong_inx = (array_position_if (fun i -> i>=nodes_nr) indices 0) in
	                if wrong_inx > (-1)
			  then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + surfaces_line_offset + 1))^": Index "^(string_of_int (wrong_inx+1))^" out of range" in
			  failwith failstring
			else let sorted_copy = array_sorted compare indices in
                        if (sorted_array_has_duplicate (fun a b -> a = b) sorted_copy) then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + surfaces_line_offset + 1))^": Duplicate index" in
			  failwith failstring
			else
			  try
			    let region1 = (fun n -> Body_Nr n ) (int_of_string sx_region1) in
			    let region2 = (fun n -> Body_Nr n ) (int_of_string sx_region2) in
			      (region1, region2, indices)
			  with
			  | _ ->
			      let failstring = filename^" - LINE "^(string_of_int (line_nr + surfaces_line_offset + 1))^": Wrong number of simplex region" in
			      failwith failstring
		    )
		    ( Array.sub lines (4+nodes_nr+1+simplices_nr) surfaces_nr)
		in
		let () = logdebug (Printf.sprintf "Going to the periodic section") in
		let periodic_line_offset = (4+nodes_nr+1+simplices_nr+1+surfaces_nr) in
		let the_periodic_indices =
		  Array.mapi
		    (fun line_nr line ->
		      let periodic_pts_indices = Str.split rx_nonnumber line in
		      let indices = Array.of_list (List.map int_of_string periodic_pts_indices ) in
			let wrong_inx = (array_position_if (fun i -> i>=nodes_nr) indices 0) in
	                if wrong_inx > (-1)
			  then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + periodic_line_offset + 1))^": Index "^(string_of_int (wrong_inx+1))^" out of range" in
			  failwith failstring
			else let sorted_copy = array_sorted compare indices in
                        if (sorted_array_has_duplicate (fun a b -> a = b) sorted_copy) then
			  let failstring = filename^" - LINE "^(string_of_int (line_nr + periodic_line_offset + 1))^": Duplicate index" in
			  failwith failstring
			else
			  indices
		    )
		    (Array.sub lines periodic_line_offset periodic_nr)
		in
		let () = loginfo (Printf.sprintf "Using mesh_from_known_delaunay...") in (* DDD *)
		let mesh = mesh_from_known_delaunay the_points the_simplices in
		let () = logdebug (Printf.sprintf "going to run over simplices ") in
		let () = logdebug (Printf.sprintf "Have rudimentary mesh...!") in (* DDD *)
		let () = Array.iteri
		  (fun sx_ix (region,indices) ->
		     let dim = Array.length indices in
		       for i=0 to (dim-1) do
                         let regs_in_pnt =
                           mesh.mm_points.(indices.(i)).mp_in_body in
			 try ignore(List.find ((=) region) regs_in_pnt)
			 with Not_found ->
                           mesh.mm_points.(indices.(i)).mp_in_body <- region::regs_in_pnt
		       done)
		  the_simplices

		in
		let () = logdebug (Printf.sprintf "after simplices") in
		let () = logdebug (Printf.sprintf "going to mesh-grow-bookkeeping-data ") in
		let () = mesh_grow_bookkeeping_data ~do_connectivity:true ~do_incircle_circumcircle:true mesh in
                let () = mesh.mm_periodic_points <- (Array.copy the_periodic_indices) in
		let nr_vertices = Array.length mesh.mm_simplices.(0).ms_points in
		let () = Array.iteri
		  (fun sx_ix sx ->
		     let backrefs = sx.ms_neighbour_backrefs in
		       (* search for any (-1) in backrefs and set all the other points as belonging to the interface with (Body_Nr (-1)) *)
		     let pos = array_position_if (fun x -> x = (-1)) backrefs 0 in
		       if pos != (-1)
		       then
			 let rec find_surf_facets pos_sf =
			   if pos_sf = nr_vertices
			   then ()
			   else
			     let () =
			       if backrefs.(pos_sf) = (-1)
			       then
				 let surf_points_ixs = Array.map ( fun p -> p.mp_id) (array_one_shorter sx.ms_points pos_sf) in
				   Array.iter (
				     fun p_ix ->
				       try ignore( List.find (fun r -> r = (Body_Nr (-1))) mesh.mm_points.(p_ix).mp_in_body  )
				       with
					 | Not_found ->
					     mesh.mm_points.(p_ix).mp_in_body  <- ((Body_Nr (-1))::mesh.mm_points.(p_ix).mp_in_body )
				   ) surf_points_ixs
			       else ()
			     in
			       find_surf_facets (1+pos_sf)
			 in
			   find_surf_facets pos
		       else
			 ()
		  )
		  mesh.mm_simplices
		in
		let () = report_mesh_quality 10 mesh in
		  Some mesh
	    )
	)
;;


(* NOTE: does this function actually work? Looks a bit suspicious to me... *)
let read_mesh_from_points_and_simplices nodes_arr simplices_indices_arr region_arr periodic_points_arr =
  let () = logdebug (Printf.sprintf "Reading mesh...%!") in (* DDD *)
  try
    let the_points = nodes_arr in
    let simplices_nr = Array.length simplices_indices_arr in
    let regions_nr = Array.length region_arr in
    let () =
      if simplices_nr <> regions_nr
      then loginfo (Printf.sprintf "Number of simplices is different from number of regions %!" )
      else ()
    in
    let the_simplices =
      Array.mapi
        (fun sx_ix sx_region ->
           (Body_Nr sx_region, simplices_indices_arr.(sx_ix)))
      region_arr
    in
    try
      let () = logdebug (Printf.sprintf "Using mesh_from_known_delaunay...%!") in (* DDD *)
      let mesh = mesh_from_known_delaunay the_points the_simplices in
      let () = logdebug (Printf.sprintf "Have rudimentary mesh...%!") in (* DDD *)
      (* Populate mesh.mm_points.(i).mp_in_body with the list of regions
         owning the point mesh.mm_points.(i) *)
      let () =
        Array.iteri
	  (fun sx_ix (region, indices) ->
	     let dim = Array.length indices in
	       for i=0 to (dim-1) do
                 let regs_of_pnt = mesh.mm_points.(indices.(i)).mp_in_body in
                   try ignore (List.find ((=) region) regs_of_pnt)
                   with Not_found ->
                     mesh.mm_points.(indices.(i)).mp_in_body <- region::regs_of_pnt
	       done)
        the_simplices
      in
      let () = logdebug (Printf.sprintf "going to mesh-grow-bookkeeping-data ") in
      let () = mesh_grow_bookkeeping_data ~do_connectivity:true ~do_incircle_circumcircle:true mesh in
      let nr_vertices = Array.length mesh.mm_simplices.(0).ms_points in
      let () = Array.iteri
	(fun sx_ix sx ->
	   let backrefs = sx.ms_neighbour_backrefs in
	     (* search for any (-1) in backrefs and set all the other points as belonging to the interface with (Body_Nr (-1)) *)
	   let pos = array_position_if (fun x -> x = (-1)) backrefs 0 in
	     if pos != (-1)
	     then
	       let rec find_surf_facets pos_sf =
		 if pos_sf = nr_vertices
		 then ()
		 else
		   let () =
		     if backrefs.(pos_sf) = (-1)
		     then
		       let surf_points_ixs = Array.map ( fun p -> p.mp_id) (array_one_shorter sx.ms_points pos_sf) in
			 Array.iter (
			   fun p_ix ->
			     try ignore( List.find (fun r -> r = (Body_Nr (-1))) mesh.mm_points.(p_ix).mp_in_body  )
			     with
			       | Not_found ->
				   mesh.mm_points.(p_ix).mp_in_body  <- ((Body_Nr (-1))::mesh.mm_points.(p_ix).mp_in_body )
			 ) surf_points_ixs
		     else ()
		   in
		     find_surf_facets (1+pos_sf)
	       in
		 find_surf_facets pos
	     else
	       ()
	)
	mesh.mm_simplices
      in
      let () = mesh.mm_periodic_points <- periodic_points_arr in
      let () = loginfo (Printf.sprintf "Mesh nodes: %d,  simplices: %d %!" (Array.length the_points) simplices_nr)
      in
      Some mesh
    with
    | _ ->
	let () = loginfo (Printf.sprintf "failed in creating mesh!") in
	None
  with
  | _ ->
      let () = loginfo (Printf.sprintf "failed in reading point coords & simplices indices!") in
      None
;;

(* When transporting a mesh over MPI to the slave nodes (which we have to do for parallel setup),
   we use bigarrays. So, we need functions that extract geometry information from a mesh into a
   set of bigarrays as well as functions that rebuild a mesh from such information...

   Actually, most of this would not be necessary if the OCaml marshaller would not exhibit a bug
   which shows up when transporting some highly networked data structures...
 *)

type mpi_meshgeom =
    {
      mmg_dim: int;
      mmg_vertex_coords: (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
      mmg_simplices: (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t;
      mmg_simplex_regions: (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t;
      mmg_periodicity: int array array;
      (* We just assume this to be small, as it usually is.  Hence, we
	 will use ordinary serialization to send it over the network.
      *)
      mmg_vertex_distribution: int array;
    }
;;


let mesh_to_mpi_meshgeom mesh =
  let dim=mesh.mm_dim in
  let nr_points = Array.length mesh.mm_points in
  let nr_simplices = Array.length mesh.mm_simplices in
  let len_vertex_coords = dim*nr_points in
  let len_simplices = (1+dim)*nr_simplices in
  let ba_points = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout len_vertex_coords in
  let ba_simplices = Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout len_simplices in
  let ba_regions = Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout nr_simplices in
  let () =
    for nr_point = 0 to Array.length mesh.mm_points-1 do
      for k = 0 to dim-1 do
	ba_points.{dim*nr_point+k} <- mesh.mm_points.(nr_point).mp_coords.(k);
      done;
    done
  in
  let () =
    for nr_sx = 0 to Array.length mesh.mm_simplices-1 do
      for k = 0 to dim+1-1 do
	ba_simplices.{(dim+1)*nr_sx+k} <- Nativeint.of_int mesh.mm_simplices.(nr_sx).ms_points.(k).mp_id;
      done;
      ba_regions.{nr_sx} <- (let Body_Nr n = mesh.mm_simplices.(nr_sx).ms_in_body in Nativeint.of_int n);
    done
  in
    {mmg_dim=dim;
     mmg_vertex_coords=ba_points;
     mmg_simplices=ba_simplices;
     mmg_simplex_regions=ba_regions;
     mmg_periodicity=mesh.mm_periodic_points;
     mmg_vertex_distribution=mesh.mm_vertex_distribution;
    }
;;



let mpi_meshgeom_to_mesh mmg =
  let dim = mmg.mmg_dim in
  let nr_points = (Bigarray.Array1.dim mmg.mmg_vertex_coords)/dim in
  let nr_simplices = (Bigarray.Array1.dim mmg.mmg_simplices)/(dim+1) in
  let a_points =
    Array.init nr_points
      (fun n ->
	 Array.init dim
	   (fun d -> mmg.mmg_vertex_coords.{n*dim+d}))
  in
  let a_simplices_regions =
    Array.init nr_simplices
      (fun n ->
	 (Body_Nr (Nativeint.to_int mmg.mmg_simplex_regions.{n}),
	  Array.init (1+dim)
	    (fun d ->
	       Nativeint.to_int mmg.mmg_simplices.{n*(1+dim)+d})))
  in
  let mesh = mesh_from_known_delaunay a_points a_simplices_regions in
  let () = mesh.mm_periodic_points <- mmg.mmg_periodicity in
  let () = mesh.mm_vertex_distribution <- mmg.mmg_vertex_distribution in
    mesh
;;

let maybe_use_cached_mesh cache_file do_the_real_work =
  let () = loginfo (Printf.sprintf "MUCM 0%!") in
  let cache_dir = get_feature ~domain:"file-cache" "directory"
  and cache_age = get_feature ~domain:"file-cache" "max-age"
  in
    match (cache_dir,cache_age) with
      | (None,_) ->  do_the_real_work ()
      | (_,None) ->  do_the_real_work ()
      | (Some cache_dir, Some cache_age_str) ->
	  let () = loginfo (Printf.sprintf "MUCM 1%!") in
	  let cache_age = float_of_string cache_age_str in
	  let () = expire_dir cache_dir cache_age in
	  let () = loginfo (Printf.sprintf "MUCM 2%!") in
	    match (get_feature "version") with
	      | None -> do_the_real_work ()
	      | Some version ->
		  let () = loginfo (Printf.sprintf "MUCM ver%!") in
		  let full_name = Printf.sprintf "%s/%s.%s.mesh" cache_dir cache_file version in
		  let cached = read_mesh full_name in
		  let () = loginfo (Printf.sprintf "MUCM 3%!") in
		    match cached with
		      | Some m ->
			  let () = loginfo (Printf.sprintf "MUCM use cached%!") in
			  let now = Unix.gettimeofday () in
			  let () = Unix.utimes full_name now now in
			    m
		      | None ->
			  let () = loginfo (Printf.sprintf "MUCM no cached%!") in
			  let the_mesh = do_the_real_work () in
			  let _ = write_mesh full_name the_mesh in
			    the_mesh
;;


let mesh_it
    ?cache_name
    ?(gendriver=(fun _ -> default_driver))
    ?(rng= Mt19937.make 97)
    ?(fixed_points=[||])
    ?(mobile_points=[||])
    ?(simply_points=[||])
    fem_geometry
    mdefaults
    length_scale
    =
  let do_the_real_work () =
    mesh_it_work ~gendriver ~rng ~fixed_points ~mobile_points ~simply_points (Hashtbl.create 1) fem_geometry mdefaults length_scale
  in
    match cache_name with
      | None -> do_the_real_work ()
      | Some "" -> do_the_real_work ()
      | Some x -> maybe_use_cached_mesh x do_the_real_work
;;

module Simplex = Simplex

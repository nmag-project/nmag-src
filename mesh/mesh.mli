(*
   (C) 2005 Dr. Thomas Fischbacher, Giuliano Bordignon, Dr. Hans Fangohr,
   SES, University of Southampton
 *)

(** {1 Mesh Generation} *)

(**
   (C) 2005 Dr. Thomas Fischbacher, Giuliano Bordignon, Dr. Hans Fangohr,
   SES, University of Southampton
 *)

(** {2 Introduction and overview} *)

(**
   Mesh generation is a complex and involved subject. There is no
   perfect recipe, but rather a collection of heuristics how to
   produce suitable meshes for various applications. Some are very simple
   and fast, but produce meshes of limited quality only. We aim for
   flexibility and meshes of comparatively high quality.

   Our approach is based on the method proposed by Persson/Strang, cf.
   [http://www-math.mit.edu/~persson/mesh/], but with own variations on
   the theme. One of our main design goals was to make the mesher
   flexible, extensible, and fast, and so there is quite a number of
   bells, gongs, and whistles which might seem intimidating at first,
   but most of them should in fact be relevant to a very small
   fraction of users only. Indeed, most users of this library will
   only have to know how to specify the non-optional parameters to
   the [mesh_it] function, which will then take care of doing
   all the work: Geometry description in, mesh out.

   For users that want (or have) to dig deeper, we briefly give an
   overview over the mesher and the underlying design goals. It should
   be noted that, as many algorithmic parts of the mesher are quite
   complicated, its design did not undergo as many evolutionary cycles
   as with other modules, and hence may still be a bit more unwieldy
   than necessary in some aspects.

   The underlying idea is to distribute particles random (initially)
   and to have forces acting between different particles to determine
   a finite element mesh. We restrict the interaction forces to act
   only between nearest neighbours (which are defined by being
   connected through 'edges' in a Delaunay triangulation of the set of
   points). The nature of the interaction force can be varied. The
   current default interaction(as suggested by Persson/Strang) is a
   linear force as a function of deviation from the equilibrium point
   which is only acting if the two particles are too close to each
   other (i.e. the force is only repulsive). Furthermore, we put some
   extra 'pressure' on the system to make sure it fills cornes of a
   geometry. This pressure means that all 'springs' in the system are
   slightly compressed. More details of the inner workings of
   the algorithms can be found in this publication (XXX need to write this,
   (fangohr 27/03/2006)).

   One important design principle of the mesher is that it always
   works in the "geometry in - mesh out" fashion. That is, no user
   should ever deal with the inner mechanics at the level of writing
   code that iteratively calls mesh relaxation functions. Rationale:
   if we were to allow such an interface for advanced users, this
   would nail down so many aspects of the structure of single-stepping
   mesh relaxation functions that our (the authors') options to apply
   major structural changes to the code would be severely limited.

   Rather, the idea is to allow function parameters that provide extra
   intelligence in a parametrizable way. Roughly speaking, the heart of
   the mesher is a complicated relaxation function that has many
   buttons and dials (i.e. knows a set of commands what to do next) as
   well as displays, gongs and whistles (i.e. reports statistics over a
   variety of mesh parameters). Now, a "controller" (which is a function
   that is fed with the output of those displays, gongs, and whistles,
   and also has some internal memory) analyses the mesh's current state and
   produces a new command for the mesh relaxation machine (which can well be:
   you are finished.)

   Note that at present, we do not implement the controller's brain as
   a simple closure, because moving that state out of the function has
   the advantage that we can more easily clone - and maybe even
   serialize - an intermediate meshing situation. This seems to be not
   as relevant as thought initially, and somewhat complicates the
   design, so it may well be conceivable that this will be simplified
   greatly in the next major version of the mesher (but we stick to
   that design for the near future).

   The controller only analyzes situations and produces new commands
   (which are not executed automatically).  Another specialist, the
   "driver" takes care of actually pushing mesh generation
   forward. This is again implemented as a (user-specifiable)
   function. A "driver" provides commands to the mesh generator engine
   which tell it to go on, or alternatively extract an intermediate
   mesh, and listens to the output of the mesh generator engine, which
   roughly says "I am finished" or "I can go on" or "I can go on,
   and I have an intermediate mesh to show you".

   The driver has considerably less intelligence than the controller;
   the simplest driver will just repeatedly tell the meshing engine to
   go on until it says that the mesh is finished. A more interesting
   driver may extract an intermediate mesh every 10 steps and write it
   out to disk, or do statistics on it and collect those, or pass them
   on to a Python function, etc.

   Note that this design, which roughly can be sketched as follows
{v
   Controller <===> Engine <===> Driver
v}
   introduces a lot of complexity by allowing a great deal of flexibility
   and separation of responsibilities between different pieces of the code.
   It may be argued that, with such a design, there may be too much flexibility
   in the wrong places, and hence a lot of unnecessary complexity.
 *)

(** {2 Notes on Fine-Tuning}

   Basically, there are two types of configuration parameters: First,
   value parameters, which specify various thresholds, debugging
   flags, default scales, etc., and second, methodological parameters,
   which are functions that implement heuristics how to achieve
   certain small, well-defined sub-goals where one would like to have
   the option to change certain aspects in more fundamental ways than
   what can be encoded in the form of a few numerical parameters.

   Virtually all configuration options are collected in the
   ['a mesher_defaults] data structure. The relevant meshing functions
   will take a [mdefaults] argument, which provides the relevant
   mesher defaults. Usually, one will want to use the contents of the
   global reference [!opt_mesher_defaults], but this cannot be made a default
   due to the type system: [!opt_mesher_defaults] has type [int mesher_defaults],
   so this would lock down ['a = int]!

   While it is expressly not forbidden to just update the
   contents of [!opt_mesher_defaults] -- the global factory settings,
   so to speak, the authors' advice is not to use such a technique
   (except maybe for quick ad-hoc hacks), but rather use the
   [copy_mesher_defaults] function to obtain a private copy of
   mesher defaults, and tune this by modifying individual members
   of the structure before passing it on to [mesh_region].

   Parameters in the ['a mesher_defaults] data structure can be
   divided into various groups:

   - Parameters related to the evaluation and enforcement of boundary conditions.
   - Parameters used by the relaxation function that performs a series
     of time steps to obtain a suitable mesh.
   - The default "controller" function that tells the relaxation function
     what to do next in a given situation, plus the abstract initial state
     of this controller function.
     (For the default controller, its "state" is nothing else but
     the current step counter.)
   - Default arguments to the default controller.

   The controller function will tell the relaxation code how large to
   make a time step, when to perform a Delaunay retriangulation to
   update topology information, when to stop trying to improve the
   mesh, and so on. When the relaxation calls the controller in order
   to find out what to do next, it will provide a
   [meshgen_controller_input] data structure, which feeds the
   controller with information about the maximal relative movement of a node
   since nearest-neighbourhood topology was last determined, how big
   the largest relative displacement of a node was in the last time step,
   how big the last time step was, etc. The controller will then make
   a decision based on those parameters (and a
   "controller internal state parameter" which it passes around
   to itself so that it can have a memory of the past),
   and return a [meshgen_controller_command] value
   (and a new controller internal state).

   The controller is also provided with the mesher defaults, and (at
   least for the default controller) will as well use quite some
   parameters from that structure that were reserved specifically for
   their use by the controller.

   While the controller implements the abstraction of {i what} to do
   {i when}, there is a second useful abstraction of {i how} to
   proceed with meshing. It must be expected that some users of this
   library want to have finer control over the meshing process, stop
   meshing intermediately, in-vivo analyze a not yet completed mesh,
   maybe generate a graphical visualization, and maybe resume, maybe
   abandon the attempt to complete the mesh. This abstraction is
   provided by the {i driver}. The default driver will just complete
   the mesh, while the stepping driver will extract an intermediate
   mesh every {i N} mesh-relaxation iterations. The user may implement
   other drivers which e.g. may even hand over strategic control to a
   foreign language, like Perl or Python.

 *)


module Simplex:
  sig
    type t

    type idx = int
    (** The simplex index *)

    val init: Mesh0.t -> t
    val get_point_matrix: t -> idx -> Base.Ba.F.array2
    val get_point_matrix_det: t -> idx -> float
    val get_inv_point_matrix: t -> idx -> Base.Ba.F.array2
    val get_face_eqn: t -> int -> int -> Base.Ba.F.array1

    val dummy : t
  end

(** {3 Geometry specification} *)

type simplex_region = Body_Nr of int

(** A mesh discretization of space may (and usually will)
   cover different objects with different properties.
   One interesting question therefore is: which region
   does a simplex belong to, resp. which regions does
   a site (e.g. vertex) on the mesh belong to?

   We just number regions, but use a special [simplex_region]
   type to make this idea more explicit through the type system.

   Normally, bodies are numbered 1,2,3,...; Numbers 0, -1, and -2
   are reserved: 0 means "outer space", -1 means "non-meshed region",
   and -2 is an error code.

   Evidently, no simplex can belong to region -1. Nevertheless, a point
   in the mesh may belong to more than one region, and those at the
   mesh boundaries will always at least belong to region -1 plus one other
   region.

   One may wonder whether it would be more appropriate to use a more
   elaborate variadic type, e.g.
{v
   [Error | Outside | Space | Body_Nr of int]
v}
   but one has to take into account that this information usually
   is also presented to some scripting language (such as python),
   and there is little to be gained from supporting two different
   labeling schemes, as virtually no scripting language has a comparable
   concept of variadic types.
 *)

(** TO BE DOCUMENTED *)

type fem_geometry = {
  fem_geo_dim : int;
  fem_geo_bounding_box : float array * float array;
  fem_geo_density : float array -> float;
  fem_geo_boundaries : (simplex_region * (float array -> float)) array;
  fem_geo_piece_hints: (float array array * int array array) array;
  fem_geo_simplex_classificator : float array array -> simplex_region;
}
(**
   A finite element geometry specification basically knows about a set of
   boundaries inside an outer, coordinate-parallel box, as well as a relative
   density function that tells by how much the size of a simplex should be
   blown up or shrunk. This furthermore contains a function that maps the
   vertex coordinates of a simplex to a region this simplex is supposed
   to belong to. (XXX is easy to explain why we need this function?
   (fangohr 26/03/2006))
 *)

val make_fem_geometry :
  ?bounding_box:float array * float array ->
  ?density:(float array -> float) ->
  ?simplex_classificator:(float array array -> simplex_region) ->
  ?boundaries:(simplex_region * (float array -> float)) array ->
  ?hints: (float array array * int array array) array ->
  unit -> fem_geometry
  (** XXX do we need to document this function? (fangohr 26/03/2006) *)

(** {3 Data Types} *)

(** A variety of type aliases help us to express more directly
   what something is about *)

type point_id = int
type simplex_id = int
type face_id = int array
type face_ix = int
type face_name = point_id array
type simplex_name = point_id array
type coords = float array
type coords_L = float array
type mesh_site_id = point_id array
type mesh_site_ix = int
type partitioning_type = int array
type dof_id = int
type field = float array

(* XXX Note: should make this internal. Right now, it is not, as
   meshphysics makes quite extensive use of some stuff in here.  I think
   this should rather work via us providing the functionality required
   by meshphysics in an abstract way!
 *)

(**
   The [point] and [simplex] data structures are quite
   elaborate. While everything that is in them is present and done in
   that particular way for a good reason, one may wonder whether these
   structures really are "universal" in the sense that whatever other
   approach one would like to take, one is bound to end up with
   something that provides at least all that information. At present
   this seems to be hard to believe, but mostly the case. This question
   is relevant as if it were otherwise, we {e never should} make these types
   visible to the outside, but rather provide accessor functions.
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
     ms_points: point array;
     ms_neighbours: simplex option array;
     ms_neighbour_backrefs: int array;
     (* If we cross a face and enter a neighbour, we have to
	know the number of the face from the perspective
	of the neighbour that leads back to us.
      *)
     ms_face_ids: face_id array;
     (* A face id is the ordered vector of all point ids.
	Note that this is computed easiest if we require the points
	in the simplex to be recorded in index-sorted order.

	However, if we do that (and we presently do),
	the problem arises that we cannot really enforce positive
	volume orientation anymore.
      *)
     mutable ms_in_body: simplex_region;
   }

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
(** Technicality: When doing linear algebra in D dimensions,
   it is often beneficial to performance to have functions
   that implement standard operations and use their own
   internal scratchpads (hence are non-reentrant), whose sizes
   depend on the dimension. Major reason: Caching.
   The [la_functions] record just collects such functions.
 *)

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
      mutable mm_boundaries: ((simplex_region * simplex_region)*((simplex_id*face_ix) array)) array;
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
      *)
     mutable mm_periodic_points : int array array;
     mutable mm_have_connectivity: bool;
     mutable mm_have_incircle_circumcircle: bool;
     mutable mm_have_regions: bool;
    }

val dummy_mesh : mesh

val simplex_surface_1form_component: mesh -> Simplex.idx -> int -> int -> float

val reordered_mesh : mesh -> int array -> mesh
(** This function allows us to renumber and re-order the points in a mesh.
    Important for distributing a mesh across a cluster. *)

type point_state = Immobile | Pinned | Mobile | Boundary
(**
   This type can be ignored for now, but may become important later
   on.

   A point in the mesh may be {i immobile} -- a property which never
   can be changed in any way.  Furthermore (this is not implemented
   yet), we may decide to temporarily "pin" a point so that it is not
   used for force calculations between this point and neighbouring
   pinned points in the next relaxation iteration. The idea is to gain
   speed by not considering those parts of the mesh that do not move
   noticeably anyway. While the controller in principle has the
   ability to issue a command to change the pinning state of nodes,
   this is not implemented at present. An efficient implementation
   will in particular require support for dynamical point insertion
   and deletion in Delaunay triangulations. While efficient algorithms are
   known, this has not been implemented yet for this package.
 *)

type point_fate = Do_Nothing_With_Point | Add_Another_Point | Delete_Point
(**
   This related to tweaking the dynamical point density control.

   While the mesher does an initial crude estimate of the approximate
   number of points required to mesh, it is useful to be able to
   dynamically change the number of points to adjust the system to a
   mis-estimated density. Note: at present, this is highly
   experimental, and key parameters (as well as their dimension
   dependency!) have not been determined properly yet. At present, the
   abstraction is to use a "point density handler" function parameter,
   which will map the average relative compression of springs
   connecting to a node to a (probabilistic) point fate.
 *)


type meshgen_controller_input = {
  mutable mci_bound_pt_between_immobiles: bool ;
  mutable mci_dimension: int;
  mutable mci_size_last_time_step : float;
  mutable mci_max_node_rel_movement_since_last_triangulation : float;
  mutable mci_largest_rel_movement_in_mesh : float;
  mutable mci_largest_effective_force_in_mesh : float;
  mutable mci_node_avg_density : (float * float) array;
}
(**
   This is relevant for users wanting to implement a controller
   different from the default one.

   The mesh relaxation engine will provide the controller with some
   information about the mesh, which the controller will use to decide
   what to do next. This is done by feeding the controller with a
   [meshgen_controller_input] value. The fields are declared as
   [mutable] so that a single entity can be easily modified
   component-wise, and also re-cycled for each iteration.
   (The controller will not receive a fresh copy at every iteration,
   and should make a copy if it wants to put it into its internal
   state!)

   It is hoped that the entry names are self-explanatory. After all,
   prospective controller writers should start by taking a look at the
   source code of the default controller to see how it uses those
   pieces of information. (Should there be any additional questions,
   feel free to send an email or documentation update request to the
   developers of this package.)

*)

type meshgen_controller_command =
    Force_Equilibrium_Reached of float
  | Step_Limit_Reached of int
  | Pin_Unpin_Nodes of point_state array
  | Determine_Forces of int
  | Retriangulate of int
  | Change_points_and_retriangulate of (point_fate array * int)
  | Time_step of (float * int)

(**
   This is mostly relevant for users wanting to implement a controller
   different from the default one.

   A controller will return a command of this form to the mesh
   relaxation engine.

   Note that the first two commands will cause termination of mesh
   relaxation and produce a final mesh. Mesh relaxation will announce
   the reason why it finished to the outside by providing the
   corresponding [meshgen_controller_command] that stopped it within its result.
 *)

type 'a mesher_defaults = {
    mutable mdefault_controller_initial_points_volume_ratio: float;                        (* functions defined but not used (constraints on pyfem) *)
    mutable mdefault_controller_splitting_connection_ratio: float;                         (* | *)
    mutable mdefault_controller_exp_neigh_force_scale: float;                              (* __ *)
  mutable mdefault_nr_probes_for_determining_volume : int;
  mutable mdefault_boundary_condition_debuglevel : int;
  mutable mdefault_boundary_condition_acceptable_fuzz : float;
  mutable mdefault_boundary_condition_max_nr_correction_steps : int;
  mutable mdefault_make_boundary_condition_simplex_classificator :
       int -> (* dim *)
      ((float array -> float) array) -> (* boundary conditions *)
	float -> (*smallest allowed volume ratio *)
       float array array -> (* simplex nodes coordinates *)
	point_state array -> (* points states *)
	     simplex_region;
  mutable mdefault_controller_movement_max_freedom : float;
  mutable mdefault_controller_topology_threshold : float;
  mutable mdefault_controller_step_limit_min: int;
  mutable mdefault_controller_step_limit_max: int;
  mutable mdefault_controller_max_time_step : float;
  mutable mdefault_controller_time_step_scale : float;
  mutable mdefault_controller_tolerated_rel_movement : float;

  (*KKK*)
  mutable mdefault_controller_shape_force_scale: float;
  mutable mdefault_controller_volume_force_scale: float;
  mutable mdefault_controller_neigh_force_scale: float;
  mutable mdefault_controller_irrel_elem_force_scale: float;

  mutable mdefault_controller_thresh_add: float;
  mutable mdefault_controller_thresh_del: float;
  mutable mdefault_initial_relaxation_weight:
    int -> int -> float -> float -> float;
  mutable mdefault_controller_initial_settling_steps: int;
  mutable mdefault_controller_sliver_correction: float;
  mutable mdefault_controller_smallest_allowed_volume_ratio: float;

  mutable mdefault_controller_handle_point_density_fun :
    Mt19937.rng -> (float * float) -> float -> float -> point_fate;
  mutable mdefault_relaxation_debuglevel : int;
  mutable mdefault_relaxation_force_fun : float -> float;
  mutable mdefault_boundary_node_force_fun : float -> float;
  mutable mdefault_meshgen_controller :
    'a mesher_defaults ->
    Mt19937.rng ->
    'a -> meshgen_controller_input -> 'a * meshgen_controller_command;
  mutable mdefault_meshgen_controller_initial : 'a;
}

(**
   The mesher defaults contain all the bells, gongs, and whistles to
   control the behaviour of the mesher. (But not its execution -- the
   "driver" is purely external.) At present, there is not much
   documentation on these parameters. TODO: This should be changed.
 *)

val opt_debuglevel_mesher : int ref

(** setting this debuglevel to values >0 will turn on debugging output
   of the mesher - the higher, the more. *)

(* val make_find_visible : 'a -> 'b -> 'c *)
(* We do not need and do not export this now. *)

val print_point : Format.formatter -> point -> unit

val print_simplex : Format.formatter -> simplex -> unit

val print_mesh : Format.formatter -> mesh -> unit



val mesh_from_known_delaunay :
  coords array -> (simplex_region * int array) array -> mesh

(** Make a mesh from vertex coordinates and data on simplices
   (i.e. what are the indices of the vertices of a given simplex,
   and what region does it belong to?)
*)

val nearest_neighbours :
  int ->
  ?triangulator:(float array array -> int array array) ->
  ?simplex_classificator:(float -> float array array -> point_state array -> simplex_region) ->
  ?fun_filter_region:(simplex_region -> bool) ->
  float ->  coords array -> point_state array -> int array array * int array array

val extract_topology_from_simplices :
  int array array -> coords array ->
  int array array * int array array

(**
   Given a dimension, plus a vector of points (give by their coordinates),
   perform a Delaunay triangulation to obtain information about nearest
   neighbours.

   Note: this runs into troule with flat faces, as some triangulators
   have to shift points slightly to resolve four-points-on-a-circle-type
   ambiguities. This often creates very slim spurious surface simplices
   at the outer boundaries.
 *)

(* XXX go on with the documentation here! *)

val make_simplex_lines_of_gravity_applicator :
  int ->
  (float array -> float array array -> float array -> float -> 'a) ->
  float array array -> 'a

val make_default_boundary_condition_simplex_classificator :
  int -> ((float array -> float) array) -> float ->
  float array array -> point_state array -> simplex_region

val mesh_align_external_indices : mesh -> unit

val mesh_grow_bookkeeping_data :
  ?do_connectivity:bool ->
  ?do_incircle_circumcircle:bool -> ?do_regions:bool -> mesh -> unit

val mesh_boundary_points : mesh -> (simplex_region list * float array) array

val for_mesh_interfaces :
  mesh -> (simplex_region -> simplex_region -> simplex -> int -> unit) ->
  unit

val solid_angle_3d_at_mesh_vertex :
  ?fun_is_inside:(simplex -> bool) -> point -> float

val accumulate_material_solid_angles_3d :
  point -> float array -> unit

val mesh_from_points :
  int ->
  (float array -> float) array ->
  ?triangulator:(float array array -> int array array) ->
  ?simplex_classificator:(float -> float array array -> point_state array -> simplex_region) ->
  float -> point_state array -> float array array -> mesh

val mesh_simplex_nr_and_L_coords_of_point :
  mesh -> float array -> (int * float array) option

val default_meshgen_controller :
  'a mesher_defaults ->
  Mt19937.rng ->
  int -> meshgen_controller_input -> int * meshgen_controller_command

(**
   This is the default controller. Anyone who wants to learn how to
   implement a new one should start by looking at its source and
   create a modified variant.
*)

val opt_mesher_defaults : int mesher_defaults ref
(**
   Most functions that have to know mesher defaults will take the
   contents of this reference if no value is provided.
 *)

val copy_mesher_defaults :
  'a mesher_defaults ->
  ('b mesher_defaults ->
   Mt19937.rng ->
   'b -> meshgen_controller_input -> 'b * meshgen_controller_command) ->
  'b -> 'b mesher_defaults

(**
   When creating a new set of mesher defaults, it is usually
   appropriate to change one or two parameters in a previously given
   set. Hence, one will want to copy defaults.

   Arguments to [copy_mesher_defaults] are an old set of defaults, a
   new controller, and a new controller initial state.

   Rationale: If we did not make those two parameters function
   arguments, we could only generate ['a mesher_defaults] from ['a
   mesher_defaults] (with fixed ['a]), and would not be able to
   replace the type of controller state with something new.
 *)


type meshgenerator_point = {
  coords_start : float array;
  coords_here : float array;
  force_here : float array;
  mutable sum_abs_forces : float;
  mutable nr_forces : int;
  mutable density_here : float;
  mutable state : point_state;
}

type meshgen_engine_command = Mesh_Engine_Do_Extract | Mesh_Engine_Do_Step
(**
   The driver will provide commands to the meshing engine. For now,
   there are only two: Extract an intermediate mesh, and do another
   iteration step. This should be of relevance only to users who want
   to implement an own driver.
 *)

type meshgen_engine_output =
    Mesh_Engine_Finished_Step_Limit_Reached of (mesh * int)
  | Mesh_Engine_Finished_Force_Equilibrium_Reached of (mesh * float)
  | Mesh_Engine_Can_Continue of
      (meshgen_engine_command -> meshgen_engine_output)
  | Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue of
      (mesh * (meshgen_engine_command -> meshgen_engine_output))

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

(** TODO: Document the following definitions as well! *)

type meshgen_engine = meshgen_engine_command -> meshgen_engine_output

val make_meshgen_engine :
  ?rng:Mt19937.rng ->
  ?rod_length:float ->
  'a mesher_defaults ->
  float array array ->
  float array array ->
  fem_geometry -> meshgen_engine_command -> meshgen_engine_output

val default_driver :
  meshgen_engine -> meshgen_engine_output

val default_gendriver :
    int -> meshgen_engine -> meshgen_engine_output

val stepping_driver :
  int ->
    meshgen_engine -> meshgen_engine_output

val do_every_n_steps_driver :
  int ->
  (int -> mesh -> unit) ->
  (meshgen_engine_command -> meshgen_engine_output) -> meshgen_engine_output

val mesh_a_piece :
  ?driver:((meshgen_engine_command -> meshgen_engine_output) ->
           meshgen_engine_output) ->
  ?rng:Mt19937.rng ->
  ?immobile_points:coords array ->
  ?mobile_points:coords array ->
  ?simply_points:coords array ->
  fem_geometry -> 'a mesher_defaults -> float -> meshgen_engine_output

type mpi_meshgeom = {
  mmg_dim : int;
  mmg_vertex_coords :
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mmg_simplices :
    (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mmg_simplex_regions :
    (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mmg_periodicity : int array array;
  mmg_vertex_distribution: int array;
}

val mesh_to_mpi_meshgeom : mesh -> mpi_meshgeom
val mpi_meshgeom_to_mesh : mpi_meshgeom -> mesh

val maybe_use_cached_mesh: string -> (unit -> mesh) -> mesh

val mesh_it :
  ?cache_name:string ->
  ?gendriver:(int -> meshgen_engine -> meshgen_engine_output) ->
  ?rng:Mt19937.rng ->
  ?fixed_points:float array array ->
  ?mobile_points:float array array ->
  ?simply_points:float array array ->
  fem_geometry -> 'a mesher_defaults -> float -> mesh

val mesh_periodic_outer_box :
    ?gendriver:(int -> meshgen_engine -> meshgen_engine_output) ->
      ?rng:Mt19937.rng -> float array array ->
	fem_geometry -> 'a mesher_defaults -> float -> float array -> float array array  * (int, float array array) Hashtbl.t

val mesh_boundaries_and_objects :
    ?gendriver:(int -> meshgen_engine -> meshgen_engine_output) ->
      ?rng:Mt19937.rng ->
	coords array -> coords array -> coords array ->
	  fem_geometry -> 'a mesher_defaults -> float -> float array -> mesh

val mesh_extract_surface:
    mesh ->
    float array array (* point locations *)
    *(point_id array * ((float array * float) * (float array * float) * int)) array
    (* element: (point_indices,((cc_mid,cc_radius),(ic_mid,ic_radius),body_nr)) *)

val mesh_plotinfo :
    mesh ->
    coords array  (* point locations *)
    * (point_id * point_id) array (* link pairs *)
    *(point_id array * ((float array * float) * (float array * float) * int)) array
     (* simplex: (point_indices,((cc_mid,cc_radius),(ic_mid,ic_radius),body_nr)) *)
    * int list array (* which bodies does the corresponding point belong to? *)

val mesh_plotinfo_points :
  mesh -> float array array  (* point locations *)

val mesh_plotinfo_pointsregions :
  mesh -> simplex_id array array (* regions that each point belongs to *)

val mesh_plotinfo_simplicesregions : mesh -> int array (* region-id for each simplex *)

val mesh_plotinfo_simplices :
  mesh -> point_id array array (* point ids for each simplex *)

val mesh_plotinfo_links : mesh -> (point_id * point_id) array (* Point tuple for all links *)

val mesh_plotinfo_surfaces_and_surfacesregions : mesh -> point_id array array * int array (* Tuple of surface simplex and region it belongs to. This double counts surfaces if they are between two regions. *)

val mesh_plotinfo_regionvolumes : mesh -> float array  (* Returns a list of region volumes, one entry for each region. *)

val mesh_plotinfo_periodic_points_indices :
  mesh -> int array array (* indices of equivalent points for each periodic point *)

val mesh_connectivity : mesh -> int list array

val mesh_do_reorder_vertices : mesh -> (int -> int) -> unit

type body_trafo = Affine_Trafo of float Snippets.array2 * float array

type body = Body of body_trafo * (float array -> float)

type mesh_with_body = mesh * body

val body_trafo_dim: body_trafo -> int

val combine_body_transformations : body_trafo -> body_trafo -> body_trafo

val body_trafo_shift : float array -> body_trafo

val body_trafo_id : int -> body_trafo

val body_trafo_scale : float array -> body_trafo

val body_trafo_rotate : int -> int -> int -> float -> body_trafo

val body_trafo_rotate_axis : int -> float array -> float -> body_trafo

val body_trafo_apply_to_bc :
  body_trafo -> (float array -> 'a) -> float array -> 'a

val bc_ellipsoid : float array -> float array -> float

val bc_box : float array -> float array -> float array -> float

val bc_helix : float array * float -> float array * float -> float array -> float

val bc_frustum : float array * float -> float array * float -> float array -> float

val bc_convex_hull : float array array -> 'a

val body_apply_trafo : body -> body_trafo -> body

val body_csg_11 : (float -> float -> float) -> body -> body -> body

val body_csg_1n : (float -> float -> float) -> body -> body array -> body

val body_csg_n : (float -> float -> float) -> body array -> body
val body_union_11 : body -> body -> body
val body_union_1n : body -> body array -> body
val body_union_n : body array -> body
val body_intersection_11 : body -> body -> body
val body_intersection_1n : body -> body array -> body
val body_intersection_n : body array -> body
val body_difference_11 : body -> body -> body
val body_difference_1n : body -> body array -> body

val fem_geometry_from_bodies :
  ?mesh_exterior:bool ->
  ?density:(float array -> float) ->
  float array * float array -> body array
  -> mesh_with_body array
  -> fem_geometry

val mesh2d_ps :
  ?scale:(float array -> float array) ->
  ?further_ps:(out_channel -> unit) -> mesh -> string -> unit
(** Function that given a 2d mesh [mesh] and a filename [string]
   will create a postscript file containing that mesh. Optional inputs
   are currently undocumented. XXX *)

val mesh_locate_point: mesh -> coords -> (simplex * coords_L)
(** Given a mesh and the coordinates of a point,
    return the simplex that contains this point,
    plus its L-coordinates for that simplex.
    Raises [Not_found] if the point is outside the mesh.
*)

val scale_node_positions: mesh -> float -> unit
  (** Given a mesh and a scaling factor, all the nodes of the
      mesh are scaled by this common factor.
*)

val write_mesh: string -> mesh -> unit
(** Function that takes a filename [string] and a mesh [mesh] and
   write the mesh as plain text into that file. The file format is very similar
   to the NEUTRAL file format of the NETGEN mesher (http:// XXX netgen).

   The file format consist of the following entries:

    1. one line starting with '#' storing the current version of the file format
    2. one line starting with '#' containing a summary of the size of the mesh with the following information:
     - the dimension of the space the mesh lives in ( for example [dim = 2])
     - the total number of nodes (vertices) (for example [nodes = 30])
     - the total number of simplicies (for example [simplices = 40])
     - the total number of surface elements (for example [surfaces = 15])

    3. one line containing the number of nodes, followed by one line per node containing the spatial coordinates of all nodes (components separated by white space). For example
[  0.000000   1.000000 ]

    4. one line containing the number of all simplices, followed by one line per simplex. For every simplex, there is first an integer encoding the [Body_Nr] (XXX GIuliano, is this correct (fangohr 26/03/2006)), then dim+1 integers containing the indices of the nodes that define the simplex. For example:
[    0          1          6          7 ]

   5. one line containing the number of surface elements, followed by one line per surface element. Each of those lines contains first the [Body_Nr] that surface element belongs to, and then a seqence of dim+1 integers which are indices of the nodes that define the surface element. For example:
[0	18 24]
*)

val read_mesh: string -> mesh option
(** Given the filename [string] this will read a mesh file as written with [write_mesh] and return the [mesh] object. *)

val read_mesh_from_points_and_simplices: float array array (* points *) -> int array array (* sx_indices *) -> int array (* sx_region *) -> int array array (* periodic_points *) -> mesh (* mesh *) option
(** Given the arrays of points [points], simplices indices [sx_indices], simplices regions [sx_region] and periodic points [periodic_points] this will return the [mesh] object. *)

val version : unit -> string
(** This function returns an unique code version string *)

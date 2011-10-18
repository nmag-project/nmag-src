(*
   (C) 2009 Dr. Thomas Fischbacher

   ocamlc -I ../mpi_petsc -I ../config -i ddpla.ml 

   Opcodes defined:

   QUIT
   HELLO
   NOP
   DESTROY(RESOURCES)
   CLEAN(RESOURCES)
   DIRTY(RESOURCES)
   VEC-CREATE(NAME,SIZES,REBUILD)
   PV<-MSV(PV,MSV)
   MSV-SYNC(MSV)
   MSV<-PV(MSV,PV)
   MAT-CREATE(NAME,TYPE,SIZES-L,SIZES-R,REBUILD,OPTARGS)
   MAT-DUPLICATE(SRC,NAME,COPY-VALUES?,REBUILD)
   MAT-COPY(SRC,DEST,SAME-NONZERO-PATTERN?)
   MAT-ZERO(MATRIX)
   MAT-ASSEMBLY(MATRIX,WHAT=("BEGIN"|"END"|"ALL"),FINAL?)
   VEC-ASSEMBLY(VEC,WHAT=("BEGIN"|"END"|"ALL"),PVEC?)
   MAT-SCALE(MATRIX,SCALE)
   MAT-ADD-IDENTITY(MATRIX,ID-COEFF)

   [ ] Implement opcodes: MAT_VIVIFY, 
       SOLVER_CREATE, {solver option setting opcodes}
       SOLVER_SETUP,
       SOLVE,
       NULLSPACE_CREATE,

   

   [ ] Main parallel loop must check if there are resource
       destructions pending and execute these if necessary.

   Dealing with parallel/multisequential vectors:

   Primary vectors:

   * multisequential:
      -  may be valid-everywhere and valid-on-master.
      -  represent "valid-on-master" as "dirty".


    === dependency-driven parallel linear algebra ===

   A more advanced successor to the centrally coordinated parallel
   linear algebra module "ccpla"

   Parallel resources are registered with a handle which holds an
   unique name tag string for identification purposes.

   Distributed resources handled:

   - Parallel Vectors
   - Multisequential Vectors
   (These are normally created in pairs!)

   - Matrices

   - KSPs

   - Matnullspaces

   - Parameters (as above, always have a parallel and a multisequential copy) [XXX ???]

   - command_sequence

   Rules for resource names:

   - Resource names are unique identifiers.
   - They are generally expected to have the structure
   "ddpla_name.simulation_name.simulation_resource{.subresource}"
   For now, sub-resources are only defined for vectors, where they are:
   ".ms" for "multi-sequential" and ".p" for "parallel"

   Dependency mechanism rules:

   0. Every resource has:
      - a name
      - an object
      - a rebuild (update) script
      - a dirtiness status
      - prerequisites (sorted-by-name mutable list of resource handles)
      - dependents (sorted-by-name mutable list of resource handles)

      The scripts, prerequisites, dependents, are processed in sync across
      all nodes.

   1. When a distributed resource gets forcefully modified,
       * it is marked as non-dirty (it was just set intentionally!)
       * All its dependents are "dirtified".

   2. * Dirtifying a dirty resource is a no-op.
      * Dirtifying a clean resource sets the "dirty" flag,
        and dirtifies all resources that depend on it.

   3. If a resource is requested, we enforce cleanliness and return it.

   4. * Enforcing cleanliness of a clean resource is a no-op.

      * Enforcing cleanliness of a dirty resource will enforce
        cleanliness of its pre-requisites, in lexicographical-in-name
        order, and then execute its own update script and remove the
        "dirty" flag.

   5. When a new resource gets registered, its build-script is scanned
      for resources it depends on. These are then sorted
      lexicographically, and form the initial prerequisites.
      Prerequisite/dependency relations then are updated for these
      entities.

   6. Special rules for vectors:

      There are primary (i.e. externally user-modifiable) vectors whose
      build-scripts are such that the parallel part will depend on
      the multisequential part, and the multisequential part will in
      such a way depend on itself that cleaning it will induce a broadcast.
      These hence have no dependencies apart from depending on one another.
      For other, read-only interim vectors, the multisequential part
      will depend on the parallel part.

   ==================================================================

   This dependency mechanism intimately depends on the role of
   "scripts", so we need some rules about these as well. One
   fundamental problem here is the question: Are we served well by the
   idea that a script is just a sequence of linear algebra operations?

   There are some indications that parametric scripts would be a good
   idea - after all, working with Dirichlet boundary conditions at
   infinity, as is often needed when dealing with electromagnetism -
   is a complex multistep process in FEM calculations. So, it may seem
   attractive to absorb this into a "parametric routine".

   A key question is: do routines have to call other routines? With
   the old CCPLA module, this clearly was the case. But one can argue
   that the role of dependencies is precisely to get rid of these
   "subroutine calls" by eliminating their underlying need: When a
   resource can rely on its prerequisites being up-to-date, there is
   no need to jump into subroutines. 

   Should we eventually find that subroutines are indeed needed, they
   should be handled in the right way by avoiding the two most
   widespread mistakes:

   - Subroutine calls must be function calls. One of the old problems
     of Spaghetti Coding was that everybody stomped on everybody
     else's variables. So, the idea of a "private workspace" and a defined
     way to export and import data are essential.

   - A "return" is a call of an unary function-implicitly-passed-as-parameter
     ("the return address on the stack"). This should be made explicit.
     I.e. there is no "return" - only calling functions.

   So, for now, we take our (preliminary) rules to be:

   1. Scripts NEVER have dependencies. They mention resources and
   enact dependency resolution, but do not depend on anything.

   2. Scripts never call other scripts.

   3. Scripts are sequences of operations that are either
      ddpla-generic, or specific extension opcodes (e.g.
      related to a FEM context).

   ==================================================================

   Design Note: The way we are dealing with op-codes in the
   predecessor module ccpla was rather awkward. Rationale:

   - We needed lengthy DCOM_xyz matching in the opcode interpreter.
   - We needed lengthy DCOM_xyz matching in the python->opcode translator.
   - When we wanted to add extension opcodes, we used parametric polymorphism
     
   Much better design:

   - Opcode sequences consist of "raw" opcode names plus parameters.

   - When we build a script from a command sequence,
     we map (opcode_name,params) -> (opcode_name,opcode_fun,param_names,param_vals)
     by looking up opcode functions in tables. Note that this
     look-up will also sort the parameters into the expected order.

     Table: opcode_name -> (opcode_fun, parameter_name_order)

   - Initially, there is just one opcode, which registers a
     ML function as a new opcode interpreter.

   - (Evil magic: We may even consider passing on a Python string
      that generates a function, wrapping that up in a closure,
      and registering this as a Python-implemented opcode!)

   - Ad opcode parameters: We have to fix what opcode parameters
     can be, and how they look like. 

     Opcode carries: array of (param_name,param_value)
     Compiled script carries: param_names and param_values

     What are parameter values? We certainly want to pass on
     the following data types:
       - integers
       - floats
       - strings
       - arrays of parameter values
       - options of parameter values
	   will be handled as length-0 or length-1 arrays,
           to match our approach at the scripting language level
	   that may not have a concept of options.
       - maybe more complicated stuff related to symbolic operations, 
         such as operator vivificators, mesh-with-element made_by info,
	 etc. 

       Proposed solution: cf. opcode_arg data type

       What about resource handles? Do we actually need them at all,
       considering our new design, as resources will refer to one
       another through build dependencies? They are useful for caching
       lookups, but dependency resolution we need not do via their
       finalizers. On the other hand, if we do, we get all that for
       free.

       So, we keep distributed resource handles: their automated
       dependency resolution is useful and reduces hassle, and being
       able to cache lookups is quite helpful.

     
   Advantages of the new design:

   - Easily extended, even at run time(!)
   - Can directly script linear algebra from the Python level,
     do not have to change interface code even if we added new opcodes.
   - Removes code clutter

  ==================================
  Note ad set-up phases and creating things (vectors, matrices, etc.)
  
  Issuing a parallel VEC-CREATE/MAT-CREATE/... opcode will result in
  resources being created (can be more than one).

  Preliminarily, these will be entered into
  ddpla.ddpla_new_resources_on_master, from where they
  then can be collected with ddpla_snatch_new
  (They are being identified via their name, which is
  derived from information that went into the opcode.)
	     

  ==================================
  Unresolved questions:

  - ddpla holds on to all registered resources.
    How do we selectively destroy resources?
    When does a resource table lookup happen?

    Partial answer: Consider creating two python-level
    "simulation objects" which independently register their
    matrices/vectors etc. with the same ddpla context.

    Evidently, holding on to some key resources on the master or not
    will have to make the difference whether something gets collected
    or not.

    NOTE: THIS WILL NEED SOME RE-DESIGN IN HERE! RATIONALE: FOR NOW,
    RESOURCES ARE MEMBERS OF A TABLE AND HOLD ON TO THEIR PRE-REQUISITES.
    WOULD NEED TO BUILD THE RESOURCE TABLE AS A WEAK HASH!

 (is this issue still open? Would it not suffice to have "mock drh keys"
  in the resource table that are copies of those that have finalizers?)

 - Ad population of matrices: What we essentially ALWAYS want to have
   is the option to:

   (a) have a specifiable function call matrix_set/matrix_inc closures
       to fill the matrix.
   (b) use mesh-based vivificators.

   The problem with (b) is that we would like not to make too specific
   assumptions about mesh data structures, but the mesh-based
   vivificator will certainly have to link back to the mesh data
   structure in some way. Hence, it cannot be sent over the net
   that easily. (In particular as the serialization provided by the 
   Marshal module showed strange behaviour on x86-32 for data >16MB.)

   So, this should perhaps be avoided.

   But how do we implement matrix vivification under the two constraints
   that (a) we should not impose specific mesh data structures,
   and (b) there may be large data a vivificator needs access to?

   After some prolonged thought (cf. rev. 6351), I came to these
   conclusions:

   1. The lowest level (parallel linear algebra) is a scripting
      language that favours dynamic typing, if only because
      ultimately, we want to make it freely specifiable by the
      highest-level scripting language (here, normally Python).

   2. We use a "pill" mechanism towards the high-level language
      that uses Obj.magic where inevitable but encapsulates things
      in such a way that everything is nice and robust.

   3. The idea of "avoiding Obj.magic" at the OCaml level is more
      based on religious faith than on practical considerations.
      For the parallel linear algebra component, we could only do
      so at a fairly big expense in terms of code complexity.
      We do use the type system to great advantage in the
      intermediate world, where it indeed makes a lot of sense.
      We have to find suitable ways to deal with constraints
      when interfacing towards a dynamically typed higher
      level - here, there is just no way around it.

      Is it then legitimate to use virtually the same techniques
      in situations where it may be theoretically possible to
      strongly enforce static typing, but only at an unreasonable
      expense in complexity? Now, this is not a matter of
      "being pragmatic", for claiming so very often is just myopic.

      However, all things considered, the "pill" mechanism served
      us very well so far, and as we have had some quite good
      experience with it, there is little reason for not using
      identical technology for structurally identical situations.
 *)

(* Note ad Matteo: Why do we use C-layout arrays here at all?!? *)

type c_int_bigarray1 = Nsimconf.c_int_bigarray1 (* - FIXME for Matteo *)
      (* (int32,Bigarray.int32_elt,Bigarray.c_layout) Bigarray.Array1.t *)
;;

let c_int_bigarray1_create size =
  Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout size
;;

let c_int_of_int x = Int32.of_int x;;
let c_int_to_int x = Int32.to_int x;;
let c_int_add = Int32.add;;

(* The default error reporting function - can be modified *)

let default_ddpla_error level str = 
 (* Levels are: ERROR, WARNING, INFO, DEBUG, "DEBUGVERBOSE", TRACE *)
  if true (* DDD Development-Only Definition! *)
    (* level = "ERROR" || level = "WARNING" *)
  then
    Printf.fprintf stderr "%s%!" str
  else ()
;;


(* Note: these are easily constructed from corresponding script-level
   values (e.g. Python), hence allowing a very smooth interface between
   the scripting world and opcodes...
 *)

type ddpla_magictype;; (* Does not exist - just so that "to-pill" and "from-pill"
			  functions can have type ddpla_magictype -> ddpla_pill
			  and ddpla_pill -> ddpla_magictype.

			  ddpla_magictype will always be created by use of Obj.magic
			*)
type ddpla_pill_payload;; (* opaque - cannot be constructed! *)
type ddpla_pilltype = string;;
type ddpla_pill = (ddpla_pilltype * ddpla_pill_payload);;


type opcode_argument =
  | OA_bool of bool
  | OA_int of int
  | OA_float of float
  | OA_string of string
  | OA_adminfunction of (ddpla -> opcode_argument array -> unit)
  | OA_resource of drh
  | OA_array of opcode_argument array
  | OA_assoc_array of (string * opcode_argument) array
  | OA_abstract_exec of
      (report_error:(?level:string -> string -> string) -> unit ->
	(opcode_argument array -> unit))
     (* New opcode executor, argument only to DEF-OPCODE *)
  | OA_buildscript of dscript
  | OA_complex_data of (ddpla_magictype * string * (drh array))
  (* NOTE: complex_data must always explicitly list the resource dependencies
     they induce.
   *)
and dres_object =
  | DRO_pvec of (  c_int_bigarray1 (* lengths *)
		     * c_int_bigarray1 (* offsets *)
		  ) * Mpi_petsc.vector
  | DRO_msvec of Mpi_petsc.vector
  | DRO_matrix of Mpi_petsc.matrix 
  | DRO_solver of Mpi_petsc.ksp
  | DRO_matnullspace of Mpi_petsc.matnullspace
  | DRO_parameters of (string array) * (float array)
  | DRO_sequence of dscript	
	(* XXX Shouldn't that be obsolete with the new design?
	   Actually, no - for we may want to e.g. run some diagnostics
	   script across all nodes!
	   - XXX TO GET SORTED OUT: What is the proper relation between
	   DRO_sequence and OA_buildscript?
	 *)
  | DRO_data of (string * ddpla_magictype)
and dcommand =
    {dc_opcode: string;
     dc_args: (string * opcode_argument) array;
    }
and dcompiled = 
    {dco_opcode: string;
     dco_param_names: string array; (* For debugging *)
     dco_params: opcode_argument array;
     dco_exec: (opcode_argument array -> unit);
   }
and dscript = dcommand array
and dbinary = dcompiled array
and dres =
    {dres_name: string;
     dres_rebuild: dbinary;
     dres_destroy: (dres -> unit);
     mutable dres_obj: dres_object option;
     (* May be None - useful at times... *)
     mutable dres_dirtiness: bool;
     mutable dres_prerequisites: distributed_resource_handle list;
     mutable dres_dependents: distributed_resource_handle list;
   }
and distributed_resource_handle =
    DRH of (string * dres option ref)
and ddpla =
    {
     ddpla_name: string;
     ddpla_comm: Mpi_petsc.communicator;
     ddpla_error: (string -> string -> unit);
     ddpla_pill_types:
       (string, ((ddpla_magictype -> ddpla_pill) * (ddpla_pill -> ddpla_magictype)))
       Hashtbl.t;
     ddpla_opcodes: (string,
		     (string array * (* param names *)
			(opcode_argument array -> unit) (* executor *)
		     )) Hashtbl.t;
     ddpla_resources: (string, dres) Hashtbl.t;
     ddpla_queue: dcommand array Queue.t;
     ddpla_quit: (unit -> unit);
     mutable ddpla_new_resources_on_master: drh list;
     (* In this list, we register all the resources that have been newly created.

	Rationale: When we are in a multistep-creation of a complex
	dependency tree, we need something that intermediately holds
	on to all the resources we generate so that they don't get GCd
	during the construction process. Once the construction is
	completed, it is expected that the entity that uses ddpla
	extracts from this list all the DRHs it wants to hold on to,
	and then clears it. From then on, GC will take care of getting
	rid of resources at the point when they are no longer needed.
      *)
     mutable ddpla_pending_finalizations: string list;
   }
and drh = distributed_resource_handle  (* Abbreviation *)
;;

let drh_name (DRH (n,_)) = n;;

let dro_typename x =
  match x with
  | DRO_pvec _ -> "parallel vector"
  | DRO_msvec _ -> "multi-sequential vector"
  | DRO_matrix _ -> "matrix"
  | DRO_solver _ -> "solver"
  | DRO_matnullspace _ ->  "matrix nullspace"
  | DRO_parameters _ -> "parameter set"
  | DRO_sequence _ -> "command sequence"
  | DRO_data (name,_) -> Printf.sprintf "DATA:%s" name
  (* | _ -> "[WARNING: martian distributed resource]" *)
;;

let rec oa_to_string oa =
  match oa with
  | OA_bool x -> if x then "True" else "False"
  | OA_int x -> Printf.sprintf "%d" x
  | OA_float x -> Printf.sprintf "%f" x
  | OA_string x -> Printf.sprintf "\"%s\"" (String.escaped x)
  | OA_adminfunction x -> "#<ADMIN FUNCTION>"
  | OA_resource x -> Printf.sprintf "#<Distributed Resource \"%s\">"
	(String.escaped (drh_name x))
  | OA_array x -> Printf.sprintf "[%s]"
	(String.concat ", " (Array.to_list (Array.map oa_to_string x)))
  | OA_assoc_array x -> Printf.sprintf "[%s]"
	(String.concat ", "
	   (Array.to_list
	      (Array.map
		 (fun (name,oa) ->
		   Printf.sprintf "'%s' => %s" name (oa_to_string oa))
		 x)))
  | OA_abstract_exec _ -> "#<EXEC>"	(* XXX ??? *)
  | OA_buildscript _ -> "#<BUILDSCRIPT>"
  | OA_complex_data (_,name,_) -> Printf.sprintf "#<COMPLEX-DATA %s>" (String.escaped name)
	(* XXX TODO: add opdata_to_string to print more info here! *)
  (* | _ -> "[Opcode-Argument ??? (FIXME)]" *)
;;

let oa_bool ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> 'a) = report_error in
  let s = "bool" in
  match oa with
  | OA_bool x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_int ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> 'a) = report_error in
  let s = "int" in
  match oa with
  | OA_int x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_float ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "float" in
  match oa with
  | OA_float x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_string ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "string" in
  match oa with
  | OA_string x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_resource ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "distributed_resource" in
  match oa with
  | OA_resource x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_abstract_exec ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "executor" in
  match oa with
  | OA_abstract_exec x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_adminfunction ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "adminfunction" in
  match oa with
  | OA_adminfunction x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_buildscript ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "buildscript" in
  match oa with
  | OA_buildscript x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_complex_data ~report_error ~opcode_name ~param_name ?required_type oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "DATA" in
  match oa with
  | OA_complex_data (x,ty,_) -> 
      (
       match required_type with
       | None -> x
       | Some rt ->
	   if rt <> ty 
	   then
	     let msg =
	       Printf.sprintf
		 "Opcode '%s' parameter '%s' error: expected '%s' of type '%s', got: '%s'"
		 opcode_name param_name s rt (oa_to_string oa)
	     in
	     let s = report_error ~level:"ERROR" msg in failwith s
	   else x
      )
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_array ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "oa array" in
  match oa with
  | OA_array x -> x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;


let oa_int_array ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "int array" in
  match oa with
  | OA_array x -> 
      Array.map (fun z -> oa_int ~report_error ~opcode_name ~param_name z) x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_float_array ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "float array" in
  match oa with
  | OA_array x -> 
      Array.map (fun z -> oa_float ~report_error ~opcode_name ~param_name z) x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_string_array ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "string array" in
  match oa with
  | OA_array x -> 
      Array.map (fun z -> oa_string ~report_error ~opcode_name ~param_name z) x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_resource_array ~report_error ~opcode_name ~param_name oa =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "distributed_resource array" in
  match oa with
  | OA_array x -> 
      Array.map (fun z -> oa_resource ~report_error ~opcode_name ~param_name z) x
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

(* These functions are for extracting parameters from 
   OA_assoc_array "optarg" arrays:
 *)
let oa_opt_bool ~report_error ~opcode_name ~param_name oa ?default name =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "assoc_array" in
  match oa with
  | OA_assoc_array x ->
      (try
	let (_,oa_v) = List.find (fun (k,_) -> k=name) (Array.to_list x)
	in Some (oa_bool ~report_error ~opcode_name ~param_name oa_v)
      with
      | Not_found -> 
	  default)
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_opt_int ~report_error ~opcode_name ~param_name oa ?default name =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "assoc_array" in
  match oa with
  | OA_assoc_array x ->
      (try
	let (_,oa_v) = List.find (fun (k,_) -> k=name) (Array.to_list x)
	in Some (oa_int ~report_error ~opcode_name ~param_name oa_v)
      with
      | Not_found -> 
	  default)
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_opt_float ~report_error ~opcode_name ~param_name oa ?default name =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "float" in
  match oa with
  | OA_assoc_array x ->
      (try
	let (_,oa_v) = List.find (fun (k,_) -> k=name) (Array.to_list x)
	in Some (oa_float ~report_error ~opcode_name ~param_name oa_v)
      with
      | Not_found -> default)
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;

let oa_opt_string ~report_error ~opcode_name ~param_name oa ?default name =
  let report_error:(?level:string -> string -> string) = report_error in
  let s = "string" in
  match oa with
  | OA_assoc_array x ->
      (try
	let (_,oa_v) = List.find (fun (k,_) -> k=name) (Array.to_list x)
	in Some (oa_string ~report_error ~opcode_name ~param_name oa_v)
      with
      | Not_found -> default)
  | _ ->
      let msg =
	Printf.sprintf
	  "Opcode '%s' parameter '%s' error: expected '%s', got: '%s'"
	  opcode_name param_name s (oa_to_string oa)
      in
      let s = report_error ~level:"ERROR" msg in failwith s
;;


let rec buildscript_prereqs dscript = 
  let ht = Hashtbl.create 17 in
  let () = Array.iter 
      (fun cmd ->
	let args = cmd.dc_args in
	Array.iter
	  (fun (argname,param) ->
	    let deps = oa_deps param in
	    Array.iter (fun d -> Hashtbl.replace ht d true) deps)
	  args)
      dscript
  in
  let x = ref [] in
  let () = Hashtbl.iter
      (fun k v -> x := k ::(!x)) ht
  in
  Array.of_list (List.sort compare (!x))
and oa_deps oa = 
  match oa with
  | OA_resource x -> [|x|]
  | OA_array x ->
      Array.concat (Array.to_list (Array.map oa_deps x))
  | OA_assoc_array x ->
      Array.concat (Array.to_list (Array.map (fun (_,y) -> oa_deps y) x))
  | OA_buildscript x -> buildscript_prereqs x
  | OA_complex_data (_,_,deps) -> deps
  | _ -> [||]
;;

let ddpla_registered_resources ddpla = 
  let r_names = ref [] in
  let () =
    Hashtbl.iter
      (fun key _ -> r_names := key :: !r_names)
      ddpla.ddpla_resources
  in
  List.sort compare !r_names
;;

let get ddpla drh =
  let DRH (name,ref_opt) = drh in
  match !ref_opt with
  | Some x -> x
  | None ->
      let x =
	try
	  Hashtbl.find ddpla.ddpla_resources name
	with | Not_found ->
	  let err = ddpla.ddpla_error "ERROR" in
	  let () = err (Printf.sprintf "Unknown parallel resource: '%s'\n" name) in
	  let names = ddpla_registered_resources ddpla in
	  let () = List.iter
	      (fun n -> err (Printf.sprintf " '%s'\n" n))
	      names
	  in
	  let () = err "<=======\n%!" in
	  raise Not_found
      in
      let () = ref_opt := Some x in
      x
;;

let register_dependency ~parent ~child ddpla =
  let dres_parent = get ddpla parent
  and dres_child = get ddpla child
  in
  let parent_dependents = dres_parent.dres_dependents
  and child_prerequisites = dres_child.dres_prerequisites
  in
  let rec insert x li =
    match li with
    | [] -> [x]
    | (hd::tl) ->
	let nh = drh_name hd
	and nx = drh_name x
	in
	if nx = nh then li (* Dependency already known, do nothing *)
	else if nx < nh	then x::li
	else hd::(insert x tl)
  in
  begin
    dres_parent.dres_dependents <- insert child parent_dependents;
    dres_child.dres_prerequisites <- insert parent child_prerequisites;
  end
;;

(* If no distribited_resource_handle is provided, we build a new
   one. In many cases, however, we will get a skeleton drh from our
   caller. Reason: the drh will typically appear in the build script,
   and we have to use that one. Note that the cyclical dependency
   resource -> buildscript -> resource is not a problem here.

   Note that this will add a finalizer to drh!
  *)

let ddpla_register_new ?drh ddpla ~prerequisites dres =
  let name = dres.dres_name in
  let the_drh = match drh with
  | None -> DRH (name, ref None)
  | Some x -> x
  in
  let _ =
    (try
      let _ = Hashtbl.find ddpla.ddpla_resources name in
      let msg = 
	Printf.sprintf "Tried to make duplicate resource entry - name: '%s'\n" name
      in
      let () = ddpla.ddpla_error "ERROR" msg in
      failwith msg
    with | Not_found -> ())
  in
  (* XXX set finalizer for DRH! *)
  let () =
    Array.iter
      (fun pre -> register_dependency ~parent:pre ~child:the_drh ddpla)
      prerequisites
  in
  let _ = Hashtbl.add ddpla.ddpla_resources name dres in
  let () = 
    let DRH (_,r) = the_drh in
    r := Some dres
  in
  let () = 
    if Mpi_petsc.comm_rank ddpla.ddpla_comm = 0
    then (* Only the master has this finalizer *)
      begin
	Gc.finalise
	  (fun drh ->
	    let () =
	      ddpla.ddpla_error
		"DEBUG"
		(Printf.sprintf "Registering '%s' for parallel destruction\n" name)
	    in
	    ddpla.ddpla_pending_finalizations <-
	      name::ddpla.ddpla_pending_finalizations
	  ) the_drh;
	ddpla.ddpla_new_resources_on_master <- the_drh::ddpla.ddpla_new_resources_on_master
      end
    else ()
  in
  the_drh
;;

let _get_type_error fname wanted ddpla drh x =
  let msg =
    Printf.sprintf
      "get_%s: Resource '%s' is not a %s, but a %s!"
      fname (drh_name drh) wanted (dro_typename x)
  in
  begin
    ddpla.ddpla_error "ERROR" msg;
    failwith msg;
  end
;;

let get_msvec ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_msvec v) -> v
  | Some x -> 
      _get_type_error "get_msvec" "multi-sequential vector" ddpla drh x
;;

let get_pvec ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_pvec (_,v)) -> v
  | Some x -> 
      _get_type_error "get_pvec" "parallel vector" ddpla drh x
;;

let get_pvec_lengths_offsets ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | (Some DRO_pvec (lo,_)) -> lo
  | Some x -> 
      _get_type_error "get_pvec_lengths_offsets" "parallel vector" ddpla drh x
;;


let get_matrix ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_matrix x) -> x
  | Some x -> 
      _get_type_error "get_matrix" "matrix" ddpla drh x
;;

let get_matnullspace ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_matnullspace x) -> x
  | Some x -> 
      _get_type_error "get_matnullspace" "matrix nullspace" ddpla drh x
;;

let get_solver ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_solver x) -> x
  | Some x -> 
      _get_type_error "get_solver" "linear solver" ddpla drh x
;;

let get_parameters ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_parameters (names,vals)) -> (names,vals)
  | Some x -> 
      _get_type_error "get_parameters" "parameter set" ddpla drh x
;;

let get_sequence ddpla drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some (DRO_sequence s) -> s
  | Some x -> 
      _get_type_error "get_sequence" "command sequence" ddpla drh x
;;

let get_data ddpla expected_type drh =
  let dres = get ddpla drh in
  match dres.dres_obj with
  | None -> failwith "XXX-TODO-Uninitialized-DRES"
  | Some ((DRO_data (ty,x)) as dro) ->
      if ty = expected_type then x
      else 
	_get_type_error "get_data" (Printf.sprintf "DATA:%s" expected_type) ddpla drh dro
  | Some x -> 
      _get_type_error "get_data" "DATA" ddpla drh x
;;



let known_dependencies ddpla = 
  let ht_deps = Hashtbl.create 17 in
  let register_dep parent child =
    let old_deps = try Hashtbl.find ht_deps parent with | Not_found -> [] in
    let new_deps = 
      if List.exists (fun x -> x = child) old_deps
      then old_deps
      else child::old_deps
    in
    Hashtbl.replace ht_deps parent new_deps
  in
  let () =
    Hashtbl.iter
      (fun name dres ->
	let parent_names = List.map drh_name dres.dres_prerequisites in
	let child_names = List.map drh_name dres.dres_dependents in
	let () = List.iter (fun p -> register_dep p name) parent_names in
	let () = List.iter (fun c -> register_dep name c) child_names in
	())
      ddpla.ddpla_resources
  in
  let all =
    Array.of_list
      (Hashtbl.fold
	 (fun k v sf -> (k,Array.of_list (List.sort compare v))::sf)
	 ht_deps [])
  in
  let () = Array.sort compare all in
  all
;;

(* We introduce this extra level of wrapping just in case we eventually
   would decide to switch over to a concept of "scripts as functions".

   Then, this function would have to be modified in such a way that it
   "wraps a (lambda () ...) abstraction around the command sequence".
*)
let commands_as_script array_of_commands =
  let a:dscript = array_of_commands in
  a
;;

let compile_script ddpla dscript = 
  let v_commands = dscript in
  Array.map
    (fun cmd ->
      let opcode = cmd.dc_opcode
      and args = cmd.dc_args
      in
      let (opcode_param_names,opcode_executor) =
	try Hashtbl.find ddpla.ddpla_opcodes opcode
	with | Not_found ->
	  let msg =
	    Printf.sprintf "Unknown Opcode encountered: '%s'\n" opcode
	  in
	  let () = ddpla.ddpla_error "ERROR" msg in
	  failwith msg
      in
      let compiled_params = 
	let report_bad_params () =
	  let msg =
	    Printf.sprintf
	      "Bad Parameters for opcode '%s' encountered!\n Expected: [FIXME]\nSeen    : [FIXME]\n"
	      opcode
	  in
	  let () = ddpla.ddpla_error "ERROR" msg in
	  failwith msg
	in
	let () =
	  (if Array.length args <> Array.length opcode_param_names
	  then report_bad_params() else ())
	in
	let get_param name =
	  let rec walk pos = 
	    if pos = Array.length args then report_bad_params()
	    else let (n,x) = args.(pos) in
	    if n = name then x
	    else walk (1+pos)
	  in walk 0
	in
	Array.map get_param opcode_param_names
      in
      {
       dco_opcode=opcode;
       dco_param_names=opcode_param_names;
       dco_params=compiled_params;
       dco_exec=opcode_executor;
     }
    )
    v_commands
;;

let execute_compiled ?(trace=false) ddpla dcompiled =
  for i=0 to Array.length dcompiled-1 do
    let cmd = dcompiled.(i) in
    let () =
      (if trace then
	ddpla.ddpla_error "TRACE"
	  (Printf.sprintf
	     "[OP %3d/%3d] %s\n" i (Array.length dcompiled)
	     cmd.dco_opcode)
      else ())
    in
    cmd.dco_exec cmd.dco_params
  done
;;

let execute_script ?(trace=false) ddpla dscript =
  execute_compiled ~trace ddpla (compile_script ddpla dscript)
    (* Note to self: Lovely SK-calculus combinator hidden here! *)
;;

let execute_rebuild ddpla drh = 
  let dres = get ddpla drh in
  execute_compiled ddpla dres.dres_rebuild
;;

let rec ensure_uptodate ddpla drh =
  let dres = get ddpla drh in
  if not(dres.dres_dirtiness) then ()
  else
    begin
      (* "Equity sees as done what ought to be done" :-)
	 What do we gain by updating the dirtiness status "too early"?
	 Answer: For tree-like dependencies, this does not matter.
	 For circular dependencies, this trades a callstack-eating
	 infinite resolution loop into "probably wrong results".

	 Now, as we auto-determine dirtiness from build-scripts, and
	 every build-script will mention the target resource, every
	 resource will depend on itself for building. Here, this
	 mechanism of resolving circularity does the right thing.
       *)
      dres.dres_dirtiness <- false;
      List.iter (ensure_uptodate ddpla) dres.dres_prerequisites;
      execute_rebuild ddpla drh;
    end
;;

let rec spoil_uptodate ddpla drh =
  (* The opposite of the above function - mark a resource as dirty. *)
  let dres = get ddpla drh in
  if dres.dres_dirtiness then ()
  else
    begin
      dres.dres_dirtiness <- true;
      List.iter (spoil_uptodate ddpla) dres.dres_dependents;
    end
;;



let mpi_get_distributed_command_sequence ddpla =
  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
  let (len,recv) = 
    try Mpi_petsc.receive_reporting_length 0 Mpi_petsc.any_tag ddpla.ddpla_comm 
	(* Note: 0 = source; this will use Mpi_petsc.receive_basic(),
	   using probe() to determine lengths.
	 *)
    with
    | _ -> 
	let () = ddpla.ddpla_error "ERROR"
	    "PROBLEM: Mpi_petsc.receive() failed - issuing DCOM_QUIT!\n%!"
	in
	(0,[|{dc_opcode="QUIT";dc_args=[||]}|])
  in
  recv
;;

let slave_mode ddpla =
  let nr_nodes = Mpi_petsc.comm_size ddpla.ddpla_comm in
  let rank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
  let () = ddpla.ddpla_error "INFO" (Printf.sprintf "[Node %d]DDD slave_mode\n" rank)
  in
  let rec work () =
    let () = ddpla.ddpla_error "DEBUG" 
	(Printf.sprintf "[Node %d] DDD Slave listening!\n" rank)
    in
    let dcommands = mpi_get_distributed_command_sequence ddpla in
    let () = execute_script ddpla (commands_as_script dcommands) in
    work ()
  in work()
;;

let master_process_queue ddpla =
  let nr_nodes = Mpi_petsc.comm_size ddpla.ddpla_comm in
  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
  try 
    let rec work () =
      let next_seq = Queue.take ddpla.ddpla_queue in
      begin
	for i=1 to nr_nodes-1 do
	  (* Note:
	     distributing commands to slave nodes at present this is quite primitive -
	     we do not scatter.  Hence, there is opportunity for refinement later on.
	   *)
	  let len =
	    Mpi_petsc.send_reporting_length next_seq
	      i (* destination *)
	      0 (* tag *)
	      ddpla.ddpla_comm
	  in
	  ()
	done;
	execute_script ddpla (commands_as_script next_seq);
	work();
      end
    in
    work ()
  with | Queue.Empty -> ()
;;

let _def_opcode ~ddpla ~opcode ?(args=[||]) abstract_exec =
  (* let ddd = Printf.fprintf stderr "DDD _def_opcode(opcode=%s)\n%!" opcode in *)
  let report_error ?(level="ERROR") msg =
    let xmsg = 
      Printf.sprintf 
	"DDPLA '%s' Opcode '%s' error: %s\n"
	ddpla.ddpla_name
	opcode msg
    in
    let () = ddpla.ddpla_error level xmsg in
    xmsg
  in
  let h = ddpla.ddpla_opcodes in
  let () = 
    try 
      let _ = (Hashtbl.find h opcode) in 
      let s = report_error ~level:"ERROR" "Opcode already exists!"
      in failwith s
    with | Not_found -> ()
  in
  Hashtbl.add h opcode
    (args, abstract_exec ~report_error:report_error ())
;;

let ddpla_create_skeleton (* Internal use only *)
    ~fun_error
    ~name
    ~parent_communicator
    ~fun_quit () =
  let comm = Mpi_petsc.comm_dup parent_communicator in
  let ddpla =
    {ddpla_name = name;
     ddpla_comm = comm;
     ddpla_error = fun_error;
     ddpla_pill_types = Hashtbl.create 17;
     ddpla_opcodes = Hashtbl.create 17;
     ddpla_resources = Hashtbl.create 17;
     ddpla_queue = Queue.create();
     ddpla_quit = fun_quit;
     ddpla_new_resources_on_master = [];
     ddpla_pending_finalizations = [];    
   }
  in
  let () =
    _def_opcode
      ~ddpla
      ~opcode:"DEF-OPCODE"
      ~args:[|":NAME";":ARGS";":EXEC"|]
      (fun ~report_error () ->
	fun args ->
	  let newop_name =
	    oa_string ~report_error ~opcode_name:"DEF-OPCODE" ~param_name:":NAME" args.(0)
	  in
	  let newop_args =
	    oa_string_array ~report_error ~opcode_name:"DEF-OPCODE" ~param_name:":ARGS" args.(1)
	  in
	  let newop_exec =
	    oa_abstract_exec ~report_error ~opcode_name:"DEF-OPCODE" ~param_name:":EXEC" args.(2) 
	  in
	  let _ =
	    report_error
	      ~level:"DEBUG"
	      (Printf.sprintf "registering opcode '%s'." newop_name)
	  in
	  _def_opcode
	    ~ddpla
	    ~opcode:newop_name
	    ~args:newop_args
	    newop_exec)
  in
  let () = 
    _def_opcode
      ~ddpla
      ~opcode:"**ADMIN-FUNCTION**"
      ~args:[|":FUN";":ARGS"|]
      (fun ~report_error () ->
	fun args ->
	  let afun =
	    oa_adminfunction ~report_error ~opcode_name:"**ADMIN-FUNCTION**" ~param_name:":FUN" args.(0)
	  in
	  let afun_args =
	    oa_array ~report_error ~opcode_name:"**ADMIN-FUNCTION**" ~param_name:":ARGS" args.(1)
	  in
	  let () = afun ddpla afun_args in
	  ())
  in
  ddpla
;;

let ddpla_add_core_opcodes
    ?(fun_error=default_ddpla_error)
    ddpla
    = 
  let mat_destroy report_error dres = 
    let report_error:(?level:string -> string -> string) = report_error in
    match dres.dres_obj with
    | Some (DRO_matrix x) -> Mpi_petsc.mat_destroy x
    | None -> ()
    | _ ->
	let s = report_error ~level:"ERROR"
	    "mat_destroy called on non-matrix resource!\n"
	in failwith s
  in
  begin
    _def_opcode
      ~ddpla
      ~opcode:"QUIT"
      ~args:[||]
      (fun ~report_error () ->
	fun args ->
	  ddpla.ddpla_quit());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"HELLO"
      ~args:[||]
      (fun ~report_error () ->
	fun args ->
	  Printf.printf "[Node %3d/%3d] Hello from '%s'\n%!"
	    (Mpi_petsc.comm_rank ddpla.ddpla_comm)
	    (Mpi_petsc.comm_size ddpla.ddpla_comm)
	    (Unix.gethostname()));
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"NOP"
      ~args:[||]
      (fun ~report_error () ->
	fun args -> ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"DESTROY"
      ~args:[|":RESOURCES"|]
      (fun ~report_error () ->
	fun args ->
	  let resources =
	    oa_resource_array ~report_error ~opcode_name:"DESTROY" ~param_name:":RESOURCES" args.(0)
	  in
	  Array.iter
	    (fun drh ->
	      let dres = get ddpla drh in
	      let _ =
		report_error ~level:"DEBUG"
		  (Printf.sprintf "Destroying resource '%s'\n" dres.dres_name)
	      in
	      dres.dres_destroy dres)
	    resources);
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"CLEAN"
      ~args:[|":RESOURCES"|]
      (fun ~report_error () ->
	fun args ->
	  let resources =
	    oa_resource_array ~report_error ~opcode_name:"CLEAN" ~param_name:":RESOURCES" args.(0)
	  in
	  Array.iter
	    (fun drh ->
	      let _ =
		report_error ~level:"DEBUG"
		  (Printf.sprintf "Marking resource '%s' as clean\n" (drh_name drh))
	      in
	      ensure_uptodate ddpla drh)
	    resources);
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"DIRTY"
      ~args:[|":RESOURCES"|]
      (fun ~report_error () ->
	fun args ->
	  let resources =
	    oa_resource_array ~report_error ~opcode_name:"DIRTY" ~param_name:":RESOURCES" args.(0)
	  in
	  Array.iter
	    (fun drh ->
	      let _ =
		report_error ~level:"DEBUG"
		  (Printf.sprintf "Marking resource '%s' as dirty\n" (drh_name drh))
	      in
	      ensure_uptodate ddpla drh)
	    resources);
    (* ----------------------- *)
    (* 
       - REBUILD parameter renders DEPENDS unnecesary!
       We can analyse REBUILD to determine the resources needed!
       
       - When REBUILD is given as [||], we assume this vector to be a
       primary vector, i.e. vec.p depends on vec.ms, and the build script
       consists of just copying.
       
       - Otherwise, we deal with an auxiliary quantity, and we make the MSV depend on the PV.
     *)
    _def_opcode
      ~ddpla
      ~opcode:"VEC-CREATE"
      ~args:[|":NAME";
	      ":SIZES"; (* int array: local share sizes in the distribution *)
	      ":REBUILD"; (* command sequence *)
	    |]
      (fun ~report_error () ->
	fun args ->
	  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
	  let name =
	    oa_string ~report_error ~opcode_name:"VEC-CREATE" ~param_name:":NAME" args.(0)
	  in
	  let sizes = 
	    oa_int_array ~report_error ~opcode_name:"VEC-CREATE" ~param_name:":SIZES" args.(1)
	  in
	  let buildscript = 
	    oa_buildscript ~report_error ~opcode_name:"VEC-CREATE" ~param_name:":REBUILD" args.(2)
	  in
	  let deps = buildscript_prereqs buildscript in
	  let is_primary = (Array.length deps = 0) in
	  let nr_nodes = Array.length sizes in
	  let _ =
	    (if nr_nodes = Mpi_petsc.comm_size ddpla.ddpla_comm then ()
	    else 
	      let s =report_error ~level:"ERROR"
		"Tried to create a parallel vector where the number of pieces does not match the number of MPI nodes!\n"
	      in failwith s)
	  in
	  let global_size = Array.fold_left (+) 0 sizes in
	  let ba_local_sizes = c_int_bigarray1_create nr_nodes in
	  let ba_offsets = c_int_bigarray1_create nr_nodes
          in
	  let () =
	    for i=0 to nr_nodes-1 do
	      ba_offsets.{i} <- c_int_of_int 0;
	      ba_local_sizes.{i} <- c_int_of_int sizes.(i);
	    done
	  in
	  let () =
	    for i=1 to nr_nodes-1 do
	      ba_offsets.{i} <- c_int_add ba_offsets.{i-1} ba_local_sizes.{i-1};
	    done
	  in
	  let _ = report_error ~level:"DEBUG"
	      (Printf.sprintf "Creating vector pair '%s' (global size=%d)\n" name global_size)
	  in
	  let name_p = Printf.sprintf "%s.p" name in
	  let name_ms = Printf.sprintf "%s.ms" name in
	  let vec_p = Mpi_petsc.vector_create_mpi global_size sizes.(myrank) name_p in
	  let vec_ms = Mpi_petsc.vector_create global_size name_ms in
	  let () = Mpi_petsc.vector_assemble vec_p in
	  let () = Mpi_petsc.vector_assemble vec_ms in
	  let drh_p = DRH (name_p,ref None) in
	  let drh_ms = DRH (name_ms,ref None) in
	  let (buildscript_p,buildscript_ms) =
	    if is_primary
	    then
	      ( (* parallel build: fetch from multisequential *)
	       [|
	       {dc_opcode="PV<-MSV";
		dc_args=[|(":PV", OA_resource drh_p);
			  (":MSV", OA_resource drh_ms)
			|]}
	       |],
                (* multisequential build: when dirty, sync *)
		[|
		  {dc_opcode="MSV-SYNC";
		   dc_args=[|(":MSV", OA_resource drh_ms)|]
		 }
		|]
	       )
	    else
	      ( (* parallel build: use dependency script *)
		buildscript,
		(* multisequential build: allgather *)
		[|{dc_opcode="MSV<-PV";
		   dc_args=[|(":MSV", OA_resource drh_ms);
			     (":PV", OA_resource drh_p);
			   |]}
		|]
	       )
	  in
	  let vec_destroy dres = 
	    match dres.dres_obj with
	    | Some (DRO_msvec x) -> Mpi_petsc.vec_destroy x
	    | Some (DRO_pvec (_,x)) -> Mpi_petsc.vec_destroy x
	    | None -> ()
	    | _ ->
		let s =
		  report_error ~level:"ERROR"
		    "vec_destroy called on non-vector resource!\n"
		in failwith s
	  in
	  let dres_p = {
	    dres_name=name_p;
	    dres_rebuild=compile_script ddpla buildscript_p;
	    dres_destroy=vec_destroy;
	    dres_obj=Some (DRO_pvec ((ba_local_sizes,ba_offsets),vec_p));
	    dres_dirtiness=false;
	    dres_prerequisites=[];
	    dres_dependents=[];
	  }
	  and dres_ms = {
	    dres_name=name_ms;
	    dres_rebuild=compile_script ddpla buildscript_ms;
	    dres_destroy=vec_destroy;
	    dres_obj=Some (DRO_msvec vec_ms);
	    dres_dirtiness=false;
	    dres_prerequisites=[];
	    dres_dependents=[];

	  }
	  in
          (* Here, breaking the chicken-and-egg dependency problem is a bit tricky: *)
	  let drh_p =
	    ddpla_register_new
	      ~drh:drh_p ddpla
	      ~prerequisites:[||]
	      (* ~prerequisites:(buildscript_prereqs buildscript_p) *)
	      dres_p
	  in
	  let drh_ms =
	    ddpla_register_new
	      ~drh:drh_ms ddpla
	      ~prerequisites:[||]
	      (* ~prerequisites:(buildscript_prereqs buildscript_ms) *)
	      dres_ms
	  in
	  let () =
	    Array.iter
	      (fun dep -> register_dependency ~parent:dep ~child:drh_p ddpla)
	      (buildscript_prereqs buildscript_p)
	  in
	  let () =
	    Array.iter
	      (fun dep -> register_dependency ~parent:dep ~child:drh_ms ddpla)
	      (buildscript_prereqs buildscript_ms)
	  in
	  ()
      );
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"PV<-MSV"
      ~args:[|":PV";":MSV"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_p =
	    oa_resource ~report_error ~opcode_name:"PV<-MSV" ~param_name:":PV" args.(0)
	  in
	  let drh_ms =
	    oa_resource ~report_error ~opcode_name:"PV<-MSV" ~param_name:":MSV" args.(1)
	  in
	  let vec_p = get_pvec ddpla drh_p in
	  let vec_ms = get_msvec ddpla drh_ms in
	  let (v_lens,v_offsets) = get_pvec_lengths_offsets ddpla drh_p in
	  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
	  let mylen = c_int_to_int v_lens.{myrank} in
	  let myoffset = c_int_to_int v_offsets.{myrank} in
	  let () = 
	    Mpi_petsc.with_petsc_vector_as_bigarray vec_p
	      (fun ba_p ->
	    Mpi_petsc.with_petsc_vector_as_bigarray vec_ms
	      (fun ba_ms ->
		for i=0 to mylen-1 do
		  ba_p.{i} <- ba_ms.{i+myoffset}
		done))
	  in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MSV-SYNC"
      ~args:[|":MSV"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_v =
	    oa_resource ~report_error ~opcode_name:"MSV-SYNC" ~param_name:":MSV" args.(0)
	  in
	  let vec = get_msvec ddpla drh_v in
	  (* DEBUG: (removed, as the sprintf may slow us down)
	  let _ = report_error
	      ?fail:None ~level:"DEBUG"
	      (Printf.sprintf "MPI broadcasting vector '%s'\n" (drh_name drh_v))
	  in
	   *)
	  let () =
	    Mpi_petsc.with_petsc_vector_as_bigarray vec
	      (fun ba_vec ->
		Mpi_petsc.broadcast_bigarray_float ba_vec 0 ddpla.ddpla_comm)
	  in ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MSV<-PV"
      ~args:[|":MSV";":PV"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_ms =
	    oa_resource ~report_error ~opcode_name:"MSV<-PV" ~param_name:":MSV" args.(0)
	  in
	  let drh_p =
	    oa_resource ~report_error ~opcode_name:"MSV<-PV" ~param_name:":PV" args.(1)
	  in
	  let vec_p = get_pvec ddpla drh_p in
	  let vec_ms = get_msvec ddpla drh_ms in
	  let (v_lens,v_offsets) = get_pvec_lengths_offsets ddpla drh_p in
	  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
	  let mylen = c_int_to_int v_lens.{myrank} in
	  let myoffset = c_int_to_int v_offsets.{myrank} in
	  let () = 
	    Mpi_petsc.vec_allgather
	      ddpla.ddpla_comm
	      vec_ms
	      vec_p
	      v_lens
	      v_offsets
	  in ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-CREATE"
      ~args:[|":NAME";
	      ":TYPE";	 (* string: "seqdense", "mpidense", "seqaij", "mpiaij"
			    -> PETSc MatCreate() *)
	      ":SIZES-L"; (* int array: local share sizes in the distribution, left index *)
	      ":SIZES-R"; (* int array: local share sizes in the distribution, left index *)
	      ":REBUILD"; (* command sequence *)
	      ":OPTARGS";   (* assoc_array, defaults:
			      ("auto_assembly" -> False XXX NOT IMPLEMENTED YET - TODO)
			      "prealloc_diagonal" -> 75
			      "prealloc_off_diagonal" -> 45
			    *)
	    |]
      (fun ~report_error () ->
	fun args ->
	  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
	  let name =
	    oa_string ~report_error ~opcode_name:"MAT-CREATE" ~param_name:":NAME" args.(0)
	  in
	  let mtype =
	    oa_string ~report_error ~opcode_name:"MAT-CREATE" ~param_name:":TYPE" args.(1)
	  in
	  let sizes_le = 
	    oa_int_array ~report_error ~opcode_name:"MAT-CREATE" ~param_name:":SIZES-L" args.(2)
	  in
	  let sizes_ri = 
	    oa_int_array ~report_error ~opcode_name:"MAT-CREATE" ~param_name:":SIZES-R" args.(3)
	  in
	  let buildscript = 
	    oa_buildscript ~report_error ~opcode_name:"MAT-CREATE" ~param_name:":REBUILD" args.(4)
	  in
	  let optargs = args.(5) in
	  let deps = buildscript_prereqs buildscript in
	  let nr_nodes_le = Array.length sizes_le in
	  let nr_nodes_ri = Array.length sizes_ri in
	  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
	  let mysize = Mpi_petsc.comm_size ddpla.ddpla_comm in
	  let _ =
	    (if ((nr_nodes_le = myrank) && (nr_nodes_ri = myrank)) 
	    then ()
	    else
	      let s =
		report_error ~level:"ERROR"
		  "Tried to create a parallel matrix where the number of pieces does not match the number of MPI nodes!\n"
	    in failwith s)
	  in
	  let global_size_le = Array.fold_left (+) 0 sizes_le in
	  let global_size_ri = Array.fold_left (+) 0 sizes_ri in
	  let mx = Mpi_petsc.matrix_create
	      ~communicator:ddpla.ddpla_comm
	      ~local_rows:sizes_le.(myrank)
	      ~local_cols:sizes_ri.(myrank)
	      ~matrix_type:mtype
	      (* ~auto_assembly:false *) ~auto_assembly:false
	      ?prealloc_diagonal:(oa_opt_int
				    ~report_error
				    ~opcode_name:"MAT-CREATE"
				    ~param_name:":OPTARG"
				    optargs
				    ~default:
				    (if mtype = "seqdense" || mtype = "mpidense"
				    then 0 else 75)
				    "prealloc_diagonal")
	      ?prealloc_off_diagonal:(oa_opt_int
					~report_error
					~opcode_name:"MAT-CREATE"
					~param_name:":OPTARG"
					optargs
					~default:
					(if mtype = "seqdense" || mtype = "mpidense"
					then 0 else 45)
					"prealloc_off_diagonal")
	      global_size_le global_size_ri name
	  in
	  let drh = DRH (name,ref None) in
	  let dres = {
	    dres_name=name;
	    dres_rebuild=compile_script ddpla buildscript;
	    dres_destroy=mat_destroy report_error;
	    dres_obj=Some (DRO_matrix mx);
	    dres_dirtiness=false;
	    dres_prerequisites=[];
	    dres_dependents=[];
	  }
	  in
	  let drh =
	    ddpla_register_new
	      ~drh:drh ddpla ~prerequisites:(buildscript_prereqs buildscript) dres
	  in
	  ()
      );
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-DUPLICATE"
      ~args:[|":SRC";":NAME";":COPY-VALUES?";":REBUILD"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_mx =
	    oa_resource ~report_error ~opcode_name:"MAT-DUPLICATE" ~param_name:":SRC" args.(0)
	  in
	  let name_new =
	    oa_string ~report_error ~opcode_name:"MAT-DUPLICATE" ~param_name:":NAME" args.(1)
	  in
	  let do_copy =
	    oa_bool ~report_error ~opcode_name:"MAT-DUPLICATE" ~param_name:":COPY-VALUES?" args.(2)
	  in
	  let buildscript = 
	    oa_buildscript ~report_error ~opcode_name:"MAT-CREATE" ~param_name:":REBUILD" args.(3)
	  in
	  let mx_src = get_matrix ddpla drh_mx in
	  let mx = Mpi_petsc.matrix_duplicate do_copy mx_src in
	  let drh = DRH (name_new,ref None) in
	  let dres = {
	    dres_name=name_new;
	    dres_rebuild=compile_script ddpla buildscript;
	    dres_destroy=mat_destroy report_error;
	    dres_obj=Some (DRO_matrix mx);
	    dres_dirtiness=false;
	    dres_prerequisites=[];
	    dres_dependents=[];
	  }
	  in
	  let drh =
	    ddpla_register_new
	      ~drh:drh ddpla ~prerequisites:(buildscript_prereqs buildscript) dres
	  in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-COPY"
      ~args:[|":SRC";":DEST";":SAME-NONZERO-PATTERN?"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_mx_src =
	    oa_resource ~report_error ~opcode_name:"MAT-COPY" ~param_name:":SRC" args.(0)
	  in
	  let drh_mx_dst =
	    oa_resource ~report_error ~opcode_name:"MAT-COPY" ~param_name:":DEST" args.(1)
	  in
	  let same_nonzero_pattern =
	    oa_bool ~report_error ~opcode_name:"MAT-DUPLICATE" ~param_name:":SAME-NONZERO-PATTERN?" args.(2)
	  in
	  let mx_src = get_matrix ddpla drh_mx_src in
	  let mx_dst = get_matrix ddpla drh_mx_dst in
	  let () = Mpi_petsc.matrix_copy same_nonzero_pattern mx_src mx_dst in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-ZERO"
      ~args:[|":MATRIX"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_mx =
	    oa_resource ~report_error ~opcode_name:"MAT-ZERO" ~param_name:":MATRIX" args.(0)
	  in
	  let mx = get_matrix ddpla drh_mx in
	  let () = Mpi_petsc.matrix_zero_entries mx in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-ASSEMBLY"
      ~args:[|":MATRIX";":WHAT";":FINAL?"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_mx =
	    oa_resource ~report_error ~opcode_name:"MAT-ASSEMBLY" ~param_name:":MATRIX" args.(0)
	  in
	  let what =
	    oa_string ~report_error ~opcode_name:"MAT-ASSEMBLY" ~param_name:":WHAT" args.(1)
	  in
	  let do_final =
	    oa_bool ~report_error ~opcode_name:"MAT-ASSEMBLY" ~param_name:":FINAL?" args.(2)
	  in
	  let mx = get_matrix ddpla drh_mx in
	  let (do_begin,do_end) =
	    match what with
	    | "BEGIN" -> (true,false)
	    | "END" -> (false,true)
	    | "ALL" -> (true,true)
	    | x -> 
		let s = report_error ~level:"ERROR"
		    (Printf.sprintf "Unknown Assembly type \"%s\" encountered!\n" (String.escaped x))
		in failwith s
	  in
	  let () = (if do_begin then Mpi_petsc.matrix_assembly_begin mx do_final else ()) in
	  let () = (if do_end then Mpi_petsc.matrix_assembly_end mx do_final else ()) in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"VEC-ASSEMBLY"
      ~args:[|":VEC";":WHAT";":PVEC?"|]
      (fun ~report_error () ->
	fun args ->
	  let drh_vec =
	    oa_resource ~report_error ~opcode_name:"VEC-ASSEMBLY" ~param_name:":VEC" args.(0)
	  in
	  let what =
	    oa_string ~report_error ~opcode_name:"VEC-ASSEMBLY" ~param_name:":WHAT" args.(1)
	  in
	  let do_pvec =
	    oa_bool ~report_error ~opcode_name:"VEC-ASSEMBLY" ~param_name:":PVEC?" args.(2)
	  in
	  let vec = 
	    (if do_pvec
	     then get_pvec ddpla drh_vec
	     else get_msvec ddpla drh_vec)
	  in
	  let (do_begin,do_end) =
	    match what with
	    | "BEGIN" -> (true,false)
	    | "END" -> (false,true)
	    | "ALL" -> (true,true)
	    | x -> 
		let s = report_error ~level:"ERROR"
		    (Printf.sprintf "Unknown Assembly type \"%s\" encountered!\n" (String.escaped x))
		in failwith s
	  in
	  let () = (if do_begin then Mpi_petsc.vector_assembly_begin vec else ()) in
	  let () = (if do_end then Mpi_petsc.vector_assembly_end vec else ()) in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-SCALE"
      ~args:[|":MATRIX";":SCALE";|]
      (fun ~report_error () ->
	fun args ->
	  let drh_mx =
	    oa_resource ~report_error ~opcode_name:"MAT-SCALE" ~param_name:":MATRIX" args.(0)
	  in
	  let scale =
	    oa_float ~report_error ~opcode_name:"MAT-SCALE" ~param_name:":SCALE" args.(1)
	  in
	  let mx = get_matrix ddpla drh_mx in
	  let () = Mpi_petsc.matrix_scale mx scale in
	  ());
    (* ----------------------- *)
    _def_opcode
      ~ddpla
      ~opcode:"MAT-ADD-IDENTITY"
      ~args:[|":MATRIX";":ID-COEFF";|]
      (fun ~report_error () ->
	fun args ->
	  let drh_mx =
	    oa_resource ~report_error ~opcode_name:"MAT-ADD-IDENTITY" ~param_name:":MATRIX" args.(0)
	  in
	  let coeff =
	    oa_float ~report_error ~opcode_name:"MAT-ADD-IDENTITY" ~param_name:":ID-COEFF" args.(1)
	  in
	  let mx = get_matrix ddpla drh_mx in
	  let () = Mpi_petsc.matrix_add_identity mx coeff in
	  ());
    ()
  end
;;

let ddpla_create
    ?(fun_error=default_ddpla_error)
    ?(name="the_ddpla")
    ~parent_communicator
    ~fun_quit () =
  let ddpla = 
    ddpla_create_skeleton (* Internal use only *)
      ~fun_error
      ~name
      ~parent_communicator
      ~fun_quit ()
  in
  let () = ddpla_add_core_opcodes ~fun_error ddpla in
  ddpla
;;

let ddpla_names_of_unsnatched_resources ddpla = 
  List.map
    (fun (DRH (name,_)) -> name)
    ddpla.ddpla_new_resources_on_master
;;

let ddpla_snatch_new ddpla name =
  let new_resources = ddpla.ddpla_new_resources_on_master in
  try
    let drh_elem = List.find (fun drh -> drh_name drh = name) new_resources in
    let rest = List.filter (fun drh -> drh != drh_elem) new_resources in
    let () = ddpla.ddpla_new_resources_on_master <- rest in
    drh_elem
  with | Not_found ->
    failwith
      (Printf.sprintf "Resource \"%s\" does not exist in ddpla \"%s\"!"
	 name ddpla.ddpla_name)
;;

let ddpla_register_pill ddpla name =
  let () = 
    (try
      let _ = Hashtbl.find ddpla.ddpla_pill_types name
      in failwith (Printf.sprintf "ddpla_register_pill: pill already exists: '%s'" name)
    with | Not_found -> ())
  in
  let fun_register_pill ddpla args =
    let fun_to_pill x = 
      let x:ddpla_magictype = x in
      let p:ddpla_pill = (name,Obj.magic x) in
      p
    in
    let fun_from_pill (pt, pp) =
      if pt <> name
      then
	failwith
	  (Printf.sprintf "Linear Algebra type mismatch: '%s' <> '%s'"
	     pt name)
      else
	let x:ddpla_magictype = Obj.magic pp in
	x
    in
    Hashtbl.add ddpla.ddpla_pill_types name (fun_to_pill, fun_from_pill)
  in
  begin
    Queue.push
      [|
	{
	 dc_opcode="**ADMIN-FUNCTION**";
	 dc_args=[|(":FUN",OA_adminfunction fun_register_pill);
		   (":ARGS",OA_array [||])|]
       }
      |]
      ddpla.ddpla_queue;
    master_process_queue ddpla;
  end
;;
  

let ddpla_exec ddpla dscript =
  begin
    Queue.push dscript ddpla.ddpla_queue;
    master_process_queue ddpla;
  end
;;

let ddpla_hello ddpla =
  ddpla_exec ddpla [|{dc_opcode="HELLO";dc_args=[||]}|]
;;

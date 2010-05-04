(* (C) 2007 Dr. Thomas Fischbacher

"Centrally Coordinated Parallel Linear Algebra"

   The typical structure of a MPI-parallel simulation program is such
   that the user has to keep in mind the individual "point of execution"
   for every MPI process. We do something different: all processes except
   #0 (the master) enter a "slave loop", where they are being fed with
   parallel commands to be executed. So, users will only have to worry
   about one single point of execution (= on the master). This is much more
   natural, in particular in conjunction with interactive scripting.

   ocamlc -I ../snippets/ -I ../mpi_petsc -i ccpla.ml

   make mrproper libuninstall libinstall; cd ../ccpla/;make mrproper top

   IMPORTANT NOTE: the system works in such a way that linalg commands
   are ml-serialized and then sent out to the slave machines (at
   present, in a somewhat tedious, improvable way), but commands on
   the master machine are executed directly. This means that it
   becomes possible to put a closure into the master slot of a
   parallel command, which allows us to affect some other
   computational state on the master. This is relevant e.g. for the
   implementation of timer accumulators.
   Actually, to make this more explicit, we include a special opcode:
   DCOM_master.

*)

(* pla = parallelized linear algebra *)

(* Note: we must work with a parallel-linalg-command-queue buffer.

   Command sequences are entered into this queue buffer and issued ONLY from there.

   Rationale: suppose master is working on execution of a sequence,
   does a GC, finds a dead DRH, and post-GC wants to inject a resource
   cleanup sequence. On master (direct execution), this will be
   spliced into the linalg sequence being worked on, but not on slaves!

   Note: this certainly needs way more thought! For now, slaves do not
   employ a queue, as they do not have to deal with similar
   out-of-band async issues(?)

   Note: we also get async issues with slave->master back-communication.
   I suppose we should just use a different communicator for that then(?)

   Note: the design decision to use a global reference to store the pla_context
   turns out to be unreasonable. Instead, it would be way better design to turn
   the pla_context (which we have to pass around anyway) into an "object", which
   provides obj.pla_vector_create() and such... (Presumably that is also more
   reasonable than going for a "module" approach.

   Also note: the issue of coupling parallelized linear algebra to
   "single-cpu" non-distributed vectors is a bit tricky. Perhaps the
   most reasonable way forward here is to demand that every
   user-visible pla_* operation that will execute commands has to
   provide an array of master-localized nonparallel petsc vectors, so
   that vector_distribute etc. can use simple number indices to refer
   to that array...
*)

open Snippets;;

let debug_mpi_buffer_lengths = false;;

(* Here, we can hook our own logging function into CCPLA at run time... *)
let rccpla_logdebug =
  ref (fun str -> ())
;;

let vec_dummy = Mpi_petsc.vector_dummy ();;

exception Ccpla_DCOM_rec_quit;;

type ccpla_coefficient =
  | CCCOEFF_var of string (* intensive parameter *)
  | CCCOEFF_float of float
;;

type distributed_resource_handle = DRH of string;;
type drh = distributed_resource_handle;; (* only here, for abbreviation *)

(* Note ad DRHs: Distributed commands will contain DRHs to refer to
   entities, so that a registered distributed script on the master will
   hold on to the relevant objects (matrices, vectors) through the DRHs
   in the script. When this is transferred to the slaves, we make use
   of the property that finalizers are not serialized - so on the slaves,
   we do not encounter any DRHs with finalizer magic associated to them...
*)

type ('opcode_type,'opdata_type) distributed_resource =
      DRES_petsc_vector of
	string * (  Nsimconf.c_int_bigarray1 (* lengths *)
		  * Nsimconf.c_int_bigarray1 (* offsets *))
	* Mpi_petsc.vector
	(* arrays tell us node-local sizes and offsets,
	   which is essential to know for scattering/gathering data!
	*)
    | DRES_petsc_vector_multiseq of string * Mpi_petsc.vector
	(* A "multi-sequential" vector is a vector where every node has a full copy
	   of all the data. We employ these e.g. for rebuilding the Jacobi matrices,
	   and later on as well for operator middle fields.
	*)
    (* For now, this is just an add-on idea, perhaps not a good one...
       More likely, the way to go will be to supersede ccpla with ddpla...
    | DRES_petsc_vector_heterotic of
	string * (  Nsimconf.c_int_bigarray1 (* lengths *)
		  * Nsimconf.c_int_bigarray1 (* offsets *)
		   ) *
	  Mpi_petsc.vector * Mpi_petsc.vector
	  (* Parallel plus multisequential vector,
	     parallel goes first.
	   *)
     *)
    | DRES_petsc_matrix of string * (Mpi_petsc.matrix option ref)
    | DRES_petsc_ksp of string * Mpi_petsc.ksp
    | DRES_petsc_matnullspace of string * Mpi_petsc.matnullspace
    | DRES_parameters of (string array) * (float array)
    | DRES_sequence of string * ('opcode_type,'opdata_type) distributed_command array
    | DRES_opdata of ('opdata_type * (unit -> unit)) (* the unit->unit part is a cleanup function. *)
and ('opcode_type,'opdata_type) distributed_command =
    | DCOM_getfrom_master of int * drh (* nr of master vector, local name *)
    | DCOM_sendto_master of int * drh  (* nr of master vector, local name *)
	(* NOTE: the two opcodes above have the problem that they have to refer to
	   the master vector through an implicit index number.
	   This may be awkward in some situations.

	   So, we try a new design, which may eventually supersede the old one:
	   (At present, we employ it for setting up the Jacobian.)

	   Note that these need special treatment in master_process_queue!
	*)
    | DCOM_getfrom_master_v of (Mpi_petsc.vector option) * drh
    | DCOM_sendto_master_v of (Mpi_petsc.vector option) * drh
    | DCOM_printvec of drh
    | DCOM_printmat of drh
	(* Here, target always goes last, as in assembly language mnemonics: *)
    | DCOM_vec_scale of (drh*float)
    | DCOM_vec_pointwise of (drh*(int -> float -> float))
    | DCOM_push of int array * drh * drh
    | DCOM_pull of int array * drh * drh
    | DCOM_mx_x_vec of drh * drh * drh
    | DCOM_vec_sety_axpby of ccpla_coefficient * ccpla_coefficient * drh * drh * drh
    | DCOM_vec_pointwise_mult of drh * drh * drh
    | DCOM_vec_pointwise_divide of drh * drh * drh
    (* --- *)
    | DCOM_NOP	(* We sometimes need to put in a no-operation,
		   e.g. in client programs where something special happens
		   at this place on the master node. *)
    | DCOM_QUIT
    | DCOM_HELLO
    | DCOM_exec of (unit -> unit)
    | DCOM_master of (unit -> unit)
    (* DCOM_rec_master is similar to DCOM_master, but it allows recursion,
       meaning that the function which is called on the master can push
       further parallel commands on the queue and execute them, even if the
       current queue has not been completely processed.
       DCOM_master has not such an ability, since the slave nodes are not
       blocked and are free to proceed with the commands following
       DCOM_master, leading to MPI deadlocks.
     *)
    | DCOM_rec_master of (unit -> unit)
    | DCOM_rec_quit
    | DCOM_destroy_resources of string list
    | DCOM_register_sequence of string * ('opcode_type,'opdata_type) distributed_command array
    | DCOM_execute_sequence of drh
    | DCOM_register_parameters of string * (string array) * (float array)
    | DCOM_manipulate_parameters of drh * (string array -> float array -> unit)
    (* *)
    | DCOM_vec_create of string * int array (* name, local_sizes *)
    | DCOM_vec_create_multiseq of string * int (* name, size *)
    | DCOM_mat_create of string * string * int array * int array (* name, type, local_sizes_left, local_sizes_right *)
    | DCOM_mat_duplicate of drh * string * bool (* src, name new, copy_values *)
    | DCOM_mat_copy of drh * drh * bool (* src, dst, same_nonzero_pattern *)
    | DCOM_mat_zero of drh
    | DCOM_mat_flush_ntimes of drh * int (* src, number of flushes *)
    | DCOM_vec_assembly_begin of drh
    | DCOM_vec_assembly_end of drh
    | DCOM_mat_assembly_begin of drh * bool
    | DCOM_mat_assembly_end of drh * bool
    | DCOM_mat_scale_and_add_identity of drh * float * float
    | DCOM_ksp_create of string * Mpi_petsc.matstructure_type * drh * drh
    | DCOM_matnullspace_create of string * bool * (drh array)
    | DCOM_ksp_set_initial_guess_nonzero of drh * bool
    | DCOM_ksp_set_matnullspace of drh * drh
    | DCOM_ksp_set_type of drh * string
    | DCOM_ksp_set_pc_type of drh * string
    | DCOM_ksp_set_pc_bjacobi_subtype of drh * string
    | DCOM_ksp_set_tolerances_opt of drh * float option * float option * float option * int option
    | DCOM_ksp_manipulate_tolerances of drh * ( (float*float*float*int)-> (float*float*float*int) )
    | DCOM_ksp_set_operators of (drh * drh * drh)
    | DCOM_ksp_set_up of drh
    | DCOM_ksp_solve of drh * drh * drh
	(* -- specific opcs (e.g. opcode_type = magnetism_operations -- *)
    | DCOM_opcode of 'opcode_type * (drh array)
;;

let opcode_tostring opcode =
  match opcode with
    | DCOM_getfrom_master _ -> "DCOM_getfrom_master"
    | DCOM_sendto_master _ -> "DCOM_sendto_master"
    | DCOM_getfrom_master_v _ -> "DCOM_getfrom_master_v"
    | DCOM_sendto_master_v _ -> "DCOM_sendto_master_v"
    | DCOM_printvec _ -> "DCOM_printvec"
    | DCOM_printmat _ -> "DCOM_printmat"
    | DCOM_vec_scale _ -> "DCOM_vec_scale"
    | DCOM_vec_pointwise _ -> "DCOM_vec_pointwise"
    | DCOM_push _ -> "DCOM_push"
    | DCOM_pull _ -> "DCOM_pull"
    | DCOM_mx_x_vec _ -> "DCOM_mx_x_vec"
    | DCOM_vec_sety_axpby _ -> "DCOM_vec_sety_axpby"
    | DCOM_vec_pointwise_mult _ -> "DCOM_vec_pointwise_mult"
    | DCOM_vec_pointwise_divide _ -> "DCOM_vec_pointwise_divide"
    | DCOM_NOP -> "DCOM_NOP"
    | DCOM_QUIT -> "DCOM_QUIT"
    | DCOM_HELLO -> "DCOM_HELLO"
    | DCOM_exec _ -> "DCOM_exec"
    | DCOM_master _ -> "DCOM_master"
    | DCOM_rec_master _ -> "DCOM_rec_master"
    | DCOM_rec_quit -> "DCOM_rec_quit"
    | DCOM_destroy_resources _ -> "DCOM_destroy_sequence"
    | DCOM_register_sequence _ -> "DCOM_register_sequence"
    | DCOM_execute_sequence _ -> "DCOM_execute_sequence"
    | DCOM_register_parameters _ -> "DCOM_register_sequence"
    | DCOM_manipulate_parameters _ -> "DCOM_manipulate_parameters"
    | DCOM_vec_create _ -> "DCOM_vec_create"
    | DCOM_vec_create_multiseq _ -> "DCOM_vec_create_multiseq"
    | DCOM_mat_create _ -> "DCOM_mat_create"
    | DCOM_mat_duplicate _ -> "DCOM_mat_duplicate"
    | DCOM_mat_copy _ -> "DCOM_mat_copy"
    | DCOM_mat_zero _ -> "DCOM_mat_zero"
    | DCOM_mat_flush_ntimes _ -> "DCOM_mat_flush_ntimes"
    | DCOM_vec_assembly_begin _ -> "DCOM_vec_assembly_begin"
    | DCOM_vec_assembly_end _ -> "DCOM_vec_assembly_end"
    | DCOM_mat_assembly_begin _ -> "DCOM_mat_assembly_begin"
    | DCOM_mat_assembly_end _ -> "DCOM_mat_assembly_end"
    | DCOM_mat_scale_and_add_identity _ -> "DCOM_mat_scale_and_add_identity"
    | DCOM_ksp_create _ -> "DCOM_ksp_create"
    | DCOM_matnullspace_create _ -> "DCOM_matnullspace_create"
    | DCOM_ksp_set_initial_guess_nonzero _ -> "DCOM_ksp_set_initial_guess_nonzero"
    | DCOM_ksp_set_matnullspace _ -> "DCOM_ksp_set_matnullspace"
    | DCOM_ksp_set_type _ -> "DCOM_ksp_set_type"
    | DCOM_ksp_set_pc_type _ -> "DCOM_ksp_set_pc_type"
    | DCOM_ksp_set_pc_bjacobi_subtype _ -> "DCOM_ksp_set_pc_bjacobi_subtype"
    | DCOM_ksp_set_tolerances_opt _ -> "DCOM_ksp_set_tolerances_opt"
    | DCOM_ksp_manipulate_tolerances _ -> "DCOM_ksp_manipulate_tolerances"
    | DCOM_ksp_set_operators _ -> "DCOM_ksp_set_operators"
    | DCOM_ksp_set_up _ -> "DCOM_ksp_set_up"
    | DCOM_ksp_solve _ -> "DCOM_ksp_solve"
    | DCOM_opcode _ -> "DCOM_opcode"
    | _ -> "UNKNOWN"
;;

let dres_type res =
  match res with
    | DRES_petsc_vector (_,_,_) -> "petsc_vector"
    | DRES_petsc_vector_multiseq (_,_) -> "petsc_vector_multiseq"
    | DRES_petsc_matrix (_,_) -> "petsc_matrix"
    | DRES_petsc_ksp (_,_) -> "petsc_ksp"
    | DRES_petsc_matnullspace (_,_) -> "petsc_matnullspace"
    | DRES_parameters (_,_) -> "parameters"
    | DRES_sequence (_,_) -> "sequence"
    | DRES_opdata _ -> "opdata"
    | _ -> failwith "unknown distributed resource object"
;;

let dres_get_vector res =
  match res with
    | DRES_petsc_vector (_,_,v) -> v
    | other_res ->
	let failstring = "Expected a petsc_vector, got a "^(dres_type other_res) in
	failwith failstring
;;

let dres_get_vector_lengths_offsets res =
  match res with
    | DRES_petsc_vector (_,lens_offsets,_) -> lens_offsets
    | other_res ->
	let failstring = "Expected a petsc_vector, got a "^(dres_type other_res) in
	failwith failstring
;;


let dres_get_vector_multiseq res =
  match res with
    | DRES_petsc_vector_multiseq (_,vms) -> vms
    | other_res ->
	let failstring = "Expected a petsc_vector_multiseq, got a "^(dres_type other_res) in
	failwith failstring
;;

let dres_get_matrix res =
  match res with
    | DRES_petsc_matrix (_,r_o_mat) ->
	(match !r_o_mat with
	  | None -> failwith "Vacuous DRES_petsc_matrix encountered!"
	  | Some mat -> mat)
    | other_res ->
	let failstring = "Expected a petsc_matrix, got a "^(dres_type other_res) in
	  failwith failstring
;;

let dres_get_ksp res =
  match res with
    | DRES_petsc_ksp (_,ksp) -> ksp
    | other_res ->
	let failstring = "Expected a petsc_ksp, got a "^(dres_type other_res) in
	failwith failstring
;;

let dres_get_matnullspace res =
  match res with
    | DRES_petsc_matnullspace (_,mns) -> mns
    | other_res ->
	let failstring = "Expected a petsc_matnullspace, got a "^(dres_type other_res) in
	failwith failstring
;;

let dres_get_parameters res =
  match res with
    | DRES_parameters (_,prs) -> prs
    | other_res ->
	let failstring = "Expected parameters, got a "^(dres_type other_res) in
	failwith failstring
;;

let dres_get_sequence res =
  match res with
    | DRES_sequence (_,seq) -> seq
    | other_res ->
	let failstring = "Expected a sequence, got a "^(dres_type other_res) in
	failwith failstring
;;

let dres_get_opdata res =
  match res with
    | DRES_opdata (od,_) -> od
    | other_res ->
	let failstring = "Expected an opdata, got a "^(dres_type other_res) in
	failwith failstring
;;

(* Note that the master node will use the same mechanisms
   (i.e. dcom_execute) as the slave nodes to execute a
   distributed_command sequence: while the master has to do some extra
   orchestration, for the task of actually performing a parallel
   computation it behaves in just the same way as every other (slave)
   node.

   So, on the master node we have to discern between arbitration and
   computation roles. For arbitration, the master will refer to
   parallel resources via handles - which basically are names that
   also have a finalizer registered that will take care of proper
   cleanup across all nodes - master and slave.
*)

type ('opcode_type,'opdata_type) ccpla =
    {
      ccpla_comm: Mpi_petsc.communicator;
      mutable ccpla_opcode_interpreter: 'opcode_type -> ('opcode_type,'opdata_type) distributed_resource array -> (string * ('opcode_type,'opdata_type) distributed_resource) array;
      ccpla_resources: (string, ('opcode_type,'opdata_type) distributed_resource) Hashtbl.t;
      ccpla_queue:
        ('opcode_type,'opdata_type) distributed_command array Queue.t ref;
      mutable ccpla_pending_finalizations: string list;
      ccpla_quit: unit -> unit;
      (* --- *)
      mutable ccpla_register_params:
	string -> string array -> float array -> distributed_resource_handle;
      mutable ccpla_manipulate_params:
	drh -> (string array -> float array -> unit) -> unit;
      mutable ccpla_sequence_create:
	string -> ('opcode_type,'opdata_type) distributed_command array array -> distributed_resource_handle;
      mutable ccpla_sequence_execute:
	?local_vectors:Mpi_petsc.vector array -> distributed_resource_handle -> unit;
      mutable ccpla_vector_create:
	string -> int array -> distributed_resource_handle;
      mutable ccpla_vector_create_multiseq:
	string -> int -> distributed_resource_handle;
      mutable ccpla_vector_distribute:
	?local_vectors:Mpi_petsc.vector array -> int -> distributed_resource_handle -> unit;
      mutable ccpla_vector_collect :
	?local_vectors:Mpi_petsc.vector array -> int -> distributed_resource_handle -> unit;
      mutable ccpla_matrix_create:
	string -> string -> int array -> int array -> distributed_resource_handle;
      mutable ccpla_matrix_duplicate:
	drh -> string -> bool -> distributed_resource_handle;
      mutable ccpla_matrix_copy:
	drh -> drh -> bool -> unit;
      mutable ccpla_matrix_scale_and_add_identity:
	drh -> float -> float -> unit;
      mutable ccpla_matrix_times_vector:
	drh -> drh -> drh -> unit;
      mutable ccpla_ksp_solve:
	drh -> drh -> drh -> unit;
      mutable ccpla_matrix_assemble :
	?final:bool -> distributed_resource_handle -> unit;
      mutable ccpla_vector_assemble :
	distributed_resource_handle -> unit;
      mutable ccpla_petsc_vector :
	distributed_resource_handle -> Mpi_petsc.vector;
      mutable ccpla_petsc_matrix :
	distributed_resource_handle -> Mpi_petsc.matrix;
      mutable ccpla_petsc_ksp :
	distributed_resource_handle -> Mpi_petsc.ksp;
      mutable ccpla_iparams: distributed_resource_handle -> (string array * float array);
      mutable ccpla_petsc_multiseq_vector :
	distributed_resource_handle -> Mpi_petsc.vector;
      mutable ccpla_ksp_create:
	string -> ?matrix_structure:Mpi_petsc.matstructure_type ->
	 ?tolerances:(float option*float option*float option*int option) -> ?initial_guess_nonzero:bool ->
	 ?ksp_type:string -> ?pc_type:string -> ?pc_subtype:string ->
	 ?matnullspace:(bool * drh array) ->
	  distributed_resource_handle -> distributed_resource_handle -> distributed_resource_handle;
      mutable ccpla_ksp_set_up:
	drh -> unit;
      mutable ccpla_ksp_set_operators:
	?do_register_new_finalizer:bool -> drh -> drh -> drh -> unit;
      mutable ccpla_ksp_manipulate_tolerances:
	drh -> ( (float*float*float*int)-> (float*float*float*int) ) -> unit;

    }
;;

let mpi_get_distributed_command ccpla =
  let myrank = Mpi_petsc.comm_rank ccpla.ccpla_comm in
  let (len,recv) =
    let () =
      (if debug_mpi_buffer_lengths
       then Printf.fprintf stderr "[Node=%d] CCPLA getting command next!\n%!" myrank
       else ())
    in
    try Mpi_petsc.receive_reporting_length 0 Mpi_petsc.any_tag ccpla.ccpla_comm
      (* Note: 0 = source; this will use Mpi_petsc.receive_basic(), using probe() to determine lengths. *)
    with
      | _ ->
	  let () = Printf.printf "XXX PROBLEM: Mpi_petsc.receive() failed - issuing DCOM_QUIT!\n%!" in
	    (0,DCOM_QUIT)
  in
  let () =
    (if debug_mpi_buffer_lengths
     then Printf.fprintf stderr "[Node=%d] CCPLA got command, len=%d\n%!" myrank len
     else ())
  in
    recv;;

let ccpla_get_resource ccpla r =
  try
    Hashtbl.find ccpla.ccpla_resources r
  with
    | Not_found ->
	let () = Printf.fprintf stderr "Unknown resource: '%s'\n%!" r in
	let r_names = ref [] in
	let () =
	  Hashtbl.iter (fun key _ -> r_names := key :: !r_names) ccpla.ccpla_resources
	in
	let sorted_names = List.sort compare !r_names in
	let () = Printf.fprintf stderr "Registered parallel resources:\n=======>\n" in
	let () = List.iter (fun n -> Printf.fprintf stderr " '%s'\n" n) sorted_names in
	let () = Printf.fprintf stderr "<=======\n%!" in
	  raise Not_found
;;

let ccpla_registered_resources ccpla =
  hashtbl_keys ~sorter:compare ccpla.ccpla_resources
;;

let rec dcom_execute ?(local_vectors=[||]) ccpla dcom =
  let comm = ccpla.ccpla_comm in
  let () = dcom_execute_1 ~local_vectors ccpla dcom in
    (*
      let () = Mpi_petsc.barrier comm in
      let () = Gc.minor() in
      let () = Gc.major() in
    *)
    ()
and dcom_execute_1 ?(local_vectors=[||]) ccpla dcom =
  let comm = ccpla.ccpla_comm in
  let opcode_interpreter = ccpla.ccpla_opcode_interpreter in
  let nr_nodes = Mpi_petsc.comm_size comm in
  let myrank = Mpi_petsc.comm_rank comm in
  let get_resource r = ccpla_get_resource ccpla r in
  let list_foreach_do li f = List.iter f li in
  (* let () = Printf.printf "[Node %d]DDD dcom_execute\n%!" myrank in *)
  try
    match dcom with
      | DCOM_NOP -> ()
      | DCOM_QUIT ->
	  (*let () = Printf.printf "[Node %d] QUIT (CMD DCOM_QUIT)\n%!" myrank in*)
	  let () = Mpi_petsc.petsc_finalize () in
	  let () = Mpi_petsc.mpi_finalize () in
	  (* let () = Printf.printf "[Node %d]DDD *** QUIT ***\n%!" myrank  in  *)
	    ccpla.ccpla_quit()
      | DCOM_HELLO -> Printf.printf "[Node %3d/%d] Hello from %s\n%!" myrank nr_nodes (Unix.gethostname ());
      | DCOM_exec f ->
	  f ()
      | DCOM_master f ->
          f ()
      | DCOM_rec_master f ->
          if myrank == 0
          then (* on the master *)
            let saved_queue = !(ccpla.ccpla_queue) in
            begin
              ccpla.ccpla_queue := Queue.create ();
              f ();
              (* tell the slave nodes to quit recursion *)
              for i = 1 to nr_nodes-1 do
                ignore(Mpi_petsc.send_reporting_length DCOM_rec_quit i 0 comm)
              done;
              ccpla.ccpla_queue := saved_queue;
            end
          else (* on the slaves *)
            (try ignore(slave_mode ccpla)
             with Ccpla_DCOM_rec_quit -> ())
      | DCOM_rec_quit ->
          if myrank == 0
          then ()
          else
            raise Ccpla_DCOM_rec_quit
            (* ^^^ This seems a good way to exit the recursion, since it will
                   cause the program termination if the program wasn't trying
                   to catch such an exception (which will be the case unless
                   we are in a DCOM_rec_master command). *)
      | DCOM_destroy_resources names ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD DCOM_destroy_resources %s\n" myrank (string_array_to_string (Array.of_list names))) in
	  list_foreach_do names
	    (fun name ->
	       let () = !rccpla_logdebug (Printf.sprintf "[Node %d]Now Destroying '%s'\n" myrank name) in
	       try
		 let res = get_resource name in
		 let () =
		   match res with
		     | DRES_sequence (_,_) -> ()
		     | DRES_petsc_vector (_,_,v) -> Mpi_petsc.vec_destroy v
			 (* Note that we actively destroy the vector here.
			    We have to, as this is collective.
			 *)
		     | DRES_petsc_vector_multiseq (_,v) -> Mpi_petsc.vec_destroy v
		     | DRES_petsc_matrix (_,r_om) ->
			 (match !r_om with | None -> () | Some m -> Mpi_petsc.mat_destroy m)
		     | DRES_petsc_ksp (_,ksp) -> Mpi_petsc.ksp_destroy ksp
		     | DRES_petsc_matnullspace (_,mns) -> Mpi_petsc.matnullspace_destroy mns
		     | DRES_opdata (_,f) -> f ()
			 (* opdata may bring along an own finalizer. *)
			 (* Other resources (such as DRES_parameters) do not have to be destroyed actively *)
		     | _ -> ()
		 in
		   Hashtbl.remove ccpla.ccpla_resources name
	       with (* Destroying something nonexistent should not be considered a problem.
		       During development, we nevertheless warn about it.
		    *)
		 | Not_found ->
		     let () = !rccpla_logdebug (Printf.sprintf "Note: Destroying non-existent resource '%s'\n" name) in
		       ()
	    )
      | DCOM_execute_sequence (DRH name) ->
	  (* let () = Printf.printf "[Node %d]CMD DCOM_execute_sequence\n%!" myrank in *)
	    (match get_resource name with
	       | DRES_sequence (_,seq) ->  Array.iter (dcom_execute ~local_vectors ccpla) seq
	       | _ -> impossible())
      | DCOM_register_sequence (name,commands) ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_register_sequence '%s'\n" myrank name) in
	    Hashtbl.replace ccpla.ccpla_resources name (DRES_sequence (name,commands))
      | DCOM_register_parameters (name,param_names,param_values) ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_register_parameters\n" myrank) in
	  let () = Hashtbl.replace ccpla.ccpla_resources name (DRES_parameters (param_names,param_values)) in
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_register_parameters DONE\n" myrank) in
	    ()
      | DCOM_manipulate_parameters (DRH name, f) ->
	  let DRES_parameters (names,values) = get_resource name in
	    f names values
      | DCOM_vec_create (name,local_sizes) ->
	  let global_size = Array.fold_left (+) 0 local_sizes in
	  let ba_local_sizes = Nsimconf.c_int_bigarray1_create nr_nodes in
	  let ba_offsets = Nsimconf.c_int_bigarray1_create nr_nodes
          in
	  let () =
	    for i=0 to nr_nodes-1 do
	      ba_offsets.{i} <- Nsimconf.c_int_of_int 0;
	      ba_local_sizes.{i} <- Nsimconf.c_int_of_int (local_sizes.(i));
	    done
	  in
	  let () =
	    for i=1 to nr_nodes-1 do
	      ba_offsets.{i} <- Nsimconf.c_int_add ba_offsets.{i-1}
                                                   ba_local_sizes.{i-1};
	    done
	  in
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD DCOM_vec_create(name=%s,gsize=%d,lsizes=%s)\n" myrank name global_size (int_array_to_string local_sizes)) in
	  let my_len = local_sizes.(myrank) in
	  let vec = Mpi_petsc.vector_create_mpi global_size local_sizes.(myrank) name in
	    (* XXX There is an issue with communicators here. Actually/presumably,
	       we should rather have one petsc-related subcommunicator of PETSC_COMM_WORLD
	       for our program, and another subcommunicator of that (or of MPI_COMM_WORLD)
	       for distributing our commands...(?)
	       XXX In any case, we should have a communicator argument to vector_create_mpi!
	    *)
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD vector_create_mpi succeeded!\n" myrank) in
	  let () =
	    Hashtbl.add
	      ccpla.ccpla_resources name
	      (DRES_petsc_vector (name,(ba_local_sizes,ba_offsets),vec))
	  in
	    ()
      | DCOM_vec_create_multiseq (name,size) ->
	  let vec = Mpi_petsc.vector_create size name in
	  let () =
	    Hashtbl.add
	      ccpla.ccpla_resources name
	      (DRES_petsc_vector_multiseq (name,vec))
	  in
	    ()
      | DCOM_mat_create (name,mtype,local_sizes_left,local_sizes_right) ->
	  let global_size_left = Array.fold_left (+) 0 local_sizes_left in
	  let global_size_right = Array.fold_left (+) 0 local_sizes_right in
	  let (prealloc_diagonal,prealloc_off_diagonal)=
	    match mtype with
	      | "seqdense" -> (None,0)
	      | "mpidense" -> (None,0)
	      | _ -> (Some 75,45)
	  in
	  let mat =
	    Mpi_petsc.matrix_create
	      ~communicator:(Mpi_petsc.petsc_get_comm_world())
	      ~local_rows:local_sizes_left.(myrank)
	      ~local_cols:local_sizes_right.(myrank)
	      ~matrix_type:mtype
	      (* ~auto_assembly:false *) ~auto_assembly:false
	      ?prealloc_diagonal
	      ~prealloc_off_diagonal
	      global_size_left global_size_right name
	  in
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD matrix_create succeeded!\n" myrank) in
	  let () =
	    Hashtbl.add
	      ccpla.ccpla_resources name
	      (DRES_petsc_matrix (name,ref (Some mat)))
	  in
	    ()
      | DCOM_mat_duplicate (DRH name_src,name_new,copy_values) ->
	  let src = get_resource name_src in
	    (match src with
	       | DRES_petsc_matrix (name,r_o_mx_src) ->
		   let Some mx_src = !r_o_mx_src in
		   let mx_new = Mpi_petsc.matrix_duplicate copy_values mx_src in
		   let () =
		     Hashtbl.add
		       ccpla.ccpla_resources name_new
		       (DRES_petsc_matrix (name_new,ref (Some mx_new)))
		   in
		     ()
	       | _ -> failwith "DCOM_mat_duplicate: non-matrix distributed resource!")
      | DCOM_mat_copy (DRH name_src,DRH name_dst,same_nonzero_pattern) ->
	  let src = get_resource name_src
	  and dst = get_resource name_dst
	  in
	    (match (src,dst) with
	       | (DRES_petsc_matrix (_,r_o_mx_src),
		  DRES_petsc_matrix (_,r_o_mx_dst))
		 ->
		   let Some mx_src = !r_o_mx_src in
		   let Some mx_dst = !r_o_mx_dst in
		   let () = Mpi_petsc.matrix_copy same_nonzero_pattern mx_src mx_dst in
		     ()
	       | _ -> failwith "DCOM_mat_copy: non-matrix distributed resource!")
      | DCOM_mat_zero (DRH name_mat) ->
	  let mat = dres_get_matrix (get_resource name_mat) in
	    Mpi_petsc.matrix_zero_entries mat
      | DCOM_mat_flush_ntimes (DRH name_mat,nr_flushes) ->
	  let mat = dres_get_matrix (get_resource name_mat) in
	  for i = 0 to nr_flushes-1 do
	    Mpi_petsc.matrix_assemble mat false;
	  done
      | DCOM_vec_assembly_begin (DRH name) ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_vec_assembly_begin\n" myrank) in
	  let res = get_resource name in
	    (match res with
	       | DRES_petsc_vector (_,_,v) ->
		   Mpi_petsc.vector_assembly_begin v
	       | DRES_petsc_vector_multiseq (_,v) ->
		   Mpi_petsc.vector_assembly_begin v
	       | _ -> failwith "DCOM_vec_assembly_begin: non-vector resource!")
      | DCOM_vec_assembly_end (DRH name) ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_vec_assembly_end\n" myrank) in
	  let res = get_resource name in
	    (match res with
	       | DRES_petsc_vector (_,_,v) ->
		   Mpi_petsc.vector_assembly_end v
	       | DRES_petsc_vector_multiseq (_,v) ->
		   Mpi_petsc.vector_assembly_end v
	       | _ -> failwith "DCOM_vec_assembly_begin: non-vector resource!")
      | DCOM_mat_assembly_begin (DRH name,do_final) ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_mat_assembly_begin\n" myrank) in
	  let m = dres_get_matrix (get_resource name) in
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD matrix_assembly_begin %s\n" myrank name) in
	    Mpi_petsc.matrix_assembly_begin m do_final
      | DCOM_mat_assembly_end (DRH name,do_final) ->
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]CMD DCOM_mat_assembly_end\n" myrank) in
	  let m = dres_get_matrix (get_resource name) in
	  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD matrix_assembly_end %s\n" myrank name) in
	    Mpi_petsc.matrix_assembly_end m do_final
      | DCOM_mat_scale_and_add_identity (DRH name,scale, id_coeff) ->
	  let m = dres_get_matrix (get_resource name) in
	    begin
	      (if scale <> 1.0 then Mpi_petsc.matrix_scale m scale else ());
	      (if id_coeff <> 0.0 then Mpi_petsc.matrix_add_identity m id_coeff else ());
	    end
      | DCOM_ksp_create (name,matrix_structure,DRH name_m1,DRH name_m2) ->
	  let m1 = dres_get_matrix (get_resource name_m1) in
	  let m2 = dres_get_matrix (get_resource name_m2) in
	  let ksp =
	    Mpi_petsc.ksp_create ~communicator:(Mpi_petsc.petsc_get_comm_world())
	      ~name ~matrix_structure m1 m2
	  in
	  let () =
	    Hashtbl.add ccpla.ccpla_resources name
	      (DRES_petsc_ksp (name,ksp))
	  in
	    ()
      | DCOM_matnullspace_create (name,has_constant,drhs_vecs) ->
	  let vecs =
	    Array.map
	      (fun (DRH name) ->
		 let DRES_petsc_vector (_,_,v) = get_resource name
		 in v)
	      drhs_vecs
	  in
	  let mns = Mpi_petsc.matnullspace_create
	    ~communicator:(Mpi_petsc.petsc_get_comm_world())
	    ~name
	    ~has_constant vecs
	  in
	  let () =
	    Hashtbl.add ccpla.ccpla_resources name
	      (DRES_petsc_matnullspace (name,mns))
	  in
	    ()
      | DCOM_ksp_set_initial_guess_nonzero (DRH name,flag) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	    Mpi_petsc.ksp_set_initial_guess_nonzero ksp flag
      | DCOM_ksp_set_matnullspace (DRH ksp_name, DRH mns_name) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource ksp_name in
	  let DRES_petsc_matnullspace (_,mns) = get_resource mns_name in
	    Mpi_petsc.ksp_set_matnullspace ksp mns
      | DCOM_ksp_set_type (DRH name, type_string) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	    Mpi_petsc.ksp_set_type ksp type_string
      | DCOM_ksp_set_pc_type (DRH name, type_string) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	    Mpi_petsc.ksp_set_pc_type ksp type_string
      | DCOM_ksp_set_pc_bjacobi_subtype (DRH name, type_string) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	    Mpi_petsc.ksp_set_pc_bjacobi_sub_type ksp type_string
      | DCOM_ksp_set_tolerances_opt (DRH name,ortol,oatol,odtol,omaxit) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	    Mpi_petsc.ksp_set_tolerances_opt ksp ortol oatol odtol omaxit
      | DCOM_ksp_set_operators (drh_ksp, drh_mx, drh_precond) ->
	  let DRH name = drh_ksp
	  and DRH name_mx = drh_mx
	  and DRH name_precond = drh_precond
	  in
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	  let mx = dres_get_matrix (get_resource name_mx) in
	  let precond = dres_get_matrix (get_resource name_precond) in
	  let () = Mpi_petsc.ksp_set_operators ksp mx precond in
	    ()
      | DCOM_ksp_set_up (DRH name) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	    Mpi_petsc.ksp_set_up ksp
      | DCOM_ksp_solve (DRH name_ksp,DRH name_v_input,DRH name_v_sol) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name_ksp in
	  let DRES_petsc_vector (_,_,v_input) = get_resource name_v_input in
	  let DRES_petsc_vector (_,_,v_sol) = get_resource name_v_sol in
	  let _ = Mpi_petsc.ksp_solve_raw ksp v_sol v_input in
	    ()
      | DCOM_ksp_manipulate_tolerances (DRH name, f) ->
	  let DRES_petsc_ksp (_,ksp) = get_resource name in
	  let tols = Mpi_petsc.ksp_get_tolerances ksp in
	  let (rtol,atol,dtol,maxit) = f tols in
	  let () = Mpi_petsc.ksp_set_tolerances ksp rtol atol dtol maxit in
	    ()
      (* --- *)
      | DCOM_getfrom_master (nr_v_master, DRH name_slave) ->
	  (*let () = Printf.printf "[Node %d]DDD DCOM_getfrom_master nr_v_master=%d (of %d) name_slave='%s'\n%!" myrank nr_v_master (Array.length local_vectors) name_slave in*)
	  let DRES_petsc_vector (_,(ba_local_sizes,ba_offsets),vec) =
	    get_resource name_slave
	  in
	    if myrank > 0
	    then (* we are on a slave node *)
	      Mpi_petsc.vec_scatter comm vec_dummy vec ba_local_sizes ba_offsets
	    else (* master node *)
	      let v_master = local_vectors.(nr_v_master) in
		Mpi_petsc.vec_scatter comm v_master vec ba_local_sizes ba_offsets
      | DCOM_sendto_master (nr_v_master, DRH name_slave) ->
	  (* let () = Printf.printf "[Node %d]DDD DCOM_sendto_master nr_v_master=%d (of %d) name_slave='%s'\n%!" myrank nr_v_master (Array.length local_vectors) name_slave in *)
	  let DRES_petsc_vector (_,(ba_local_sizes,ba_offsets),vec) =
	    get_resource name_slave
	  in
	    if myrank > 0
	    then
	      Mpi_petsc.vec_gather comm vec_dummy vec ba_local_sizes ba_offsets
	    else
	      let v_master = local_vectors.(nr_v_master) in
		Mpi_petsc.vec_gather comm v_master vec ba_local_sizes ba_offsets
      | DCOM_getfrom_master_v (opt_v_master, DRH name_slave) ->
	  (* NOTE: opt_v_master will be None on slave machines, the master's vector on the master *)
          (*let () = Printf.printf "[Node %d]DDD DCOM_getfrom_master_v name_slave='%s'\n%!" myrank name_slave in*)
	  let DRES_petsc_vector (_,(ba_local_sizes,ba_offsets),vec) =
	    get_resource name_slave
	  in
	  let v = match opt_v_master with | None -> vec_dummy | Some x -> x
	  in
	    Mpi_petsc.vec_scatter comm v vec ba_local_sizes ba_offsets
      | DCOM_sendto_master_v (opt_v_master, DRH name_slave) ->
          (*let () = Printf.printf "[Node %d]DDD DCOM_sendto_master_v name_slave='%s'\n%!" myrank name_slave in*)
	  let DRES_petsc_vector (_,(ba_local_sizes,ba_offsets),vec) =
	    get_resource name_slave
	  in
	  let v = match opt_v_master with | None -> vec_dummy | Some x -> x
	  in
	    Mpi_petsc.vec_gather comm v vec ba_local_sizes ba_offsets
      | DCOM_printvec (DRH name) ->
	  let DRES_petsc_vector (_,_,vec) = get_resource name in
	    Mpi_petsc.with_petsc_vector_as_bigarray vec
	      (fun ba ->
		 let d = Bigarray.Array1.dim ba in
		   for i=0 to d-1 do
		     !rccpla_logdebug (Printf.sprintf "VEC '%s'@%d.%d: %f\n" name myrank i ba.{i})
		   done)
      | DCOM_printmat (DRH name) ->
	  let mat = dres_get_matrix (get_resource name) in
	    Mpi_petsc.mat_view mat
      | DCOM_vec_scale (DRH name,s) ->
	  let DRES_petsc_vector (_,_,vec) = get_resource name in
	    if s=0.0 then
	      Mpi_petsc.vector_zero vec
	    else
	      Mpi_petsc.vector_scale vec s
      | DCOM_vec_pointwise (DRH name,f) ->
	  (match get_resource name with
	     | DRES_petsc_vector (_,(lengths,offsets),vec) ->
		 let my_offset = Nsimconf.c_int_to_int offsets.{myrank}
		 and my_len = Nsimconf.c_int_to_int lengths.{myrank} in
		   Mpi_petsc.with_petsc_vector_as_bigarray vec
		     (fun ba ->
			for i=0 to my_len-1 do
			  ba.{i} <- f (my_offset+i) ba.{i};
			done)

	     | DRES_petsc_vector_multiseq (_,vec) ->
		 Mpi_petsc.with_petsc_vector_as_bigarray vec
		   (fun ba ->
		      let dim = Bigarray.Array1.dim ba in
			for i=0 to dim-1 do
			  ba.{i} <- f i ba.{i};
			done)
	     | _ -> failwith "DCOM_vec_pointwise: resource was not a vector!")
      | DCOM_push (locations,DRH name_src,DRH name_target) ->
	  (* Entries of short vector go into long vector *)
	  let DRES_petsc_vector (_,(src_lengths,src_offsets),v_src) =
	    get_resource name_src
	  in
	  let DRES_petsc_vector (_,_,v_target) = get_resource name_target
	  in
	  let my_src_length = Nsimconf.c_int_to_int src_lengths.{myrank} in
	    (* actually not used, as bigarray carries length data! *)
	  let my_src_offset = Nsimconf.c_int_to_int src_offsets.{myrank} in
	  let () = Mpi_petsc.with_petsc_vector_as_bigarray v_src
	    (fun ba_src ->
	       for i=0 to my_src_length-1 do
		 Mpi_petsc.vector_set v_target locations.(my_src_offset+i) ba_src.{i}
	       done;)
	  in
	  let () = Mpi_petsc.vector_assemble v_target in
	    ()
      | DCOM_pull (locations,DRH name_src,DRH name_target) ->
	  (* Entries of long vector go into short vector. The "locations" data structure
	     is precisely the same as for DCOM_push: long_index_by_short_index!
	  *)
	  let DRES_petsc_vector (_,(src_lengths,src_offsets),v_src) =
	    get_resource name_src
	  in
	  let DRES_petsc_vector (_,_,v_target) = get_resource name_target
	  in
	  let my_src_length = Nsimconf.c_int_to_int src_lengths.{myrank} in
	  let my_src_offset = Nsimconf.c_int_to_int src_offsets.{myrank} in
	  let () = Mpi_petsc.with_petsc_vector_as_bigarray v_src
	    (fun ba_src ->
	       for i=0 to Array.length locations-1 do
		 let pos = locations.(i)-my_src_offset in
		   if pos >= 0 && pos < my_src_length
		   then
		     Mpi_petsc.vector_set v_target i ba_src.{pos}
		   else ()
	       done;)
	  in
	  let () = Mpi_petsc.vector_assemble v_target in
	    ()
      | DCOM_mx_x_vec (DRH name_mx,DRH name_src,DRH name_target) ->
	  let mx = dres_get_matrix (get_resource name_mx) in
	  let DRES_petsc_vector (_,_,v_src) = get_resource name_src in
	  let DRES_petsc_vector (_,_,v_target) = get_resource name_target
	  in
	    Mpi_petsc.matrix_times_vector mx v_src v_target
      | DCOM_vec_sety_axpby (ca,cb,DRH name_iparams, DRH name_x,DRH name_y) ->
	  let DRES_parameters (param_names,param_vals) = get_resource name_iparams in
	  let DRES_petsc_vector (_,_,vx) = get_resource name_x in
	  let DRES_petsc_vector (_,_,vy) = get_resource name_y in
	  let resolve c =
	    match c with
	      | CCCOEFF_float x -> x
	      | CCCOEFF_var n ->
		  let p = array_position n param_names 0 in
		    param_vals.(p)
	  in
	    Mpi_petsc.vector_AXPBY (resolve ca) (resolve cb) vx vy
      | DCOM_vec_pointwise_mult (DRH name_x, DRH name_y, DRH name_result) ->
	  let DRES_petsc_vector (_,_,vx) = get_resource name_x in
	  let DRES_petsc_vector (_,_,vy) = get_resource name_y in
	  let DRES_petsc_vector (_,_,v_res) = get_resource name_result in
	  Mpi_petsc.vector_pointwise_mult vx vy v_res
      | DCOM_vec_pointwise_divide (DRH name_x, DRH name_y, DRH name_result) ->
	  let DRES_petsc_vector (_,_,vx) = get_resource name_x in
	  let DRES_petsc_vector (_,_,vy) = get_resource name_y in
	  let DRES_petsc_vector (_,_,v_res) = get_resource name_result in
	  Mpi_petsc.vector_pointwise_divide vx vy v_res
      | DCOM_opcode (op,distributed_resources) ->
	  let new_names_and_resources =
	    opcode_interpreter op (Array.map (fun (DRH r) -> get_resource r) distributed_resources)
	  in
	    array_foreach_do new_names_and_resources
	      (fun (name,res) -> Hashtbl.add ccpla.ccpla_resources name res)
      | _ -> failwith "XXX TODO: Distributed Command not implemented yet!"
  with
    | Not_found ->
	  failwith "dcom_execute failed, presumably due to unknown resource!"
    | Match_failure _ -> failwith "dcom_execute failed - mangled resource table"
    (* | _ -> failwith "dcom_execute failed for yet unknown reason!"  *)
and slave_mode ccpla =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let rank = Mpi_petsc.comm_rank ccpla.ccpla_comm in
  let () = !rccpla_logdebug (Printf.sprintf "[Node %d]DDD slave_mode\n" rank) in
  let rec work () =
    (* let () = Printf.printf "[Node %d]DDD Slave listening!\n%!" rank in *)
    let dcom = mpi_get_distributed_command ccpla in
    let () = dcom_execute ccpla dcom in
      work ()
  in work()
;;

let master_process_queue ?(local_vectors=[||]) ccpla =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let myrank = Mpi_petsc.comm_rank ccpla.ccpla_comm in
  try
    let rec work () =
      let next_cmds = Queue.take !(ccpla.ccpla_queue) in
      let nr_cmds = Array.length next_cmds in
        (*let () = Printf.printf "DDD master_process_queue() sending %d commands\n%!" nr_cmds in
        let () = Printf.printf "---%s---\n%!" (opcode_tostring next_cmds.(0))
        in*)
        (*let _ = read_line () in*)
	begin
	  for i=1 to nr_cmds-1 do
	    (* Note: at present this is quite primitive - we do not scatter.
	       Will have ample opportunity for refinement of the concept later on.
	    *)
	    match next_cmds.(i) with
	      | DCOM_master _ -> ()
	      | DCOM_sendto_master_v (_, drh) ->
		  Mpi_petsc.send (DCOM_sendto_master_v (None,drh)) i 0 ccpla.ccpla_comm;
	      | DCOM_getfrom_master_v (_,drh) ->
		  Mpi_petsc.send (DCOM_getfrom_master_v (None,drh)) i 0 ccpla.ccpla_comm;
	      | _ ->
		  let len = Mpi_petsc.send_reporting_length next_cmds.(i) i 0 ccpla.ccpla_comm in
		    (if debug_mpi_buffer_lengths
		     then Printf.fprintf stderr "[Node=%d] CCPLA mpi_send command len=%d\n%!" myrank len
		     else ())
	  done;
	  dcom_execute ~local_vectors ccpla next_cmds.(0);
          (*Printf.printf "---\n%!";*)
	  work();
	end
    in
      work ()
  with | Queue.Empty -> ()
;;

(* master functions -- XXX NOTE TODO: these should take an extra optarg
   after the ccpla: ?(proecess_queue_immediately=true)
 *)
let ccpla_vector_create ccpla name node_dimensions =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_vec_create (name,node_dimensions)) in
  let cmds_assembly_begin =
    Array.make nr_nodes (DCOM_vec_assembly_begin (DRH name)) in
  let cmds_assembly_end =
    Array.make nr_nodes (DCOM_vec_assembly_end (DRH name)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let () = Queue.push cmds_assembly_begin !(ccpla.ccpla_queue) in
  let () = Queue.push cmds_assembly_end !(ccpla.ccpla_queue) in
  let the_vector = DRH name in
  let () = !rccpla_logdebug (Printf.sprintf "DDD DRES vec: create finalizer for '%s'\n" name) in
  let () = Gc.finalise
    (fun drh ->
       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES vec: parfinalize '%s'\n" name) in
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_vector
  in
  let () = master_process_queue ccpla in
    the_vector
;;

let ccpla_vector_create_multiseq ccpla name dim =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_vec_create_multiseq (name,dim)) in
  let cmds_assembly_begin =
    Array.make nr_nodes (DCOM_vec_assembly_begin (DRH name)) in
  let cmds_assembly_end =
    Array.make nr_nodes (DCOM_vec_assembly_end (DRH name)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let () = Queue.push cmds_assembly_begin !(ccpla.ccpla_queue) in
  let () = Queue.push cmds_assembly_end !(ccpla.ccpla_queue) in
  let the_vector = DRH name in
  let () = !rccpla_logdebug (Printf.sprintf "DDD DRES vec_multiseq: create finalizer for '%s'\n" name) in
  let () = Gc.finalise
    (fun drh ->
       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES vec_multiseq: parfinalize '%s'\n" name) in
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_vector
  in
  let () = master_process_queue ccpla in
    the_vector
;;

let ccpla_vector_distribute ccpla ?(local_vectors=[||]) nr_master_vector ((DRH slave_name) as drh_slave) =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds =
    Array.init nr_nodes
      (fun n ->
	 if n=0 then
	   DCOM_getfrom_master (nr_master_vector,drh_slave)
	 else
	   DCOM_getfrom_master (-1,drh_slave))
  in
  let () = Queue.push cmds !(ccpla.ccpla_queue) in
  let () = master_process_queue ~local_vectors ccpla in
    ()
;;

let ccpla_hello ccpla =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds =
    Array.make nr_nodes DCOM_HELLO
  in
  let () = Queue.push cmds !(ccpla.ccpla_queue) in
  let () = master_process_queue ~local_vectors:[||] ccpla in
    ()
;;


let ccpla_vector_collect ccpla ?(local_vectors=[||]) nr_master_vector ((DRH slave_name) as drh_slave) =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds =
    Array.init nr_nodes
      (fun n ->
	 if n=0 then
	   DCOM_sendto_master (nr_master_vector,drh_slave)
	 else
	   DCOM_sendto_master (-1,drh_slave))
  in
  let () = Queue.push cmds !(ccpla.ccpla_queue) in
  let () = master_process_queue ~local_vectors ccpla in
    ()
;;

let ccpla_matrix_create ccpla name mtype node_dimensions_left node_dimensions_right =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_mat_create (name,mtype,node_dimensions_left,node_dimensions_right)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
    (*
      let cmds_assembly_begin =
      Array.make nr_nodes (DCOM_mat_assembly_begin name) in
      let cmds_assembly_end =
      Array.make nr_nodes (DCOM_mat_assembly_begin name) in
      let () = Queue.push cmds_assembly_begin !(ccpla.ccpla_queue) in
      let () = Queue.push cmds_assembly_end !(ccpla.ccpla_queue) in
    *)
  let the_matrix = DRH name in
  let () = !rccpla_logdebug (Printf.sprintf "DDD DRES mat: create finalizer for '%s'\n" name) in
  let () = Gc.finalise
    (fun drh ->
       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES mat: parfinalize '%s'\n" name) in
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_matrix
  in
  let () = master_process_queue ccpla in
    the_matrix
;;

let ccpla_matrix_duplicate ccpla drh_src name_new copy_values =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_mat_duplicate (drh_src,name_new,copy_values)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_matrix = DRH name_new in
  let () = !rccpla_logdebug (Printf.sprintf "DDD DRES mat: create finalizer for '%s'\n" name_new) in
  let () = Gc.finalise
    (fun drh ->
       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES mat: parfinalize '%s'\n" name_new) in
       let () = ccpla.ccpla_pending_finalizations <- name_new::ccpla.ccpla_pending_finalizations
       in ()) the_matrix
  in
  let () = master_process_queue ccpla in
    the_matrix
;;

let ccpla_matrix_copy ccpla drh_src drh_dst same_nonzero_pattern =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_mat_copy (drh_src,drh_dst,same_nonzero_pattern)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_matrix_scale_and_add_identity ccpla drh_mx scale id_coeff =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_mat_scale_and_add_identity (drh_mx,scale,id_coeff)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_matrix_times_vector ccpla drh_mx drh_src drh_dst =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_mx_x_vec (drh_mx,drh_src,drh_dst)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_ksp_solve ccpla drh_ksp drh_v_in drh_v_sol =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes (DCOM_ksp_solve (drh_ksp,drh_v_in,drh_v_sol)) in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_matrix_assemble ccpla ?(final=true) drh_mx =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_assembly_begin =
    Array.make nr_nodes (DCOM_mat_assembly_begin (drh_mx,final)) in
  let cmds_assembly_end =
    Array.make nr_nodes (DCOM_mat_assembly_end (drh_mx,final)) in
  let () = Queue.push cmds_assembly_begin !(ccpla.ccpla_queue) in
  let () = Queue.push cmds_assembly_end !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_vector_assemble ccpla drh_v =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_assembly_begin =
    Array.make nr_nodes (DCOM_vec_assembly_begin drh_v) in
  let cmds_assembly_end =
    Array.make nr_nodes (DCOM_vec_assembly_end drh_v) in
  let () = Queue.push cmds_assembly_begin !(ccpla.ccpla_queue) in
  let () = Queue.push cmds_assembly_end !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_petsc_matrix ccpla (DRH name) =
  let mx = dres_get_matrix (ccpla_get_resource ccpla name) in
    mx
;;

let ccpla_petsc_vector ccpla (DRH name) =
  let DRES_petsc_vector (_,_,v) = ccpla_get_resource ccpla name in
    v
;;

let ccpla_petsc_ksp ccpla (DRH name) =
  let DRES_petsc_ksp (_,ksp) = ccpla_get_resource ccpla name in
    ksp
;;


let ccpla_iparams ccpla (DRH name) =
  let DRES_parameters (names,vals) = ccpla_get_resource ccpla name in
    (names,vals)
;;

let ccpla_petsc_multiseq_vector ccpla (DRH name) =
  let () = !rccpla_logdebug (Printf.sprintf "DDD ccpla_petsc_multiseq_vector name='%s'\n" name) in
  let DRES_petsc_vector_multiseq (_,v) = ccpla_get_resource ccpla name in
    v
;;


let ccpla_ksp_create
    ccpla name
    ?(matrix_structure=Mpi_petsc.DIFFERENT_NONZERO_PATTERN)
    ?tolerances
    ?initial_guess_nonzero
    ?ksp_type
    ?pc_type
    ?pc_subtype
    ?matnullspace
    drh_m1 drh_m2
    =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let push_parallel cmd =
    Queue.push (Array.make nr_nodes cmd) !(ccpla.ccpla_queue)
  in
    begin
      push_parallel (DCOM_ksp_create (name,matrix_structure,drh_m1,drh_m2));
      (
	match tolerances with
	  | None -> ()
	  | Some (ortol,oatol,odtol,omaxit) ->
	      push_parallel (DCOM_ksp_set_tolerances_opt (DRH name,ortol,oatol,odtol,omaxit))
      );
      (
	match initial_guess_nonzero with
	  | None -> ()
	  | Some flag ->
	      push_parallel (DCOM_ksp_set_initial_guess_nonzero (DRH name,flag))
      );
      (
	match ksp_type with
	  | None -> ()
	  | Some str ->
	      push_parallel (DCOM_ksp_set_type (DRH name,str))
      );
      (
	match pc_type with
	  | None -> ()
	  | Some str ->
	      push_parallel (DCOM_ksp_set_pc_type (DRH name,str))
      );
      push_parallel (DCOM_ksp_set_up (DRH name));
      (* mf: ksp_set_pc_bjacobi_sub_type requires ksp to be set up *)
      (
	match pc_subtype with
	  | None -> ()
	  | Some str ->
	      push_parallel (DCOM_ksp_set_pc_bjacobi_subtype (DRH name,str))
      );
      let the_ksp = DRH name in
	(* Tricky issue: The KSP is closely connected to its matrices.

	   How do we deal with the issue that a matrix DRH goes out
	   of scope while the KSP DRH that uses this matrix still exists?

	   If we were to just "let things happen", then a matrix DRH going
	   out of scope would induce calling MatDestroy() on the matrix -
	   which would be fatal to the KSP. So, how to approach this?

	   One trick that "almost" should do the job is to hide a matrix DRH
	   reference inside the KSP finalizer. (Almost, because (1) we get a
	   "lag-behind" of one GC cycle (but we presumably get this with many
	   other schemes as well), and (2) because we have to write horrendous
	   code that may get optimized away by super-clever compilers...
	*)
      let () =
	match matnullspace with
	  | None -> ()
	  | Some (has_constant,vecs) ->
	      let mns_name = Printf.sprintf "%s_nullspace" name in
		(* NOTE: vecs will be ignored for now! *)
		begin
		  push_parallel (DCOM_matnullspace_create (mns_name,has_constant,vecs));
		  let the_mns = DRH mns_name in
		  let () = Gc.finalise
		    (fun drh ->
		       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES matnullspace: parfinalize '%s'\n" mns_name) in
		       let tricky_gc_refs = vecs in
		       let () = (if Array.length vecs < 0 then Printf.printf "Boo!" else ()) in
		       let () = ccpla.ccpla_pending_finalizations <- mns_name::ccpla.ccpla_pending_finalizations
		       in ()) the_mns
		  in ()
		end
      in
      let () = !rccpla_logdebug (Printf.sprintf "DDD DRES ksp: create finalizer for '%s'\n" name) in
      let () = Gc.finalise
	(fun drh ->
	   let () = !rccpla_logdebug (Printf.sprintf "DDD DRES ksp: parfinalize '%s'\n" name) in
	   let tricky_gc_refs = [|drh_m1;drh_m2|] in
	   let () = (if Array.length tricky_gc_refs <> 2 then Printf.printf "Boo!" else ()) in
	   let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
	   in ()) the_ksp
      in
      let () = master_process_queue ccpla in
	the_ksp
    end
;;

let ccpla_ksp_set_up ccpla drh_ksp =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let push_parallel cmd =
    Queue.push (Array.make nr_nodes cmd) !(ccpla.ccpla_queue)
  in
    begin
      push_parallel (DCOM_ksp_set_up drh_ksp);
      master_process_queue ccpla;
    end
;;

let ccpla_ksp_set_operators
    ccpla
    ?(do_register_new_finalizer=true)
    drh_ksp drh_mx drh_precond
    =
  (* XXX How does this work with Garbage Collection?
     After all, when we replace the matrix operators, we must ensure
     KSP garbage collection no longer holds on to the old matrices,
     but to the new ones! Unfortunately, OCaml does not allow us to grab the
     old finalizer and replace it with a new one that holds on to the new matrices,
     so we use an ugly workaround for now: the user has to specify if he wants to register
     a new finalizer (if the matrix which we replace here is the same entity which we
     used before, we will not do this). If he does, then this will be in addition to all
     previously used finalizers, so replacing a KSP operator that way will not release
     the KSP's association to the matrices it has used earlier in its history. Not nice.
  *)
  let DRH name = drh_ksp in
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let push_parallel cmd =
    Queue.push (Array.make nr_nodes cmd) !(ccpla.ccpla_queue)
  in
    begin
      push_parallel (DCOM_ksp_set_operators (drh_ksp,drh_mx,drh_precond));
      master_process_queue ccpla;
      (if do_register_new_finalizer
       then
	 let () = !rccpla_logdebug (Printf.sprintf "DDD DRES ksp: add finalizer for '%s'\n" name) in
	   Gc.finalise
	     (fun drh ->
		let () = !rccpla_logdebug (Printf.sprintf "DDD DRES ksp: parfinalize '%s'\n" name) in
		let tricky_gc_refs = [|drh_mx;drh_precond|] in
		let () = (if Array.length tricky_gc_refs <> 2 then Printf.printf "Boo!" else ()) in
		let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
		in ()) drh_ksp
       else ())
    end
;;

let ccpla_register_params ccpla name param_names param_vals =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes
      (DCOM_register_parameters(name,param_names,param_vals))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_params = DRH name in
  let () = !rccpla_logdebug (Printf.sprintf "DDD DRES params: create finalizer for '%s'\n" name) in
  let () = Gc.finalise
    (fun drh ->
       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES params: parfinalize '%s'\n" name) in
       let () = ccpla.ccpla_pending_finalizations <-
	 name::ccpla.ccpla_pending_finalizations
       in ()) the_params
  in
  let () = master_process_queue ccpla in
    the_params
;;

let ccpla_manipulate_params ccpla params f =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_manip =
    Array.make nr_nodes
      (DCOM_manipulate_parameters(params,f))
  in
  let () = Queue.push cmds_manip !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

let ccpla_sequence_create ccpla name misd_commands =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.init nr_nodes
      (fun nr_node ->
	 let node_commands =
	   Array.map
	     (fun a ->
		let cmd = a.(nr_node) in
		  if nr_node=0 then cmd
		  else
		    (* Some commands have to be modified in the sequence as
		       we send them in a very different form to the clients
		       (removing closures over master-only values!)
		       XXX NOTE: should be unified with special-opcode handling
		       in the master_process_queue function.
		    *)
		  match cmd with
		    | DCOM_master _ -> DCOM_NOP
                    | DCOM_rec_master _ -> DCOM_rec_master (fun () -> ())
		    | DCOM_sendto_master_v (_,drh) ->
			DCOM_sendto_master_v (None,drh)
		    | DCOM_getfrom_master_v (_,drh) ->
			DCOM_getfrom_master_v (None,drh)
		    | _ -> cmd
	     )
	     misd_commands
	 in
	   (DCOM_register_sequence(name,node_commands)))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_seq = DRH name in
  let () = !rccpla_logdebug (Printf.sprintf "DDD DRES seq: create finalizer for '%s'\n" name) in
  let () = Gc.finalise
    (fun drh ->
       let () = !rccpla_logdebug (Printf.sprintf "DDD DRES seq: parfinalize '%s'\n" name) in
       let () = ccpla.ccpla_pending_finalizations <-
	 name::ccpla.ccpla_pending_finalizations
       in ()) the_seq
  in
  let () = master_process_queue ccpla in
    the_seq
;;

let ccpla_sequence_execute ccpla ?(local_vectors=[||]) drh_sequence =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_exec = (* XXX can we get rid of that consing? *)
    Array.make nr_nodes (DCOM_execute_sequence drh_sequence)
  in
  let () = Queue.push cmds_exec !(ccpla.ccpla_queue) in
    master_process_queue ~local_vectors ccpla
;;

let ccpla_ksp_manipulate_tolerances ccpla drh_ksp f =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_manip =
    Array.make nr_nodes
      (DCOM_ksp_manipulate_tolerances(drh_ksp,f))
  in
  let () = Queue.push cmds_manip !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

(* Tricky issue:

   * Whenever master loses a resource, this has to be entered in a finalization list.
     From this list, we generate and distribute parallel destruction commands,
     in sync with parallel command execution.

     (-> Finalizers of distributed resource handles: enter in finalization list)
     (-> GC alarm: process finalization list, push DCOM_destroy_resources command into
         parallel action queue)

   * How do we clean up if master actually loses a ccpla?

   -> Upon closer consideration, such a question does not make sense,
   as our CCPLAs are not *that* dynamic. Every parallelized application
   will have one, for now, and this will be created at the beginning
   and destroyed at the end!
*)

let _master_set_finalization_alarm ccpla =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let fun_destroy_resources () =
    let to_destroy = ccpla.ccpla_pending_finalizations in
      (* XXX GLITCH: NO RESOURCE-HANDLE-DELETING GC MUST HAPPEN BETWEEN THE CODE
	 LINES ABOVE AND BELOW!
      *)
    let () = ccpla.ccpla_pending_finalizations <- [] in
      if to_destroy = [] then ()
      else
	let destroy_commands =
	  Array.make nr_nodes (DCOM_destroy_resources to_destroy)
	in
	  Queue.push destroy_commands !(ccpla.ccpla_queue)
  in
  let a = Gc.create_alarm fun_destroy_resources in
    () (* don't bother to delete that alarm. The ccpla will stay around
	  as long as the program is running.
       *)
;;

(* XXX at_exit function must also make sure all parallel resources
   are freed before we actually send the DCOM_QUIT!
*)

let _master_exit ccpla =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let resources = Array.to_list (hashtbl_keys ccpla.ccpla_resources) in
  let () = Queue.push (Array.make nr_nodes (DCOM_destroy_resources resources)) !(ccpla.ccpla_queue) in
  let () = Queue.push (Array.make nr_nodes DCOM_QUIT) !(ccpla.ccpla_queue) in
    master_process_queue ccpla
;;




(* End-of-module setup: *)
let setup ?(petsc_argv=[||]) mpi_argv opcode_interpreter fun_quit =
  let _ = Mpi_petsc.mpi_init mpi_argv in
  let p =
    let petscrc_filename = (Printf.sprintf "%s/.petscrc" (Unix.getenv "HOME"))
    in
      Mpi_petsc.petsc_init petsc_argv petscrc_filename "help-message" true
  in
  let ccpla_comm = Mpi_petsc.comm_dup Mpi_petsc.comm_world (* (Mpi_petsc.petsc_get_comm_world()) *)  in
  let nr_nodes = Mpi_petsc.comm_size ccpla_comm in
  let f_dummy _ = failwith "" in
  let f_ldummy ?local_vectors _ = failwith "" in
  let ccpla =
    {
      ccpla_comm=ccpla_comm;
      ccpla_opcode_interpreter=f_dummy;
      ccpla_resources=Hashtbl.create 10;
      ccpla_queue=ref (Queue.create ());
      ccpla_pending_finalizations = [];
      ccpla_quit = fun_quit;
      ccpla_register_params=f_dummy;
      ccpla_manipulate_params=f_dummy;
      ccpla_sequence_create=f_dummy;
      ccpla_sequence_execute=f_ldummy;
      ccpla_vector_create=f_dummy;
      ccpla_vector_create_multiseq=f_dummy;
      ccpla_vector_distribute=f_ldummy;
      ccpla_vector_collect=f_ldummy;
      ccpla_matrix_create=f_dummy;
      ccpla_matrix_duplicate=f_dummy;
      ccpla_matrix_copy=f_dummy;
      ccpla_matrix_scale_and_add_identity=f_dummy;
      ccpla_matrix_times_vector=f_dummy;
      ccpla_ksp_solve=f_dummy;
      ccpla_matrix_assemble=(fun ?final _ -> failwith "");
      ccpla_vector_assemble=f_dummy;
      ccpla_petsc_matrix=f_dummy;
      ccpla_petsc_vector=f_dummy;
      ccpla_petsc_ksp=f_dummy;
      ccpla_iparams=f_dummy;
      ccpla_petsc_multiseq_vector=f_dummy;
      ccpla_ksp_create=f_dummy;
      ccpla_ksp_set_up=f_dummy;
      ccpla_ksp_set_operators=(fun ?do_register_new_finalizer _ -> failwith "");
      ccpla_ksp_manipulate_tolerances=f_dummy;
    }
  in
    begin
      ccpla.ccpla_opcode_interpreter <- opcode_interpreter ccpla;
      ccpla.ccpla_register_params <- ccpla_register_params ccpla;
      ccpla.ccpla_manipulate_params <- ccpla_manipulate_params ccpla;
      ccpla.ccpla_sequence_create <- ccpla_sequence_create ccpla;
      ccpla.ccpla_sequence_execute <- ccpla_sequence_execute ccpla;
      ccpla.ccpla_vector_create <- ccpla_vector_create ccpla;
      ccpla.ccpla_vector_create_multiseq <- ccpla_vector_create_multiseq ccpla;
      ccpla.ccpla_vector_distribute <- ccpla_vector_distribute ccpla;
      ccpla.ccpla_vector_collect <- ccpla_vector_collect ccpla;
      ccpla.ccpla_matrix_create <- ccpla_matrix_create ccpla;
      ccpla.ccpla_matrix_duplicate <- ccpla_matrix_duplicate ccpla;
      ccpla.ccpla_matrix_copy <- ccpla_matrix_copy ccpla;
      ccpla.ccpla_matrix_scale_and_add_identity <- ccpla_matrix_scale_and_add_identity ccpla;
      ccpla.ccpla_matrix_times_vector <- ccpla_matrix_times_vector ccpla;
      ccpla.ccpla_ksp_solve <- ccpla_ksp_solve ccpla;
      ccpla.ccpla_matrix_assemble <- ccpla_matrix_assemble ccpla;
      ccpla.ccpla_vector_assemble <- ccpla_vector_assemble ccpla;
      ccpla.ccpla_petsc_matrix <- ccpla_petsc_matrix ccpla;
      ccpla.ccpla_petsc_vector <- ccpla_petsc_vector ccpla;
      ccpla.ccpla_petsc_ksp <- ccpla_petsc_ksp ccpla;
      ccpla.ccpla_iparams <- ccpla_iparams ccpla;
      ccpla.ccpla_petsc_multiseq_vector <- ccpla_petsc_multiseq_vector ccpla;
      ccpla.ccpla_ksp_create <- ccpla_ksp_create ccpla;
      ccpla.ccpla_ksp_set_up <- ccpla_ksp_set_up ccpla;
      ccpla.ccpla_ksp_set_operators <- ccpla_ksp_set_operators ccpla;
      ccpla.ccpla_ksp_manipulate_tolerances <- ccpla_ksp_manipulate_tolerances ccpla;
      if Mpi_petsc.comm_rank ccpla_comm = 0
      then
	let r_have_been_called = ref false in
	begin
	  _master_set_finalization_alarm ccpla;
	  at_exit
	    (fun () ->
	       let () = !rccpla_logdebug (Printf.sprintf "DDD master at_exit() called!\n") in
		 if !r_have_been_called then ()
		 else
		   let () = r_have_been_called := true in
		     _master_exit ccpla);
	  ccpla
	end
      else
	slave_mode ccpla
    end
;;



let do_test ccpla =
  let mysleep n = () (* Unix.sleep n *) in
  let describe_vector s =
    let DRES_petsc_vector (_,_,v) = Hashtbl.find ccpla.ccpla_resources s in
    let _ = Mpi_petsc.vec_describe v in
      ()
  in
  let _ =
    let myrank = Mpi_petsc.comm_rank ccpla.ccpla_comm in
    if myrank <> 0
    then ()
    else
      let v_distributed = ccpla.ccpla_vector_create "Vec_dist" [|40;40;40;40;40;40|] in
      let vseq_master = Mpi_petsc.vector_pack (Array.make 240 0.0) in
      let vseq_master_back = Mpi_petsc.vector_pack (Array.make 240 0.0) in
      let local_vectors=[|vseq_master;vseq_master_back|] in
      let () = Printf.printf "[Node %d]DDD Step 1\n%!" myrank in let () = mysleep 3 in
      let () =
	Mpi_petsc.with_petsc_vector_as_bigarray vseq_master
	  (fun ba_m ->
	     for i=0 to 240-1 do
	       Printf.printf "Populating master entry %d\n%!" i;
	       ba_m.{i} <- float_of_int(i);
	     done)
      in
      let () = ccpla.ccpla_vector_distribute ~local_vectors 0 v_distributed in
      let v_distributed2 = ccpla.ccpla_vector_create "Vec_dist2" [|40;40;40;40;40;40|] in
      let () = Printf.printf "[Node %d]DDD === AFTER SCATTER - BEFORE COLLECT ===\n%!" myrank in let () = mysleep 3 in
      let () = Queue.push (Array.make 6 (DCOM_printvec v_distributed)) !(ccpla.ccpla_queue) in
      let () = master_process_queue ccpla in
      let () = ccpla.ccpla_vector_collect ~local_vectors 1 v_distributed in
      let () =
	Mpi_petsc.with_petsc_vector_as_bigarray vseq_master_back
	  (fun ba_m ->
	     for i=0 to 240-1 do
	       Printf.printf "[Node %d]Master-Back: %3d -> %8.4f\n%!" myrank i ba_m.{i};
	     done)
      in
      let v_distributed3 = ccpla.ccpla_vector_create "Vec_dist3" [|40;40;40;40;40;40|] in
      let mx1 = ccpla.ccpla_matrix_create "Mat" "mpiaij" [|40;40;40;40;40;40|] [|40;40;40;40;40;40|] in
      let () = master_process_queue ccpla in
      let () = mysleep 3 in ()
  in ()
;;


let do_test2 ccpla =
  let distrib = [|30;30;30;30;30;30;30;30|] in
  let nr_nodes = Array.length distrib in
  let len_total = Array.fold_left (+) 0 distrib in
  let mx = ccpla.ccpla_matrix_create "The_MX" "mpiaij" distrib distrib in
  let petsc_mx = ccpla.ccpla_petsc_matrix mx in
  (* Need local matrix for population *)
  let () =
    begin
      for i=0 to len_total-2 do
	Mpi_petsc.matrix_set petsc_mx i i 2.0;
	Mpi_petsc.matrix_set petsc_mx i (i+1) (-1.0);
	Mpi_petsc.matrix_set petsc_mx (i+1) i (-1.0);
      done;
      Mpi_petsc.matrix_set petsc_mx (len_total-1) (len_total-1) 3.0;
      (* make it regular, remove phi->phi+c shift symmetry; also: add missing last entry. *)
    end
  in
  let () = ccpla.ccpla_matrix_assemble ~final:true mx in
  let ksp = ccpla.ccpla_ksp_create "The_KSP" mx mx in
  let v_data_par = ccpla.ccpla_vector_create "Vec_data" distrib in
  let v_solution_par = ccpla.ccpla_vector_create "Vec_sol" distrib in
  let v_data_ser = Mpi_petsc.vector_pack (Array.make len_total 0.0) in
  let v_solution_ser = Mpi_petsc.vector_pack (Array.make len_total 0.0) in
  let local_vectors=[|v_data_ser;v_solution_ser|] in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v_data_ser
      (fun ba_v ->
	 for i=0 to len_total-1 do
	   Printf.printf "Populating master entry %d\n%!" i;
	   ba_v.{i} <- cos(float_of_int(i)/.50.0);
	 done)
  in
  let () = ccpla.ccpla_vector_distribute ~local_vectors 0 v_data_par in
  let () = Queue.push
    (Array.make nr_nodes (DCOM_ksp_solve (ksp,v_data_par,v_solution_par)))
    !(ccpla.ccpla_queue)
  in
  let () = master_process_queue ccpla in
  let () = ccpla.ccpla_vector_collect ~local_vectors 1 v_solution_par in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v_solution_ser
      (fun ba_v_sol ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v_data_ser
	   (fun ba_v_data ->
	      for i=0 to len_total-1 do
		Printf.printf "%3d %8.4f %8.4f\n%!" i ba_v_data.{i} ba_v_sol.{i};
	      done))
  in
    ()
;;


let do_test3 ccpla =
  let myrank = Mpi_petsc.comm_rank ccpla.ccpla_comm in
  let distrib = [|60;60;60;60|] in
  let nr_nodes = Array.length distrib in
    if myrank <> 0
    then ()
    else
      let par_exec cmd =
	let () = Queue.push (Array.make nr_nodes cmd) !(ccpla.ccpla_queue)
	in
	let () = master_process_queue ccpla in
	  ()
      in
      let len_total = Array.fold_left (+) 0 distrib in
      let mx = ccpla.ccpla_matrix_create "The_MX" "mpiaij" distrib distrib in
      let petsc_mx = ccpla.ccpla_petsc_matrix mx in
	(* Need local matrix for population *)
      let () =
	begin
	  for i=0 to len_total-2 do
	    Mpi_petsc.matrix_inc petsc_mx i i 2.0;
	    Mpi_petsc.matrix_inc petsc_mx i (i+1) (-1.0);
	    Mpi_petsc.matrix_inc petsc_mx (i+1) i (-1.0);
	    Printf.printf "MX ROW %d\n%!" i;
	  done;
	  Mpi_petsc.matrix_inc petsc_mx (len_total-1) (len_total-1) 2.0;
	  Mpi_petsc.matrix_inc petsc_mx 0 0 1.0;
	  (* make it regular, remove phi->phi+c shift symmetry *)
	end
      in
      let () = Printf.printf "*** DDD Matrix pre final assembly! ***\n%!" in
      let () = ccpla.ccpla_matrix_assemble ~final:true mx in
      let () = Printf.printf "*** DDD Matrix assembled! ***\n%!" in
      let v_data_par = ccpla.ccpla_vector_create "Vec_data" distrib in
      let v_solution_par = ccpla.ccpla_vector_create "Vec_sol" distrib in
      let ml_data_ser = Array.init len_total (fun n -> cos((float_of_int n)/.50.0)) in
      let v_data_ser = Mpi_petsc.vector_pack ml_data_ser in
      let v_solution_ser = Mpi_petsc.vector_pack (Array.make len_total 0.0) in
      let local_vectors=[|v_data_ser;v_solution_ser|] in
      let () = ccpla.ccpla_vector_distribute ~local_vectors 0 v_data_par in
      let () = par_exec (DCOM_printmat mx) in
      let () = par_exec (DCOM_mx_x_vec (mx,v_data_par,v_solution_par)) in
      let () = master_process_queue ccpla in
      let () = ccpla.ccpla_vector_collect ~local_vectors 1 v_solution_par in
      let ml_sol = Mpi_petsc.vector_extract v_solution_ser in
      let () =
	for i=0 to Array.length ml_sol-1 do
	  Printf.printf "%3d %10.6f %10.6f\n%!" i ml_data_ser.(i) (200.0*.ml_sol.(i));
	done
      in
	()
;;


(*
let ccpla = setup Sys.argv (fun _ -> failwith "No opcode!") (fun () -> ()) in
let _ = do_test ccpla in
  exit 0
;;
*)


(*
let ccpla = setup Sys.argv (fun _ -> failwith "No opcode!") (fun () -> exit 0) in
let _ = do_test2 ccpla in
  exit 0
;;
*)

(* Status: we can run the toplevel with:

   mpirun -np 6 ccpla.top

   Everything then works in a neat and tidy fashion. Nice.
*)

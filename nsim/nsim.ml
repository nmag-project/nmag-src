(* (C) 2007 Dr. Thomas Fischbacher *)

(* XXX RELIC:
   ocamlc -I ../config -I ../snippets -I ../ccpla -I ../mesh -I ../fem -I ../mpi_petsc -I ../nsim_grammars -I ../bem3d -I ../hlib -I ../nlog -I ../sundials_sp -i nsim.ml

 TODO:

 [X] Remove Debugging Output
 [X] New parallel timestepper must take and report sub-specialist timings
 [X] Check setting timestepper KSP defaults (such as InitialGuessNonzero),
      introduce parameter tweaking functions
 [ ] Provide derive_me for localeqn (so that we can also add H_anis)
 [ ] Add sequential brother of the lam-integrated timestepper.

*)

open Snippets;;
open Mesh;;
open Fem;;
open Ccpla;;
open Localeqn_parser;;


let logmsg = Nlog.getLogger("nfem.ocaml");;
let loginfo = logmsg Nlog.Info;;
let loginfo2 = logmsg Nlog.Info2;;
let logdebug =logmsg Nlog.Debug;;

let reportmem tag =
  let (time, vmem, rss) = time_vmem_rss () in
    loginfo2 (* logdebug *)
      (Printf.sprintf "Memory report: T=%7.3f VMEM=%8.0f KB RSS=%8.0f KB %s"
                      time vmem rss tag)
;;

let sundials_path fullpath =
  (* given the fullpath (i.e. path and filename) (typically from
  nsimconf) we return it as is, unless the environment variable
  NSIM_SUNDIALS_PATH has been defined. If this is defined, we take
  NSIM_SUNDIALS_PATH as the directory, and append the filename
  from the fullpath.

  This is needed for the debian installation of the software (in
  conjunction with the way we configure the whole package with
  nsimconf).  *)
  let (input_path,input_filename) = path_and_filename_of_fullpath fullpath in
  let directory=
    try Unix.getenv "NSIM_SUNDIALS_PATH" with | Not_found -> input_path
  in Printf.sprintf "%s/%s" directory input_filename
;;

(* === The inference / field dependency mechanism ===
   This is short and simple enough to be included in nsim.ml
   and does not really warrant an own module...
 *)

type inference_entity =
    {ien_name: string;
     ien_depends_on: inference_entity array;
     ien_prerequisites: inference_entity array;
     ien_remake: unit -> unit;
     ien_also_updates: inference_entity array;
     mutable ien_is_uptodate: bool
    }
;;

let rec rebuild x =
  if x.ien_is_uptodate then ()
  else
    let () = Array.iter rebuild x.ien_prerequisites in
    let () = x.ien_remake () in
    let () = x.ien_is_uptodate <- true in
      Array.iter (fun z -> z.ien_is_uptodate <- true) x.ien_also_updates
;;


let rec invalidate x =
  if x.ien_is_uptodate
  then
    let () = x.ien_is_uptodate <- false in
    let () = Array.iter invalidate x.ien_depends_on in
      ()
  else
    (* Do not redo the entire tree in case we are already
       classified as being invalidated. If we did this,
       we may end up walking the same tree multiple times
       unnecessarily...
    *)
    ()
;;

(* === End inference mechanism === *)



type linalg_command =
  | LC_vec_distribute of string * string
  | LC_vec_collect of string * string
      (* NOTE: for these first two functions, the first string must designate
	 NOT a parallel resource, but a master-local vector!

	 XXX For now, the only master-local vectors will be
	 linalg_machine params, but that may/should change!
      *)
  | LC_pvec_scale of string * float (* "p" meaning (potentially) "parrallel" *)
  | LC_pvec_pointwise of string * (int -> float -> float)
  | LC_pvec_axpby of ccpla_coefficient * string * ccpla_coefficient * string
  | LC_iparams_axpby of ccpla_coefficient * string * ccpla_coefficient * string (* first string may be "", meaning "use just this number" *)
  | LC_pvec_pull of int array * string * string
  | LC_pvec_push of int array * string * string
  | LC_pvec_pull_fem of string * string * string * string
  | LC_pvec_push_fem of string * string * string * string
  | LC_smx_x_pvec of string * string * string (* sparse matrix *)
  | LC_dmx_x_pvec of string * string * string (* dense matrix *)
  | LC_vec_pointwise_mult of string * string * string
  | LC_cofield_to_field of string * string
  | LC_jplan_execute of string * (string array)
  | LC_psolve of string * string * string
  | LC_psite_wise_iparams of string * string array * string array
  | LC_gosub of string
  | LC_callext of (float -> unit)
  | LC_debug_printvec_1cpu of string * string * int (* this is a no-operation for the parallel machine *)
  | LC_start_timer of string
  | LC_stop_timer of string
  | LC_report_timers of string array
  | LC_clear_timers of string array
;;

type fun_derive_me =
    (int -> (* Desired target row *)
       ((int * int) * (float * string * int array) array) array);;

type local_equations =
    (Fem.dof_name * (float * Fem.dof_name array) array) array;;

type jacobi_plan =
    ((int * int) * ((float * string * int array) array)) array array;;

type las_op_matrix_spec =
    (* XXX TODO: cannot yet handle "middle" matrices - for design reasons!
       Need another abstraction for field_mid dependent op_matrices!
    *)
    {
      loms_name: string;
      loms_mwe_name_le: string;
      loms_mwe_name_ri: string;
      loms_mwe_name_mid: string option;
      loms_symbolic_operator: string;
      loms_matoptions: Mpi_petsc.matoption_type array;
      (* Here, we encounter a somewhat awkward situation: when creating an operator matrix,
	 we may have to keep around the entries as they will also go into the plan for
	 the Jacobian. This at present is quite unwieldy, but the question how to improve
	 this design is not really an easy one...
      *)
    }
;;

(* XXX PRELIMINARY DATA STRUCTURE XXX

   For now, we just provide the information here needed to fill a BEM matrix.
   Should we (later on) ever want to deal with other dense matrices as well,
   we can modify/extend this!
*)

(* Note: this is internal only: *)
type las_dense_matrix =
    BEM_PETSC of Ccpla.distributed_resource_handle
  | BEM_HLib of (Hlib.hmatrix * Mpi_petsc.vector * Mpi_petsc.vector)

type las_dense_matrix_spec =
    {
      (* XXX MUST BE REPAIRED - SEARCH FOR "REPAIR" *)
      ldms_name: string;
      ldms_hlib: bool;
      ldms_hlib_params: (int*int*int*float*float*float*int*int);
      ldms_mwe_name: string;
      ldms_dof_name: dof_name;
      ldms_boundary_spec: string;
      ldms_lattice_info: (float array array array * ((int * float * (float array)) array)) option;
      ldms_matoptions: Mpi_petsc.matoption_type array;
    }
;;

type las_ksp_spec =
    {
      lks_name: string;
      lks_matrix_name: string;
      lks_precond_name: string;
      lks_ksp_type: string option;
      lks_pc_type: string option;
      lks_nullspace: (bool * (string array)) option;
      lks_initial_guess_nonzero: bool option;
      lks_rtol: float option;
      lks_atol: float option;
      lks_dtol: float option;
      lks_maxits: int option;
    }
;;

type las_swex_spec =
    {
      lss_name: string;
      lss_c_code: string;
      lss_aux_args: string array;
      lss_field_mwes: string array;
      lss_cofield_mwes: string array;
    }
;;

type jplan_derivative =
    JPLAN_OPERATOR of string
  | JPLAN_EQUATION of string
;;

(* Weird: this is a plan for a plan for a jacobi matrix: *)
type las_jacobi_plan_spec =
    {
      ljps_name: string;
      ljps_debugprint: bool;
      ljps_mwe_lhs: string;
      ljps_mwe_names: string array;	(* leading entry is the state vector "y" *)
      ljps_derive_me: jplan_derivative array array;
      (* XXX CHANGE: for now, this can only provide links to operator matrices,
	 such as dH/dM = op_H_exch
	 We have to generalize this in the future!
      *)
      ljps_eqns: local_equations;
    }
;;

type las_timestepper_spec =
    {
      lts_name: string;
      lts_jacobian: string; (* name of the jacobi plan/jacobi matrix to be used for this timestepper -- OBSOLETE! *)
      lts_name_seq_velocities: string;
      lts_nr_primary_fields: int;
      (* usually, just one (m) for micromagnetism. May be 2 if we have
	 dynamical dofs e.g. in T and m, and hence carry around two
	 fields, temperature and magnetism.
      *)
      lts_names_phys_field_mwes: string array;
      lts_names_phys_field_buffers: string array;
      lts_phys_field_derivs: (string * string) array;
      lts_jacobi_prealloc_diagonal: int;
      lts_jacobi_prealloc_off_diagonal: int;
      lts_use_jacobian: bool;
      lts_jacobi_equations: local_equations;
      (* We may want to turn off using Jacobians for some timesteppers. *)
      lts_pc_same_nonzero_pattern: bool;
      lts_pc_opt_rtol: float option;
      lts_pc_opt_atol: float option;
      lts_pc_opt_dtol: float option;
      lts_pc_opt_maxit: int option;
      lts_max_order: int;
      lts_krylov_max: int;
    }
;;

type linalg_script =
    {
     las_intensive_params: string array;
     las_mwes: string array; (* by name, must have exact matches below - XXX do we use that actually? *)
     las_internal_buffers: (string * string * Fem.dvss * bool * float) array;
     (* (buffer_name, mwe_name,subfield_restr,is_field,initial_value).

	Note that we internally do not use FEM_field/FEM_cofield, but just the buffers!
	(Maybe that is a mistake?)
	Later on, functions such as "get_field" will include checks for field/cofield!
      *)
      las_op_matrices: las_op_matrix_spec array;
      las_dense_matrices: las_dense_matrix_spec array;
      las_ksps: las_ksp_spec array;
      las_swexs: las_swex_spec array;
      las_jplans: las_jacobi_plan_spec array;
      las_command_sequences:
	(string *  (* command sequence name *)
	   ((string*string) array) *	(* args_fields (arg_name,mwe_name) *)
	   ((string*string) array) *	(* args_cofields (arg_name,mwe_name) *)
	   linalg_command array) array;
      las_timesteppers: las_timestepper_spec array;
   }
;;

type petsc_or_ccpla_matrix =
    POCM_petsc of Mpi_petsc.matrix
  | POCM_ccpla of drh
;;

type linalg_machine =
    {
      (* NOTE: get_field/get_cofield will modify a given data vector.
	 This way, it is up to the user to provide and take care of
	 data storage.

	 NOTE: in principle, we could write mini-scripts based on
	 execute_on that implement get_field and get_cofield, so
	 these operations are (strictly speaking) non-orthogonal.
	 For now, we keep them nevertheless, as having accessors
	 for all the intermediate fields readily available is just
	 so convenient...
      *)
      mutable _resources_to_stick_to: Ccpla.distributed_resource_handle list;
      get_mwe: string -> float Fem.mesh_with_elements;
      (* in case we have to create a buffer first... *)
      get_iparam: string -> float;
      set_iparam: string -> float -> unit;
      set_ksp_tolerances: string -> (float option * float option * float option * int option) -> unit;
      get_ksp_tolerances: string -> (float * float * float * int);
      get_field: string -> float Fem.fem_field -> unit;
      get_cofield: string -> float Fem.fem_cofield -> unit;
      set_field: string -> float Fem.fem_field -> unit;
      set_cofield: string -> float Fem.fem_cofield -> unit;
      get_jacobian: string -> petsc_or_ccpla_matrix;
      get_potential: string -> string -> float array -> float;
      (* BEM-spec name, shortvec name, pos_observer -> potential *)
      get_timers: string array -> (string * float) array;
      timestepper_initialize_from_physics: string -> float -> float -> float -> unit;
      (* Name of timestepper to init -> initial_time -> rel_tol -> abs_tol -> () *)
      timestepper_advance: string -> bool -> float -> int -> float;
      (* Name of timestepper -> target_time -> maxsteps_or_-1 -> () *)
      timestepper_get_cvode: string -> Sundials_sp.cvode;
      timestepper_timings: string -> string -> (string*float*float) array;
      internal_ccpla_resources: unit -> string array;
      mutable execute_on: string -> (float Fem.fem_field array) -> (float Fem.fem_cofield array) -> unit;
    }
;;

let dummy_linalg_machine =
  let fail () = failwith "The dummy linalg_machine will not do any useful work!" in
    {
      _resources_to_stick_to=[];
      get_mwe=(fun _ -> fail());
      get_iparam=(fun _ -> fail());
      set_iparam=(fun _ -> fail());
      set_ksp_tolerances=(fun _ -> fail());
      get_ksp_tolerances=(fun _ -> fail());
      get_field=(fun _ -> fail());
      get_cofield=(fun _ -> fail());
      set_field=(fun _ -> fail());
      set_cofield=(fun _ -> fail());
      get_jacobian=(fun _ -> fail());
      get_potential=(fun _ -> fail());
      get_timers=(fun _ -> fail());
      timestepper_initialize_from_physics=(fun _ -> fail());
      timestepper_advance=(fun _ -> fail());
      timestepper_get_cvode=(fun _ -> fail());
      timestepper_timings=(fun _ -> fail());
      internal_ccpla_resources=(fun _ -> fail());
      execute_on=(fun _ -> fail());
    }
;;

type par_timestepper_timings =
    {
      mutable ptt_rhs_n: float;
      mutable ptt_rhs_t: float;
      mutable ptt_jacobi_n: float;
      mutable ptt_jacobi_t: float;
      mutable ptt_jv_n: float;
      mutable ptt_jv_t: float;
      mutable ptt_pc_setup_n: float;
      mutable ptt_pc_setup_t: float;
      mutable ptt_pc_solve_n: float;
      mutable ptt_pc_solve_t: float;
      mutable ptt_extra_n: float;
      mutable ptt_extra_t: float;
    }
;;

let make_par_timestepper_timings () =
  {
    ptt_rhs_n=0.0;
    ptt_rhs_t=0.0;
    ptt_jacobi_n=0.0;
    ptt_jacobi_t=0.0;
    ptt_jv_n=0.0;
    ptt_jv_t=0.0;
    ptt_pc_setup_n=0.0;
    ptt_pc_setup_t=0.0;
    ptt_pc_solve_n=0.0;
    ptt_pc_solve_t=0.0;
    ptt_extra_n=0.0;
    ptt_extra_t=0.0;
  }
;;

(* The "parallel timestepper" resource, to be used as a NSIM_RES_timestepper *)
type par_timestepper =
    {
      pts_name: string;
      pts_timings: par_timestepper_timings;
      mutable pts_time_reached: float;
      mutable pts_cvode: Sundials_sp.cvode option ref;
      (* This is an option, as the CVode will only be initialized
	 when we first set it from the physical vector.

	 It also is a reference, as this simplifies turning-live
	 this cvode from inside some of the helper functions which
	 do not yet have access to the par_timestepper data structure.
      *)
      mutable pts_set_initial_from_phys: ?initial_time:float -> ?rel_tol:float -> ?abs_tol:float -> unit -> unit;
      mutable pts_advance: ?exact_tstop:bool -> float -> int -> float;
      mutable pts_set_tolerances: (float option * float option * float option * int option) ->
					   (float * float) -> unit;
      mutable pts_timing_control: string -> (string*float*float) array;
      (*
      mutable pts_reinit: unit -> unit;
	Note that we do not need a "reinitialize" function - the user can just again
	call pts_set_initial_from_phys!
      *)
    }
;;


(* We need FEM "Site Wise Execution" as ccpla extension, and similar things. *)
(* Also: we offer capability to serialize and distribute the important MWEs in such a way
   that the mesh data structure only has to be serialized once.
*)

type ('matrix) nsim_ccpla_resource =
  | NSIM_RES_swex of (float array -> Mpi_petsc.vector array -> Mpi_petsc.vector array -> bool)
  | NSIM_RES_mwes of float Fem.mesh_with_elements array
  | NSIM_RES_vivificators of (string*'matrix Fem.vivificator) array
  | NSIM_RES_jacobi_operators of (string * Mpi_petsc.matrix) array
  | NSIM_RES_timestepper of par_timestepper
;;

type nsim_ccpla_opcode =
  | NSIM_OP_swex_create of
      (string (* Name *)
       * string (* C code *)
       * ((string * int array * float array) array) (* fields, offsets, coords *)
       * (string * string list * string) list (* information on defines *)
      )
  | NSIM_OP_swex of int * string array
      (* site_wise_execute: nr_fields, intensive_params
	 DRES args: [|swex;params;field1;field2;...;cofield1;cofield2;...|]
      *)
  | NSIM_OP_mwes_create of string * ((float mwe_made_by array) option) * (float Fem.mesh_with_elements array option)
      (* Note that we use different payloads on Master and Slave:
	 master just takes the original mwes, slaves build them
	 from geometry information.
      *)
  | NSIM_OP_vivificators_create of string * ((string * Ddiffop_parser.ddiffop * string * string * (string option)) array)
      (* (name_RES_vivificators, [|(name_this_vivificator,ddiffop,name_mwe_le,name_mwe_ri,opt_name_mwe_mid)|])
	 DRES args: [|drh_mwes|]
      *)
  | NSIM_OP_vivificator_exec of (string * bool * bool * (string option))
      (* (vivificator_name,is_this_a_revivification,do_we_need_to_assemble_after,opt_drh_field_mid_mwe_name)
	 DRES args: [|drh_vivificators;drh_matrix;drh_mwes;opt. drh_field_mid|]
      *)
  | NSIM_OP_jacobi_operators_create of (string * string array)
      (*
	DRES args: drhs_ops
      *)
  | NSIM_OP_fill_bem of (las_dense_matrix_spec)
      (*
	DRES args: [|drh_mwes;drh_mx|]
      *)
  | NSIM_OP_make_timestepper of (string * las_timestepper_spec)
      (*
	 DRES args:
	 [|
	 drh_script_compute_velocities;
         (* the script that computes the parallel dy/dt data from parallel y data,
	    NOT the one that does I/O from and to master-sequential buffers.
          *)
         drh_mwes;
	 drh_jacobi_operators;
	 (* The mwes - we need them e.g. to work out the distribution of y components
	 to primary fields *)
	 drh_physical_primary_config_1;
	 drh_physical_primary_config_2;
	 drh_physical_primary_config_3; # of config vectors = # of velocity vectors = lts_nr_phys_fields
	 drh_physical_primary_velocity_1;
	 drh_physical_primary_velocity_2;
	 drh_physical_primary_velocity_3;
	 drh_auxiliary_1;
	 drh_auxiliary_2;
	 drh_auxiliary_3;
	 |]
      *)
  | NSIM_OP_init_timestepper of (float * float * float)
      (* DRES args: [|timestepper|] *)
  | NSIM_OP_advance_timestepper of (bool * float * int)
      (* DRES args: [|timestepper|] *)
  | NSIM_OP_hmatrix_mult of ((Hlib.hmatrix * Mpi_petsc.vector * Mpi_petsc.vector) * Mpi_petsc.vector * Mpi_petsc.vector) option
;;

let nsim_opcode_to_string op =
  match op with
    NSIM_OP_swex_create _ -> "NSIM_OP_swex_create"
  | NSIM_OP_swex _ -> "NSIM_OP_swex"
  | NSIM_OP_mwes_create _ -> "NSIM_OP_mwes_create"
  | NSIM_OP_vivificators_create _ -> "NSIM_OP_vivificators_create"
  | NSIM_OP_vivificator_exec _ -> "NSIM_OP_vivificator_exec"
  | NSIM_OP_jacobi_operators_create _ -> "NSIM_OP_jacobi_operators_create"
  | NSIM_OP_fill_bem _ -> "NSIM_OP_fill_bem"
  | NSIM_OP_make_timestepper _ -> "NSIM_OP_make_timestepper"
  | NSIM_OP_init_timestepper _ -> "NSIM_OP_init_timestepper"
  | NSIM_OP_advance_timestepper _ -> "NSIM_OP_advance_timestepper"
  | NSIM_OP_hmatrix_mult _ -> "NSIM_OP_hmatrix_mult"
;;

(* NOTE: could be generalized to also allow
   delta(a,b, i,j) = 1/2*eps(a,b,p)*eps(i,j,p)
   or delta(a,b,c, i,j,k)
*)

let special_tensor_delta2 ~ix_ranges indices =
  let range v =
    let (_,r) = array_find (fun (n,_) -> n=v) ix_ranges
    in r
  in
  let nr_indices = Array.length indices in
    if nr_indices <> 2
    then failwith "delta tensor can only carry 2 indices!"
    else
      try
	let process_nv n v =
	  ([v],Array.init (range v) (fun n -> (1.0,[n])))
	in
	  match indices with
	    | [|IX_int n1;IX_int n2|] ->
		if n1=n2 then ([],[| (1.0,[]) |]) else ([],[||])
	    | [|IX_var v1;IX_var v2|] ->
		let r1 = range v1 and r2 = range v2 in
		  if r1<>r2
		  then failwith "delta tensor used with different-range indices!"
		    (* May want to modify/adjust this later on! *)
		  else
		    if v1=v2
		      (* same name, hence this is just a combinatorical factor
			 (subtleties arise when the same index occurs yet
			 another time in a product... We should prevent this,
			 but actually neither can nor do at the moment...
			 ) *)
		    then
		      ([],[| (float_of_int r1,[]) |])
		    else
		      ([v1;v2],Array.init r1 (fun n -> (1.0,[n;n])))
	    | [|IX_var v1; IX_int n1|] ->
		process_nv n1 v1
	    | [|IX_int n1; IX_var v1|] ->
		process_nv n1 v1
	    | _ -> failwith "delta tensor carries strange indices!"
      with | Not_found -> failwith "delta tensor carries index of unspecified range!"
;;


let special_tensor_epsilon ~ix_ranges indices =
  (* Note: we silently assume we are not fed with junk like eps(1,2,3,8)! *)
  let range v =
    let (_,r) = array_find (fun (n,_) -> n=v) ix_ranges
    in r
  in
  let nr_indices = Array.length indices in
  let sorted_indices = array_sorted compare indices in
    if sorted_array_has_duplicate (=) sorted_indices
    then (* epsilon reduces to zero! *)
      ([],[||])
    else
      let named_indices =
	array_mapfilter
	  (fun ix -> match ix with | IX_int _ -> None | IX_var n -> Some n) sorted_indices
      in
      let nr_named_indices = Array.length named_indices in
      let possible_choices =
	(* Not maximally efficient (O(indices)^2), but robust: *)
	let all_indices = Array.init nr_indices identity in
	let fixed_indices = array_mapfilter (fun ix -> match ix with | IX_int n -> Some n | _ -> None)
	  sorted_indices
	in
	  array_filter (fun n -> -1==array_position n fixed_indices 0) all_indices
      in
      let named_index_positions =
	Array.map (fun n -> array_position_if (fun ix -> ix = IX_var n) indices 0) named_indices
      in
      let r_result= ref [] in
      let full_perm = Array.map (fun ix -> match ix with | IX_var _ -> (-1) | IX_int n -> n) indices in
      let () = for_permutations nr_named_indices
	(fun partial_perm ->
	   begin
	     for i=0 to nr_named_indices-1 do
	       full_perm.(named_index_positions.(i)) <- possible_choices.(partial_perm.(i));
	     done;
	     let sign = float_of_int (permutation_sign full_perm) in
	       r_result:= (sign,Array.to_list (Array.map (fun n -> possible_choices.(n)) partial_perm))::(!r_result)
	   end)
      in
	(Array.to_list named_indices,Array.of_list (List.rev !r_result))
;;

let default_special_tensors=[("delta",special_tensor_delta2);("eps",special_tensor_epsilon)];;

(* XXX NOTE: this code is not really good, and quite ad-hoc! *)
let local_equation_normal_form ?(special_tensors=default_special_tensors) ~ix_ranges ~lhs ~rhs () =
  let rec nf1 term =
    match term with
      | Tensor_func _ ->
	  failwith "Non-polynomial tensor functions are not handled yet!"
      | Tensor_float x -> [|(x,[||])|]
      | Tensor_varindexed tensor -> [|(1.0,[|tensor|])|]
      | Tensor_sum summands ->
	  array_join (Array.map nf1 (Array.of_list summands))
      | Tensor_product factors ->
	  let n_factors = Array.map nf1 (Array.of_list factors) in
	  let factor_nr_contribs = Array.map Array.length n_factors in
	  let ht_result = Hashtbl.create 17 in
	  let () = multifor factor_nr_contribs
	    (fun _ selection ->
	       let factors = Array.mapi (fun n j -> n_factors.(n).(j)) selection
	       in
	       let coeff = Array.fold_left (fun sf (c,_) -> sf*.c) 1.0 factors in
	       let tensors = array_sorted compare (array_join (Array.map (fun (_,x) -> x) factors)) in
		 hashtbl_increase (+.) ht_result tensors coeff)
	  in
	    map_hashtbl_to_array ~sorter:(fun (_,x) (_,y) -> compare x y)
	      (fun k c -> (c,k))
	      ht_result
  in
  let fix_indices fixed_indices these_indices =
    Array.map
      (fun ix ->
	 match ix with
	   | IX_int _ -> ix
	   | IX_var name ->
	       try
		 let (_,resolved) =
		   List.find (fun (n,_) -> n=name) fixed_indices
		 in IX_int resolved
	       with | Not_found -> ix)
      these_indices
  in
  let sorted_array_of_list li =
    array_sorted compare (Array.of_list li)
  in
  let ht_special_tensors =
    map_array_to_hashtbl ~mapper:identity (Array.of_list special_tensors)
  in
  let open_indices indices =
    array_mapfilter
      (fun ix -> match ix with | IX_var n -> Some n | _ -> None)
      indices
  in
  let ranges_of indices_to_fix =
    try
      Array.map
	(fun name ->
	   let (_,range) = array_find (fun (n,r) -> n=name) ix_ranges in range)
	indices_to_fix
    with | Not_found ->
      failwith "Encountered named index with unknown range!"
  in
  let extend_index_fixings indices_to_fix fix_ranges fixed_so_far =
    let extra_fixings =
      array_map2 (fun x y -> (x,y))
	indices_to_fix fix_ranges
    in
      List.append (Array.to_list extra_fixings) fixed_so_far
  in
  let resolve_running_indices fixed_indices nf_term =
    let ht_explicit_contribs = Hashtbl.create 17 in
    let rec walk_contrib coeff factors_done factors_todo fixed_indices =
      match factors_todo with
	| [] ->
	    hashtbl_increase (+.)
	      ht_explicit_contribs
	      (sorted_array_of_list factors_done) coeff
	| (stem,raw_indices)::rest_factors_todo ->
	    (* let () = Printf.printf "DDD equation processor - stem=%s raw_indices=%s fixed_indices=%s\n" stem (string_array_to_string (Array.map (fun x -> match x with | IX_int n -> string_of_int n | IX_var s -> s) raw_indices)) (string_array_to_string (Array.of_list (List.map (fun (n,x) -> Printf.sprintf "%s=%d" n x) fixed_indices))) in *)
	    let indices = fix_indices fixed_indices raw_indices in
	      if Hashtbl.mem ht_special_tensors stem
	      then
		let fun_special = Hashtbl.find ht_special_tensors stem in
		let (extra_varindices,v_relevant_entries) =
		  fun_special ~ix_ranges indices
		in
		(* let () = Printf.printf "DDD local_eqn_normal_form special_tensor=%s extra_vi=%s relevant_entries=%s\n%!" stem (string_array_to_string (Array.of_list extra_varindices)) (String.concat ";" (Array.to_list (Array.map (fun (c,ff) -> Printf.sprintf "%f*%s" c (int_array_to_string (Array.of_list ff))) v_relevant_entries))) in *)
		  Array.iter
		    (fun (tensor_coeff,further_fixings) ->
		       walk_contrib (coeff*.tensor_coeff)
			 factors_done (* constant tensor is eliminated here! *)
			 rest_factors_todo
			 (List.append (List.map2 (fun x y -> (x,y)) extra_varindices further_fixings)
			    fixed_indices))
		    v_relevant_entries
	      else (* non-special (i.e. non-constant) tensor *)
		let indices_to_fix = open_indices indices in
		let fix_ranges = ranges_of indices_to_fix in
		let () = multifor fix_ranges
		  (fun _ instantiation ->
		     let new_fixed_indices = extend_index_fixings indices_to_fix instantiation fixed_indices in
		     let fixed_here = fix_indices new_fixed_indices raw_indices in
		       walk_contrib
			 coeff
			 ((stem,fixed_here)::factors_done)
			 rest_factors_todo
			 new_fixed_indices)
		in ()
    in
    let (coeff,factors) = nf_term in
    let () = walk_contrib coeff [] (Array.to_list factors) fixed_indices in
      map_hashtbl_to_array ~sorter:compare (fun factors coeff -> (coeff,factors)) ht_explicit_contribs
  in
  let nf1_rhs = nf1 rhs in
  let ((lhs_name:string),lhs_indices) = lhs in
  let lhs_open_indices = open_indices lhs_indices in
  let lhs_index_ranges = ranges_of lhs_open_indices in
  let r_result = ref [] in
  let () = multifor lhs_index_ranges
    (fun _ lhs_index_values ->
       let lhs_fixings = extend_index_fixings lhs_open_indices lhs_index_values [] in
       let lhs_fixed = fix_indices lhs_fixings lhs_indices
       in
       let rhs_fixed = array_join (Array.map (resolve_running_indices lhs_fixings) nf1_rhs) in
	 r_result:= ((lhs_name,lhs_fixed),rhs_fixed)::(!r_result)
    )
  in
  let remap_dof (name,indices) =
    (name,Array.map (fun ix -> match ix with | IX_int n -> n | _ -> impossible()) indices)
  in
    Array.map (fun (lhs,rhs) ->
		 (remap_dof lhs, Array.map
		    (fun (c,factors) ->
		       let () = logdebug (Printf.sprintf "DDD c=%f factors=%s" c (String.concat "*" (Array.to_list (Array.map (fun f -> dof_name_to_string (remap_dof f)) factors)))) in
			 (c,Array.map remap_dof factors)) rhs))
      (Array.of_list (List.rev !r_result))
;;


let parse_localeqn str =
  let parsed = parse_or_error Localeqn_lexer.token Localeqn_parser.parse_localeqn str in
  let (local_tensors,ix_ranges,v_lhs_rhs) = parsed in
    (local_tensors,Array.map (fun (lhs,rhs) -> local_equation_normal_form ~ix_ranges ~lhs ~rhs ()) v_lhs_rhs)
;;

(* Example:

parse_localeqn "%range i:3,j:3,k:3; dM(i) <- eps(i,j,k)*M(j)*H(k);";;

-->

([||],
 [|[|(("dM", [|0|]),
      [|(-1., [|("H", [|1|]); ("M", [|2|])|]);
        (1., [|("H", [|2|]); ("M", [|1|])|])|]);
     (("dM", [|1|]),
      [|(-1., [|("H", [|2|]); ("M", [|0|])|]);
        (1., [|("H", [|0|]); ("M", [|2|])|])|]);
     (("dM", [|2|]),
      [|(-1., [|("H", [|0|]); ("M", [|1|])|]);
        (1., [|("H", [|1|]); ("M", [|0|])|])|])|]|])
*)

(* XXX NOTE: the grammar does need to be sorted out to some degree here!
   The +(-1.0) actually is necessary at present!
*)


let parsed_eqn_ccode (local_tensors,vv_lhs_rhs) =
  try
    let aconcat separator a = String.concat separator (Array.to_list a) in
    let local_tensor_alloc_def_undef (stem,indices) =
      let nr_indices = Array.length indices in
      let index_weights =
	let a = Array.make nr_indices 1 in
	  begin
	    for i=0 to nr_indices-2 do
	      a.(nr_indices-2-i) <- indices.(nr_indices-2-i)*a.(nr_indices-1-i)
	    done;
	    a
	  end
      in
      let range = Array.fold_left ( * ) 1 indices in
	(Printf.sprintf " static double _%s[%d];\n" stem range,
	 Printf.sprintf "#define %s(%s) _%s[%s]\n" stem
	   (aconcat "," (Array.init nr_indices (fun n -> Printf.sprintf "ix%d" n)))
	   stem
	   (aconcat "+" (Array.init nr_indices (fun n -> Printf.sprintf "%d*ix%d" index_weights.(n) n))),
	 Printf.sprintf "#undef %s\n" stem)
    in
    let lt_allocs_defs_undefs = Array.map local_tensor_alloc_def_undef local_tensors in
    let tensor_accessor (stem,indices) =
      if Array.length indices=0
      then stem
      else
	Printf.sprintf "%s(%s)" stem
	  (aconcat "," (Array.map string_of_int indices))
    in
    let xlate_lhs_rhs ((lhs_stem,lhs_indices) as lhs,rhs) =
      Printf.sprintf "   %s = %s+0.0;\n"
	(* The "+0.0" is a dirty hack to deal with empty products... *)
	(tensor_accessor lhs)
	(aconcat "\n            +"
	   (Array.map
	      (fun (coeff,factors) ->
		 Printf.sprintf "(%g)%s" coeff
		   (aconcat ""
		      (Array.map
			 (fun factor ->
			    Printf.sprintf "*%s" (tensor_accessor factor))
			 factors)))
	      rhs))
    in
    let xlate_v_lhs_rhs v_lhs_rhs =
      if Array.length v_lhs_rhs=0 then ""
      else
	let body = aconcat "" (Array.map xlate_lhs_rhs v_lhs_rhs) in
	let ((lhs_stem,_),_) = v_lhs_rhs.(0) in
	  if(-1<>array_position_if (fun (stem,_) -> stem=lhs_stem) local_tensors 0)
	  then body
	  else
	    Printf.sprintf " if(have_%s){\n%s\n }\n" lhs_stem body
    in
      Printf.sprintf
"
{
%s

%s

%s

%s
}
"
	(aconcat "" (Array.map (fun (x,_,_) -> x) lt_allocs_defs_undefs))
	(aconcat "" (Array.map (fun (_,x,_) -> x) lt_allocs_defs_undefs))
	(aconcat "" (Array.map xlate_v_lhs_rhs vv_lhs_rhs))
	(aconcat "" (Array.map (fun (_,_,x) -> x) lt_allocs_defs_undefs))
  with | _ -> failwith "parsed_eqn_ccode failed!"
;;

let localeqn_ccode str = parsed_eqn_ccode (parse_localeqn str);;


(* For a given set of physical vectors containing primary fields, we
   have to determine the mapping to and from timestepper configuration
   vectors y, as well as the splitting of configuration vectors y.

   This is tricky in the presence of periodic boundary conditions.
*)

let layout_y_timestepper_from_primary_fields v_mwes_primary_fields =
  let vv_distrib = Array.map (fun mwe -> mwe.mwe_distribution) v_mwes_primary_fields in
    (* Evidently, we want to arrange y such that parallel distribution of the y vector
       matches parallel distribution of the mesh. This means that we have to describe
       y-components by site. As we generally take sites to be in lexicographical
       ordering, this will then automatically induce the distribution of the y-vector.
    *)
  let v_sites =
    let ht = Hashtbl.create 1987 in
    let () = array_foreach_do v_mwes_primary_fields
      (fun mwe ->
	 hashtbl_foreach_do mwe.mwe_dofs_by_site
	   (fun site _ ->
	      Hashtbl.replace ht site true))
    in
      hashtbl_keys ~sorter:compare ht
  in
  let nr_machines = Array.length v_mwes_primary_fields.(0).mwe_distribution in
  let nr_y_compts_by_machine = Array.make nr_machines 0 in
  let r_y_compts = ref [] in
  let no_dofs = [||] in
  let () =
    array_foreach_do v_sites
      (fun site ->
	 array_foreach_do_n v_mwes_primary_fields
	   (fun nr_mwe mwe ->
	      let v_dofs = try Hashtbl.find mwe.mwe_dofs_by_site site with | Not_found -> no_dofs in
		array_foreach_do v_dofs
		  (fun dof ->
		     let dof_nr = dof.dof_nr in
		     let machine = the_dof_machine mwe dof in
		     let periodic_images = mwe.mwe_dof_periodic_images.(dof_nr) in
		       if Array.length periodic_images <= 1
		       then
			 (* This is a non-miraged DOF *)
			 begin
			   r_y_compts :=  ((nr_mwe,[|dof_nr|])) :: !r_y_compts;
			   nr_y_compts_by_machine.(machine) <- 1+ nr_y_compts_by_machine.(machine);
			 end
		       else
			 (* miraged DOF *)
			 if periodic_images.(0) <> dof_nr
			 then ()
			   (* mirage copy - ignored! *)
			 else
			   begin
			     r_y_compts :=  (nr_mwe,periodic_images) :: !r_y_compts;
			     nr_y_compts_by_machine.(machine) <- 1+ nr_y_compts_by_machine.(machine);
			   end)))
  in
    (nr_y_compts_by_machine, Array.of_list (List.rev !r_y_compts))
;;

(* === BEGIN distributed Jacobian code ===

PTS: Parallel Time Stepper


*)


(* Initialising the Jacobian for the parallel timestepper is a bit tricky... *)

(* v_derive_me gives derivatives of the auxiliary fields
   entering the local equations: one

    (fun fun_make_entry nr_row extra_factor -> unit)

   for every physical auxiliary field.

   We will use lts.lts_names_phys_field_mwes for this...

 *)


let pts_jacobi_derive_me__dummy
    ~fun_make_entry
    ~callbuff_fun_make_entry_indices
    ~contrib_other_factors
    =
  ()
;;

(* Situation: we have got a primary field
   (such as 'm' in micromagnetism) and want
   to take a derivative by some m[k].
   Evidently, this just gives us a delta
   contribution.
*)

let pts_jacobi_derive_me__primary_field
    ~fun_make_entry
    ~callbuff_fun_make_entry_indices
    ~contrib_other_factors
    =
  let nr_field_le = callbuff_fun_make_entry_indices.(0)
  and ix_field_le = callbuff_fun_make_entry_indices.(1)
  and nr_field_ri = callbuff_fun_make_entry_indices.(2)
  and ix_field_ri = callbuff_fun_make_entry_indices.(3)
  in
  (* let () = Printf.printf "DDD fun_make_entry %d %d %d %d %f\n%!" nr_field_le ix_field_le nr_field_ri ix_field_ri contrib_other_factors in *)
    fun_make_entry
      nr_field_le ix_field_le nr_field_ri ix_field_ri
      contrib_other_factors
;;


(* Note: for this to work, it is ESSENTIAL that the distribution of
   differential operators must match the distribution which we use
   for the sites when we compute the Jacobian.
*)

let pts_jacobi_derive_me__ddiffop v_mwes nr_mwe_le nr_mwe_ri mx =
  let mwe_le = v_mwes.(nr_mwe_le) in
  let () = mwe_ensure_has_volumes mwe_le in
  let v_vols_le = match !(mwe_le.mwe_dof_funs_volumes)
    (* We need these volumes for the "box method" cofield-to-field mapping! *)
  with
    | None -> impossible()
    | Some x -> x
  in
    (fun
       ~fun_make_entry
       ~callbuff_fun_make_entry_indices
       ~contrib_other_factors
       ->
	 let nr_field_le = callbuff_fun_make_entry_indices.(0)
	 and ix_field_le = callbuff_fun_make_entry_indices.(1)
	 and nr_field_ri = callbuff_fun_make_entry_indices.(2)
	 and ix_field_ri = callbuff_fun_make_entry_indices.(3)
	 in
	   Mpi_petsc.petsc_matrix_call_on_rows
	     (* Note that for this to work properly, it is absolutely essential
		that the site distribution employed for the Jacobian is the same
		as that employed for the differential operators!
	     *)
	     mx
	     ~start_row:ix_field_ri
	     ~end_row:(1+ix_field_ri)
	     (fun nr_row ba_indices ba_vals ->
		let nr_entries = Bigarray.Array1.dim ba_indices in
		  for i=0 to nr_entries-1 do
		    (* let () = Printf.printf "OP row=%d entry=%d/%d col=%d val=%f contrib=%f\n%!" nr_row i nr_entries (Nativeint.to_int ba_indices.{i}) ba_vals.{i} (ba_vals.{i}*.contrib_other_factors/.v_vols_le.(ix_field_le)) in *)
		    (* let () = Printf.printf "DDD fun_make_entry %d %d %d %d %f\n%!" nr_field_le ix_field_le nr_mwe_ri (Nativeint.to_int ba_indices.{i}) (ba_vals.{i}*.contrib_other_factors/.v_vols_le.(ix_field_le)) in *)
		      fun_make_entry
			nr_field_le ix_field_le
			nr_mwe_ri (Nativeint.to_int ba_indices.{i})
			(ba_vals.{i}*.contrib_other_factors/.v_vols_le.(ix_field_le))
		  done)
    )
;;


(* Compute derivatives of functions which are polynomial in degrees of
   freedoms which live at the site under consideration only (and which do
   not involve any spatial derivatives).

   In the context of micromagnetism (nmag), this can be used to work out
   contributions to the Jacobian from the anisotropy. *)

let pts_jacobi_derive_me__localpolynomial ccpla v_mwes equation =
  let comm = ccpla.ccpla_comm in
  let nr_cpu = Mpi_petsc.comm_rank comm in
  let (local_tensors,vv_lhs_rhs) = parse_localeqn equation in
  let v_lhs_rhs = array_join vv_lhs_rhs in
    (* vv_lhs_rhs was one expansion per equation line in the symbolic formula.
       Note: the user must not feed in equations that build on one another here!
    *)
  let () =
    (if Array.length local_tensors <> 0
     then failwith "Jacobian: polynomial equation uses local tensors. It should not (for now. This could be fixed, with some effort)."
     else ())
  in
  let subfields_to_derive_by = Array.map (fun (stem,_) -> stem) v_mwes.(0).mwe_subfields in
  let is_derivative_dof ((dof_stem,_) as dof_name) =
    -1 <> array_position dof_stem subfields_to_derive_by 0
  in
  let matrix_thunk_derivatives =
    let ht = Hashtbl.create 17 in
    let () = array_foreach_do v_lhs_rhs
      (fun (dof_name_lhs,v_summands) ->
	 array_foreach_do v_summands
	   (fun (coeff,v_factors) ->
	      array_foreach_do_n v_factors
		(fun nr_factor factor_dof ->
		   if not(is_derivative_dof factor_dof)
		   then ()
		   else
		     let other_factors = array_one_shorter v_factors nr_factor in
		       hashtbl_push ht (dof_name_lhs,factor_dof)
			 (coeff,other_factors))))
    in
      (* Now, our (dof_dm,dof_m) hash table entries may have multiple unify-able entries... *)
    let ht_normalized = Hashtbl.create 17 in
    let h = Hashtbl.create 17 in
    let () = Hashtbl.iter
      (fun key li_summands ->
	 let () = Hashtbl.clear h in
	 let () = List.iter
	   (fun (coeff,other_factors) -> hashtbl_increase (+.) h other_factors coeff)
	   li_summands
	 in
	   Hashtbl.add ht_normalized key
	     (array_filter
		(fun (c,_) -> c<>0.0)
		(map_hashtbl_to_array ~sorter:(fun (_,f1) (_,f2) -> compare f1 f2)
		   (fun factors coeff -> (coeff,factors)) h)))
      ht
    in
      ht_normalized
  in
  let (site_structure,vv_site_info,_) =
    site_wise_structure_and_offsets_and_defines
      ~parallel:true
      v_mwes
      [||] (* cofield mwes *)
      [||] (* aux arg names *)
  in
  let ddd = Printf.printf "DDD NOTE TODO: site_wise_structure_and_offsets_and_defines can be improved considerably!\n%!" in
    (* Note: a site definition is (str_nr_field_indices,v_offsets,pos).
       Note furthermore that an offset of (-1) means that the corresponding
       DOF is not present at that site.
    *)
  let site_info_offset_by_dof_name =
    let ht = Hashtbl.create 17 in
    let () = Array.iteri (fun n dof_name -> Hashtbl.add ht dof_name n) site_structure in
      ht
  in
    (* XXX "proofread" up to here. The whole thing is quite complicated.

    *)
    fun
      ~fun_make_entry
      ~callbuff_fun_make_entry_indices
      ~contrib_other_factors
      ->
	failwith "WRITE ME"
;;

(*
	let () = array_foreach_do vv_site_info.(nr_cpu)
	  (fun (str_nr_field_indices,v_offsets,pos) ->
	     hashtbl_foreach_do matrix_thunk_derivatives
	       (fun (dof_name_lhs,dof_name_deriv) v_summands ->
		  (* We have to ensure that this particular site does indeed have
		     all the relevant DOFs. This is a two-step process. First,
		     we have to check dof_name_lhs and dof_name_deriv, then,
		     for every summand, all its factors.
		  *)
		  let site_info_ix_lhs = Hashtbl.find site_info_offset_by_dof_name dof_name_lhs
		  and site_info_ix_deriv = Hashtbl.find site_info_offset_by_dof_name dof_name_deriv
		  in
		    if v_offsets.(site_info_ix_lhs) = (-1)
		      || v_offsets.(site_info_ix_deriv) = (-1)
		    then ()
		    else
		      Array.iter
			(fun (coeff,v_factors) ->
			   let nr_factors = Array.length v_factors in
			   let v_factor_offset_indices =
			     Array.map (Hashtbl.find site_info_offset_by_dof_name) v_factors
			   in
			     if !(array_all_satisfy (fun ix -> -1 <> v_offsets.(ix)) v_factor_offset_indices)
			     then ()
			     else
			       let contrib =
				 (coeff,
				  (let s = String.create nr_factors in
				   let () =
				     for i=0 to nr_factors-1 do
				       s.[i] <- str_nr_field_indices.[v_factor_offset_indices.(i)];
				     done
				   in
				     s),
				  (Array.map (fun off_ix -> v_offsets.(off_ix)) v_factor_offset_indices))
			       in
			       let key = (v_offsets.(site_info_ix_lhs),
					  v_offsets.(site_info_ix_deriv))
			       in
				 hashtbl_push ht_contribs key contrib)
			v_summands))
	in
  let all = Array.make (Array.length v_mwes.(0).mwe_dofs) [||] in
  let () = hashtbl_foreach_do ht_contribs
    (fun ((ix_lhs,_) as key) li_summands ->
       all.(ix_lhs) <- Array.append [|(key, Array.of_list li_summands)|] all.(ix_lhs))
  in
    fun n -> all.(n)
;;

    ~fun_make_entry
    ~callbuff_fun_make_entry_indices
    ~contrib_other_factors
    =
  failwith "XXX write me!"
;;
*)

(*

  The pts_jacobi_vivificator is the central part of the Jacobi
  calculation.

  As every vivificator this has two stages:

  1. when it is called, we set up a lot of context and produce a
  closure over this compiled context which allows fast look-ups and
  fast execution of the resulting function.

  2. when the resulting function is used, this context allows fast
  recomputation of the Jacobian.



  This pts_jacobi_vivificator makes a function that fills the parallel
  timestepper Jacobian. This is a collective (MPI) command so that
  each machine fills its own share of the Jacobi matrix.

  val pts_jacobi_vivificator :
  ?nr_cpu:int ->
  v_mwes:'a Fem.mesh_with_elements array ->
  v_derive_me:(fun_make_entry:'b ->
  callbuff_fun_make_entry_indices:int array ->
  contrib_other_factors:float -> 'c)
  array ->
  local_equations:((string * int array) *
  (float * (string * int array) array) array)
  array ->
  (float, 'd, 'e) Bigarray.Array1.t array -> 'b -> unit

  where

  * v_mwes: Vector (array) Mesh With ElementS. Needed to obtain
  site-related indices of the degrees of freedom that occur in the
  equations of motion.

  * v_derive_me: This is a vector of specialist functions indexed in
  the same way as v_mwes, i.e. the first entry in v_derive_me
  corresponds to the first entry in v_mwes. Every such specialist
  tells what the derivatives of the degrees of freedom in the
  corresponding mwe are with respect to the primary degrees of
  freedom.

  For example, v_mwes = [mwe_m,mwe_dmdt,mwe_H_total,mwe_H_anis,mwe_pin]
  and v_derive_me is generated internally from this Python specification:

  phys_field_derivs=[("PRIMARY",""),
  ("PRIMARY",""),
  ("OPERATOR","op_H_exch"),
  ("IGNORE",""),
  ("IGNORE",""),]

  so that we get v_derive_me = [< pts_jacobi_derive_me__primary_field closure >,
  < pts_jacobi_derive_me__primary_field closure >,
  < pts_jacobi_derive_me__ddiffop closure >,
  < pts_jacobi_derive_me__dummy closure >,
  < pts_jacobi_derive_me__dummy closure >]

  It is a requirement that the first mwe contains the primary
  degrees of freedom (i.e. 'm' in nmag) and the next one contains
  the degrees of freedom of the left hand side of the equation of
  motion (i.e. 'dm/dt' in nmag).

  See also nmag_lam.py.

  * local_equations: Equations of motion as returned by equation
  parser (see nsim_grammars/localeqn_parser.mly). The physics has
  to be specified such that equations of motion involve only
  quantities of the local site. Therefore, contributions from
  differential operators have to be computed beforehand (such as
  H_exch for nmag).

  and this returns

  * a function with signature
  (float, 'd, 'e) Bigarray.Array1.t array -> 'b -> unit,
  which is the vivificator.

  This function requires an array containing all the data on the
  present values of the fields. (Note: if we run this in parallel the
  vectors have to be complete non-distributed, i.e. all machines have
  to have all data of all the fields.) In addition, it requires a
  function (type 'b) that adds a contribution to the Jacobi matrix.

  The idea is to zero out the Jacobi matrix, and then call this
  vivificator which will (re-)populate the matrix with subsequent
  calls of the function of type 'b.


  Design flaw:

  As it is right now we cannot do the derive_me_localpolynomial due
  to missing information (which cannot be provided with the current
  interface).

  Notes added 12 May 2008 (TF,MF,HF)

*)


let pts_jacobi_vivificator
    ?nr_cpu
    ~v_mwes ~v_derive_me ~local_equations =
  let mesh = v_mwes.(0).mwe_mesh in
  let mwe_sites mwe = mwe.mwe_sites in
  let v_sites = mwe_sites v_mwes.(0) in
  let () =
    (if (array_all_satisfy (fun mwe -> mwe_sites mwe = v_sites) v_mwes)
     then ()
     else failwith "Fatal: Jacobian: MWEs have non-uniform site structure!")
  in
  let local_eqns_max_nr_factors =
  (* determine the maximum degree local_eqns_max_nr_factors of the
     polynomial(s) in the equation of motion. This is needed to create
     offset buffers (buf_factors_fields and buf_factors_offsets) which
     have sufficient length to accommodate information about all the
     factors of every summand. *)
    Array.fold_left
      (fun sf (_,v_poly) ->
	 Array.fold_left
	   (fun sf2 (coeff,v_dofs) -> max sf2 (Array.length v_dofs))
	   sf v_poly)
      0
      local_equations
  in
    (* let ddd = Printf.printf "LOCAL-EQN: MAX NR FACTORS: %d\n%!" local_eqns_max_nr_factors in *)
  let buf_factors_fields = Array.make local_eqns_max_nr_factors 0 in
  let buf_factors_offsets = Array.make local_eqns_max_nr_factors 0 in
  let site_on_this_cpu site =
    (* return true or false depending on whether the given site has been allocated to this CPU *)
    match nr_cpu with
      | None -> true
      | Some this_cpu ->
	  let lowest_point_index = site.(0) in
	  let distrib = mesh.mm_vertex_distribution in
            if Array.length distrib=0 then true
            else
               let rec the_cpu nr_cpu offset =
                 if lowest_point_index < offset then
                   nr_cpu
                 else
                   let next_cpu = nr_cpu + 1 in
                   if next_cpu < Array.length distrib then
                     the_cpu next_cpu (offset + distrib.(next_cpu))
                   else
                     let () = Printf.fprintf
                                stderr
                                "Broken mesh vertex distribution!\n%!"
                     in
                     (* ^ make sure this gets printed even when the line below
                        sends us into Nirvana. *)
                       failwith "Broken mesh vertex distribution!"
              in (this_cpu = (the_cpu 0 distrib.(0)))
  in
  let the_dof_nr_field_and_index_range =
  (* This hash table the_dof_nr_field_and_index_range maps a degree of
     freedom stem (such as 'H_exch_Py') to the index of the field in
     v_mwes that is responsible for this, as well as the shape of the
     tensor, e.g. [3] for the exchange field.  *)
    let ht = Hashtbl.create 17 in
    let () =
      array_foreach_do_n v_mwes
	(fun nr_mwe mwe ->
	   array_foreach_do mwe.mwe_subfields
	     (fun (stem,indices) ->
		Hashtbl.replace ht stem (nr_mwe,indices)))
    in (fun (stem,_) -> Hashtbl.find ht stem)
  in
  let the_dof_nr mwe (dof_stem,dof_indices) site =
  (* The the_dof_nr_mwe function will -- for a given DOF stem+indices
     (and its mwe) at a given site -- find the corresponding dof_nr
     index, or -1 if it doesn't exist.  Note: this all eventually should
     be streamlined and combined with our SWEX techniques.

     This is long and complicated and could be improved. As long as
     you think it works okay, don't look.  *)
    try
      let dofs_this_site =
	Hashtbl.find mwe.mwe_dofs_by_site site
      in
	(* let () = Printf.printf "SITE=%s dofs=%s\n%!" (int_array_to_string site) (int_array_to_string (Array.map (fun dof -> dof.dof_nr) dofs_this_site)) in *)
      let nr_dofs_this_site = Array.length dofs_this_site in
      let rec seek offset =
	if offset=nr_dofs_this_site
	then -1
	else
	  let (leading_dof_stem,leading_dof_indices) as leading_dof =
	    the_dof_name mwe dofs_this_site.(offset)
	  in
	  (* let () = Printf.printf "Leading DOF: %s wanted: %s\n%!" (dof_name_to_string leading_dof) dof_stem in *)
	    if leading_dof_stem <> dof_stem
	    then
	      let step =
		let (_,ixrange) =
		  the_dof_nr_field_and_index_range leading_dof
		in Array.fold_left ( * ) 1 ixrange
	      in seek (offset+step)
	    else
	      (* DOF stem matches.
		 Now have to find the proper index.
		 NOTE: for now, we do this in a trivial and
		 straightforward manner. This is reasonable
		 as long as the most complicated situations
		 we encounter are not much more complex than
		 small numbers (say, 1) of vector DOFs.
		 Once we do much long-index higher-tensor stuff,
		 this should be refined.
	      *)
	      let rec seek_step1 offset =
		let (_,dof_indices_this_site) =
		  the_dof_name mwe dofs_this_site.(offset)
		in
		  if dof_indices_this_site = dof_indices
		  then dofs_this_site.(offset).dof_nr
		  else seek_step1 (1+offset)
	      in seek_step1 offset
      in
	seek 0
    with | Not_found -> -1
  in
  let r_this_eqn_is_applicable = ref true in
    (* We keep a flag around where we register if a particular equation
       turned out to be non-applicable at some site as some of the DOFs
       it mentions are not present there.
    *)
  let callbuff_fun_make_entry_indices = Array.make 4 0 in
  let process_site v_ba_fields_msv fun_make_entry site =
    let myrank = (match nr_cpu with | None -> -1 | Some n -> n) in
    (* let () = Printf.printf "[Node=%d] JVIV site=%s\n%!" myrank (int_array_to_string site) in *)
    for nr_eqn=0 to Array.length local_equations-1 do
      (* let () = Printf.printf "[Node=%d] eqn=%d\n%!" myrank nr_eqn in *)
      let (eom_dof_lhs,eom_polynomial) = local_equations.(nr_eqn) in
      let (nr_field_lhs, _) =
	the_dof_nr_field_and_index_range eom_dof_lhs
      in
      let ix_lhs = the_dof_nr v_mwes.(nr_field_lhs) eom_dof_lhs site in
      (* let () = Printf.printf "[Node=%d] ix_lhs=%d site=%s\n%!" myrank ix_lhs (int_array_to_string site) in *)
      let () = r_this_eqn_is_applicable := (ix_lhs <> -1) in
	for nr_summand=0 to Array.length eom_polynomial-1 do
	  let (coeff,dof_factors) = eom_polynomial.(nr_summand) in
	  let nr_factors = Array.length dof_factors in
	    (* Prepare (nr_field,field_index) data for all DOF factors *)
	  (* let () = Printf.printf "[Node=%d] nr_summand=%d/%d\n%!" myrank nr_summand (Array.length eom_polynomial) in *)
	  let () =
	    for nr_f = 0 to nr_factors-1 do
	      let dof_factor = dof_factors.(nr_f) in
	      (* let () = Printf.printf "[Node=%d] nr_factor=%d/%d %s\n%!" myrank nr_f nr_factors (dof_name_to_string dof_factor) in *)
	      let (factor_nr_field,_) =
		the_dof_nr_field_and_index_range dof_factor
	      in
	      let () = buf_factors_fields.(nr_f) <- factor_nr_field in
	      let factor_ix =
		the_dof_nr v_mwes.(factor_nr_field) dof_factor site
	      in
	      (* let () = Printf.printf "[Node=%d] factor_ix=%d\n%!" myrank factor_ix in *)
	      let () = buf_factors_offsets.(nr_f) <- factor_ix in
		(if factor_ix = -1
		 then r_this_eqn_is_applicable := false
		 else ())
	    done
	  in
	  (* let () = Printf.printf "[Node=%d] JVIV Done Summands\n%!" myrank in *)
	    (if not !r_this_eqn_is_applicable
	     then ()
	     else
	       (* Now, we can do the derivatives. *)
	       for pos_deriv = 0 to nr_factors-1 do
		 let rec walk_other_factors nr_factor contrib =
		   if nr_factor = nr_factors then contrib
		   else if nr_factor = pos_deriv
		   then walk_other_factors (1+nr_factor) contrib
		   else
		     let this_factor = v_ba_fields_msv.(buf_factors_fields.(nr_factor)).{buf_factors_offsets.(nr_factor)} in
		     let () =
		       let fc = classify_float this_factor in
			 if fc <> FP_zero && fc <> FP_normal
			   || (abs_float this_factor > 1e20)
			 then Printf.printf "ROGUE FACTOR: buf_factors_fields=%s buf_factors_offsets=%s nr_factor=%d factor=%f\n%!" (int_array_to_string buf_factors_fields) (int_array_to_string buf_factors_offsets) nr_factor this_factor
			 else ()
		     in
		       walk_other_factors
			 (1+nr_factor)
			 (contrib*.this_factor)
		 in
		 let contrib_other_factors = walk_other_factors 0 coeff in
		 let () =
		   begin
		     callbuff_fun_make_entry_indices.(0) <- nr_field_lhs;
		     callbuff_fun_make_entry_indices.(1) <- ix_lhs;
		     callbuff_fun_make_entry_indices.(2) <- buf_factors_fields.(pos_deriv);
		     callbuff_fun_make_entry_indices.(3) <- buf_factors_offsets.(pos_deriv)
		   end
		 in
		 (* let () = Printf.printf "[Node=%d] call v_derive_me indices=%s\n%!" myrank (int_array_to_string callbuff_fun_make_entry_indices) in *)
		   v_derive_me.(buf_factors_fields.(pos_deriv))
		     ~fun_make_entry
		     ~callbuff_fun_make_entry_indices
		     ~contrib_other_factors
	       done
	    )
	done (* nr_summand *)
    done (* nr_eqn *)
  in
    (fun
       v_ba_fields_msv (* multi-sequential field vectors to draw upon *)
       fun_make_entry
       ->
	 for nr_site = 0 to Array.length v_sites-1 do
	   let site = v_sites.(nr_site) in
	   if site_on_this_cpu site then
	     (* let () = Printf.printf "VIV SITE %s\n%!" (int_array_to_string site) in *)
	     process_site v_ba_fields_msv fun_make_entry site
	   else ()
	 done
    )
;;

(* === END distributed Jacobian code === *)

let local_equations_to_c_code eom =
  let dof_ccode (dof_stem,indices) =
    if Array.length indices=0
    then dof_stem
    else
      Printf.sprintf "%s(%s)" dof_stem (String.concat "," (Array.to_list indices))
  in
  let c_pieces =
    Array.map
      (fun ((dof_stem,indices) as dof_name,contribs) ->
	 let contrib_c_pieces =
	   Array.map
	     (fun (coeff,factors) ->
		Printf.sprintf "(%f)*%s"
		  coeff
		  (String.concat "*"
		     (Array.to_list (Array.map dof_ccode factors))))
	     contribs
	 in
	   Printf.sprintf "if(have_%s){%s=%s;}\n" dof_stem (dof_ccode dof_name)
	     (String.concat "+" (Array.to_list contrib_c_pieces)))
      eom
  in String.concat "\n" (Array.to_list c_pieces)
;;

(* BAD HACK: JUST AN EXPERIMENT. TO BE FIXED SOON. mf *)
let time_in_rhs = ref 0.0;;

let nsim_opcode_interpreter ccpla op v_distributed_resources =
(*  let () = Printf.printf
             "nsim_opcode_interpreter: executing %s\n%!"
             (nsim_opcode_to_string op) in*)
  let comm = ccpla.ccpla_comm in
  match op with
    | NSIM_OP_swex_create (name,c_code,site_info_this_machine,defines) ->
	let swex = site_wise_executor site_info_this_machine defines c_code in
	  [|(name,DRES_opdata (NSIM_RES_swex swex,do_nothing))|]
    | NSIM_OP_swex (nr_fields,param_names) ->
	let nr_cofields = (Array.length v_distributed_resources)-nr_fields-2 in
	let DRES_opdata (NSIM_RES_swex swex,_) = v_distributed_resources.(0) in
	let DRES_parameters (all_param_names,all_param_vals) = v_distributed_resources.(1) in
	let params =
	  Array.map
	    (fun name ->
	       let p = array_position name all_param_names 0 in
		 all_param_vals.(p))
	    param_names
	in
	let v_petsc_fields =
	  Array.init nr_fields
	    (fun n -> let DRES_petsc_vector (_,_,v) = v_distributed_resources.(2+n) in v)
	in
	let v_petsc_cofields =
	  Array.init nr_cofields
	    (fun n -> let DRES_petsc_vector (_,_,v) = v_distributed_resources.(n+2+nr_fields) in v)
	in
	let result = swex params v_petsc_fields v_petsc_cofields in
	  (* We do not care whether site-wise execution was stopped early... *)
	  [||]
    | NSIM_OP_mwes_create (name,opt_geom,opt_mwes) ->
	let mwes =
	  match opt_mwes with
	    | Some master_mwes ->
		(* The master has to do more than just mirroring back
		   the array of mwes he already has: we also have
		   to broadcast geometry data on the mesh:
		*)
		let mesh = master_mwes.(0).mwe_mesh in
		let mpi_geom = Mesh.mesh_to_mpi_meshgeom mesh in
		let dim = mpi_geom.mmg_dim in
		let layout =
		  (mpi_geom.mmg_dim,
		   Bigarray.Array1.dim mpi_geom.mmg_vertex_coords,
		   Bigarray.Array1.dim mpi_geom.mmg_simplices,
		   Bigarray.Array1.dim mpi_geom.mmg_simplex_regions)
		in
		let () =
		  begin
		    ignore(Mpi_petsc.broadcast layout 0 comm);
		    Mpi_petsc.broadcast_bigarray_float mpi_geom.mmg_vertex_coords 0 comm;
		    Mpi_petsc.broadcast_bigarray_nativeint mpi_geom.mmg_simplices 0 comm;
		    Mpi_petsc.broadcast_bigarray_nativeint mpi_geom.mmg_simplex_regions 0 comm;
		    ignore(Mpi_petsc.broadcast mpi_geom.mmg_periodicity 0 comm);
		    ignore(Mpi_petsc.broadcast mpi_geom.mmg_vertex_distribution 0 comm);
		  end
		in
		  master_mwes
	    | None ->
		match opt_geom with
		  | None -> failwith "NSIM_OP_mwes_create: impossible situation - neither master_mwes nor geometry information provided!"
		  | Some v_madeby ->
		      (* We first have to fetch the mesh over the network... *)
		      let (dim,len_vertex_coords,len_simplices,len_regions) =
			Mpi_petsc.broadcast (-1,-1,-1,-1) 0 comm
		      in
		      let ba_vertex_coords =
			Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout len_vertex_coords
		      in
		      let ba_simplices =
			Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout len_simplices
		      in
		      let ba_regions =
			Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout len_regions
		      in
		      let () =
			begin
			  Mpi_petsc.broadcast_bigarray_float ba_vertex_coords 0 comm;
			  Mpi_petsc.broadcast_bigarray_nativeint ba_simplices 0 comm;
			  Mpi_petsc.broadcast_bigarray_nativeint ba_regions 0 comm;
			end
		      in
		      let periodicity = Mpi_petsc.broadcast [||] 0 comm in
		      let vdist = Mpi_petsc.broadcast [||] 0 comm in
		      let mesh =
			Mesh.mpi_meshgeom_to_mesh
			  {
			    mmg_dim=dim;
			    mmg_vertex_coords=ba_vertex_coords;
			    mmg_simplices=ba_simplices;
			    mmg_simplex_regions=ba_regions;
			    mmg_periodicity=periodicity;
			    mmg_vertex_distribution=vdist;
			  }
		      in
			(* Now we face an awkward problem: some of our mwes are primary,
			   while others are not. Evidently, we have to generate the primary ones first.
			   But as the derived ones are generated from primary ones, a two-stage process
			   will suffice to make them all...
			*)
		      let nr_mwes = Array.length v_madeby in
		      let ht_mwe_by_name = Hashtbl.create 17 in
		      let opt_mwes =
			Array.map
			  (fun madeby ->
			     match madeby with
			       | MWEMB_make_mwe (name,properties_by_region,regions,sx_to_el) ->
				   (* let ddd = Printf.printf "DDD reconstructing mwe %s: regions %s\n%!" name (int_array_to_string regions) in *)
				   let mwe =
				     Fem.make_mwe
				       name
				       ~fun_outer_region:(fun nr_site pos -> regions.(nr_site))
				       ~properties_by_region
				       sx_to_el
				       mesh
				   in
				   let () = logdebug (Printf.sprintf "reconstructed mwe \"%s\" CPU %d distrib=%s\n%!" mwe.mwe_name (Mpi_petsc.comm_rank comm) (int_array_to_string mwe.mwe_distribution)) in
				   let () = Hashtbl.add ht_mwe_by_name name mwe in
				     Some mwe
			       | _ -> None
			  ) v_madeby
		      in
			Array.mapi
			  (fun nr_mwe opt_mwe ->
			     let mwe =
			       match opt_mwe with
				 | Some mwe -> mwe
				 | None ->
				     match v_madeby.(nr_mwe) with
				       | MWEMB_mwe_sibling (new_name,rprefix,dof_renamings,mwe_name) ->
					   let orig_mwe = Hashtbl.find ht_mwe_by_name mwe_name in
					   let mwe = Fem.mwe_sibling new_name rprefix dof_renamings orig_mwe in
					     mwe
				       | _ -> impossible()
			     in
			       mwe
			  )
			  opt_mwes
	in
	  [|(name,DRES_opdata (NSIM_RES_mwes mwes,do_nothing))|]
    | NSIM_OP_vivificators_create (name_vivs,viv_specifiers) ->
	let DRES_opdata (NSIM_RES_mwes v_mwes,_) = v_distributed_resources.(0) in
	let get_mwe name =
	  let pos = array_position_if (fun mwe -> mwe.mwe_name = name) v_mwes 0 in
	    v_mwes.(pos)
	in
	let named_vivs =
	  Array.map
	    (fun (name_this_viv,ddiffop,name_mwe_le,name_mwe_ri,opt_name_mwe_mid) ->
	       let mwe_le = get_mwe name_mwe_le
	       and mwe_ri = get_mwe name_mwe_ri
	       and opt_mwe_mid = optionally get_mwe opt_name_mwe_mid
	       in
	       let viv =
		 Fem.ddiffop_vivified
		   ~nr_cpu:(Mpi_petsc.comm_rank comm)
		   ddiffop
		   ?mwe_mid:opt_mwe_mid mwe_le mwe_ri
	       in
		 (name_this_viv,viv))
	    viv_specifiers
	in
	  [|(name_vivs,DRES_opdata (NSIM_RES_vivificators named_vivs,do_nothing))|]
    | NSIM_OP_vivificator_exec (name_viv,is_this_a_revivification,do_we_need_to_assemble_after,opt_mwe_name_field_mid) ->
	(*let ddd = Printf.printf "DDD [Node %d] NSIM_OP_vivificator_exec '%s'\n%!" (Mpi_petsc.comm_rank comm) name_viv in
	let () =
	  let mem = memstats() in
            loginfo (* logdebug *)
	      (Printf.sprintf "VMEM=%8.0f KB RSS=%8.0f KB vivifying..." mem.(0) mem.(1))
        in*)
	let () =
	  (if Mpi_petsc.comm_rank comm =0 then
	     logdebug (Printf.sprintf "NSIM_OP_vivificator_exec '%s'" name_viv)
	   else ())
	in
	let DRES_opdata (NSIM_RES_vivificators v_vivs,_) = v_distributed_resources.(0) in
	let mx = dres_get_matrix v_distributed_resources.(1) in
	let DRES_opdata (NSIM_RES_mwes v_mwes,_) = v_distributed_resources.(2) in
	let field_mid =
	  match opt_mwe_name_field_mid with
	    | None -> None
	    | Some mwe_name ->
		let mwe_mid = array_find (fun mwe -> mwe.mwe_name = mwe_name) v_mwes in
		let vec = dres_get_vector_multiseq v_distributed_resources.(3) in
		let field_mid = Fem.FEM_field (mwe_mid,None,vec) in
		  Some field_mid
	in
	let viv =
	  let pos = array_position_if (fun (name,_) -> name = name_viv) v_vivs 0 in
	  let () =
	    (if pos = -1
	     then failwith "Fatal: could not find vivificator '%s' (known: %s)"
	       name_viv (string_array_to_string (Array.map (fun (n,_) -> n) v_vivs)) else ())
	  in
	  let (_,viv) = v_vivs.(pos) in viv
	in
	let () = (if is_this_a_revivification then Mpi_petsc.matrix_zero_entries mx else ()) in
	let _ =
	  viv
	    ~fun_make_matrix:(fun _ _ -> mx)
	    ~fun_finish_matrix:(fun mx -> Mpi_petsc.matrix_assemble mx true)
	    ?field_mid
	    (fun _ row col coeff -> Mpi_petsc.matrix_inc mx row col coeff)
	in
	let () = (if not is_this_a_revivification then Mpi_petsc.matrix_reincarnate mx else ()) in
	let () = (if do_we_need_to_assemble_after then Mpi_petsc.matrix_assemble mx true else ()) in
	  (* XXX NOTE: this does not work, as call_on_rows() will just flatly deny operating on non-local rows!
	     let ddd =
	     if name_viv = "bigbar_par_op_grad_phi_viv"
	     then
	     Mpi_petsc.petsc_matrix_call_on_rows mx
	     (fun nr_row cols vals ->
	     (* Printf.printf "grad_phi N=%d row=%4d: %d entries\n%!" (Mpi_petsc.comm_rank comm) nr_row (Bigarray.Array1.dim cols)) *)
	     else ()
	     in
	  *)
	  [||]
    | NSIM_OP_jacobi_operators_create (name,v_names) ->
	let v_names_and_mxs =
	  array_map2
	    (fun name dres ->
	       let mx = dres_get_matrix dres in
		 (name,mx)) v_names v_distributed_resources
	in
	  [|(name,DRES_opdata (NSIM_RES_jacobi_operators v_names_and_mxs,do_nothing))|]
    | NSIM_OP_fill_bem (ldms) ->
	let myrank = Mpi_petsc.comm_rank comm in
	let DRES_opdata (NSIM_RES_mwes v_mwes,_) = v_distributed_resources.(0) in
	let mx = dres_get_matrix v_distributed_resources.(1) in
	let (dof_stem,_) = ldms.ldms_dof_name in
	let mwe =
	  let pos = array_position_if (fun mwe -> mwe.mwe_name = ldms.ldms_mwe_name) v_mwes 0 in
	    v_mwes.(pos)
	in
	let (lts,stl,shortvec_distrib) = boundary_shortvec_info dof_stem mwe ldms.ldms_boundary_spec
	in
	let shortvec_offset=
	  let rec walk sum pos =
	    if pos=myrank then sum
	    else walk (sum+shortvec_distrib.(pos)) (1+pos)
	  in walk 0 0
	in
	(* let ddd = Printf.printf "DDD Doing 3d geom info\n%!" in *)
	let geom_info =
	  Bem3d.mwe_dof_3d_surface_triangles_and_space_angles_and_coords
	    ~inside_property:"material"
	    mwe (lts,stl)
	in
	let rows_to_fill = Array.init shortvec_distrib.(myrank) (fun n -> n+shortvec_offset) in
	(* let ddd = Printf.printf "DDD [Node %d] NSIM_OP_fill_bem (len=%d/%d) rows=%s\n%!" (Mpi_petsc.comm_rank comm) (Array.length lts) (Array.length stl) (int_array_to_string rows_to_fill) in *)
	let () =
	  Bem3d.nas_partially_fill_bem_matrix_row_constrained
	    ~geom_info
	    ?lattice_info:ldms.ldms_lattice_info
	    ~fun_add_contrib:(Mpi_petsc.matrix_inc mx)
	    ~fun_assemble_matrix:(Mpi_petsc.matrix_assemble mx)
	    ~rows_to_fill
	    ~inside_property:"material"
	    ~outside_property:"outer"
	    (* XXX specify field name stem! *)
	    ldms.ldms_dof_name mwe
	in [||]
    | NSIM_OP_advance_timestepper (exact_tstop, t_final, maxits) ->
	let DRES_opdata ((NSIM_RES_timestepper pts),_) = v_distributed_resources.(0) in
	let t_reached = pts.pts_advance ~exact_tstop t_final maxits in
	let () = pts.pts_time_reached <- t_reached in
	  [||]
    | NSIM_OP_init_timestepper (initial_time,rel_tol,abs_tol) ->
	let DRES_opdata ((NSIM_RES_timestepper pts),_) = v_distributed_resources.(0) in
	let () = pts.pts_set_initial_from_phys ~initial_time ~rel_tol ~abs_tol () in
	  [||]
    | NSIM_OP_hmatrix_mult (opt_hmx_buffers) ->
	let myrank = Mpi_petsc.comm_rank comm in
	let (opt_hmx,buf_src,buf_dst) = match opt_hmx_buffers with
	  | None ->
	      (* This should only happen for rank>0, i.e. not on the master node *)
	      (None,Ccpla.vec_dummy,Ccpla.vec_dummy)
	  | Some (mx,s,d) -> (Some mx,s,d)
	in
	let v_par_src = v_distributed_resources.(0) in
	let v_par_dst = v_distributed_resources.(1) in
	let (ba_lengths,ba_offsets) = Ccpla.dres_get_vector_lengths_offsets v_par_src in
	let v_src = Ccpla.dres_get_vector v_par_src in
	let v_dst = Ccpla.dres_get_vector v_par_dst in
	let () =
	  begin
	    Mpi_petsc.vec_gather comm buf_src v_src ba_lengths ba_offsets;
	    (match opt_hmx with
	       | Some h -> Bem3d.apply_bem_hmatrix h buf_dst buf_src
	       | None -> ());
	    Mpi_petsc.vec_scatter comm buf_dst v_dst ba_lengths ba_offsets;
	  end
	in [||]
    | NSIM_OP_make_timestepper (name_ts,lts) ->
	(* C4R (mf, 2 Jun 2008): let ddd = Printf.fprintf stderr "DDD Note: (Harmless design flaw, should be repaired nevertheless)\n timestepper does not hold on to the DRHs of the distributed resources it uses.\n It must (otherwise, we may get GC trouble in later versions of nsim, though not this one)!\n%!"
	in*)
	let () =
	  Sundials_sp.sundials_init
	    ~path_cvode:(sundials_path Nsimconf.sundials_cvode_lib) (*"libsundials_cvode.so" Nsimconf.sundials_cvode_lib*)
	    ~path_nvec_serial:(sundials_path Nsimconf.sundials_nvecserial_lib) (*"libsundials_cvode.so"*)
	    ~path_nvec_parallel:(sundials_path Nsimconf.sundials_nvecparallel_lib) (*"libsundials_cvode.so"*)
	    ()
	in
	let v_distributed_vectors =
	  Array.sub v_distributed_resources 3 (Array.length v_distributed_resources-3)
	in
	  (* Note: sub_velocities is NOT the rhs script that is wrapped up for master-vector data I/O,
	     i.e. for nmag, this is not "rhs" but "update_dmdt"!
	  *)
	let sub_velocities = dres_get_sequence v_distributed_resources.(0) in
	let DRES_opdata ((NSIM_RES_mwes v_all_relevant_mwes),_) = v_distributed_resources.(1) in
	let v_mwes =
	  Array.map
	    (fun name ->
	       array_find (fun mwe -> mwe.mwe_name=name) v_all_relevant_mwes)
	    lts.lts_names_phys_field_mwes
	in
	let DRES_opdata ((NSIM_RES_jacobi_operators jacobi_names_ops),_) = v_distributed_resources.(2) in
	let v_mwes_primary_fields = Array.sub v_mwes 0 lts.lts_nr_primary_fields in
	let (y_distribution,y_phys) = layout_y_timestepper_from_primary_fields v_mwes_primary_fields in
	let len_y = Array.length y_phys in
	let mysize = Mpi_petsc.comm_size comm in
	let myrank = Mpi_petsc.comm_rank comm in
	let y_initial_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_initial_pv" lts.lts_name)
	in
	let ba_y_initial_pv = Mpi_petsc._vector_as_bigarray_open_raw y_initial_pv in
	let y_final_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_final_pv" lts.lts_name)
	in
	let y_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_pv" lts.lts_name)
	in
	let ydot_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_ydot_pv" lts.lts_name)
	in
	let y_pcsolve_in_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_pcsolve_in_pv" lts.lts_name)
	in
	let y_pcsolve_out_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_pcsolve_out_pv" lts.lts_name)
	in
	let y_JxV_in_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_JxV_in_pv" lts.lts_name)
	in
	let y_JxV_out_pv =
	  Mpi_petsc.vector_create_mpi
	    ~communicator:comm
	    len_y y_distribution.(myrank)
	    (Printf.sprintf "%s_y_JxV_out_pv" lts.lts_name)
	in
	let y_msv =
	  Mpi_petsc.vector_create len_y
	    (Printf.sprintf "%s_y_msv" lts.lts_name)
	in
	let ydot_msv =
	  Mpi_petsc.vector_create len_y
	    (Printf.sprintf "%s_y_msv" lts.lts_name)
	in
	let () =
	  begin
	    Mpi_petsc.vector_assemble y_initial_pv;
	    Mpi_petsc.vector_assemble y_final_pv;
	    Mpi_petsc.vector_assemble y_pv;
	    Mpi_petsc.vector_assemble ydot_pv;
	    Mpi_petsc.vector_assemble y_pcsolve_in_pv;
	    Mpi_petsc.vector_assemble y_pcsolve_out_pv;
	    Mpi_petsc.vector_assemble y_JxV_in_pv;
	    Mpi_petsc.vector_assemble y_JxV_out_pv;
	    Mpi_petsc.vector_assemble y_msv;
	    Mpi_petsc.vector_assemble ydot_msv;
	  end
	in
	  (* We need mapping functions that transfer data in both
	     directions for multisequential and physical y/ydot vectors:
	  *)
	let (ba_y_pv_lens,ba_y_pv_offsets) =
	  let a_lens = Nsimconf.c_int_bigarray1_create mysize in
	  let a_offsets = Nsimconf.c_int_bigarray1_create mysize
	  in
	    begin
	      a_offsets.{0} <- Nsimconf.c_int_of_int 0;
	      a_lens.{0} <- Nsimconf.c_int_of_int y_distribution.(0);
	      for i=1 to mysize-1 do
		a_lens.{i} <- Nsimconf.c_int_of_int y_distribution.(i);
		a_offsets.{i} <- Nsimconf.c_int_add a_offsets.{i-1}
                                                    a_lens.{i-1};
	      done;
	      (a_lens,a_offsets)
	    end
	in
	let fun_y_ydot_pv_to_msv y_or_ydot_pv y_or_ydot_msv =
	  Mpi_petsc.vec_allgather comm y_or_ydot_msv y_or_ydot_pv ba_y_pv_lens ba_y_pv_offsets
	in
	let fun_y_ydot_msv_to_pv y_or_ydot_msv y_or_ydot_pv =
	  let my_len = Nsimconf.c_int_to_int ba_y_pv_lens.{myrank}
	  and my_offset = Nsimconf.c_int_to_int ba_y_pv_offsets.{myrank}
	  in
	  Mpi_petsc.with_petsc_vector_as_bigarray y_or_ydot_msv
	    (fun ba_msv ->
	       Mpi_petsc.with_petsc_vector_as_bigarray y_or_ydot_pv
		 (fun ba_pv ->
		    for i=0 to my_len-1 do
		      ba_pv.{i} <- ba_msv.{i+my_offset}
		    done))
	in
	let fun_y_pv_to_msv () = fun_y_ydot_pv_to_msv y_pv y_msv
	and fun_ydot_pv_to_msv () = fun_y_ydot_pv_to_msv ydot_pv ydot_msv
	and fun_y_msv_to_pv () = fun_y_ydot_msv_to_pv y_msv y_pv
	and fun_ydot_msv_to_pv () = fun_y_ydot_msv_to_pv ydot_msv ydot_pv
	in
	let v_phys_lengths_displacements =
	  Array.map Ccpla.dres_get_vector_lengths_offsets v_distributed_vectors
	in
	let v_phys_pv =
	  (* Note: these vectors include
	     (a) configuration "positions",
	     (b) configuration "velocities",
	     (c) auxiliary fields.
	  *)
	  Array.map Ccpla.dres_get_vector v_distributed_vectors
	in
	  (* We need multisequential buffers for every one of the physical fields: *)
	let v_phys_msv =
	  Array.mapi
	    (fun nr_vec vec ->
	       let len = Mpi_petsc.vector_global_size vec in
	       let v_ms = Mpi_petsc.vector_create len (Printf.sprintf "%s_v_ms_%d" lts.lts_name nr_vec) in
	       let () = Mpi_petsc.vector_assemble v_ms in
		 v_ms)
	    v_phys_pv
	in
	  (* Also, we need functions to distribute data between
	     physical multisequential and parallel vectors:
	  *)
	let v_fun_phys_pv_to_msv =
	  Array.init (Array.length v_phys_msv)
	    (fun nr_vec ->
	       (fun () ->
		  let (len,disp) = v_phys_lengths_displacements.(nr_vec) in
		    Mpi_petsc.vec_allgather comm v_phys_msv.(nr_vec) v_phys_pv.(nr_vec) len disp))
	in
	let v_fun_phys_msv_to_pv =
	  (* This is rather simple and straightforward now: we can just "open" the vectors and
	     copy the relevant pieces of data manually:
	  *)
	  Array.init (Array.length v_phys_msv)
	    (fun nr_vec ->
	       let (lens,offsets) = v_phys_lengths_displacements.(nr_vec) in
	       let piece_len = Nsimconf.c_int_to_int lens.{myrank} in
	       let piece_offset = Nsimconf.c_int_to_int offsets.{myrank} in
		 (fun () ->
		    Mpi_petsc.with_petsc_vector_as_bigarray v_phys_msv.(nr_vec)
		      (fun ba_msv ->
			 Mpi_petsc.with_petsc_vector_as_bigarray v_phys_pv.(nr_vec)
			   (fun ba_pv ->
			      for i=0 to piece_len-1 do
				ba_pv.{i} <- ba_msv.{i+piece_offset}
			      done))))
	in
	  (* Now that we have access to all physical vectors, and buffers to map them
	     from parallel to multisequential, we need the mapping of y/ydot to and
	     from the multisequential physical vectors.

	     Also, we need a data structure for the Jacobi matrix physical->y index mapping.
	  *)
	let y_ix_and_factor_by_phys_field_nr_and_index =
	  Array.init lts.lts_nr_primary_fields
	    (fun nr_primary_field ->
	       let len_field = Array.length v_mwes.(nr_primary_field).mwe_dofs in
	       let result_y_indices = Array.make len_field 0 in
	       let result_y_factors = Array.make len_field 0.0 in
	       let () =
		 Array.iteri
		   (fun ix_y (nr_y_field,field_indices) ->
		      if nr_y_field <> nr_primary_field then ()
		      else
			let nr_copies = Array.length field_indices in
			let scale = 1.0/.(float_of_int nr_copies) in
			  array_foreach_do field_indices
			    (fun ix_f ->
			       begin
				 result_y_indices.(ix_f) <- ix_y;
				 result_y_factors.(ix_f) <- scale;
			       end))
		   y_phys
	       in
		 (result_y_indices,result_y_factors))
	in
	let fun_y_msv_to_phys_msv () =
	  begin
	    (* We are somewhat cautious here and first zero the target vectors.
	       This should actually be unnecessary in practically all cases.
	       Defensive programming.
	    *)
	    for i=0 to lts.lts_nr_primary_fields-1 do
	      Mpi_petsc.vector_zero v_phys_msv.(i)
	    done;
	    Mpi_petsc.with_petsc_vector_as_bigarray y_msv
	      (fun ba_msv ->
		 for i=0 to len_y-1 do
		   let (target_field_i, target_entries_i) = y_phys.(i) in
		     for k=0 to Array.length target_entries_i-1 do
		       Mpi_petsc.vector_set v_phys_msv.(target_field_i) target_entries_i.(k) ba_msv.{i}
		     done
		 done
	      )
	  end
	in
	let fun_phys_msv_to_y_ydot_msv vec_y ~use_config_vectors () =
	  let () = Mpi_petsc.vector_zero vec_y in
	  let vecs_phys =
	    if use_config_vectors then
	      Array.sub v_phys_msv 0 lts.lts_nr_primary_fields
	    else
	      Array.sub v_phys_msv lts.lts_nr_primary_fields (2*lts.lts_nr_primary_fields)
	  in
	    Mpi_petsc.with_petsc_vectors_as_bigarrays vecs_phys
	      (fun bas_phys ->
		 for i=0 to len_y-1 do
		   let (target_field_i, target_entries_i) = y_phys.(i) in
		   let weight = 1.0/.(float_of_int (Array.length target_entries_i)) in
		     for k=0 to Array.length target_entries_i-1 do
		       Mpi_petsc.vector_inc vec_y i (weight*.bas_phys.(target_field_i).{target_entries_i.(k)})
		     done
		 done
	      )
	in
	  (* Now, the sundials specialists... *)
	let fill_pv_from_par_ba pv ba =
	  Mpi_petsc.with_petsc_vector_as_bigarray pv
	    (fun ba_pv ->
	       for i=0 to Bigarray.Array1.dim ba-1 do
		 ba_pv.{i} <- ba.{i}
	       done)
	in
	let fill_par_ba_from_pv ba pv =
	  Mpi_petsc.with_petsc_vector_as_bigarray pv
	    (fun ba_pv ->
	       for i=0 to Bigarray.Array1.dim ba-1 do
		 ba.{i} <- ba_pv.{i}
	       done)
	in
	  (* "optional" data structures to be generated at first usage... *)
	let ropt_cvode = ref None in
	let ropt_jacobian = ref None in
	let ropt_precond = ref None in	(* (KSP, 1-gamma*J matrix) *)
	let get_jacobian () =
	  (*let () = Printf.printf "DDD [Node=%d] get_jacobian #1\n%!" myrank in*)
	  match !ropt_jacobian with
	    | Some mx -> (mx,true)
	    | None ->
		(*let () = Printf.printf "DDD [Node=%d] get_jacobian #2 (making matrix)\n%!" myrank in*)
                let () = loginfo (Printf.sprintf
		                    "Making Jacobi MX: %d x %d (%s x %s)"
				    len_y len_y
				    (int_array_to_string y_distribution)
				    (int_array_to_string y_distribution)) in
		let mx_jacobi =
		  Mpi_petsc.matrix_create
		    ~communicator:comm
		    ~local_rows:y_distribution.(myrank)
		    ~local_cols:y_distribution.(myrank)
		    ~matrix_type:"mpiaij"
		    ~prealloc_diagonal:lts.lts_jacobi_prealloc_diagonal
		    ~prealloc_off_diagonal:lts.lts_jacobi_prealloc_off_diagonal
		    ~auto_assembly:false
		    len_y len_y (Printf.sprintf "%s_jacobian" lts.lts_name)
		in
		(*let () = Printf.printf "DDD [Node=%d] get_jacobian #3 (made matrix)\n%!" myrank in*)
		let () = ropt_jacobian:=Some mx_jacobi in
		  (mx_jacobi,false)
	in
	let v_derive_me = Array.mapi
	  (fun nr_phys (deriv_type,deriv_data) ->
	     match deriv_type with
	       | "IGNORE" -> pts_jacobi_derive_me__dummy
	       | "PRIMARY" -> pts_jacobi_derive_me__primary_field
	       | "OPERATOR" ->
		   let (_,mx) = array_find (fun (name,_) -> name = deriv_data) jacobi_names_ops in
		   (* C4R (mf, 2 Jun 2008): let () = Printf.printf "XXX REPAIR-ME: pts_jacobi_derive_me__ddiffop nr_mwe_le=nr_mwe_ri=hard-coded 0!\n%!" in*)
		     pts_jacobi_derive_me__ddiffop
		       v_mwes
		       0 0
		       mx
	       | x ->
		   let () = Printf.printf "DDD WARN: Jacobi Derivative type '%s' unsupported!\n%!" x in
		     pts_jacobi_derive_me__dummy
	  ) lts.lts_phys_field_derivs
	in
	let jacobi_vivificator =
	  pts_jacobi_vivificator
	    ~nr_cpu:myrank
	    ~v_mwes
	    ~v_derive_me
	    ~local_equations:lts.lts_jacobi_equations
	in
	let jacobi_fun_make_entry nr_field_le ix_field_le nr_field_ri ix_field_ri contrib =
	  match !ropt_jacobian with
	    | None -> failwith "Jacobi vivificator called on non-existent matrix!"
	    | Some mx ->
		let nr_field_le =
		  if nr_field_le >= lts.lts_nr_primary_fields
		  then nr_field_le - lts.lts_nr_primary_fields else nr_field_le
		in
		let nr_field_ri =
		  if nr_field_ri >= lts.lts_nr_primary_fields
		  then nr_field_ri - lts.lts_nr_primary_fields else nr_field_ri
		in
		let (y_le_v_ix,y_le_v_scale) = y_ix_and_factor_by_phys_field_nr_and_index.(nr_field_le) in
		let (y_ri_v_ix,y_ri_v_scale) = y_ix_and_factor_by_phys_field_nr_and_index.(nr_field_ri) in
		  (* let () = Printf.printf "MATRIX-INC %d %d %f\n%!" y_le_v_ix.(ix_field_le) y_ri_v_ix.(ix_field_ri) (contrib*.y_le_v_scale.(ix_field_le)) in *)
		  Mpi_petsc.matrix_inc
		    mx
		    y_le_v_ix.(ix_field_le)
		    y_ri_v_ix.(ix_field_ri)
		    (contrib*.y_le_v_scale.(ix_field_le))
	in
	let get_precond gamma =
	  (*let () = Printf.printf "DDD [Node=%d] get_precond #1\n%!" myrank in*)
	  match !ropt_precond with
	    | Some ksp_and_mx -> ksp_and_mx
	    | None ->
		let (jacobian,was_present) = get_jacobian() in
		let () =
		  (if not was_present
		   then
		     failwith "This should never happen: Sundials requested \
                               PC without making Jacobian first!"
		   else ())
		in
		  (* let () = Printf.printf "DDD [Node=%d] get_precond - duplicate jacobian\n%!" myrank in *)
		let mx = Mpi_petsc.matrix_duplicate true jacobian in
		  (* let () = Printf.printf "DDD [Node=%d] get_precond - ksp_create\n%!" myrank in *)
		let () =
		  begin
		    Mpi_petsc.matrix_copy lts.lts_pc_same_nonzero_pattern jacobian mx;
		    Mpi_petsc.matrix_scale mx (-.gamma);
		    Mpi_petsc.matrix_add_identity mx 1.0;
		  end
		in
		let ksp = Mpi_petsc.ksp_create
                            ~communicator:(Mpi_petsc.petsc_get_comm_world())
                            ~matrix_structure:Mpi_petsc.SAME_PRECONDITIONER
                            mx
                            mx
                in
		  (* let () = Printf.printf "DDD [Node=%d] get_precond - ksp_set_up\n%!" myrank in *)
                (*let () = Mpi_petsc.ksp_set_initial_guess_nonzero ksp true in*)
                let () = Mpi_petsc.ksp_set_type ksp "gmres" in
		let () = Mpi_petsc.ksp_set_up ksp in

		  (* let () = Printf.printf "DDD [Node=%d] get_precond - bjacobi\n%!" myrank in *)
		let () = Mpi_petsc.ksp_set_pc_bjacobi_sub_type ksp "ilu" in
		  (* let () = Printf.printf "DDD [Node=%d] get_precond - initial_guess_nonzero\n%!" myrank in *)
		  (* let () = Mpi_petsc.ksp_set_initial_guess_nonzero ksp true in *)
		  (* ^ Actually, this turns out to be a rather bad idea here. Seems as if sundials used
		     the KSP in a way where it probes such different regions in parameter space that
		     we are better off not activating this.
		  *)
		  (* let () = Printf.printf "DDD [Node=%d] get_precond - tolerances\n%!" myrank in *)
		let () =
		  Mpi_petsc.ksp_set_tolerances_opt
		    ksp
		    lts.lts_pc_opt_rtol
		    lts.lts_pc_opt_atol
		    lts.lts_pc_opt_dtol
		    lts.lts_pc_opt_maxit
		in
		  (* let () = Printf.printf "DDD [Node=%d] get_precond - have ksp and mx\n%!" myrank in *)
		let ksp_and_mx = (ksp,mx) in
		let () = ropt_precond := Some ksp_and_mx in
		  ksp_and_mx
	in
	let debugdump_vec tag petsc_vec =
	  Mpi_petsc.with_petsc_vector_as_bigarray petsc_vec
	    (fun ba ->
	       Printf.printf "[N=%d] %s: %8.4f %8.4f %8.4f  %8.4f %8.4f %8.4f...\n%!"
		 myrank tag ba.{0} ba.{1} ba.{2}  ba.{3} ba.{4} ba.{5})
	in
	let pts_timings = make_par_timestepper_timings()
	in
	  (* === CVODE RHS === *)
	let cvode_fun_rhs (time,ba_y,ba_ydot) () =
	  (* Here, y and ydot are being given to us as bigarrays
	     (which contain part of a parallelized Sundials NVector)
	  *)
	  let t0 = Unix.gettimeofday() in
          let () = time_in_rhs := time in
	  let () =
	    begin
	      (* Printf.printf "RHS [Node=%d]  TIME=%f\n%!" myrank time; *)
	      (* Printf.printf "RHS [Node=%d]  ba_y: %8.4f %8.4f %8.4f \n%!" myrank ba_y.{0}  ba_y.{1}  ba_y.{2}; *)
	      fill_pv_from_par_ba y_pv ba_y;
	      (* debugdump_vec "y_pv" y_pv; *)
	      fun_y_pv_to_msv ();
	      (* debugdump_vec "y_msv" y_msv; *)
	      fun_y_msv_to_phys_msv ();
	      (* debugdump_vec "phys[0]_msv" v_phys_msv.(0); *)
	      for i=0 to lts.lts_nr_primary_fields-1 do
		v_fun_phys_msv_to_pv.(i) ();
	      done;
	      (* debugdump_vec "phys[0]_pv" v_phys_pv.(0); *)
	      Array.iter (Ccpla.dcom_execute ccpla) sub_velocities;
	      (* debugdump_vec "phys[1]_pv" v_phys_pv.(1); *)
	      (* Now, copy back velocities: *)
	      for i=lts.lts_nr_primary_fields to 2*lts.lts_nr_primary_fields-1 do
		(* Printf.printf "Copy velocity vector (%d)\n%!" i; *)
		v_fun_phys_pv_to_msv.(i) ();
	      done;
	      (* debugdump_vec "phys[1]_msv" v_phys_msv.(1); *)
	      fun_phys_msv_to_y_ydot_msv ydot_msv ~use_config_vectors:false ();
	      (* debugdump_vec "ydot_msv" ydot_msv; *)
	      fun_ydot_msv_to_pv ();
	      (* debugdump_vec "ydot_pv" ydot_pv; *)
	      fill_par_ba_from_pv ba_ydot ydot_pv;
	      (* Printf.printf "RHS [Node=%d] step #10 ba_ydot: %8.4f %8.4f %8.4f\n%!" myrank ba_ydot.{0} ba_ydot.{1} ba_ydot.{2}; *)
	    end
	  in
	  let t1 = Unix.gettimeofday() in
	  let () = pts_timings.ptt_rhs_n <- 1.0 +. pts_timings.ptt_rhs_n in
	  let () = pts_timings.ptt_rhs_t <- pts_timings.ptt_rhs_t +. (t1-.t0) in
	    0
	in
	  (* === CVODE re-Jacobi === *)
	let cvode_fun_do_compute_jacobi time ba_y ba_ydot =
	  (* let () = Printf.printf "[Node=%d] CVODE re-jacobi!\n%!" myrank in *)
	  let (jacobian,was_present) = get_jacobian() in
	    (* We must at least make sure the Jacobian has a diagonal,
	       so that the 1-gamma*J matrix has the same nonzero structure
	       as this.
	    *)
	    begin
	      (if was_present
	       then Mpi_petsc.matrix_zero_entries jacobian
	       else reportmem "jacobi_before");
	      (if lts.lts_use_jacobian
	       then
		 (* Note that we do not count the time to actually build the matrix for the first time here. *)
		 let t0 = Unix.gettimeofday() in
		 let () =
		   begin
		     (* Make sure we have the correct physical vectors around:
			Work as in the first part of RHS.
		     *)
		     fill_pv_from_par_ba y_pv ba_y;
		     fun_y_pv_to_msv ();
		     fun_y_msv_to_phys_msv ();
		     for i=0 to lts.lts_nr_primary_fields-1 do
		       v_fun_phys_msv_to_pv.(i) ();
		     done;
		     Array.iter (Ccpla.dcom_execute ccpla) sub_velocities;
		     for i=0 to Array.length v_fun_phys_pv_to_msv-1 do
		       v_fun_phys_pv_to_msv.(i) ();
		     done;
		     Mpi_petsc.with_petsc_vectors_as_bigarrays v_phys_msv
		       (fun ba_fields_msv ->
			    (*
			  let () = Printf.printf "DDD Calling Jacobi Vivificator!\n%!" in
			  let ddd =
			    for i=0 to Array.length v_phys_msv-1 do
			      Printf.printf "N%d P%d %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f ...\n%!" myrank i ba_fields_msv.(i).{0} ba_fields_msv.(i).{1} ba_fields_msv.(i).{2} ba_fields_msv.(i).{3} ba_fields_msv.(i).{4} ba_fields_msv.(i).{5}
			    done
			  in
			    *)
			  let () =
			    jacobi_vivificator
			      ba_fields_msv
			      jacobi_fun_make_entry
			  in
			    ()
		       );
		   end
		 in
		 let t1 = Unix.gettimeofday() in
		 let () = pts_timings.ptt_jacobi_n <- 1.0 +. pts_timings.ptt_jacobi_n in
		 let () = pts_timings.ptt_jacobi_t <- pts_timings.ptt_jacobi_t +. (t1-.t0) in
		   ()
	       else
		 Printf.printf "[Node=%d] Re-Jacobi DISABLED/DONE!\n%!" myrank;
	      );
	      (* Ensure we do have the diagonal entries - note that this extra effort is not timed... *)
	      (let (start_mypart,end_mypart) = Mpi_petsc.matrix_get_ownership_range jacobian in
		 for i=start_mypart to end_mypart-1 do
		   Mpi_petsc.matrix_inc jacobian i i 1e-200
		 done);
	      Mpi_petsc.matrix_assemble jacobian true;
	      (if not was_present
	       then
	         let () = Mpi_petsc.matrix_reincarnate jacobian
	         in
	           reportmem "jacobi_after"
	       else ());
	      (* DDD *)
	      (*
	      Printf.printf "N%dJacobi Y: %10.8f %10.8f %10.8f %10.8f %10.8f %10.8f ...\n%!" myrank ba_y.{0} ba_y.{1} ba_y.{2} ba_y.{3} ba_y.{4} ba_y.{5};
	      Mpi_petsc.petsc_matrix_call_on_rows jacobian
		(fun nr_row ba_indices ba_vals ->
		   if nr_row mod 1000 = 0 then
		     let nr_indices = Bigarray.Array1.dim ba_indices in
		     let v_indices = Array.init nr_indices (fun n -> Nativeint.to_int ba_indices.{n}) in
		     let v_vals = Array.init nr_indices (fun n -> ba_vals.{n}) in
		     let () = Printf.printf "N%d R=%4d i=%s\n%!" myrank nr_row (int_array_to_string v_indices) in
		     let () = Printf.printf "N%d R=%4d v=%s\n%!" myrank nr_row (float_array_to_string v_vals) in
		       ());
	      Printf.printf "[Node=%d] Re-Jacobi done!\n%!" myrank;
	      *)
	    end
	in
	  (* === CVODE Jacobi*Vector === *)
	let cvode_fun_jacobi_times_vector args () =
	  (* let () = Printf.printf "[Node=%d] CVODE J*v!\n%!" myrank in *)
	  let (time,ba_in,ba_out,ba_y,ba_ydot,v_tmp) = args in
	  let (jacobian,was_present) = get_jacobian () in
	  let () =
	    (if not was_present
	     then failwith "This should never happen: sundials called J*V without building a Jacobian first!"
	     else ())
	  in
	  let t0 = Unix.gettimeofday() in
	  let () = fill_pv_from_par_ba y_JxV_in_pv ba_in in
	  let () = Mpi_petsc.matrix_times_vector jacobian y_JxV_in_pv y_JxV_out_pv in
	  let () = fill_par_ba_from_pv ba_out y_JxV_out_pv in
	  let t1 = Unix.gettimeofday() in
	  let () = pts_timings.ptt_jv_n <- 1.0 +. pts_timings.ptt_jv_n in
	  let () = pts_timings.ptt_jv_t <- pts_timings.ptt_jv_t +. (t1-.t0) in
	    0
	in
	  (* === CVODE PC-Setup === *)
	let cvode_fun_preconditioner_setup args () =
	  let (j_ok,time,gamma,ba_y,ba_ydot,ba_tmp1,ba_tmp2,ba_tmp3) = args in
	  (* let () = Printf.printf "DDD [Node=%d] PC-Setup #1\n%!" myrank in *)
          (* If sundials tells us we may re-use the jacobi matrix, we do
             re-use the jacobi matrix *)
	  let j_recomputed =
	    if j_ok
	    then false
	    else
              let () = cvode_fun_do_compute_jacobi time ba_y ba_ydot in true
	  in
	  let (jacobian, was_present) = get_jacobian () in
	  let () =
	    (if not was_present
	     then
	       let () = cvode_fun_do_compute_jacobi time ba_y ba_ydot in
	       let () = Printf.printf "NOTE: This should never happen: \
                                       sundials called PC Setup without \
                                       building a Jacobian first!\n%!" in
		 ()
	     else ())
	  in
	  let t0 = Unix.gettimeofday() in
	  let (ksp, mx) = get_precond gamma in
	  let () =
            let mxstruc =
              if j_recomputed
              then Mpi_petsc.DIFFERENT_NONZERO_PATTERN
              else Mpi_petsc.SAME_NONZERO_PATTERN
            in
              begin
                Mpi_petsc.matrix_copy lts.lts_pc_same_nonzero_pattern jacobian mx;
                Mpi_petsc.matrix_scale mx (-.gamma);
                Mpi_petsc.matrix_add_identity mx 1.0;
                Mpi_petsc.ksp_set_operators ~matrix_structure:mxstruc ksp mx mx;
              end
	  in
	  let t1 = Unix.gettimeofday() in
	  let () = pts_timings.ptt_pc_setup_n <- 1.0 +. pts_timings.ptt_pc_setup_n in
	  let () = pts_timings.ptt_pc_setup_t <- pts_timings.ptt_pc_setup_t +. (t1-.t0) in
	    (* let () = Printf.printf "DDD [Node=%d] PC-Setup #4\n%!" myrank in *)
	    (j_recomputed, 0)
	in
	  (* === CVODE PC-Solve === *)
        let ts_ksp_name = Printf.sprintf "TS_KSP_%s" name_ts in
        let log_precond_ksp = Mpi_petsc.petsc_log_stage_register ts_ksp_name in
	let cvode_fun_preconditioner_solve args () =
	  (* let () = Printf.printf "[Node=%d] pc-solve\n%!" myrank in *)
	  let t0 = Unix.gettimeofday() in
	  let (left_right,time,gamma,delta,ba_y,ba_fy,ba_rhs,ba_result,ba_tmp) = args in
	  let is_left = (left_right=1) in
	  let () = (if not(is_left)
		    then failwith "CVODE PROBLEM: left_right is right - should never be!"
		    else ())
	  in
	  let (ksp,_) = get_precond gamma in
	  let () = fill_pv_from_par_ba y_pcsolve_in_pv ba_rhs in
          let () = Mpi_petsc.petsc_log_stage_push log_precond_ksp in
	  let nr_iterations = Mpi_petsc.ksp_solve_raw ksp y_pcsolve_out_pv y_pcsolve_in_pv in (* this is really what scales badly!!! *)
          let () = Mpi_petsc.petsc_log_stage_pop () in
	  let () = fill_par_ba_from_pv ba_result y_pcsolve_out_pv in
	  let t1 = Unix.gettimeofday() in
	  let () = pts_timings.ptt_pc_solve_n <- 1.0 +. pts_timings.ptt_pc_solve_n in
	  let () = pts_timings.ptt_pc_solve_t <- pts_timings.ptt_pc_solve_t +. (t1-.t0) in
	    0
	in
	let timestepper_set_initial_from_phys ?(initial_time=0.0) ?(rel_tol=1e-6) ?(abs_tol=1e-6) () =
	  (*let () = Printf.printf "DDD: NSIM timestepper '%s': set_initial()\n%!" lts.lts_name in*)
	    (* We have to see that we properly initialize the v_initial timestepper vector first.
	       For this, we have to get the data from the distributed physical linalg_machine vectors.
	    *)
	  let () =
	    begin
	      for i=0 to lts.lts_nr_primary_fields-1 do
		v_fun_phys_pv_to_msv.(i) ()
	      done;
	      (* We can use the multiseq y-buffer here to fill the parallel y_initial buffer: *)
	      fun_phys_msv_to_y_ydot_msv y_msv ~use_config_vectors:true ();
	      fun_y_ydot_msv_to_pv y_msv y_initial_pv;
	    end
	  in
	    match !ropt_cvode with
	      | None ->
		  (*let () = Printf.printf "DDD: Making new CVODE!\n%!" in*)
		  let cvode =
		    Sundials_sp.cvode_make_raw_ba
		      (ccpla.ccpla_comm,
		       true,
		       cvode_fun_rhs,
		       len_y,
		       ba_y_initial_pv,
		       initial_time,
		       rel_tol,
		       abs_tol
		      )
		  in
		  let () =
		    Sundials_sp.cvode_setup_cvspgmr_raw
		      cvode
		      lts.lts_krylov_max
		      Sundials_sp.PREC_LEFT
		      cvode_fun_preconditioner_setup
		      cvode_fun_preconditioner_solve
		  in
		  let () = Sundials_sp.cvode_set_max_order cvode lts.lts_max_order in
		  let () = Sundials_sp.cvode_set_max_num_steps cvode 100000000 in
		  let () =
		    Sundials_sp.cvode_setup_jacobi_times_vector_raw
		      cvode
		      cvode_fun_jacobi_times_vector
		  in
		    ropt_cvode := Some cvode
	      | Some cvode ->
		  (*let () = Printf.fprintf stderr "DDD Re-initialise timestepper\n%!" in*)
		  Sundials_sp.cvode_reinit
		    ~initial_time ~rel_tolerance:rel_tol ~abs_tolerance:abs_tol
		    cvode
	in
	let timestepper_advance ?exact_tstop time max_nr_steps =
	  match !ropt_cvode with
	    | None ->
		let () =
		  if myrank=0
		  then
		    loginfo (Printf.sprintf "NOTE: timestepper_advance() not done - CVODE has not yet been created!")
		  else ()
		in
		  0.0
	    | Some cvode ->
		let result = ref None in
		let () = Mpi_petsc.with_petsc_vector_as_bigarray y_final_pv
		  (fun ba_final ->
		     result :=
		       Some (Sundials_sp.cvode_advance_ng
			       ~comm:ccpla.ccpla_comm ~max_nr_steps ?exact_tstop
			       cvode
			       time
			       ba_final))
		in
		let () =
		  (* We still have to copy the final configuration back to the distributed physical
		     configuration vector. Note that we do not re-compute the dependent fields -
		     Doing this properly will be a task for the higher level field dependency
		     "make" mechanism.
		  *)
		  begin
		    (* Printf.printf "[Node=%d] timestepper_advance finished!\n"; *)
		    (* debugdump_vec "post-TS y_final_pv" y_final_pv; *)
		    fun_y_ydot_pv_to_msv y_final_pv y_msv;
		    (* debugdump_vec "post-TS y_msv" y_msv; *)
		    fun_y_msv_to_phys_msv ();
		    (* debugdump_vec "post-TS phys_msv.(0)" v_phys_msv.(0); *)
		    for i=0 to lts.lts_nr_primary_fields-1 do
		      v_fun_phys_msv_to_pv.(i) ();
		    done;
		    (* debugdump_vec "post-TS phys_pv.(0)" v_phys_pv.(0); *)
		  end
		in
		  match !result with
		    | None -> impossible ()
		    | Some x -> x
	in
	let timestepper_set_tolerances ksp_tolerances (cvode_rtol,cvode_atol) =
	  let (ksp,_) = get_precond 1e-10 in
	  let (opt_rtol,opt_atol,opt_dtol,opt_maxit) = ksp_tolerances in
	  let () =
	    Mpi_petsc.ksp_set_tolerances_opt ksp opt_rtol opt_atol opt_dtol opt_maxit
	  in
	    match !ropt_cvode with
	      | None ->
		  if myrank=0 then
		    loginfo
		      (Printf.sprintf "NOTE: timestepper_set_tolerances() not done - CVODE was not created first!")
		  else ()
	      | Some cvode ->
		  Sundials_sp.cvode_set_tolerances cvode cvode_rtol cvode_atol
	in
	let timestepper_finalizer () =
	  begin
	    (* Printf.fprintf stderr "[Node %d] Finalize parallel timestepper...\n%!" myrank; *)
	    Mpi_petsc._vector_as_bigarray_close_raw y_initial_pv ba_y_initial_pv;
	    Mpi_petsc.vec_destroy y_initial_pv;
	    Mpi_petsc.vec_destroy y_final_pv;
	    Mpi_petsc.vec_destroy y_pv;
	    Mpi_petsc.vec_destroy ydot_pv;
	    Mpi_petsc.vec_destroy y_pcsolve_in_pv;
	    Mpi_petsc.vec_destroy y_pcsolve_out_pv;
	    Mpi_petsc.vec_destroy y_pcsolve_in_pv;
	    Mpi_petsc.vec_destroy y_JxV_in_pv;
	    Mpi_petsc.vec_destroy y_JxV_out_pv;
	    Mpi_petsc.vec_destroy y_msv;
	    Mpi_petsc.vec_destroy ydot_msv;
	    Array.iter Mpi_petsc.vec_destroy v_phys_msv;
	    (match !ropt_jacobian with | None -> () | Some mx -> Mpi_petsc.mat_destroy mx);
	    (match !ropt_precond with | None -> ()
	       | Some (ksp,mx) ->
		   begin
		     Mpi_petsc.ksp_destroy ksp;
		     Mpi_petsc.mat_destroy mx;
		   end
	    );
	  end
	in
	let pts_timing_control cmd =
	  match cmd with
	    | "RESET" ->
		begin
		  pts_timings.ptt_rhs_n = 0.0;
		  pts_timings.ptt_rhs_t = 0.0;
		  pts_timings.ptt_jacobi_n = 0.0;
		  pts_timings.ptt_jacobi_t = 0.0;
		  pts_timings.ptt_jv_n = 0.0;
		  pts_timings.ptt_jv_t = 0.0;
		  pts_timings.ptt_pc_setup_n = 0.0;
		  pts_timings.ptt_pc_setup_t = 0.0;
		  pts_timings.ptt_pc_solve_n = 0.0;
		  pts_timings.ptt_pc_solve_t = 0.0;
                  pts_timings.ptt_extra_n = 0.0;
                  pts_timings.ptt_extra_t = 0.0;
		  [||]
		end
	    | "REPORT" ->
		[|
		  ("RHS",pts_timings.ptt_rhs_n,pts_timings.ptt_rhs_t);
		  ("Jacobi",pts_timings.ptt_jacobi_n,pts_timings.ptt_jacobi_t);
		  ("J*V",pts_timings.ptt_jv_n,pts_timings.ptt_jv_t);
		  ("PCSetup",pts_timings.ptt_pc_setup_n,pts_timings.ptt_pc_setup_t);
		  ("PCSolve",pts_timings.ptt_pc_solve_n,pts_timings.ptt_pc_solve_t);
                  ("Extra",pts_timings.ptt_extra_n,pts_timings.ptt_extra_t);
		|]
	    | x ->
		let () = Printf.fprintf stderr "Unknown timer command: '%s'\n%!" x in
		  [||]
	in
	let par_timestepper =
	  let cvode = ()
	  in
	    {pts_name = lts.lts_name;
	     pts_timings = pts_timings;
	     pts_time_reached = 0.0;
	     pts_cvode = ropt_cvode;
	     pts_set_initial_from_phys = timestepper_set_initial_from_phys;
	     pts_advance = timestepper_advance;
	     pts_set_tolerances = timestepper_set_tolerances;
	     pts_timing_control = pts_timing_control;
	    }
	in
	  [|(name_ts,DRES_opdata (NSIM_RES_timestepper par_timestepper,timestepper_finalizer))|]
    | _ -> failwith "operator not implemented!"
;;

let nsim_swex_create ccpla name c_code v_field_mwes v_cofield_mwes aux_arg_names =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let (_,par_site_offsets,defines) =
    site_wise_structure_and_offsets_and_defines ~parallel:true v_field_mwes v_cofield_mwes aux_arg_names
  in
  let cmds_create =
    Array.init nr_nodes
      (fun n -> DCOM_opcode (NSIM_OP_swex_create (name,c_code,par_site_offsets.(n),defines),[||]))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_swex = DRH name in
  (* let () = Printf.printf "DDD DRES swex: create finalizer for '%s'\n%!" name in *)
  let () = Gc.finalise
    (fun drh ->
       (* let () = Printf.printf "DDD DRES swex: parfinalize '%s'\n%!" name in *)
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_swex
  in
  let () = master_process_queue ccpla in
    the_swex
;;


let nsim_mwes_create ccpla name mwes =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.init nr_nodes
      (fun n ->
	 if n = 0
	 then
	   (* master will just take the mwes we have... *)
	   DCOM_opcode ((NSIM_OP_mwes_create (name,None,Some mwes)),[||])
	 else
	   (DCOM_opcode ((NSIM_OP_mwes_create
			    (name,
			     Some (Array.map (fun mwe -> mwe.mwe_made_by) mwes),
			     None))
			   ,[||])))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_mwes = DRH name in
  (* let () = Printf.printf "DDD DRES mwes: create finalizer for '%s'\n%!" name in *)
  let () = Gc.finalise
    (fun drh ->
       let () = Printf.printf "DDD DRES mwes: parfinalize '%s'\n%!" name in
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_mwes
  in
  let () = master_process_queue ccpla in
    the_mwes
;;


let nsim_vivificators_create ccpla name ~mwes ~vivificator_specs =
  let () = logdebug (Printf.sprintf "nsim_vivificators_create #1") in
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes
      (DCOM_opcode ((NSIM_OP_vivificators_create (name,vivificator_specs)),[|mwes|]))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_vivs = DRH name in
  let () = Gc.finalise
    (fun drh ->
       let () = Printf.printf "DDD DRES vivificators: parfinalize '%s'\n%!" name in
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_vivs
  in
  let () = master_process_queue ccpla in
    the_vivs
;;


let nsim_jacobi_operators_create ccpla name ~jacobi_operator_names ~jacobi_drhs =
  let () = logdebug (Printf.sprintf "nsim_jacobi_operators_create #1") in
  let ddd = Printf.printf  in
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes
      (DCOM_opcode ((NSIM_OP_jacobi_operators_create (name,jacobi_operator_names)),jacobi_drhs))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_jops = DRH name in
  let () = Gc.finalise
    (fun drh ->
       let () = Printf.printf "DDD DRES jacobi operators: parfinalize '%s'\n%!" name in
       let () = ccpla.ccpla_pending_finalizations <- name::ccpla.ccpla_pending_finalizations
       in ()) the_jops
  in
  let () = master_process_queue ccpla in
    the_jops
;;



let nsim_timestepper_create
    ccpla ~prefix ~vecs_fields
    ~script_compute_velocities ~mwes ~timestepper_spec
    ~jacobi_operators
    =
  let () = logdebug (Printf.sprintf "nsim_timesteppers_create #1") in
  let name_ts = Printf.sprintf "%s$timestepper.%s" prefix timestepper_spec.lts_name in
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let cmds_create =
    Array.make nr_nodes
      (DCOM_opcode ((NSIM_OP_make_timestepper (name_ts,timestepper_spec)),
		    (Array.append
		       [|script_compute_velocities;mwes;jacobi_operators|]
		       vecs_fields)))
  in
  let () = Queue.push cmds_create !(ccpla.ccpla_queue) in
  let the_timestepper = DRH name_ts in
  let () = Gc.finalise
    (fun drh ->
       let () = Printf.printf "DDD DRES timestepper: parfinalize '%s'\n%!" name_ts in
       let () = ccpla.ccpla_pending_finalizations <- name_ts::ccpla.ccpla_pending_finalizations
       in ()) the_timestepper
  in
  let () = master_process_queue ccpla in
    the_timestepper
;;

let nsim_execute_all_CPUs ccpla f =
  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
  let commands = Array.make nr_nodes (DCOM_exec f) in
  let () = Queue.push commands !(ccpla.ccpla_queue) in
  let () = master_process_queue ccpla in
    ()
;;

(* Note: make_linalg_machine basically is a "linear algebra compiler"!

   The interesting bit below is that, depending on whether
   ?ccpla is provided or not, we can build either a fast, efficient
   single-cpu linalg machine without all the parallel communication
   overhead, or a distributed one that has more comm overhead, but can
   utilize multiple machines!

   NOTE: function below uses a bit of cut&paste programming to deal
   with the single-process as well as the MPI-parallel case.
   Should be cleaned up & beautified!
*)

let make_linalg_machine
    ?ccpla ~prefix ~relevant_mwes
    script =
  (* First, we have to see we get all the buffers established. For this,
     we need means to identify the proper mwes.
  *)
  let () = loginfo (Printf.sprintf "Compiling linalg_machine") in
  let () = Array.iter mwe_ensure_has_volumes relevant_mwes in
  let timers_accum = Hashtbl.create 17 in
  let timers_running = Hashtbl.create 17 in
  let fun_get_timers names =
    let all_names =
      if Array.length names=0 then hashtbl_keys ~sorter:compare timers_accum else names
    in
      Array.map (fun name -> let time = Hashtbl.find timers_accum name in (name,time)) all_names
  in
  let mwe_restr_offsets mwe restr =
    let restr =
      parse_or_error Ddiffop_lexer.token
	Ddiffop_parser.short_vector_restriction restr
    in
    let (_,dof_mapping_s_to_l,_) = mwe_shortvec_info mwe restr in
      dof_mapping_s_to_l
  in
  let () = reportmem "prelude" in
  let v_mwe_vols =
    Array.map
      (fun mwe ->
	 match !(mwe.mwe_dof_funs_volumes)
	 with
	   | Some v -> Mpi_petsc.vector_pack v
	   | _ -> impossible())
      relevant_mwes
  in
  let get_mwe_index name =
    let pos =
      array_position_if (fun mwe -> mwe.mwe_name=name) relevant_mwes 0
    in
      if pos = (-1) then failwith (Printf.sprintf "Could not find mwe '%s'" name)
      else pos
  in
  let get_mwe name = relevant_mwes.(get_mwe_index name) in
  let cseq_arg_vectors =
    (* Whenever we execute_on a bunch of fields and co-fields,
       we first copy the local data vectors here.
       Evidently, every such script will have its own "vector of arguments".
    *)
    let v_dummy = Mpi_petsc.vector_dummy() in
      Array.map
	(fun (cseq_name,cseq_arg_fields,cseq_arg_cofields,cseq_cmds) ->
	   Array.make
	     ((Array.length cseq_arg_fields)
	      +(Array.length cseq_arg_cofields))
	     v_dummy
	)
		script.las_command_sequences
  in
  let locate_master_local_vector nr_sequence name =
    let (_,args_fields,args_cofields,_) = script.las_command_sequences.(nr_sequence) in
    let pos = array_position_if (fun (arg_name,_) -> name=arg_name) args_fields 0 in
      if pos>=0 then pos
      else
	let pos = array_position_if (fun (arg_name,_) -> name=arg_name) args_cofields 0 in
	  if pos>=0 then pos+(Array.length args_fields)
	  else failwith (Printf.sprintf "make_linalg_machine: sequence %d, unknown argument '%s'" nr_sequence name)
  in
    match ccpla with
      | None ->
	  failwith
"Note: the nsim core no longer supports sequential complation.
While the purely sequential mode of execution offered some speed
gain due to simplified bookkeeping, maintaining two different
versions of the opcode compiler turned out too much of a burden
for limited manpower. So, this piece of code has been removed
for now. (Also, while things are still in a state of flux and
have not settled down in their final form, having to do work
twice for both compilers is not a good idea.)

Therefore, sequential runs now must use the parallel nsim code,
with N=1 MPI processes.

Thomas Fischbacher, 13.05.2008
"
      | Some ccpla ->
	  (* === BUILDING THE PARALLEL MACHINE === *)
	  let () = reportmem "par_start" in
	  let nr_nodes = Mpi_petsc.comm_size ccpla.ccpla_comm in
	  let intensive_params =
	    ccpla.ccpla_register_params
	      (Printf.sprintf "%s$iparams" prefix)
	      script.las_intensive_params
	      (Array.make (Array.length script.las_intensive_params) 0.0)
	  in
	  let () = reportmem "par_iparams" in
	  let the_mwes =
	    nsim_mwes_create ccpla
	      (Printf.sprintf "%s$mwes" prefix)
	      relevant_mwes
	  in
	    (* We also have to parallelize the inverse-volume vectors
	       for the "cofield to field" mappings. Here, we do not care
	       too much about possibly allocating a few extra unused
	       vectors.
	    *)
	  let () = reportmem "par_mwes" in
	  let () = Gc.full_major () in
	  let () = Gc.minor () in
	  let () = reportmem "par_mwes - checked heap" in
	  let additional_internal_buffers = Array.map
	    (fun mwe -> (Printf.sprintf "__cofield_to_field__%s" (mwe.mwe_name), mwe.mwe_name,None,true,0.0))
	    relevant_mwes
	  in
	  let effective_internal_buffers = Array.append script.las_internal_buffers additional_internal_buffers
	  in
	  let the_internal_buffers =
	    Array.mapi
	      (fun nr_buffer (buffer_rel_name,mwe_name,opt_restr,_,initial_value) ->
		 let mwe = get_mwe mwe_name in
		 let (lts,stl,distrib) = mwe_shortvec_info mwe opt_restr in
		 let buffer_abs_name = Printf.sprintf "%s$vector.%s" prefix buffer_rel_name in
		 (* let ddd =  (Printf.printf "Making internal buffer #%d '%s' for mwe '%s'\n" nr_buffer buffer_abs_name mwe_name) in *)
		   (* let () = Printf.printf "DDD vec lts=%s stl=%s distrib=%s\n%!"
		      (int_array_to_string lts) (int_array_to_string stl) (int_array_to_string distrib)
		      in
		   *)
		 let (DRH name) as ccpla_v =
		   ccpla.ccpla_vector_create buffer_abs_name distrib
		 in
		 let DRES_petsc_vector (_,_,p_v) = Hashtbl.find ccpla.ccpla_resources name in
		 let () =
		   (if initial_value = 0.0 then ()
		    else
		      begin
			for i=0 to (Array.length stl)-1 do
			  Mpi_petsc.vector_set p_v i initial_value;
			done;
			ccpla.ccpla_vector_assemble ccpla_v;
		      end)
		 in
		   (* We also automatically allocate a master buffer copy of that vector.
		      XXX NOTE TODO: this also should be the copy which Python will
		      eventually be seeing and using!
		   *)
		 let local_v = Mpi_petsc.vector_create (Array.length stl) (Printf.sprintf "%s_seq" buffer_abs_name)
		 in let () = Mpi_petsc.vector_assemble local_v in
		   (ccpla_v,local_v)
	      )
	      effective_internal_buffers
	  in
	  let () = reportmem "par_vectors" in
	  let nr_script_buffers = Array.length script.las_internal_buffers in
	  let () =
	    begin
	      for n=0 to Array.length relevant_mwes-1 do
		ccpla.ccpla_vector_distribute
		  ~local_vectors:v_mwe_vols n
		  (let (p,s) = the_internal_buffers.(nr_script_buffers+n) in p)
	      done
	    end
	  in
	  let get_buffer_index name =
	    let pos =
	      array_position_if (fun (rel_name,_,_,_,_) -> name=rel_name)
		script.las_internal_buffers 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find buffer '%s'" name)
	      else pos
	  in
	  let get_pvec name =
	    let (v,_) =the_internal_buffers.(get_buffer_index name) in v
	  in
	  let get_svec name =
	    let (_,v) =the_internal_buffers.(get_buffer_index name) in v
	  in
	  let v_viv_names =
	    Array.map
	      (fun loms -> Printf.sprintf "%s$vivificator.%s" prefix loms.loms_name)
	      script.las_op_matrices
	  in
	  let v_ddiffops =
	    Array.map
	      (fun loms -> ddiffop_from_string loms.loms_symbolic_operator)
	      script.las_op_matrices
	  in
	  let the_vivificators =
	    let name_vivs = Printf.sprintf "%s$vivificators" prefix in
	      nsim_vivificators_create ccpla name_vivs the_mwes
		(Array.mapi
		   (fun nr_loms loms ->
		      let name_this_viv = v_viv_names.(nr_loms)
		      and ddiffop = v_ddiffops.(nr_loms)
		      and mwe_name_le = loms.loms_mwe_name_le
		      and mwe_name_ri = loms.loms_mwe_name_ri
		      and opt_mwe_name_mid = loms.loms_mwe_name_mid
		      in
			(name_this_viv,ddiffop,mwe_name_le,mwe_name_ri,opt_mwe_name_mid))
		   script.las_op_matrices)
	  in
	  let the_op_matrices =
	    Array.mapi
	      (fun nr_loms loms ->
		 let mx_abs_name = Printf.sprintf "%s$op.%s" prefix loms.loms_name in
		 let () =
		   logdebug
		     (Printf.sprintf "make_linalg_machine - making matrix '%s' op='%s'"
			mx_abs_name loms.loms_symbolic_operator)
		 in
		 let ddiffop = v_ddiffops.(nr_loms) in
		 let (mxdim_le,mxdim_ri) = ddiffop.Ddiffop_parser.diff_mxdim in
		 let mwe_le = get_mwe loms.loms_mwe_name_le in
		 let mwe_ri = get_mwe loms.loms_mwe_name_ri in
		 let (_,_,distrib_le) = ddiffop_mxdim_index_mapping mwe_le mxdim_le in
		 let (_,_,distrib_ri) = ddiffop_mxdim_index_mapping mwe_ri mxdim_ri in
		 let () = logdebug
		   (Printf.sprintf " Sparse matrix distrib=(%s,%s)"
		      (int_array_to_string distrib_le)
		      (int_array_to_string distrib_ri))
		 in
		 let ccpla_matrix =
		   ccpla.ccpla_matrix_create
		     mx_abs_name "mpiaij"
		     distrib_le
		     distrib_ri
		 in
		   ccpla_matrix)
	      script.las_op_matrices
	  in
	  let () = (* Execute all the vivificators in parallel *)
	    let cmds_vivify =
	      Array.init
		(Array.length script.las_op_matrices)
		(fun nr_loms ->
		   let name_viv = v_viv_names.(nr_loms)
		   and the_matrix = the_op_matrices.(nr_loms)
		   in
		     Array.make nr_nodes
		       (DCOM_opcode
			  (NSIM_OP_vivificator_exec
			     (name_viv,
			      false, (* Is this a re-vivification of an initialised matrix? *)
			      true,  (* Do we need to execute assembly after vivification?  *)
			      None),
			   [|the_vivificators;
			     the_matrix;
			     the_mwes;
			     (* optionally drh_field_mid, XXX unsupported now *)
			   |]))
		)
	    in
	    let () = Array.iter (fun c -> Queue.push c !(ccpla.ccpla_queue)) cmds_vivify in
	    let () = master_process_queue ccpla in
	      ()
	  in
	  let get_sparse_matrix name =
	    let pos = array_position_if (fun loms_spec -> loms_spec.loms_name=name)
	      script.las_op_matrices 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find sparse matrix '%s'" name)
	      else the_op_matrices.(pos)
	  in
	  let () = reportmem "par_sparse_matrices" in
	  let tmp_start_dense_matrices = Unix.gettimeofday() in
	  let () = loginfo( Printf.sprintf "Starting to build dense matrices (BEM)" ) in
	  let () = loginfo2( Printf.sprintf "T=%.6f dense_matrices start" (tmp_start_dense_matrices)) in
	  let the_dense_matrices =
	    Array.map
	      (fun ldms ->
		 if ldms.ldms_hlib
		 then
		   let mwe = get_mwe ldms.ldms_mwe_name in
		     (* Ugly code ahead: Somehow we seem not to have found the proper language yet
			for this geometry stuff...
		     *)
		   let (dof_stem,_) = ldms.ldms_dof_name in
		   let (lts,stl,_) =
		     boundary_shortvec_info dof_stem mwe ldms.ldms_boundary_spec
		   in
		   let ddd = logdebug "Computing 3d space angles" in
		   let geom_info =
		     Bem3d.mwe_dof_3d_surface_triangles_and_space_angles_and_coords
		       ~inside_property:"material" (* DDD XXX REPAIR: need ldms.ldms_inside_property *)
		       mwe (lts,stl)
		   in
		   let (algorithm,nfdeg,nmin,eta,eps_aca,eps,p,kmax) = ldms.ldms_hlib_params in
		   let hmx =
		     Bem3d.bem_hmatrix
		       ~algorithm ~nfdeg ~nmin ~eta ~eps_aca ~eps ~p ~kmax
		       ~geom_info
                       ?lattice_info:ldms.ldms_lattice_info
		       (* ldms.ldms_inside_regions
			  ldms.ldms_dof_name
		       *)
		       ldms.ldms_dof_name
			  mwe
		   in BEM_HLib hmx
		 else
		   let mx_abs_name = Printf.sprintf "%s$bem.%s" prefix ldms.ldms_name in
		   let mwe = get_mwe ldms.ldms_mwe_name in
		   let (dof_stem,_) = ldms.ldms_dof_name in
		   let (_,_,distrib) = boundary_shortvec_info dof_stem mwe ldms.ldms_boundary_spec in
		   let mx =
		     ccpla.ccpla_matrix_create mx_abs_name "mpidense" distrib distrib
		   in
		   let cmds_bem =
		     Array.make nr_nodes
		       (DCOM_opcode ((NSIM_OP_fill_bem ldms), [|the_mwes; mx|]))
		   in
		   let () = Queue.push cmds_bem !(ccpla.ccpla_queue) in
		     BEM_PETSC mx
	      ) script.las_dense_matrices
	  in
	  let () = master_process_queue ccpla in
	  let tmp_end_dense_matrices = Unix.gettimeofday() in
	  let () = loginfo2 (Printf.sprintf "T=%.6f dense_matrices end" tmp_end_dense_matrices) in
	  let () = loginfo (Printf.sprintf "Populating BEM took %f seconds"
			      (tmp_end_dense_matrices-. tmp_start_dense_matrices))
	  in
	  let get_dense_matrix name =
	    let pos = array_position_if (fun ldms_spec -> ldms_spec.ldms_name=name)
	      script.las_dense_matrices 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find dense matrix '%s'" name)
	      else the_dense_matrices.(pos)
	  in
	  let () = reportmem "par_dense_matrices" in
	  let get_ksp_mwe_ri ksp_name =
	    let pos_ksp = array_position_if (fun ksp_spec -> ksp_spec.lks_name=ksp_name)
	      script.las_ksps 0
	    in
	    let ksp_spec = script.las_ksps.(pos_ksp) in
	    let mx_name = ksp_spec.lks_matrix_name in
	    let pos_loms = array_position_if (fun loms_spec -> loms_spec.loms_name=mx_name)
	      script.las_op_matrices 0
	    in
	    let loms_spec = script.las_op_matrices.(pos_loms) in
	    let mwe_name = loms_spec.loms_mwe_name_ri in
	      get_mwe mwe_name
	  in
	  let the_ksps =
	    Array.map
	      (fun ksp_spec ->
		 let mx = get_sparse_matrix ksp_spec.lks_matrix_name in
		 let mxp = get_sparse_matrix ksp_spec.lks_precond_name in
		 let ksp_abs_name = Printf.sprintf "%s$ksp.%s" prefix ksp_spec.lks_name in
		 (* C4R (mf, 2 Jun 2008): let () = Printf.printf "DDD setup KSP %s -- NOTE: disabled ksp_type and PC_type as matrix type mpiaij will throw an error when we try to do the symbolic ILU\n%!" ksp_abs_name in *)
		 let matnullspace =
		   match ksp_spec.lks_nullspace with
		     | None -> None
		     | Some (has_constant,subfields) ->
			 let mwe = get_ksp_mwe_ri ksp_spec.lks_name in
			 let vecs =
			   mwe_matnullspace_vectors
			     ~fun_make_vector:(fun ~nr_vec:n ->
						 ccpla.ccpla_vector_create
						   (Printf.sprintf "%s_vnullspace_%d" ksp_spec.lks_name n)
						   mwe.mwe_distribution)
				~fun_add_entry:(fun v n x ->
						  (* NOTE: this is awkwardly inefficient, but
						     as we have to do a hash lookup for every vec
						     entry we set. But we do not use much time here
						     anyway.
						  *)
						  let vp = ccpla.ccpla_petsc_vector v in
						    Mpi_petsc.vector_set vp n x)
				~fun_assemble_vector:ccpla.ccpla_vector_assemble
				mwe subfields
			 in
			   Some (has_constant,vecs)
		 in
		 let ksp =
		   ccpla.ccpla_ksp_create
		     ~tolerances:(ksp_spec.lks_rtol,ksp_spec.lks_atol,ksp_spec.lks_dtol,ksp_spec.lks_maxits)
		     ?initial_guess_nonzero:ksp_spec.lks_initial_guess_nonzero
		     (* XXX Disabled, as symbolic ILU does not work for parallelized matrices:
			?ksp_type:ksp_spec.lks_ksp_type ?pc_type:ksp_spec.lks_pc_type
		     *)
		     ?ksp_type:ksp_spec.lks_ksp_type
		     ?pc_subtype:ksp_spec.lks_pc_type
		     ?matnullspace
		     ksp_abs_name mx mxp in
		 (* C4R (mf, 2 Jun 2008): let () = Printf.printf "XXX TODO: use las_ksp_spec params!\n%!" in *)
		   ksp)
	      script.las_ksps
	  in
	  let get_ksp name =
	    let pos = array_position_if (fun ksp_spec -> ksp_spec.lks_name=name)
	      script.las_ksps 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find ksp '%s'" name)
	      else the_ksps.(pos)
	  in
	  let set_ksp_tolerances name (rtol,atol,dtol,maxits) =
	    let ksp = get_ksp name in
	      ccpla.ccpla_ksp_manipulate_tolerances ksp
		(fun (old_rtol,old_atol,old_dtol,old_maxits) ->
		   let new_rtol = match rtol with | None -> old_rtol | Some x -> x
		   and new_atol = match atol with | None -> old_atol | Some x -> x
		   and new_dtol = match dtol with | None -> old_dtol | Some x -> x
		   and new_maxits = match maxits with | None -> old_maxits | Some x -> x
		   in
		     (new_rtol,new_atol,new_dtol,new_maxits))
	  in
	  let get_ksp_tolerances name =
	    let ksp = ccpla.ccpla_petsc_ksp (get_ksp name) in
	      Mpi_petsc.ksp_get_tolerances ksp
	  in
	  let () = reportmem "par_ksps" in
	  let the_swexs =
	    Array.map
	      (fun swex_spec ->
		 let () =
		   loginfo (Printf.sprintf "Creating site-wise-executor '%s'" swex_spec.lss_name)
		   (* logdebug (Printf.sprintf "Creating site-wise-executor '%s'" swex_spec.lss_name) *)
		 in
		 let swex_name = Printf.sprintf "%s$swex.%s" prefix swex_spec.lss_name in
		 let swex =
		   nsim_swex_create ccpla
		     swex_name swex_spec.lss_c_code
		     (Array.map get_mwe swex_spec.lss_field_mwes)
		     (Array.map get_mwe swex_spec.lss_cofield_mwes)
		     swex_spec.lss_aux_args
		 in
		   swex)
	      script.las_swexs
	  in
	  let get_swex name =
	    let pos = array_position_if (fun swex_spec -> swex_spec.lss_name=name)
	      script.las_swexs 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find site-wise-executor '%s'" name)
	      else the_swexs.(pos)
	  in
	  let () = reportmem "par_swexs" in
	  let the_jplans = [||] in (* old relic *)
	  let get_jplan name =
	    let pos = array_position_if (fun jplan_spec -> jplan_spec.ljps_name=name)
	      script.las_jplans 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find jacobi plan '%s'" name)
	      else the_jplans.(pos)
	  in
	  let () = reportmem "par_jplans" in
	  let compile_command nr_sequence cmd =
	    (* Every command is compiled to a (short, usually 1-statement) DRES_sequence
	       (= DCOM_command array). These are then assembled.
	    *)
	    match cmd with
	      | LC_vec_distribute (name_master,name_data) ->
		  let pos_master = locate_master_local_vector nr_sequence name_master in
		    [|Array.make nr_nodes (DCOM_getfrom_master(pos_master,get_pvec name_data))|]
	      | LC_vec_collect (name_master,name_data) ->
		  let pos_master = locate_master_local_vector nr_sequence name_master in
		    [|Array.make nr_nodes (DCOM_sendto_master(pos_master,get_pvec name_data))|]
	      | LC_pvec_scale (name,scale) ->
		  [|Array.make nr_nodes (DCOM_vec_scale(get_pvec name,scale))|]
	      | LC_pvec_pointwise (name,f) ->
		  [|Array.make nr_nodes (DCOM_vec_pointwise(get_pvec name,f))|]
	      | LC_pvec_axpby (ca,x,cb,y) ->
		  [|Array.make nr_nodes (DCOM_vec_sety_axpby (ca,cb,intensive_params,get_pvec x,get_pvec y))|]
	      | LC_iparams_axpby (ca,x,cb,y) ->
		  [|Array.make nr_nodes
		      (DCOM_manipulate_parameters
			 (intensive_params,
			  (fun names vals ->
			     let param_pos name = array_position name names 0 in
			     let a = match ca with
			       | CCCOEFF_float x -> x
			       | CCCOEFF_var s -> vals.(param_pos s)
			     in
			     let b = match cb with
			       | CCCOEFF_float x -> x
			       | CCCOEFF_var s -> vals.(param_pos s)
			     in
			     let pos_x = param_pos x
			     and pos_y = param_pos y in
			     let val_x = if pos_x = (-1) then 1.0 else vals.(pos_x)
			     and val_y = if pos_y = (-1) then 1.0 else vals.(pos_y)
			     in
			       vals.(pos_y) <- a*.val_x+.b*.val_y)))
		  |]
	      | LC_pvec_pull (offsets,source,dest) ->
		  [|Array.make nr_nodes (DCOM_pull (offsets,get_pvec source,get_pvec dest))|]
	      | LC_pvec_push (offsets,source,dest) ->
		  [|Array.make nr_nodes (DCOM_push (offsets,get_pvec source,get_pvec dest))|]
	      | LC_pvec_pull_fem (mwe_name,restr,source,dest) ->
		  let offsets = mwe_restr_offsets (get_mwe mwe_name) restr in
		  [|Array.make nr_nodes (DCOM_pull (offsets,get_pvec source,get_pvec dest))|]
	      | LC_pvec_push_fem (mwe_name,restr,source,dest) ->
		  let offsets = mwe_restr_offsets (get_mwe mwe_name) restr in
		  [|Array.make nr_nodes (DCOM_push (offsets,get_pvec source,get_pvec dest))|]
	      | LC_smx_x_pvec (mx,src,dest) ->
		  [|Array.make nr_nodes (DCOM_mx_x_vec (get_sparse_matrix mx,get_pvec src,get_pvec dest))|]
	      | LC_dmx_x_pvec (mx,src,dest) ->
		  (* XXX NOTE: has to be adjusted for HLib support! *)
		  (match get_dense_matrix mx with
		     | BEM_PETSC mx ->
			 [|Array.make nr_nodes
			     (DCOM_mx_x_vec (mx,get_pvec src,get_pvec dest))
			 |]
		     | BEM_HLib hmx ->
			 [|Array.init nr_nodes
			     (fun n ->
				let hmx_buffers =
				  if n = 0
				  then
				    Some (hmx,get_svec src, get_svec dest)
				  else None
				in
				  (DCOM_opcode
				     (NSIM_OP_hmatrix_mult hmx_buffers,
				      [|get_pvec src;get_pvec dest|])))
			 |])
	      | LC_vec_pointwise_mult (src1,src2,dest) ->
		  [|Array.make nr_nodes (DCOM_vec_pointwise_mult (get_pvec src1,get_pvec src2,get_pvec dest))|]
	      | LC_cofield_to_field (mwe_name,v_name) ->
		  let v = get_pvec v_name in
		  let (v_scale,_) = the_internal_buffers.(nr_script_buffers+get_mwe_index mwe_name) in
		  [|Array.make nr_nodes (DCOM_vec_pointwise_divide (v,v_scale,v))|]
	      | LC_jplan_execute (name_jplan,names_vecs) -> [||]
	      | LC_psolve (ksp,src,dest) ->
		  [|Array.make nr_nodes (DCOM_ksp_solve (get_ksp ksp,get_pvec src,get_pvec dest))|]
	      | LC_psite_wise_iparams (name_swex,names_fields,names_cofields) ->
		  let nr_drh = 2 + (Array.length names_fields) + (Array.length names_cofields) in
		  let distributed_resources =
		    Array.init nr_drh
		      (fun n ->
			 if n=0 then get_swex name_swex
			 else if n=1 then intensive_params
			 else if n <= 1+Array.length names_fields then
			   get_pvec names_fields.(n-2)
			 else
			   get_pvec names_cofields.(n-2-Array.length names_fields))
		  in
		    [|Array.make nr_nodes
			(DCOM_opcode (NSIM_OP_swex
					(Array.length names_fields,script.las_intensive_params),
				      distributed_resources))|]
		    (* XXX OBSOLETED!
		       | LC_psite_wise (name_swex,names_fields,names_cofields,params) ->
		       let drh_fields = Array.map get_pvec names_fields
		       and drh_cofields = Array.map get_pvec names_cofields
		       and swex = get_swex name_swex in
		       let drh_args = array_join [|[|swex|];drh_fields;drh_cofields|] in
		       [|Array.make nr_nodes
		       (DCOM_opcode
		       ((NSIM_OP_swex (Array.length drh_fields,params)),
		       drh_args))|]
		    *)
	      | LC_gosub name_stem ->
		  (* Note: this works even though we "fake" a DRH here because the DRH only serves to
		     "hold on to" the distributed resource "script". This, however, also is done by
		     the execute_on function of the lam, so as long as the LAM is around, we do not
		     forget the script.
		  *)
		  let name = Printf.sprintf "%s$commseq.%s" prefix name_stem in
		  [|(* Array.make nr_nodes (DCOM_master (fun () -> Printf.printf "DDD GOSUB '%s'\n%!" name)); *)
		    Array.make nr_nodes (DCOM_execute_sequence (DRH name));
		    (* Array.make nr_nodes (DCOM_master (fun () -> Printf.printf "DDD RETURN (from GOSUB '%s')\n%!" name)); *)
		  |]
              | LC_callext fn ->
                  [|Array.make nr_nodes (DCOM_rec_master (fun () -> fn !time_in_rhs));|]
	      | LC_debug_printvec_1cpu (title,name,max_len) ->
		  (* When executing in parallel, we do not print the vector data, as we would have
		     to collect it first. We do, however, at least send out a message which debug
		     statement we encountered.
		  *)
		  [|Array.make nr_nodes
		      (DCOM_master
			 (fun () ->
			    let pvec = Ccpla.ccpla_petsc_vector ccpla (get_pvec name) in
			    let () = Printf.printf "DDD DEBUG '%s' vec='%s'\n***\n%!" title name in
			      Mpi_petsc.with_petsc_vector_as_bigarray pvec
				(fun ba ->
				   let ba_len = Bigarray.Array1.dim ba in
				   let len = if max_len=0 then ba_len else min max_len ba_len in
				   let () =
				     for i=0 to len-1 do
				       Printf.printf " %f" ba.{i};
				     done
				   in Printf.printf "\n***\n%!")))
		  |]
	      | LC_start_timer name ->
		  [|Array.make nr_nodes
		      (DCOM_master
			 (fun () ->
			    let t0 = Unix.gettimeofday() in
			      Hashtbl.replace timers_running name t0
			 ))
		  |]
	      | LC_stop_timer name ->
		  [|Array.make nr_nodes
		      (DCOM_master
			 (fun () ->
			    try
			      let t1 = Unix.gettimeofday() in
			      let t0 = Hashtbl.find timers_running name in
			      let accum = try Hashtbl.find timers_accum name with | Not_found -> 0.0 in
				Hashtbl.replace timers_accum name (accum+.(t1-.t0))
			    with
			      | Not_found -> failwith (Printf.sprintf "Invalid timer: '%s'" name))
		      )
		  |]
	      | LC_report_timers names ->
		  [|Array.make nr_nodes
		      (DCOM_master
			 (fun () ->
			    let () = Printf.printf "=== TIMERS ===\n" in
			    let () =
			      array_foreach_do names
				(fun name ->
				   let dt = try Hashtbl.find timers_accum name with | Not_found -> 0.0 in
				     Printf.printf "%-20s: %10.6f sec\n" name dt)
			    in Printf.printf "\n%!"))
		  |]
	      | LC_clear_timers names ->
		  [|Array.make nr_nodes
		      (DCOM_master
			 (fun () ->
			    array_foreach_do names
			      (fun name ->
				 Hashtbl.replace timers_accum name 0.0)))|]
	      | _ ->
		  failwith "XXX write me!"
	  in
	  let () = logdebug (Printf.sprintf "DDD make_linalg_machine #8") in
	  let compiled_sequences =
	    Array.mapi
	      (fun nr_sequence (seq_raw_name,_,_,seq) ->
		 let () = logdebug (Printf.sprintf "make_linalg_machine: compiling sequence '%s'" seq_raw_name) in
		 let seq_name = Printf.sprintf "%s$commseq.%s" prefix seq_raw_name in
		 let cseq =
		   (*
		     array_join (Array.map (compile_command nr_sequence) seq)
		   *)
		   array_join
		     (Array.mapi
			(fun n s ->
			   let () = logdebug (Printf.sprintf " COMP s=%3d c=%3d" nr_sequence n) in
			     (compile_command nr_sequence s))
			seq)
		 in
		 let pseq = ccpla.ccpla_sequence_create seq_name cseq in
		   pseq)
	      script.las_command_sequences
	  in
	  let () = reportmem "par_compiled" in
	  let ts_remember_drhs = ref [] in
	  let the_timesteppers = Array.map
	    (fun lts ->
	       let () = logdebug (Printf.sprintf "Building parallel timestepper '%s'" lts.lts_name) in
	       let vecs_fields = Array.map get_pvec lts.lts_names_phys_field_buffers in
	       let script_compute_velocities =
		 let nr_seq =
		   array_position_if
		     (fun (name,_,_,_) -> lts.lts_name_seq_velocities=name)
		     script.las_command_sequences 0
		 in
		 let () =
		   (if nr_seq = (-1)
		    then
		      failwith
			(Printf.sprintf "Unknown velocity command sequence '%s' (known: %s)"
			   lts.lts_name_seq_velocities
			   (string_array_to_string
			      (Array.map (fun (n,_,_,_) -> n) script.las_command_sequences)))
		    else ())
		 in
		   compiled_sequences.(nr_seq)
	       in
	       let jacobi_op_names =
		 array_mapfilter
		   (fun (pfd_type,pfd_data) -> if pfd_type = "OPERATOR" then Some pfd_data else None)
		   lts.lts_phys_field_derivs
	       in
	       let jacobi_op_drhs =
		 Array.map get_sparse_matrix jacobi_op_names
	       in
	       let () =
		 Array.iter
		   (fun drh -> ts_remember_drhs := drh :: !ts_remember_drhs) jacobi_op_drhs
	       in
	       let jacobi_operators =
		 nsim_jacobi_operators_create ccpla
		   (Printf.sprintf "%s$jops.%s" prefix lts.lts_name)
		   jacobi_op_names jacobi_op_drhs
	       in
	       let () = ts_remember_drhs := jacobi_operators :: !ts_remember_drhs in
	       let pts =                              (* pts=Parallel TimeStepper *)
		 nsim_timestepper_create ccpla
		   ~prefix
		   ~vecs_fields
		   ~script_compute_velocities
		   ~mwes:the_mwes
		   ~timestepper_spec:lts
		   ~jacobi_operators
	       in
		 pts)
	    script.las_timesteppers
	  in
	  let get_timestepper name =
	    let pos = array_position_if (fun lts -> lts.lts_name=name)
	      script.las_timesteppers 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find timestepper '%s'" name)
	      else the_timesteppers.(pos)
	  in
	  let get_timestepper_spec name =
	    let pos = array_position_if (fun lts -> lts.lts_name=name)
	      script.las_timesteppers 0
	    in
	      if pos = (-1) then failwith (Printf.sprintf "Could not find timestepper '%s'" name)
	      else script.las_timesteppers.(pos)
	  in
	  let () = reportmem "par_timesteppers" in
	  let fun_execute_on seq_name fields cofields =
	    let nr_seq =
	      array_position_if (fun (name,_,_,_) -> seq_name=name) script.las_command_sequences 0 in
	    let this_cseq_arg_vectors=cseq_arg_vectors.(nr_seq) in
	    let () =
	      (if nr_seq = (-1)
	       then failwith (Printf.sprintf "Unknown command sequence '%s'" seq_name)
	       else ())
	    in
	    let nr_fields = Array.length fields in
	    let nr_cofields = Array.length cofields in
	    let () =
	      begin
		for i=0 to nr_fields-1 do
		  let () = ensure_field_is_unrestricted fields.(i) in (* XXX *)
		  let FEM_field (_,_,v) = fields.(i) in
		    this_cseq_arg_vectors.(i) <- v
		done;
		for i=0 to nr_cofields-1 do
		  let () = ensure_cofield_is_unrestricted cofields.(i) in (* XXX *)
		  let FEM_cofield (_,_,v) = cofields.(i) in
		    this_cseq_arg_vectors.(nr_fields+i) <- v
		done;
	      end
	    in
	    let pseq = compiled_sequences.(nr_seq) in
	      ccpla.ccpla_sequence_execute ~local_vectors:this_cseq_arg_vectors pseq
	  in
	  let fun_get_set_field do_get field_name (FEM_field (mwe_target,restr_target,v_target) as the_field) =
	    let buf_ix = get_buffer_index field_name in
	    let (_,mwe_name,restr,is_field,_) = script.las_internal_buffers.(buf_ix) in
	    let () = (if not(mwes_are_compatible ~restr1:restr_target ~restr2:restr mwe_target (get_mwe mwe_name))
		      then failwith (Printf.sprintf "get_field: incompatible field and target MWEs (may be due to different DOF selections)! (requested field='%s' target mwe='%s'" field_name mwe_target.mwe_name)
		      else if not is_field then failwith (Printf.sprintf "get_field: Not a field: '%s'" field_name)
		      else ())
	    in
	      (if do_get
	       then ccpla.ccpla_vector_collect
	       else ccpla.ccpla_vector_distribute)
		~local_vectors:[|v_target|] 0
		(let (v,_) = the_internal_buffers.(buf_ix) in v)
	  in
	  let fun_get_set_cofield do_get cofield_name (FEM_cofield (mwe_target,restr_target,v_target) as the_cofield) =
	    let buf_ix = get_buffer_index cofield_name in
	    let (_,mwe_name,restr,is_field,_) = script.las_internal_buffers.(buf_ix) in
	    let () = (if not(mwes_are_compatible ~restr1:restr_target ~restr2:restr mwe_target (get_mwe mwe_name))
		      then failwith (Printf.sprintf "get_cofield: incompatible cofield and target MWEs (may be due to different DOF selections)! (requested cofield='%s' target mwe='%s'" cofield_name mwe_target.mwe_name)
		      else if not is_field then failwith (Printf.sprintf "get_cofield: Not a cofield: '%s'" cofield_name)
		      else ())
	    in
	      (if do_get
	       then ccpla.ccpla_vector_collect
	       else ccpla.ccpla_vector_distribute)
		~local_vectors:[|v_target|] 0
		(let (v,_) = the_internal_buffers.(buf_ix) in v)
	  in
	  let master_iparams = ccpla.ccpla_iparams intensive_params in
	  let get_iparam name =
	    let (names,vals) = master_iparams in
	    let pos = array_position name names 0 in
	      if pos= (-1)
	      then
		failwith (Printf.sprintf "Unknown intensive parameter: '%s' (Have: %s)"
			    name (string_array_to_string names))
	      else
		vals.(pos)
	  in
	  let set_iparam name v =
	    let (names,vals) = master_iparams in
	    let pos = array_position name names 0 in
	      if pos= (-1)
	      then
		failwith (Printf.sprintf "Unknown intensive parameter: '%s' (Have: %s)"
			    name (string_array_to_string names))
	      else
		ccpla.ccpla_manipulate_params intensive_params (fun names vals -> vals.(pos) <- v)
	  in
	  let fun_get_jacobian name =
	    let (mx_J,mx_PC_J,jplan) = get_jplan name in
	      POCM_ccpla mx_J
	  in
	  let fun_timestepper_initialize_from_physics name t_initial rel_tol abs_tol =
	    let drh_pts = get_timestepper name in
	    let cmds =
	      Array.make nr_nodes
		(DCOM_opcode ((NSIM_OP_init_timestepper (t_initial,rel_tol,abs_tol)),
			      [|drh_pts|]))
	    in
	    let () = Queue.push cmds !(ccpla.ccpla_queue) in
	    let () = master_process_queue ccpla in
	      ()
	  in
	  let fun_timestepper_advance name exact_tstop t_final maxsteps =
	    let drh_pts = get_timestepper name in
	    let cmds =
	      Array.make nr_nodes
		(DCOM_opcode ((NSIM_OP_advance_timestepper (exact_tstop, t_final, maxsteps)),[|drh_pts|]))
	    in
	    let () = Queue.push cmds !(ccpla.ccpla_queue) in
	    let () = master_process_queue ccpla in
	      (* Bad hack: should not resolve distributed resource handle (drh) on master, 24 Jan 2008 *)
	    let (DRH pts_name) = drh_pts in
	    let DRES_opdata ((NSIM_RES_timestepper pts),_) = Ccpla.ccpla_get_resource ccpla pts_name in
	      pts.pts_time_reached
	  in
	  let fun_timestepper_get_cvode name =
	    let drh_pts = get_timestepper name in
	      (* Bad hack: should not resolve distributed resource handle (drh) on master, 24 Jan 2008 *)
	    let (DRH pts_name) = drh_pts in
	    let DRES_opdata ((NSIM_RES_timestepper pts),_) = Ccpla.ccpla_get_resource ccpla pts_name in
	      match !(pts.pts_cvode) with
		| None -> failwith "time stepper was not initialised yet"
		| Some cvode -> cvode
	  in
	  let fun_timestepper_timings name cmd =
	    let drh_pts = get_timestepper name in
	      (* Bad hack: should not resolve distributed resource handle (drh) on master, 24 Jan 2008 *)
	    let (DRH pts_name) = drh_pts in
	    let DRES_opdata ((NSIM_RES_timestepper pts),_) = Ccpla.ccpla_get_resource ccpla pts_name in
	      pts.pts_timing_control cmd
	  in
	  let fun_internal_ccpla_resources () =
	    ccpla_registered_resources ccpla
	  in
	  let () = reportmem "par_final" in
	    {
	      _resources_to_stick_to=
		(the_vivificators :: the_mwes :: !ts_remember_drhs);
	      get_mwe=get_mwe;
	      get_iparam=get_iparam;
	      set_iparam=set_iparam;
	      set_ksp_tolerances=set_ksp_tolerances;
	      get_ksp_tolerances=get_ksp_tolerances;
	      get_field=fun_get_set_field true;
	      get_cofield=fun_get_set_cofield true;
	      set_field=fun_get_set_field false;
	      set_cofield=fun_get_set_cofield false;
	      get_jacobian=fun_get_jacobian;

	      get_potential=(fun _-> failwith "XXX TODO: get_potential() not implemented yet for parallel linalg_machine!");
	      get_timers=fun_get_timers;
	      timestepper_initialize_from_physics=fun_timestepper_initialize_from_physics;
	      timestepper_advance=fun_timestepper_advance;
	      timestepper_get_cvode=fun_timestepper_get_cvode;
	      timestepper_timings=fun_timestepper_timings;
	      internal_ccpla_resources=fun_internal_ccpla_resources;
	      execute_on=fun_execute_on;
	    }
;;

let bem_field_strength ?epsilon lam ~bem_name ~boundary_vec_name pos_observer =
  let dim = Array.length pos_observer in
    (* ^ At present, we can only do dim=3, but let's keep this generic *)
  let v_grad = Array.make dim 0.0 in
  let fun_grad = symm_grad_storing_result ?epsilon dim in
  let fun_pot pos =
    let z = lam.get_potential bem_name boundary_vec_name pos in
    let () = Printf.printf "phi@%s=%.18f\n%!" (float_array_to_string pos) z in
      z
  in
  let () = fun_grad v_grad fun_pot pos_observer in
    v_grad
;;

let ddd_bem_potential lam ~bem_name ~boundary_vec_name pos_observer =
  let dim = Array.length pos_observer in
    (* ^ At present, we can only do dim=3, but let's keep this generic *)
  let fun_pot pos =
    lam.get_potential bem_name boundary_vec_name pos
  in
    fun_pot pos_observer
;;

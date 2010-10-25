(* (C) 2006 Dr. Thomas Fischbacher
   SUNDIALS interface

   ocamlc -I ../snippets -I ../mpi_petsc -i sundials_sp.ml

   NOTE: this can utilize both the serial as well as the parallel
   sundials N_Vector interface. Above this, the interface is rather
   primitive, not trying to wrap up too much for higher levels.

   (We actually discovered that too much intelligence can be more of a
   hindrance than helpful with this module.)

*)

exception Sundials_sp_caml_interface_exn of string;;

let _ = Callback.register_exception
  "ocaml_exn_sundials_sp_caml_interface"
  (Sundials_sp_caml_interface_exn "")
;;

let version () = Snippets.md5
  (String.concat ":" ["$Id$";
                      (Snippets.version());
                      (Mpi_petsc.version())])
;;

type cvode;;

type simple_c_array =
    (float, Bigarray.float64_elt, Bigarray.c_layout)
      Bigarray.Array1.t;;

let make_simple_c_array n =
  Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
;;

type mirage_dof_mapping =
    {mdm_expand: ba_short:simple_c_array -> ba_long:simple_c_array -> unit -> unit;
     mdm_reduce: ba_long:simple_c_array -> ba_short:simple_c_array -> unit -> unit;
     mdm_longbuffer_lts: simple_c_array;
     mdm_longbuffer_stl: simple_c_array;
     mdm_shortbuffer_lts: simple_c_array;
     mdm_shortbuffer_stl: simple_c_array;
    }
;;

type preconditioning_type =
    PREC_NONE | PREC_LEFT | PREC_RIGHT | PREC_BOTH
;;

(* XXX REDO: *)
type ty_fun_rhs_raw =
    (float * simple_c_array * simple_c_array) -> unit -> int;;

type ty_fun_preconditioner_setup_raw =
    (bool *
       float * float *
       simple_c_array *
       simple_c_array *
       simple_c_array *
       simple_c_array *
       simple_c_array) -> unit -> (bool * int)
;;

type ty_fun_preconditioner_solve_raw =
    (int *
       float * float * float *
       simple_c_array *
       simple_c_array *
       simple_c_array *
       simple_c_array *
       simple_c_array) -> unit -> int
;;


type ty_fun_jacobian_times_vector_raw =
    (float *
       simple_c_array *
       simple_c_array *
       simple_c_array *
       simple_c_array *
       simple_c_array) -> unit -> int
;;

external _sundials_init:
  string -> string -> string -> unit =
    "caml_sundials_sp_init";;


let sundials_init ~path_cvode ~path_nvec_serial ~path_nvec_parallel () =
  _sundials_init path_nvec_serial path_nvec_parallel path_cvode
;;

external cvode_make_dummy: unit -> cvode = "caml_sundials_sp_cvode_make_dummy";;

let dummy_cvode = cvode_make_dummy();;


external cvode_make_raw_ba :
  (Mpi_petsc.communicator
   * bool (* is_parallel *)
   * ty_fun_rhs_raw
   * int (* ba_initial_global_length *)
   * simple_c_array (* ba_initial *)
   * float	    (* initial time *)
   * float	    (* rel_tol *)
   * float          (* abs_tol *)
  ) -> cvode
  = "caml_sundials_sp_cvode_make_raw_ba"
;;



external cvode_reinit_raw: cvode -> float -> float -> float -> unit =
  "caml_sundials_sp_cvode_reinit";;

external cvode_setup_cvspgmr_raw:
  cvode -> int -> preconditioning_type -> ty_fun_preconditioner_setup_raw ->
  ty_fun_preconditioner_solve_raw -> unit =
  "caml_sundials_sp_cvode_setup_cvspgmr_raw";;

external cvode_setup_jacobi_times_vector_raw:
  cvode -> ty_fun_jacobian_times_vector_raw -> unit =
  "caml_sundials_sp_cvode_setup_jacobi_times_vector_raw";;

external cvode_set_max_order: cvode -> int -> unit =
  "caml_sundials_sp_cvode_set_max_ord";;

external cvode_set_max_num_steps: cvode -> int -> unit =
  "caml_sundials_sp_cvode_set_max_num_steps";;

external cvode_set_init_step: cvode -> float  -> unit =
  "caml_sundials_sp_cvode_set_init_step";;

external cvode_set_min_step: cvode -> float  -> unit =
  "caml_sundials_sp_cvode_set_min_step";;

external cvode_set_max_step: cvode -> float  -> unit =
  "caml_sundials_sp_cvode_set_max_step";;

external cvode_set_tolerances: cvode -> float -> float -> unit =
  "caml_sundials_sp_cvode_set_tolerances";;

external cvode_get_num_steps: cvode -> float =
  "caml_sundials_sp_cvode_get_num_steps";;

external cvode_get_step_info: cvode -> float * float * float =
  "caml_sundials_sp_cvode_get_step_info";;

external cvode_advance_raw_ba:
  (Mpi_petsc.communicator * bool * cvode * simple_c_array *
   int * float * int * bool) -> float =
  "caml_sundials_sp_cvode_advance_raw_ba";;

external cvode_get_integrator_stats_raw:
  cvode -> (float*float*float*float*float*float*float*float*float*float)
  = "caml_sundials_sp_cvode_get_integrator_stats";;

external cvode_get_precond_stats_raw:
  cvode -> (float*float*float*float*float*float)
  = "caml_sundials_sp_cvode_get_precond_stats";;

external cvode_get_current_time:
  cvode -> float = "caml_sundials_sp_cvode_get_current_time";;

external cvode_get_nonlinsolv_stats_raw:
  cvode -> (float*float) = "caml_sundials_sp_cvode_get_nonlinsolv_stats";;

external cvode_get_tol_scale_factor:
  cvode -> float = "caml_sundials_sp_cvode_get_tol_scale_factor";;

external cvode_get_num_ord_red:
  cvode -> float = "caml_sundials_sp_cvode_get_num_ord_red";;


let cvode_reinit
    ?(initial_time=0.0)
    ?(rel_tolerance=1e-6)
    ?(abs_tolerance=1e-6)
    cvode =
  cvode_reinit_raw cvode initial_time rel_tolerance abs_tolerance
;;

(* This is the next generation (ng) interface for cvode_advance *)
let cvode_advance_ng ?comm ?max_nr_steps ?(exact_tstop=false)
                     ?(verbose=true) cvode time ba_result =
  let advmode_exacttime = 1 in (* must be consistent with the C enum! *)
  let advmode_step      = 2 in
  let advmode_time      = 4 in
  let (ns, check_ns) =
    match max_nr_steps with
      Some x -> if x > 0 then (x, advmode_step) else (0, 0)
    | None -> (0, 0)
  in
  let exact_ts = if exact_tstop then advmode_exacttime else 0 in
  let advance_mode = advmode_time + check_ns + exact_ts in
  match comm with
  | None ->
    let c = Mpi_petsc.petsc_get_comm_self() in
      cvode_advance_raw_ba (c, false, cvode, ba_result, advance_mode,
                            time, ns, verbose)
  | Some c ->
    cvode_advance_raw_ba (c, true, cvode, ba_result, advance_mode,
                          time, ns, verbose)
;;

(* For now we stick to the old interface: this is the default! *)
let cvode_advance ?comm cvode time ba_result max_its =
    cvode_advance_ng ?comm ~max_nr_steps:max_its cvode time ba_result;;

let cvode_get_integrator_stats cvode =
  let (nsteps, nfevals, nlinsetups, netfails, qlast,
       qcur, hinused, hlast, hcur, tcur) =
    cvode_get_integrator_stats_raw cvode
  in
    [|("nsteps", nsteps); ("nfevals", nfevals); ("nlinsetups", nlinsetups);
      ("netfails", netfails); ("qlast", qlast); ("qcur", qcur);
      ("hinused", hinused); ("hlast", hlast); ("hcur", hcur);
      ("tcur", tcur)|]
;;

let cvode_get_precond_stats cvode =
  let (npevals, npsolves, nliters, nlcfails, njvevals, nfevalsLS)
      = cvode_get_precond_stats_raw cvode
  in
    [|("npevals", npevals); ("npsolves", npsolves); ("nliters", nliters);
      ("nlcfails", nlcfails); ("njvevals", njvevals);
      ("nfevalsLS", nfevalsLS)|]
;;

let cvode_get_stats cvode =
  let integrator_stats = cvode_get_integrator_stats cvode in
  let precond_stats = cvode_get_precond_stats cvode in
  let (nniters, nncfails) = cvode_get_nonlinsolv_stats_raw cvode in
  let tolsfac = cvode_get_tol_scale_factor cvode in
  let nslred = cvode_get_num_ord_red cvode in
  let extra = [|("nniters", nniters); ("nncfails", nncfails);
                ("tolsfac", tolsfac); ("nslred", nslred)|]
  in
    Array.concat [integrator_stats; precond_stats; extra]
;;


(** This function we use to just set up a problem in a simple and straightforward way.
    Mostly intended to lower the initial barrier for playing around with some systems.

    The intention is that one should be able to investigate the Korteweg-de-Vries
    equation from the command line.

    Notes:

    1. We do
    - nonparallel execution,
    - assume the "effective Jacobian" is easily accessible,
    - make a number of standard assumptions on parameters.

    2. While we may not need the Jacobian J itself in explicit form,
    if we employ the preconditioner (which we here do), we will need
    a way to solve linear equations, i.e. deal with problems of the form:

    v = (1+gamma*J)^(-1) * w

    For this reason, having the Jacobian as a matrix as well as
    resorting to PETSc is practically inevitable in our framework.
 *)

(* A CVODE time integrator needs a bunch of parameters that tweak its
   behaviour. It perhaps makes sense to deal with this complexity
   by introducing the notion of a "CVODE context" which behaves much
   like an X11 "Graphics Context"...
 *)

type cvode_context = (* "opaque type" *)
    (string, float option) Hashtbl.t
;;
(* Some uncertainty on the appropriate-ness of this internal
   representation remains, as we will map int to float and back
   for integer parameters such as "krylov_max_order",
   but we can always change that...
*)

let default_cvode_context:cvode_context =
 let ht = Hashtbl.create 17 in
 let () =
   begin
     Hashtbl.add ht "cvode_rtol" (Some 1e-8);
     Hashtbl.add ht "cvode_atol" (Some 1e-5);
     Hashtbl.add ht "cvode_max_order" (Some 2.0);
     Hashtbl.add ht "ksp_maxiter" (Some 200000000.0);
     Hashtbl.add ht "krylov_max" (Some 300.0);
     Hashtbl.add ht "ksp_rtol" None;
     (* Note: users might want to tweak these in a .petscrc
	instead, so None must be  sensible option! *)
     Hashtbl.add ht "ksp_atol" None;
     Hashtbl.add ht "ksp_dtol" None;
     Hashtbl.add ht "jacobi_prealloc_diagonal" (Some 25.0);
     Hashtbl.add ht "jacobi_prealloc_off_diagonal" (Some 50.0);
   end
 in
 ht
;;

let cvode_context_copy (ccontext:cvode_context) =
  ((Hashtbl.copy ccontext):cvode_context)
;;

let cvode_context_set_float (ccontext:cvode_context) key opt_param =
  Hashtbl.replace ccontext key opt_param
;;

let cvode_context_set_int (ccontext:cvode_context) key opt_param =
  let opt_param_f =
    match opt_param with
    | None -> None
    | Some x -> Some (float_of_int x)
  in
  Hashtbl.replace ccontext key opt_param_f
;;


let cvode_context_get_float (ccontext:cvode_context) key =
  Hashtbl.find ccontext key
;;

let cvode_context_get_int (ccontext:cvode_context) key =
  match Hashtbl.find ccontext key
  with
  | None -> None
  | Some x -> Some (int_of_float x)
;;


let ensure_libraries_are_initialized () =
  let () =
    (if Mpi_petsc.petsc_check_initialized ()
    then ()
    else
      let _ =
	Mpi_petsc.petsc_init
          [|"foo"; (*"-info"*)|]
          (Printf.sprintf "%s/.petscrc" (Unix.getenv "HOME"))
	  "[help message]"
          true (* Enable petsc signal handler *)
      in ()
    )
  in
  let () =
    (* If the user already initialized sundials beforehand, fine.
       Otherwise, we do it here, with default paths. Note that this
       might give rise to quite strange problems if the library paths
       are not correct...

       XXX this evidently has to be changed if sundials uses a
       different path than /usr/local/lib. We use that here as the
       debian package, which has sundials in /usr/lib, does not
       provide nvecparallel.so.
     *)
    sundials_init
      ~path_cvode:"/usr/local/lib/libsundials_cvode.so"
      ~path_nvec_serial:"/usr/local/lib/libsundials_nvecserial.so"
      ~path_nvec_parallel:"/usr/local/lib/libsundials_nvecparallel.so"
      ()
  in ()
;;

let cvode_setup_simple
    ?(name="cvode") (* For PETSc objects to be created *)
    ?(cvode_context=default_cvode_context)
    ?(use_jacobian=true)
    ?(debug=fun level message -> Printf.fprintf stderr "[CVODE %02d]: %s%!" level message)
    ?(fun_fill_jacobian=fun ~jacobian ~y ~ydot ~time -> ())
    ?(t_initial=0.0)
    ~y_initial
    ~fun_rhs
    (* Return value of this function:
       =0 -> Success
       >0 -> recoverable error
       <0 -> nonrecoverable error
    *)
    () =
  let () = Printf.fprintf stderr "DDD cvode_setup_simple is EXPERIMENTAL/UNTESTED code!\n%!" in
  (* === Obtain Tweaking Parameters === *)
  let rel_tol =
    match cvode_context_get_float cvode_context "cvode_rtol" with
    | Some x -> x | None -> 1e-8
  in
  let abs_tol =
    match cvode_context_get_float cvode_context "cvode_atol" with
    | Some x -> x | None -> 1e-5
  in
  let cvode_max_order =
    match cvode_context_get_int cvode_context "cvode_max_order"
    with
    | None -> 2
    | Some x -> x
  in
  let krylov_max =
    match cvode_context_get_int cvode_context "krylov_max"
    with
    | None -> 300
    | Some x -> x
  in
  let ksp_rtol = cvode_context_get_float cvode_context "ksp_rtol" in
  let ksp_atol = cvode_context_get_float cvode_context "ksp_atol" in
  let ksp_dtol = cvode_context_get_float cvode_context "ksp_dtol" in
  let ksp_maxiter = cvode_context_get_int cvode_context "ksp_maxiter" in
  let jacobi_prealloc_diagonal =
    match cvode_context_get_int cvode_context "jacobi_prealloc_diagonal"
    with
    | None -> 25
    | Some x -> x
  in
  let jacobi_prealloc_off_diagonal =
    match cvode_context_get_int cvode_context "jacobi_prealloc_off_diagonal"
    with
    | None -> 50
    | Some x -> x
  in
  (* === End Obtain Tweaking Parameters === *)
  let () = ensure_libraries_are_initialized () in
  let comm = Mpi_petsc.petsc_get_comm_self () in
  let len_y = Bigarray.Array1.dim y_initial in
  let ba_y_initial = y_initial in
  let cvode_fun_rhs (time,ba_y,ba_ydot) () =
    let success = fun_rhs ~time:time ~y:ba_y ~ydot:ba_ydot in
      success
  in
  let ropt_jacobian = ref None in
  let ropt_precond = ref None in	(* (KSP, 1-gamma*J matrix) *)
  let get_jacobian () =
    match !ropt_jacobian with
      | Some mx -> (mx,true)
      | None ->
          let () = debug 10 (Printf.sprintf "Making Jacobi MX: %d x %d\n" len_y len_y)
	  in
	  let mx_jacobi =
	    Mpi_petsc.matrix_create
	      ~communicator:comm
	      ~local_rows:len_y
	      ~local_cols:len_y
	      ~matrix_type:"seqaij"
	      ~prealloc_diagonal:jacobi_prealloc_diagonal
	      ~prealloc_off_diagonal:jacobi_prealloc_off_diagonal
	      ~auto_assembly:false
	      len_y len_y (Printf.sprintf "%s_jacobian" name)
	  in
	  let () = ropt_jacobian:=Some mx_jacobi in
	    (mx_jacobi,false)
  in
  let get_precond gamma =
    match !ropt_precond with
      | Some ksp_and_mx -> ksp_and_mx
      | None ->
	  let (jacobian,was_present) = get_jacobian() in
	  let () =
	    (if not was_present
	     then
	       failwith
		 "This should never happen: Sundials requested PC without making Jacobian first!"
	     else ())
	  in
	    (* let () = Printf.printf "DDD [Node=%d] get_precond - duplicate jacobian\n%!" myrank in *)
	  let mx = Mpi_petsc.matrix_duplicate true jacobian in
	    (* let () = Printf.printf "DDD [Node=%d] get_precond - ksp_create\n%!" myrank in *)
	  let () =
	    begin
	      Mpi_petsc.matrix_copy true jacobian mx; (* true = "Same nonzero pattern" *)
	      Mpi_petsc.matrix_scale mx (-.gamma);
	      Mpi_petsc.matrix_add_identity mx 1.0;
	    end
	  in
	  let ksp = Mpi_petsc.ksp_create ~communicator:comm mx mx in
	  let () = Mpi_petsc.ksp_set_up ksp in
	  (* let () = Mpi_petsc.ksp_set_pc_bjacobi_sub_type ksp "ilu" in
	    (* I presume this was for "parallel KSP" only... *)
	  *)
	  let () =
	    Mpi_petsc.ksp_set_tolerances_opt
	      ksp
	      ksp_rtol
	      ksp_atol
	      ksp_dtol
	      ksp_maxiter
	  in
	    (* let () = Printf.printf "DDD [Node=%d] get_precond - have ksp and mx\n%!" myrank in *)
	  let ksp_and_mx = (ksp,mx) in
	  let () = ropt_precond := Some ksp_and_mx in
	    ksp_and_mx
  in
    (* === CVODE re-Jacobi === *)
  let cvode_fun_do_compute_jacobi time ba_y ba_ydot =
    let (jacobian,was_present) = get_jacobian() in
      (* We must at least make sure the Jacobian has a diagonal,
	 so that the 1-gamma*J matrix has the same nonzero structure
	 as this.
      *)
    let () =
      (
	if was_present
	then Mpi_petsc.matrix_zero_entries jacobian
	else
	  (if not(use_jacobian)
	   then debug 5 "XXX Re-Jacobi DISABLED!\n" (* XXX adjust: MUST NOT set a Jacobi function at the end then! *)
	   else
	     let () = fun_fill_jacobian ~jacobian ~y:ba_y ~ydot:ba_ydot ~time in
	     let () = Mpi_petsc.matrix_assemble jacobian true in
	       ()
	  ))
    in
      (* Ensure we do have the diagonal entries: *)
    let (start_mypart,end_mypart) =
      Mpi_petsc.matrix_get_ownership_range jacobian
    in
    let () =
      for i=start_mypart to end_mypart-1 do
	Mpi_petsc.matrix_inc jacobian i i 1e-300;
	Mpi_petsc.matrix_inc jacobian i i (-1e-300);
      done
    in
    let () = Mpi_petsc.matrix_assemble jacobian true in
    let () =
      (	(* If we built the Jacobian for the first time,
	   we copy and destroy the matrix, to get rid of
	   excessive pre-allocated data.
	   (A "trick" the PETSc team told us about...)
	*)
	if not was_present
	then
	  let () = Mpi_petsc.matrix_reincarnate jacobian in ()
	else ()
      )
    in
      ()
  in
    (* === CVODE Jacobi*Vector === *)
  let cvode_fun_jacobi_times_vector args () =
    let (time,ba_in,ba_out,ba_y,ba_ydot,v_tmp) = args in
    let (jacobian,was_present) = get_jacobian () in
    let () =
      (if not was_present
       then failwith "This should never happen: sundials called J*V without building a Jacobian first!"
       else ())
    in
    let () =
      Mpi_petsc.with_bigarray_as_petsc_vector ba_in
	(fun v_in ->
	   Mpi_petsc.with_bigarray_as_petsc_vector ba_out
	     (fun v_out ->
		Mpi_petsc.matrix_times_vector jacobian v_in v_out))
    in
      0	(* XXX Success code?! *)
  in
    (* === CVODE PC-Setup === *)
  let cvode_fun_preconditioner_setup args () =
    let (j_ok,time,gamma,ba_y,ba_ydot,ba_tmp1,ba_tmp2,ba_tmp3) = args in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #1\n%!" myrank in *)
    let () =
      (if j_ok (* If sundials tells us we may re-use the jacobi matrix,
		  we do re-use the jacobi matrix *)
       then ()
       else
	   cvode_fun_do_compute_jacobi time ba_y ba_ydot)
    in
    let (jacobian,was_present) = get_jacobian () in
    let () =
      (if not was_present
       then
	 let () = cvode_fun_do_compute_jacobi time ba_y ba_ydot in
	 let () = Printf.printf "NOTE: This should never happen: sundials called PC Setup without building a Jacobian first!\n%!" in
	   ()
       else ())
    in
    let (ksp,mx) = get_precond gamma in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #3\n%!" myrank in *)
    let () =
      begin
	(* Maybe we should matrix_zero mx at this point?! *)
	Mpi_petsc.matrix_copy true jacobian mx;
	Mpi_petsc.matrix_scale mx (-.gamma);
	Mpi_petsc.matrix_add_identity mx 1.0;
	Mpi_petsc.ksp_set_operators ksp mx mx; (* XXX Q: is this actually necessary? Suppose so... *)
	Mpi_petsc.ksp_set_up ksp;
      end
    in
      (true,0)
  in
    (* === CVODE PC-Solve === *)
  let cvode_fun_preconditioner_solve args () =
    let (left_right,time,gamma,delta,ba_y,ba_fy,ba_rhs,ba_result,ba_tmp) = args in
    let is_left = (left_right=1) in
    let () = (if not(is_left)
	      then failwith "CVODE PROBLEM: left_right is right - should never be!"
	      else ())
    in
    let (ksp,_) = get_precond gamma in
    let () = Mpi_petsc.with_bigarray_as_petsc_vector ba_rhs
      (fun v_in ->
	 Mpi_petsc.with_bigarray_as_petsc_vector ba_result
	   (fun v_out ->
	      let nr_iterations = Mpi_petsc.ksp_solve_raw ksp v_out v_in in
		()))
    in
      0	(* XXX TO-DO: check nr_iterations above - this will tell us if iteration succeeded! *)
  in
  let cvode =
    cvode_make_raw_ba
      (comm,
       false, (* true, ( * false, ( * parallel? *)
       cvode_fun_rhs,
       len_y,
       ba_y_initial,
       t_initial,
       rel_tol,
       abs_tol
      )
  in
  let () =
    cvode_setup_cvspgmr_raw
      cvode
      krylov_max
      PREC_LEFT
      cvode_fun_preconditioner_setup
      cvode_fun_preconditioner_solve
  in
  let () = cvode_set_max_order cvode cvode_max_order in
  let () = cvode_set_max_num_steps cvode 100000000 in
  let () =
    cvode_setup_jacobi_times_vector_raw
      cvode
      cvode_fun_jacobi_times_vector
  in
    cvode
;;

(* Ad "dealing with multiphysics and constraints":

   There are two separate conceptual issues here that should be
   treated separately:

   1. We may want to assemble multiple physical vectors into one
      single y-vector, e.g. thermal and magnetic degrees of freedom.

   2. We may want to impose additional constraints on degrees of
      freedom. This essentially amounts to a "reduction" of the
      "big vector Y of convenient physical degrees of freedom" to
      "a shortened vector y of actually independent degrees of freedom".

   These two issues are, essentially, unrelated and should be treated
   separately. Let us consider the second one for now.

   Some constraints imposed by CVODE:

   - We need a preconditioner matrix (1-gamma*J)^(-1).

   - The Jacobian J must operate on (short) y-vectors, not (long)
   Y-vectors.

   Explanation: This has to do with the way CVODE works. Essentially,
   the system "internally" tries to find a coordinate transformation
   from the given basis of degrees of freedom to another basis of
   "effective, collective, low-energy" mode degrees of freedom.  If any
   degree of freedom gets duplicated in the y-vector describing the
   system, then, CVODE "by design" would try to "get an idea of the
   stiffness of the system with respect to forcing the duplicated
   copies of the degrees of freedom apart" - which is a nonsensical
   question and so, trying to answer it will most likely "break the
   numerics". (I.e. calculation tops due to "desired accuracy
   inachievable" type failures.)

   Concerning DOF constraints,

   - There may be quite many of them (consider an anti-ferromagnet:
     Constraints halve the number of degrees of freedom!)

   - Here, we only consider algebraic constraints of the form
     c_n(y1,...,yk) = 0, where the gradient of the constraint
     equation gives a "correction vector":

     ydot[satisfying constraint n]_m =
        ydot_unconstrained - c_n,m *(c_n,k * ydot_k)/(c_n,p * c_n,p)

     i.e. we added a suitable multiple of the constraint's gradient to
     the velocity vector to ensure the new velocity's projection on
     the constraint gradient is zero.

   This modification of the velocities introduces a number of new
   terms into the Jacobian.

   So, dealing with the constraints requires solving two problems:

   - y <-> Y mapping

   - Setting up the Jy Jacobian when we only may have an idea about
     the JY jacobian which would be appropriate for the unconstrained
     system.

   Note that, in principle, constraints could be considered to reduce
   the number of micromagnetic degrees of freedom from 3N to 2N, N
   being the number of sites. The reason why this is not really a good
   idea there is that the 2-dimensional manifold (which is the 2-sphere) is
   not easily coordinate-parametrized in a nice way which furthermore
   would be compatible with simplex-interpolated integration.

   Essentially, what we want to achieve with the Jacobian is to build
   a function:

   make_fun_populate_Jy: empty_jacobian_y -> fun_popuate_JY -> fun_populate_Jy

   Note signature:

   fun_populate_JY:
   fun_inc_JY: (sparse_matrix -> row -> col -> contrib -> ()) -> ()

   So, our task is to

   (a) dress up fun_inc_JY in such a way that it can be called by
       fun_populate_JY, but will re-map entries and put them into Jy.

   (b) add an extra post-processing stage which adds the constraint-
       related entries.


   === Modification of the equations of motion due to the constraints ===

   Constraint equations c_n(y) =!= 0 (Note: [c] is to mean "with constraints".)

   Ydot_k = F(y)

   Ydot[c]_k = F(y) - sum_n Ydot_m * c_n,m c_n,k / (c_n,p c_n,p)

   Introduce C_n = 1/|(grad c_n)|^2:

   Ydot[c]_k = F(y) - sum_n Ydot_m * c_n,m c_n,k * C_n

   Hence, for the Jacobian, we get extra terms:

   JY_km = d Ydot_k / d Ym

   JY[c]_km =   JY_km
            - sum_n (  JY_lm c_n,l c_n,k C_n
                     + Ydot_l c_n,lm c_n,k C_n
                     + Ydot_l c_n,k c_n,km C_n
                     + Ydot_l c_n,l c_n,k C_n,m)

   (Note: Things considerably simplify if we can guarantee |grad c_n(Y)| = 1.
    But we may not be able to always enforce that, so we need the more
    generic formula above. If you, as a user, can ensure C_n =!= 1 ,
    you are well advised to do so, however!)

   So, we need to know (i.e. what should be provided externally to us
   - even though some of those parameters may be auto-generated):
   (Note that we do not really need the constraints themselves at that stage - maybe
    we would, to add them to the equations of motion in a
    Ydot_corrected = Ydot - s*constraint like fashion to ensure they do not get
    violated by numerical drift - but maybe the Y -> y equalizing is just enough
    to get that sorted out)

   c_n,k
   c_n,lm
   C_n
   C_n,k

   Furthermore note that we get considerable simplifications if the c_n,lm are =0,
   i.e. if there are linear constraints of the type m1_x + m2_x =!= 0.

   As we will usually have "sparse" constraints, these functions
   perhaps should be supposed to return a
   (array_of_indices,array_of_entries) tuple - and they may be
   permitted to re-use the arrays in there across subsequent
   evaluations for efficiency (i.e. we don't cons in there, and hence
   not trigger GC.)

   This essentially "almost" addresses all the Jacobi related issues.

   There is, of course, one annoying issue here when we use this for
   micromagnetism: the local orthonormal frames m /m x H / m x (m x H)
   are defined in terms of H_total. So, d [constraint] / d[Y_k] will
   contain d [H_total] / dm terms. We cannot include H_demag here, or
   we will risk ending up with a dense Jacobian. The best we can
   do is to just use the localizing contributions, dH_exchange / dM,
   and dH_anisotropy / dM.


   Now, about Y <-> y (which, of course, has implications on
   fun_inc_JYc <-> fun_inc_Jy, which is the missing piece...)

   The independent y's are supposed to be simple functions of the Y's
   (such as: a subset - as in Antiferromagnetism or Periodic Boundary
   Conditions). In principle, they could be more complicated.

   For our purpose, we need:

   y -[A]-> Y -> Ydot -[B]-> Ydot[c] -[C]-> ydot

   The [B] function can be auto-generated if information on the
   constraints is given, as explained above.

   So, we need the user to provide y->Y mapping and Ydot -> ydot
   mapping.

   It makes sense to parametrize these out as well, the expectation
   being that a micromagnetic simulator would somehow contain a
   function "create_y_to_Y_mapper_for_PBC_and_antiferromagnetic_constraints".
 *)


type ('configuration_vector,'fun_jacobi_filler) cvode_constraints =
    (* Note that we abstract out the configuration vector.
       Rationale: In a distributed system, it may in some
       systems be possible to evaluate constraints without
       the expensive MPI_allgather() synchronizing effort
       - and not in others. So, we have to allow for both
       situations.
     *)
    {
     cc_nr_constraints : int;
     cc_funs:
       (int (* nr_constraint *) ->
	 'configuration_vector (* vector_Y *) ->
	   float); (* violation *)
     cc_grads:
       (int (* nr_constraint *) ->
	 'configuration_vector (* vector_Y *) ->
	   (int array * float array));
     (* The result gives indices and coefficients of the
	constraint gradient. Note that the cc_grads fun provided
	here is permitted to re-use these arrays between calls.
      *)
     cc_grads2:
       (int (* nr_constraint *) ->
	 'configuration_vector (* vector_Y *) ->
	   (int array * int array * float array));
     (* These are 2nd derivatives, also needed to go into the
	modified Jacobian.
      *)
     cc_invnormsqs:
       (int (* nr_constraint *) ->
	 'configuration_vector (* vector_Y *) ->
	   float);
     (* These give 1/|grad constraint(Y)|^2
	Note that a make_cvode_constraints function
	could easily produce defaults here that
	call the constraint functions.
      *)
     cc_invnormsq_grads:
       (int (* nr_constraint *) ->
	 'configuration_vector (* vector_Y *) ->
	   (int array * float array));
     (* Again, make_cvode_constraints could easily produce these! *)
     cc_y_to_Y: (vec_y:'configuration_vector -> vec_Y:'configuration_vector -> unit);
     cc_Ydot_to_ydot: (vec_Ydot:'configuration_vector -> vec_ydot:'configuration_vector -> unit);
     cc_fun_build_Jy_from_fun_build_JY: 'fun_jacobi_filler -> 'fun_jacobi_filler;
   }
;;


(* === BEGIN SKETCHES === *)

(* ===
   Some thoughts/sketches/notes on a slightly more sophisticated version of
   cvode_setup_simple that can deal with parallel time integration:
   ===

   0. There are two different issues involved which, most likely,
      should be separated conceptually:

      (a) Assembly of a collection of vectors into a "long vector"
          (e.g. physical dof vectors -> "Y" vector, or assembly of
           auxiliary vectors)

      (b) Constraint-induced reduction of a "Y" vector to a shorter "y"
          vector:

          - long/short mapping of indices and dof distribution on a cluster
          - constraint implementation on the equations of motion
          - constraints and the Jacobian: Matrix size reduction and new entries.

   1. The beast is to be called on the master and supposed to set up the whole
      thing in parallel.

   2. It will need access to ccpla-like functionality, which, however,
      should be abstracted out (i.e. this module must not depend on
      nsim's ccpla, as it is useful on its own!)

      More specifically, it must be able to:

       - execute a given function across the cluster that registers
         a parallel resource (and returns a resource handle).

       Is this the only situation we are facing?
       Functions on the master being able to induce evaluation
       across the cluster that registers resources? Clearly, that
       technology would already take us quite far.

       Other issues:

       Signatures of important functions:

        fun_rhs:(time:float -> y:simple_c_array -> ydot:simple_c_array -> int)

       In parallel execution, y/ydot will just be partial vectors.
       Depending on the MPI node, we will have to know the offset
       (we do of course know the length of the vector). We also have to
       have a way to get at the full y-vector that contains all entries.

       Suggestion: We only need ydot as an output, so we are all fine there.

       Relevant tricky issues:

       - We would like to extend the interface in such a way that we
       have an extra function get_y which allows us to "peek" at the
       entries of the full y-vector *if* these are needed. The idea
       being that, if this function never gets called, we save on the
       synchronizing communication. We can safely assume that the max
       index to get_y is available through some closure parameter.

       Problem: We cannot really ensure get_y gets called for the
       first time in the same place in the execution sequence across
       all nodes. Enforcing this by conventions would be somewhat
       counter-intuitive.

       Problem: We may still need the full y-vector for some purpose,
       such as a Petsc Matrix/vector operation that involves it.

       - Just having a simple_c_array certainly is not enough: In a
       simple case, ydot = A*y with some fixed matrix A. Evidently,
       this essentially is just a petsc operation on the distributed
       vectors.

       - We should directly support multiphysics in the sense that
       there is some automatic mapping going on between the degrees
       of freedom of the y-vector and a set of different physical
       fields that provide these (i.e. fun_rhs can directly access
       vec_magnetization and vec_current and vec_temperature in a
       coupled M/j/T simulation).

       Basically, there are a number of nasty issues involved, so
       perhaps the most reasonable approach for now to proceed about
       this is to define the signature to be

        fun_rhs:(time:float -> wizard:par_rhs_wizard -> int) ->

       where par_rhs_wizard is a structure that offers a number of
       "methods" (functions, really) which can be used to do all
       sorts of things:

       wiz.ensure_y_fully_known() ->
         will initiate an MPI allgather() to ensure each node has
         a full copy of the y-vector.

       [NOTE: "pv" = "parallelized vector",
              "msv" = "multisequential vector", i.e. everyone has a full copy]

       wiz.y_pv_petsc ~dot:false () -> return the petsc-vector of the local part of the y-vector
       wiz.y_pv_petsc ~dot:true ()  -> ditto, but ydot

       wiz.y_pv_ba ~dot:false ()    -> return a bigarray of the local part of the y-vector

       The same for "multi-sequential" vectors:

       (Note that this raises an exception on the master (and just
       kills the slaves) unless wiz.ensure_y_fully_known() has been
       called before!)

       wiz.y_msv_petsc ~dot:false () -> return the petsc-vector of the local part of the y-vector
       wiz.y_msv_ba ~dot:false ()    -> return a bigarray of the local part of the y-vector

       Then, these "multi-sequential" vectors actually come from physical vectors
       that are being mapped to them in a tricky way. Two cases must be discerned here:

       - Without DOF identification (e.g. periodic BC):
         physical/y-mapping does not involve parallel communication, but pieces can
         be read off directly.

       - With DOF identification: We need to have the y-vector synchronized first.

       How is the question decided which strategy to use? Must be decided at setup
       of the timestepper - but then, we can do without the wiz.ensure_y_fully_known()!

       Simplifying this interface:

       wiz.vec_pv_petsc ?(dot=false) name -> petsc vector part
       wiz.vec_pv_ba    ?(dot=false) name -> bigarray vector part

       wiz.vec_msv_petsc ?(dot=false) name -> petsc fully synchronized vector
       wiz.vec_msv_ba    ?(dot=false) name -> bigarray fully synchronized vector

       wiz.indices name -> indices of the local vector part.


       Names that start with "$" are "system names". At present, there are
       just two: "$y"/"$ydot".

       All alphanumeric names are "user names" and can be used e.g. for a temperature
       field "T", magnetisation "m", etc.

       ---------
       What parallel resources are we going to deal with?

       - Vectors: y/ydot (pv/msv), physical primary fields m/mdot, T/Tdot, j/jdot, etc.

       - Implicitly:
            - The CVODE,
            - The Jacobi Preconditioner and its KSP
       - Matrices: The Jacobian


    Strategy: Let us first build this time integrator and see what we
    have to abstract out. Then, let us see how to make it replace the
    present time integration mess in nsim.ml...


    When setting up a timestepper, this has to link to
    (a) physical field arrays to go into the y-vector,
    (b) physical field arrays to be read off from the ydot-vector,
    (c) possibly additional auxiliary read-only arrays (to set parameters)
    (d) other exterior parameters that just depend on t (think of an applied
	oscillatory external field).

    Evidently, we have to know about both (a) the distribution of
    physical degrees of freedom across MPI nodes, and (b) duplications.
    From this information, we can derive index mapping functions.

    Note: Problem - to populate the Jacobian, we have to have access
    to the corresponding nonlocal operators. Quite generally, this
    timestepper will depend on a number of other parallel resources.
    (Parallel resources actually always will do.)

    Essentially, we will also have to re-structure the ccpla module
    later on, in order to take this into account. I think the
    abstraction changes are:

    -> there are two sorts of dependencies of parallel resources:

    "depends" = "needs to be defined" - e.g.
      - a KSP depends on its matrix,
      - a timestepper depends on the operator
        matrices for the Jacobian.

    "monitors" = "has to be updated when the other thing is updated"

     - H_total monitors H_exch, H_demag, H_ext, H_anis

    "monitoring" will be important in the future. How to deal with
    the "dependency" issue? Right now, we essentially do this by
    requiring the master process to somehow tie the parallel matrix
    handles to the parallel KSP handle. Perhaps this should be done
    in a more systematic way. (Maybe even at the petsc interface
    level, but presumably not.)

*)


(* This is supposed to provide a set of functions  *)

type 'parallel_handle cvode_par_functions =
    {
     cpf_exec_in_parallel_return_handles_on_master :
       (unit -> 'parallel_handle array) -> 'parallel_handle array;
   }
;;


(* We occasionally have to associate a "set of physical parallel-vectors"
   with a "single multisequential vector with identified duplicates removed".

   So far, this only is used in conjunction with assembling multi-physics
   degrees of freedom (which may feature periodic boundary conditions)
   into a single y-vector, and associated tech (ydot/setting Jacobian entries).

   Presumably, we will not have much use beyond that, for other
   auxiliary/supplemental fields most likely will always be treated
   (and needed) as multisequential vectors, e.g. for site-wise
   execution, but still, this seems to be a useful abstract concept in
   itself.

   (Yes, the name of the function is a particle physics pun - T.F....)

   Why do we do this at all - what fundamental conflicts have to be
   resolved here?

   - Meshes carry sites, to which we attach degrees of freedom.

   - We use ParMETIS to automatically work out a suitable distribution
     of our mesh to multiple MPI nodes.

   - When there are identifications between surface nodes,
     e.g. periodic boundary conditions, these may not respect the
     distribution suggested by ParMETIS.

   - In the time integrator's y/ydot vectors, we MUST NOT have any
     duplication! If we had, the stiff ODE solver would try to determine
     the system's stiffness with respect to forcing these duplicates apart,
     which would totally mess up the calculation.

 *)

(* Note that this MUST NOT depend on fem.ml - so we abstracted out all the
   field-structure related issues. The mapping that will link this to fem.ml
   is:

   ~fun_fstruct_getdistrib:(fun mwe -> mwe.distribution)
   ~fun_fstruct_

*)

(* ===
let grand_field_unification_layout
    ~fun_fstruct_getdistrib (* e.g. fun mwe -> mwe.distribution *)
    ~fun_fstruct_
    v_fieldstructures (* e.g. v_mwes *)
    =
  let vv_distrib = Array.map fun_fstruct_getdistrib v_fieldstructures in
    (* Evidently, we want to arrange y such that parallel distribution of the y vector
       matches parallel distribution of the mesh. This means that we have to describe
       y-components by site. As we generally take sites to be in lexicographical
       ordering, this will then automatically induce the distribution of the y-vector.
    *)
  let v_sites =
    let ht = Hashtbl.create 1987 in
    let () = array_foreach_do v_fieldstructures
      (fun fstruct ->
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
=== *)

(* === TO-DO ===
let cvode_setup_simple_par
    ~cvode_par_functions
    ?(name="cvode") (* For PETSc objects to be created *)
    ?(use_jacobian=true)
    ?(debug=fun level message -> Printf.fprintf stderr "[CVODE %02d]: %s%!" level message)
    ?(fun_fill_jacobian=fun ~jacobian ~time ~cp_wizard -> ())
    ?(jacobi_prealloc_diagonal=25)
    ?(jacobi_prealloc_off_diagonal=50)
    ?(t_initial=0.0)
    ?(rel_tol=1e-8)
    ?(abs_tol=1e-5)
    ?ksp_rtol
    ?ksp_atol
    ?ksp_dtol
    ?ksp_maxiter
    ?(krylov_max=300)
    ?(cvode_max_order=2)
    ~y_initial
    ~fun_rhs
    (* Return value of this function:
       =0 -> Success
       >0 -> recoverable error
       <0 -> nonrecoverable error
    *)
    () =
  let () = Printf.fprintf stderr "DDD cvode_setup_simple is EXPERIMENTAL/UNTESTED code!\n%!" in
  let () =
    (if Mpi_petsc.petsc_check_initialized ()
     then ()
     else
       let _ =
	 Mpi_petsc.petsc_init
           [|"foo"; (*"-info"*)|]
           (Printf.sprintf "%s/.petscrc" (Unix.getenv "HOME"))
	   "[help message]"
           true (* Enable petsc signal handler *)
       in ()
    )
  in
  let comm = Mpi_petsc.petsc_get_comm_self () in
  let () =
    (* If the user already initialized sundials beforehand, fine.
       Otherwise, we do it here, with default paths. Note that this
       might give rise to quite strange problems if the library paths
       are not correct...

       XXX this evidently has to be changed if sundials uses a
       different path than /usr/local/lib. We use that here as the
       debian package, which has sundials in /usr/lib, does not
       provide nvecparallel.so.
    *)
    sundials_init
      ~path_cvode:"/usr/local/lib/libsundials_cvode.so"
      ~path_nvec_serial:"/usr/local/lib/libsundials_nvecserial.so"
      ~path_nvec_parallel:"/usr/local/lib/libsundials_nvecparallel.so"
      ()
  in
  let len_y = Array.length y_initial in
  let ba_y_initial =
    let ba =
      Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout len_y
    in
    let () = Array.iteri (fun ix v -> ba.{ix} <- v) y_initial in
      ba
  in
  let cvode_fun_rhs (time,ba_y,ba_ydot) () =
    let success = fun_rhs ~time:time ~y:ba_y ~ydot:ba_ydot in
      success
  in
  let ropt_jacobian = ref None in
  let ropt_precond = ref None in	(* (KSP, 1-gamma*J matrix) *)
  let get_jacobian () =
    match !ropt_jacobian with
      | Some mx -> (mx,true)
      | None ->
          let () = debug 10 (Printf.sprintf "Making Jacobi MX: %d x %d\n" len_y len_y)
	  in
	  let mx_jacobi =
	    Mpi_petsc.matrix_create
	      ~communicator:comm
	      ~local_rows:len_y
	      ~local_cols:len_y
	      ~matrix_type:"seqaij"
	      ~prealloc_diagonal:jacobi_prealloc_diagonal
	      ~prealloc_off_diagonal:jacobi_prealloc_off_diagonal
	      ~auto_assembly:false
	      len_y len_y (Printf.sprintf "%s_jacobian" name)
	  in
	  let () = ropt_jacobian:=Some mx_jacobi in
	    (mx_jacobi,false)
  in
  let get_precond gamma =
    match !ropt_precond with
      | Some ksp_and_mx -> ksp_and_mx
      | None ->
	  let (jacobian,was_present) = get_jacobian() in
	  let () =
	    (if not was_present
	     then
	       failwith
		 "This should never happen: Sundials requested PC without making Jacobian first!"
	     else ())
	  in
	    (* let () = Printf.printf "DDD [Node=%d] get_precond - duplicate jacobian\n%!" myrank in *)
	  let mx = Mpi_petsc.matrix_duplicate true jacobian in
	    (* let () = Printf.printf "DDD [Node=%d] get_precond - ksp_create\n%!" myrank in *)
	  let () =
	    begin
	      Mpi_petsc.matrix_copy true jacobian mx; (* true = "Same nonzero pattern" *)
	      Mpi_petsc.matrix_scale mx (-.gamma);
	      Mpi_petsc.matrix_add_identity mx 1.0;
	    end
	  in
	  let ksp = Mpi_petsc.ksp_create ~communicator:comm mx mx in
	  let () = Mpi_petsc.ksp_set_up ksp in
	  (* let () = Mpi_petsc.ksp_set_pc_bjacobi_sub_type ksp "ilu" in
	    (* I presume this was for "parallel KSP" only... *)
	  *)
	  let () =
	    Mpi_petsc.ksp_set_tolerances_opt
	      ksp
	      ksp_rtol
	      ksp_atol
	      ksp_dtol
	      ksp_maxiter
	  in
	    (* let () = Printf.printf "DDD [Node=%d] get_precond - have ksp and mx\n%!" myrank in *)
	  let ksp_and_mx = (ksp,mx) in
	  let () = ropt_precond := Some ksp_and_mx in
	    ksp_and_mx
  in
    (* === CVODE re-Jacobi === *)
  let cvode_fun_do_compute_jacobi time ba_y ba_ydot =
    let (jacobian,was_present) = get_jacobian() in
      (* We must at least make sure the Jacobian has a diagonal,
	 so that the 1-gamma*J matrix has the same nonzero structure
	 as this.
      *)
    let () =
      (
	if was_present
	then Mpi_petsc.matrix_zero_entries jacobian
	else
	  (if not(use_jacobian)
	   then debug 5 "XXX Re-Jacobi DISABLED!\n" (* XXX adjust: MUST NOT set a Jacobi function at the end then! *)
	   else
	     let () = fun_fill_jacobian ~jacobian ~y:ba_y ~ydot:ba_ydot ~time in
	     let () = Mpi_petsc.matrix_assemble jacobian true in
	       ()
	  ))
    in
      (* Ensure we do have the diagonal entries: *)
    let (start_mypart,end_mypart) =
      Mpi_petsc.matrix_get_ownership_range jacobian
    in
    let () =
      for i=start_mypart to end_mypart-1 do
	Mpi_petsc.matrix_inc jacobian i i 1e-300;
	Mpi_petsc.matrix_inc jacobian i i (-1e-300);
      done
    in
    let () = Mpi_petsc.matrix_assemble jacobian true in
    let () =
      (	(* If we built the Jacobian for the first time,
	   we copy and destroy the matrix, to get rid of
	   excessive pre-allocated data.
	   (A "trick" the PETSc team told us about...)
	*)
	if not was_present
	then
	  let () = Mpi_petsc.matrix_reincarnate jacobian in ()
	else ()
      )
    in
      ()
  in
    (* === CVODE Jacobi*Vector === *)
  let cvode_fun_jacobi_times_vector args () =
    let (time,ba_in,ba_out,ba_y,ba_ydot,v_tmp) = args in
    let (jacobian,was_present) = get_jacobian () in
    let () =
      (if not was_present
       then failwith "This should never happen: sundials called J*V without building a Jacobian first!"
       else ())
    in
    let () =
      Mpi_petsc.with_bigarray_as_petsc_vector ba_in
	(fun v_in ->
	   Mpi_petsc.with_bigarray_as_petsc_vector ba_out
	     (fun v_out ->
		Mpi_petsc.matrix_times_vector jacobian v_in v_out))
    in
      0	(* XXX Success code?! *)
  in
    (* === CVODE PC-Setup === *)
  let cvode_fun_preconditioner_setup args () =
    let (j_ok,time,gamma,ba_y,ba_ydot,ba_tmp1,ba_tmp2,ba_tmp3) = args in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #1\n%!" myrank in *)
    let () =
      (if j_ok (* If sundials tells us we may re-use the jacobi matrix,
		  we do re-use the jacobi matrix *)
       then ()
       else
	   cvode_fun_do_compute_jacobi time ba_y ba_ydot)
    in
    let (jacobian,was_present) = get_jacobian () in
    let () =
      (if not was_present
       then
	 let () = cvode_fun_do_compute_jacobi time ba_y ba_ydot in
	 let () = Printf.printf "NOTE: This should never happen: sundials called PC Setup without building a Jacobian first!\n%!" in
	   ()
       else ())
    in
    let (ksp,mx) = get_precond gamma in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #3\n%!" myrank in *)
    let () =
      begin
	(* Maybe we should matrix_zero mx at this point?! *)
	Mpi_petsc.matrix_copy true jacobian mx;
	Mpi_petsc.matrix_scale mx (-.gamma);
	Mpi_petsc.matrix_add_identity mx 1.0;
	Mpi_petsc.ksp_set_operators ksp mx mx; (* XXX Q: is this actually necessary? Suppose so... *)
	Mpi_petsc.ksp_set_up ksp;
      end
    in
      (true,0)
  in
    (* === CVODE PC-Solve === *)
  let cvode_fun_preconditioner_solve args () =
    let (left_right,time,gamma,delta,ba_y,ba_fy,ba_rhs,ba_result,ba_tmp) = args in
    let is_left = (left_right=1) in
    let () = (if not(is_left)
	      then failwith "CVODE PROBLEM: left_right is right - should never be!"
	      else ())
    in
    let (ksp,_) = get_precond gamma in
    let () = Mpi_petsc.with_bigarray_as_petsc_vector ba_rhs
      (fun v_in ->
	 Mpi_petsc.with_bigarray_as_petsc_vector ba_result
	   (fun v_out ->
	      let nr_iterations = Mpi_petsc.ksp_solve_raw ksp v_out v_in in
		()))
    in
      0	(* XXX TO-DO: check nr_iterations above - this will tell us if iteration succeeded! *)
  in
  let cvode =
    cvode_make_raw_ba
      (comm,
       false, (* true, ( * false, ( * parallel? *)
       cvode_fun_rhs,
       len_y,
       ba_y_initial,
       t_initial,
       rel_tol,
       abs_tol
      )
  in
  let () =
    cvode_setup_cvspgmr_raw
      cvode
      krylov_max
      PREC_LEFT
      cvode_fun_preconditioner_setup
      cvode_fun_preconditioner_solve
  in
  let () = cvode_set_max_order cvode cvode_max_order in
  let () = cvode_set_max_num_steps cvode 100000000 in
  let () =
    cvode_setup_jacobi_times_vector_raw
      cvode
      cvode_fun_jacobi_times_vector
  in
    cvode
;;
========= *)

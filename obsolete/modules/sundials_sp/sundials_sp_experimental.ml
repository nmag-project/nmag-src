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

type cvode;;

type simple_c_array = 
    (float, Bigarray.float64_elt, Bigarray.c_layout)
      Bigarray.Array1.t;;

let make_simple_c_array n =
  Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout n
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
  (Mpi_petsc.communicator * bool * cvode * float * simple_c_array * int) -> float =
  "caml_sundials_sp_cvode_advance_raw_ba";;

external cvode_get_integrator_stats_raw:
  cvode -> (float*float*float*float*float*float*float*float*float*float)
  = "caml_sundials_sp_cvode_get_integrator_stats";;

external cvode_get_precond_stats_raw:
  cvode -> (float*float*float*float*float*float)
  = "caml_sundials_sp_cvode_get_precond_stats";;

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


let cvode_advance ?comm cvode time ba_result max_its =
  match comm with
    | None ->
	let c = Mpi_petsc.petsc_get_comm_self() in
	  cvode_advance_raw_ba (c,false,cvode,time,ba_result,max_its)
    | Some c ->
	cvode_advance_raw_ba (c,true,cvode,time,ba_result,max_its)		 
;;

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


(* XXX should use bigarray throughout...
   XXX really have to do something about the callsignature here!
*)
let cvode_setup_vanilla
    ?(name="cvode") (* For PETSc objects to be created *)
    ?(use_jacobian=true)
    ?(debug=fun level message -> Printf.fprintf stderr "[CVODE %02d]: %s%!" level message)
    ?(fun_fill_jacobian=fun ~jacobian ~y ~ydot ~time -> ())
    ~fun_jacobi_times_vector
    ?(jacobi_prealloc_diagonal=25)
    ?(jacobi_prealloc_off_diagonal=50)
    ?(fun_recompute_jacobi = fun time ba_y ba_ydot -> ())
    (* ^ This will be called both:

       - when we compute the Jacobian for the first time,
       
       - when the need arises to recompute state related to the Jacobian. 

       It is implicitly understood that, if needed, this will change some
       of the state that is shared with the fun_jacobi_times_vector closure,
       i.e. the jacobian.

       This may be a matrix option ref, whose content, upon first invocation,
       is changed from None to Some matrix.
    *)
    ?(t_initial=0.0)
    ?(rel_tol=1e-8)
    ?(abs_tol=1e-5)
    ?(ksp_rtol=1e-8)
    ?(ksp_atol=1e-5)
    ?(ksp_dtol=1.0)
    ?(ksp_maxiter=1000000)
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
  let () =
    (if Mpi_petsc.petsc_check_initialized ()
     then ()
     else
       Mpi_petsc.petsc_init 
         [|"foo"; (*"-info"*)|]
         [|Printf.sprintf "%s/.petscrc" (Unix.getenv "HOME")|]
         true (* Enable petsc signal handler *)
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
  let ba_initial =
    let ba = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout len_y  in
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
          let () = debug 10 (Printf.sprintf "Making Jacobi MX: %d x %d" len_y len_y)
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
	   then debug 5 "Re-Jacobi DISABLED!\n"
	   else
	     let () = fun_fill_jacobian ~jacobian ~y:ba_y ~ydot:ba_ydot ~time in
	     let () = Mpi_petsc.matrix_assemble jacobian true in
	       ()
	  ))
    in
      (* Ensure we do have the diagonal entries: *)
    let (start_mypart,end_mypart) = Mpi_petsc.matrix_get_ownership_range jacobian in
    let () =
      for i=start_mypart to end_mypart-1 do
	Mpi_petsc.matrix_inc jacobian i i 1e-300;
	Mpi_petsc.matrix_inc jacobian i i (-1e-300);
      done;
    in
    let () = Mpi_petsc.matrix_assemble jacobian true
    in
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
    let () = Mpi_petsc.with_bigarray_as_vector ba_in
      (fun v_in ->
	 let () = Mpi_petsc.with_bigarray_as_vector ba_out
	   (fun v_out ->
	      let () = Mpi_petsc.matrix_times_vector jacobian v_in v_out in
		0))
    in
      ()
  in
    (* === CVODE PC-Setup === *)
  let cvode_fun_preconditioner_setup args () =
    let (j_ok,time,gamma,ba_y,ba_ydot,ba_tmp1,ba_tmp2,ba_tmp3) = args in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #1\n%!" myrank in *)
    let () =
      (if j_ok (* If sundials tells us we may re-use the jacobi matrix,
		  we do re-use the jacobi matrix *)
       then ()
       else cvode_fun_do_compute_jacobi time ba_y ba_ydot)
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
    let () = Mpi_petsc.with_bigarray_as_vector ba_rhs
      (fun v_in ->
	 let () = Mpi_petsc.with_bigarray_as_vector ba_result
	   (fun v_out ->
	      let nr_iterations = Mpi_petsc.ksp_solve_raw ksp v_out v_in in
		0))
    in
      ()
  in
  let cvode =
    Sundials_sp.cvode_make_raw_ba
      (comm,
       false, (* parallel? *)
       cvode_fun_rhs,
       len_y,
       ba_y_initial,
       initial_time,
       rel_tol,
       abs_tol
      )
  in
  let () =
    Sundials_sp.cvode_setup_cvspgmr_raw
      cvode
      krylov_max
      Sundials_sp.PREC_LEFT
      cvode_fun_preconditioner_setup
      cvode_fun_preconditioner_solve
  in
  let () = Sundials_sp.cvode_set_max_order cvode cvode_max_order in
  let () = Sundials_sp.cvode_set_max_num_steps cvode 100000000 in
  let () =
    Sundials_sp.cvode_setup_jacobi_times_vector_raw 
      cvode
      cvode_fun_jacobi_times_vector
  in
    cvode
;;


(* (C) 2005 Dr. Thomas Fischbacher

   This is the outside interface of high-level functions only which we
   make visible towards Python.

   Note: for reasons that should be evident, we do not export anything.
   We just modify/extend python.

   TODO: use py_number_*, guarded_py_number_as_float, and cousins!

*)

open Snippets;;
open Pycaml;;
open Mesh;;
open Fem;;
open Nsim;;
open Ccpla;;

exception Py_Exn of string;;

let do_not_use_logger=false;;

(* The next line can activate debugging of the mpi-logging mechanism *)
let debug_mpi_loggers=false;;


let logmsg = Nlog.getLogger("nfem.ocaml");;
let logwarn = logmsg Nlog.Warn;;
let loginfo = logmsg Nlog.Info;;
let loginfo2 = logmsg Nlog.Info2;;
let logdebug = logmsg Nlog.Debug;;
let logerror = logmsg Nlog.Error;;


(* Nsimconf does not need to be in the following list:
   it is automatically generated!
 *)
let version_string = (String.concat ":" ["$Id$";
		      (Nlog.version());
		      (Snippets.version());
		      (Qhull.version());
		      (Mt19937.version());
		      (Fastfields.version());
		      (Pycaml.version());
		      (Mesh.version());
		      (Fem.version());
		      (Mpi_petsc.version());
		      (Sundials_sp.version());
		     ])
;;

let version () = Snippets.md5 version_string;;

(* NOTE: we have to make sure PETSC is initialized.
   (Actually, pyfem somewhere has to do this.)
   XXX the initialization stage can and should
   be improved and streamlined.

   Note: the Mpi_petsc module was written in such a way that we can
   disable installing the petsc signal handler. Unfortunately, the
   most important signal for us - SIGSEGV - always is being
   intercepted by petsc, so there is little point in trying to export
   the means to play around with this to the end user level...
*)

(* ad-hoc MPI related startup code
   Note: our arg parsing sucks. It may work in practice,
   but there is little excuse for not using some
   far more reasonable strategy!
*)

let effective_argv = ref [||];;
let effective_petsc_argv = ref [||]

let pyfem_ccpla = ref None;;

let initial_working_directory_ref = ref (Unix.getcwd());;

let mpi_prefix = "-p4" in
let (args, petsc_args) =
  let initial_group = "--nsim-opts" in
  let petsc_group = "--petsc-opts" in
  let groups = [|petsc_group; initial_group|] in
  let args_by_group = Hashtbl.create 10 in
  let () = Hashtbl.add args_by_group petsc_group [|Sys.argv.(0)|] in
  (* ^^^ just to make sure that petsc receives at least one argument,
   * otherwise it gets angry. It segfaults :( *)
  let _ = preparse_cmdline groups initial_group args_by_group Sys.argv in
  let args = Hashtbl.find args_by_group initial_group in
  let petsc_args = try Hashtbl.find args_by_group petsc_group with _ -> [||]
  in
    (args, petsc_args)
in
let (mpi_args, non_mpi_args) =
  (* this ugly code snippet is used only to support mpich 1.0. We may get rid of
   * it soon, when we decide not to support this implementation of the MPI
   * standard.
   *)
  let rec scan pos mpi non_mpi =
    if pos=Array.length args
    then (List.rev mpi,List.rev non_mpi)
    else
      let arg = args.(pos) in
	if (string_compare_n (String.length mpi_prefix) mpi_prefix arg) && pos<Array.length args-1
	then scan (2+pos) ((args.(pos),args.(pos+1))::mpi) non_mpi
	else scan (1+pos) mpi (args.(pos)::non_mpi)
  in
    scan 1 [] []
in
  begin
    effective_argv := Array.of_list (Sys.argv.(0)::non_mpi_args);
    effective_petsc_argv := petsc_args
  end
;;

let petscrc_file =
  let home = Unix.getenv "HOME" in
  let file_name = Printf.sprintf "%s/.petscrc" home in
  let () = (* If the petscrc file did not already exist, create it.
              Users who do know about this will generally want
              it this way, to avoid cryptic messages.

              XXX Note: Eventually, we will want to provide per-simulation
              petsc fine-tuning!
            *)
    try let _ = Unix.stat file_name in () with
      | Unix.Unix_error (Unix.ENOENT,"stat",_) ->
          let h = open_out file_name in
          let () = Printf.fprintf h "\n" in
          let () = close_out h in
            ()
      | _ -> ()
  in file_name
;;

(* The Ccpla.setup function does also call Mpi_petsc.petsc_init, so we do not
   need to do this later. Actually only the master node will return from this
   function call. The slave nodes will enter the wait-for-command loop
   (Ccpla.slave_mode) and will never return! Therefore WHATEVER YOU NEED TO
   INITIALISE ON THE SLAVE NODES, THIS HAS TO COME BEFORE THIS POINT!
   mf, 20 Aug 2009
 *)
let cd = Sys.getcwd () in
let () = pyfem_ccpla :=
  Some (Ccpla.setup ~petsc_argv:!effective_petsc_argv Sys.argv
                    nsim_opcode_interpreter
                    (fun () -> exit 0))
in
let () = Sys.chdir cd in (* Can anyone explain why the command above ^^^
                            changes the current working directory???
                            Anyway, here is a temporary fix! *)

  ()
;;

(* FIXME: we should put this function somewhere else (this is a quick bad
          hack): Indeed, there is a similar function in nsim.ml...
 *)
let reportmem tag =
  let (time, vmem, rss) = time_vmem_rss () in
    loginfo2 (* logdebug *)
      (Printf.sprintf "Memory report: T=%7.3f VMEM=%8.0f KB RSS=%8.0f KB %s"
                      time vmem rss tag)
;;

let _py_get_initial_working_directory =
  (* Is this function still used? No -- schedule for removal? HF 21/12/2009 *)
  python_pre_interfaced_function
    ~doc:"Return working directory (from OCaml's perspective) in which pyfem3 was started"
    [||]
    ( fun args ->
	pystring_fromstring !initial_working_directory_ref
);;

let _py_getcwd =
  python_pre_interfaced_function
  (* Is this function still used? No -- schedule for removal? HF 21/12/2009 *)
    ~doc:"Return current working directory as string"
    [||]
    ( fun args ->
      let cwd = (Unix.getcwd()) in
    pystring_fromstring cwd
);;

let _py_mpi_status =
  python_pre_interfaced_function
    ~doc:"Print number of nodes in MPI ring (executes on master only)."
    [||]
    ( fun args ->
      let () = (Mpi_petsc.mpi_status ()) in
    pynone()
);;


let _py_ccpla_hello =
  python_pre_interfaced_function
    ~doc:"Ping hello messages from all nodes (in the Centrally Coordinated Parallel Linear Algebra (CCPLA) Machine). Useful for mpi-testing and debugging via stdout. Report hostname and rank for each node."
    [||]
    ( fun args ->
      let () = match !pyfem_ccpla with
	| None -> failwith "No ccpla found"
	| Some x -> Ccpla.ccpla_hello x
      in
	pynone()
);;

let _py_execute_on_all_nodes_hello =
  python_pre_interfaced_function
    ~doc:"Ping hello messages from all nodes Centrally Coordinated Parallel Linear Algebra (CCPLA) Machine, triggered via the nsim_execute_all_CPUs function (useful for mpi-testing)"
    [||]
    ( fun args ->
      let () = match !pyfem_ccpla with
	| None -> failwith "No ccpla found"
	| Some ccpla -> Nsim.nsim_execute_all_CPUs ccpla (
	    fun ()->
	      let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
	      let size = Mpi_petsc.comm_size Mpi_petsc.comm_world in
		Printf.printf "[Node %d/%d] Hello (execute_all)\n%!" rank size
	  )
      in
	pynone()
    )
;;

(* While we are at it, we also initialize caching.
   XXX NOTE: needs to be updated/user-controllable.
 *)

let () =
  begin
    register_version [version()];
    register_feature ~domain:"file-cache" "directory"
      (Printf.sprintf "/tmp/mumag-cache.%s" (Unix.getenv "USER"));
    register_feature ~domain:"file-cache" "max-age" "7.0";
  end
;;

let pysym_mesh = "Mesh"
and pysym_mesher_defaults_int = "Mesher Defaults (with state of type int)"
and pysym_mg_gendriver = "Mesh Generator Driver Generator"
and pysym_mgo_int = "Mesh Generator Engine Output (with state of type int)" (* XXX TO GO AWAY! *)
and pysym_meshed_physics = "Meshed Differential Operator" (* XXX OBSOLETE! *)
and pysym_fem_body = "FEM Body"
and pysym_element = "FEM Element"
and pysym_field = "FEM Field"
and pysym_cofield = "FEM Co-Field"
and pysym_operator = "FEM Operator"
and pysym_solver = "FEM Linear Solver"
and pysym_ksp = "PETSC KSP"
and pysym_mwe = "FEM Mesh with elements"
and pysym_mesh_fca = "FEM Mesh fine/coarse association data"
and pysym_cvode = "CVODE time integrator context"
and pysym_lindholm = "An opaque implementation of Lindholm's formula"
and pysym_linalg_machine = "A 'FEM linear algebra' machine"
and pysym_jacobian = "An (opaque) Jacobi matrix"
;;

let () = register_ocamlpill_types
    [|pysym_mesh;
      pysym_mesher_defaults_int;
      pysym_mg_gendriver;
      pysym_mgo_int;
      pysym_meshed_physics;
      pysym_fem_body;
      pysym_element;
      pysym_field;
      pysym_cofield;
      pysym_operator;
      pysym_solver;
      pysym_ksp;
      pysym_mwe;
      pysym_mesh_fca;
      pysym_cvode;
      pysym_lindholm;
      pysym_linalg_machine;
      pysym_jacobian;
    |]
;;

let rec
(*    _sample_mesh =
  let m =
    mesh_from_known_delaunay
      [| [|1.0|];[|2.0|];[|3.0|] |]
      [| (Body_Nr 0,[|0;1|]); (Body_Nr 0,[|1;2|]); |]
  in
  let () = mesh_grow_bookkeeping_data ~do_connectivity:true m
  in m
and
*)
    _sample_mesher_defaults_int= !opt_mesher_defaults
and
    _sample_mg_gendriver = default_gendriver
and
    _sample_mgo_int =
  Mesh_Engine_Finished_Step_Limit_Reached (dummy_mesh,0)
and _sample_fem_body = (Body (body_trafo_id 2,(fun (pos:(float array)) -> 1.0)))
and _sample_element = make_element 1 ("T",[||]) 1
;;


(* let _sample_mwe =
  make_mwe "sample mwe" (fun sx -> _sample_element) _sample_mesh;; *)
let _sample_mwe =
  make_mwe "sample mwe" (fun sx -> _sample_element) dummy_mesh;;


let _sample_mesh_fca = (([||],[||]) : (((Mesh.point_id array * int array) * int) array * (int array)));;

let _sample_field =
  FEM_field (_sample_mwe, None, Mpi_petsc.vector_dummy());;

let _sample_cofield =
  FEM_cofield (_sample_mwe,None, Mpi_petsc.vector_dummy());;

let _sample_jacobian = Mpi_petsc.matrix_dummy();;

let _sample_ksp = Mpi_petsc.ksp_dummy();;

let (ocamlpill_from_mesh, mesh_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_mesh dummy_mesh;;

let (ocamlpill_from_mesher_defaults_int, mesher_defaults_int_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_mesher_defaults_int _sample_mesher_defaults_int;;

let (ocamlpill_from_mg_gendriver, mg_gendriver_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_mg_gendriver _sample_mg_gendriver;;

let (ocamlpill_from_mgo_int, mgo_int_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_mgo_int _sample_mgo_int;;

let (ocamlpill_from_fem_body, fem_body_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_fem_body _sample_fem_body;;

let (ocamlpill_from_element, element_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_element _sample_element;;

let (ocamlpill_from_field, field_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_field _sample_field;;

let (ocamlpill_from_cofield, cofield_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_cofield _sample_cofield;;

let (ocamlpill_from_ksp, ksp_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_ksp _sample_ksp;;

let (ocamlpill_from_mwe, mwe_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_mwe _sample_mwe;;

let (ocamlpill_from_mesh_fca, mesh_fca_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_mesh_fca _sample_mesh_fca;;

let (ocamlpill_from_cvode, cvode_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_cvode Sundials_sp.dummy_cvode;;

let (ocamlpill_from_linalg_machine, linalg_machine_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_linalg_machine Nsim.dummy_linalg_machine;;

let (ocamlpill_from_jacobian, jacobian_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_jacobian _sample_jacobian;;


let _py_mesh_dim =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let mesh = mesh_from_ocamlpill args.(0) in
      pyint_fromint mesh.mm_dim
    )
;;

let _py_mesh_nr_points =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let mesh = mesh_from_ocamlpill args.(0) in
      let points = mesh.mm_points in
      pyint_fromint (Array.length points))
;;

let _py_mesh_nr_simplices =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let mesh = mesh_from_ocamlpill args.(0) in
      let s = mesh.mm_simplices in
      pyint_fromint (Array.length s))
;;

let _py_mesh_set_vertex_distribution =
  python_pre_interfaced_function
    ~doc:"Set mesh information how to distribute responsibility for individual vertices across the cluster"
    [|CamlpillType;ListType|]
    (fun args ->
       let mesh = mesh_from_ocamlpill args.(0) in
       let nr_nodes = Mpi_petsc.comm_size (Mpi_petsc.petsc_get_comm_world()) in
       let dist = py_int_list_as_array ~length:nr_nodes args.(1) in
       let () = mesh.mm_vertex_distribution <- dist in
	 pynone())
;;

let _py_petsc_mpi_nr_nodes =
  python_pre_interfaced_function
    ~doc:"Return the number of MPI nodes"
    [||]
    (fun args ->
       pyint_fromint (Mpi_petsc.comm_size (Mpi_petsc.petsc_get_comm_world()))
    )
;;


let _py_petsc_is_mpi =
  python_pre_interfaced_function
    ~doc:"Return True if called with mpirun"
    [||]
    (fun args ->
      let () = Printf.printf "This function (_py_petsc_is_mpi) should be obsolete! (fangohr 20 May 2008)\n" in
       match !pyfem_ccpla with
	 |None -> py_false
	 |_ -> py_true
    )
;;

let _py_init_hlib =
  python_pre_interfaced_function
    [|StringType|]
    (fun args ->
       let path = pystring_asstring args.(0) in
       let () = Hlib.hlib_init path in
	 pynone())
;;

let _py_petsc_cpu_cycles =
  python_pre_interfaced_function
    ~doc:"Obtain information about CPU cycles spent inside PETSc"
    [|StringType|]
    (fun args ->
       let cmd = pystring_asstring args.(0) in
	 match cmd with
	   | "reset" ->
	       let () = Mpi_petsc.petsc_reset_cpu_cycle_counters () in
		 pynone()
	   | "report" ->
	       let (nr_ksp,nr_mat,nr_vec) = Mpi_petsc.petsc_get_cpu_cycle_counters () in
		 pytuple3(
			   pytuple2(pystring_fromstring "Matrix Solver",pyfloat_fromdouble nr_ksp),
			   pytuple2(pystring_fromstring "Matrix*Vector",pyfloat_fromdouble nr_mat),
			   pytuple2(pystring_fromstring "Vector",pyfloat_fromdouble nr_vec))
	   | _ ->
	       failwith "Bad command!")
;;




(*  We'd like to be able to register an ocaml-nlog logger from
Python. (Once this is done we can manipulate it from Python by
providing the function that is called for logging events.) Nlog's
mechanist of 'setting up' or 'registering' loggers is that the
function Nlog.getLogger X will create a new logger X if it doesn't
exist yet. So to create an (Ocaml) Nlog logger from Python we use
getLogger to achieve this but ignore the return value (as there seems
not to be much point in getting a handle of the ocaml-logging function
(that is used to log events in ocaml) in Python.

*)


let _py_nlog_setupLogger =
  python_pre_interfaced_function
    ~doc:"Register a logger in Nlog (through calling Nlog.getLogger(logname)).\n This has to be called before we can register Pythoncallbacks as the \nhandlers for these loggers. \n\nArguments are LOGNAME (string). Returns None."
    [|StringType;|]
    (fun py_args ->
       let logname = pystring_asstring py_args.(0) in
       let () = match !pyfem_ccpla with
	 | None -> failwith "No ccpla found"
	 | Some ccpla -> Nsim.nsim_execute_all_CPUs ccpla (
	     fun ()->
	       let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
	       let _ = Nlog.getLogger logname in
	       if debug_mpi_loggers
	       then
		 Printf.printf "Registered logger %s on proc %d\n%!" logname rank
	       else
		 ()
	   )
       in
	 pynone() );;



let _py_nlog_setLogLevel =
  python_pre_interfaced_function
    ~doc:"Setting logging level (int) of one of Nlog's loggers (on all nodes).\nInput parameters (LOGGERNAME:string,LOGLEVEL:int).\nReturns previous value"
    [|StringType;IntType|]
    (fun py_args ->
       let logname = pystring_asstring py_args.(0) in
       let loglevel = pyint_asint py_args.(1) in
	 if do_not_use_logger then pyint_fromint 0
	 else
	   (* Set level on Master, and return last level value from here *)
	   let previous_loglevel = Nlog.setLogLevel logname
	     (Nlog.level_of_int loglevel)
	   in
	   let () = match !pyfem_ccpla with
	     | None -> failwith "No ccpla found"
	     | Some ccpla -> Nsim.nsim_execute_all_CPUs ccpla (
		 fun ()->
		   let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
		   if rank != 0
		   then
		     let () = if debug_mpi_loggers then
		       let () = Printf.printf "Setting level of %s on rank %d:\n%!" logname rank in ()
		     else
		       ()
		     in
		     let oldlevel_slave = Nlog.setLogLevel logname (Nlog.level_of_int loglevel)
		     in
		       if (Nlog.int_of_level oldlevel_slave) != (Nlog.int_of_level previous_loglevel) then
			 logwarn (Printf.sprintf
				    "Loglevel on rank %d is %d != master level =%d"
				    rank
				    (Nlog.int_of_level oldlevel_slave)
				    (Nlog.int_of_level previous_loglevel)
				 )
		       else
			 ()
	       )
	   in pyint_fromint (Nlog.int_of_level previous_loglevel)
    );;

let _py_nlog_printLoggerInfo =
  python_pre_interfaced_function
    ~doc:"Printing an overview of all registered loggers (on the master node) to stdout."
    [||]
    (fun py_args ->
       let () =
	 (if do_not_use_logger
	  then ()
	  else Nlog.printLoggerInfo ())
      in pynone() );;


let _py_nlog_printLoggerInfo_mpi =
  python_pre_interfaced_function
    ~doc:"Printing an overview of all registered loggers on rank n to stdout. (Debug only.)"
    [|IntType|]
    (fun py_args ->
       let n = pyint_asint py_args.(0) in
       let () = (
	 if do_not_use_logger
	 then ()
	 else
	   let () = match !pyfem_ccpla with
	     | None -> failwith "No ccpla found"
	     | Some ccpla -> Nsim.nsim_execute_all_CPUs ccpla (
		 fun ()->
		   let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
		     if rank == n then
		       let () = if debug_mpi_loggers then
			 Printf.printf "Registered loggers on rank %d:\n%!" rank
		       else
			 ()
		       in
		       let _ = Nlog.printLoggerInfo ()
		       in ()
		     else
		       (*
		       let () = if debug_mpi_loggers then
			 Printf.printf "Skipping loggers on rank %d:\n%!" rank
		       else
			 ()
		       in *) ()
	       )
	   in ())
       in
	pynone()
    );;




let _py_nlog_logmsg =
  python_pre_interfaced_function
    ~doc:"Sending message to ocaml-nlog logger.\nArguments: loggerName(str), loggingLevel(int), message(str).\nThis is only useful for debugging."
    [|StringType;IntType;StringType|]
    (fun py_args ->
       if do_not_use_logger then pynone()
       else
	 let loggername = pystring_asstring py_args.(0) in
	 let level = Nlog.level_of_int (pyint_asint py_args.(1)) in
	 let message = pystring_asstring py_args.(2) in
	 let () = Nlog.logmsg loggername level message in
	   pynone())
;;

let _py_nlog_logmsg_mpi =
  python_pre_interfaced_function
    ~doc:"Sending message to ocaml-nlog logger (on all nodes).\nArguments: loggerName(str), loggingLevel(int), message(str).\nThis is only useful for debugging."
    [|StringType;IntType;StringType|]
    (fun py_args ->
       if do_not_use_logger then pynone()
       else
	 let loggername = pystring_asstring py_args.(0) in
	 let level = Nlog.level_of_int (pyint_asint py_args.(1)) in
	 let message = pystring_asstring py_args.(2) in

	 let () = match !pyfem_ccpla with
	   | None -> failwith "No ccpla found"
	   | Some ccpla -> Nsim.nsim_execute_all_CPUs ccpla (
	     fun ()->
	       let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
	       let () = if debug_mpi_loggers then
		 Printf.printf "logmsg_mpi on %d\n%!" rank
	       else
		 ()
	       in
		 Nlog.logmsg loggername level message
    )
	 in
	   pynone())
;;



let _py_nlog_register_handler =
  python_pre_interfaced_function
    ~doc:"Register a python-handler for nlog. \nArguments: Logger name (str) and callback function cb().\nThe signature of cb() is cb( level: int, message:str).\n\nThis does only allow to replace the standard handler for this logger but this is usually what one wants because Python's logging will be used to do the hard work in this context. "
    [|StringType;CallableType|]
    (fun py_args ->
       let logname = pystring_asstring py_args.(0) in
       let pyfun = pycallable_asfun py_args.(1) in

       let logdebug = (fun m -> ()) in  (* remove this line to activate debugging output
					   below. This code is executed before the loggers
					   system is set up, and thus will always produce
					   output (because by default there is not loglevel
					   set. *)

       (* let () =  if do_not_use_logger
	  then
	  ()
	  else *)

       let do_logging_on_slaves =
	 let slavelog = Snippets.get_feature ~domain:"nmag" "slavelog" in
	   match slavelog with
	     | Some slavelog ->
		 if slavelog = "None" || slavelog = "false" || slavelog = "False" then
		   let () = logdebug (Printf.sprintf "slavolog=%s->False\n%!" slavelog) in
		     false
		 else
		   if (slavelog = "True" || slavelog = "true") then
		     let () = logdebug( Printf.sprintf "slavolog=%s->True\n%!" slavelog) in
		       true
		   else
		     let msg = Printf.sprintf "Unknown value for feature 'nmag':'slavelog': '%s'" slavelog in
		     let () = logerror msg in
		       failwith msg
	     | None -> let () = logdebug (Printf.sprintf "slavolog -> No slavelog entry\n%!") in
		 false
       in
       let pyemitfunc level msg =
	 let _ = pyfun [|pyint_fromint level; pystring_fromstring msg|] in
	   ()
       in
       let rank1 = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
       let () =
	 if rank1 = 0 then
	   Nlog.setLogHandler logname pyemitfunc
	 else
	   failwith "This code is only executed on the master. Impossible"
       in
       let register_slaveemitter =
	 ( fun () ->
	     let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
	     let () = if debug_mpi_loggers then
	       Printf.printf "Entering register_slaveemmiter, rank=%d\n%!" rank
	     else
	       ()
	     in
	     let slave_string = Printf.sprintf "S%02d" rank in
	     let slaveemitfunc = (
	       fun level msg ->
		 (* Provide a slave emitter *)
		 let () = Printf.printf "%s-%10s:%s %6s %s\n%!"
		   slave_string
		   logname
		   (Nlog.localtime ())
		   (Nlog.string_of_level (Nlog.level_of_int level))
		   msg
		 in ()
	     )
	     in
	       if rank = 0 then
		 ()           (* Not for the master -> has Python logger *)
	       else
		   if do_logging_on_slaves then
		     Nlog.setLogHandler logname slaveemitfunc
		   else
		     Nlog.setLogHandler logname (fun l m -> ())
	 )
       in
       let () = match !pyfem_ccpla with
	 | None -> failwith "No ccpla found"
	 | Some ccpla -> Nsim.nsim_execute_all_CPUs ccpla (
	     fun ()->
	       let rank = Mpi_petsc.comm_rank Mpi_petsc.comm_world in
	       let () = register_slaveemitter () in
	       let () = if debug_mpi_loggers then
		 Printf.printf "Calling register_slaveemitter (-> setLogHandler on node %d)\n%!" rank
	       else
		 ()
	       in
		 ()
	   )
       in pynone ()
) ;;




let _py_version =
  python_pre_interfaced_function
    ~doc:"Provides the current version of the (Ocaml) code (output of 'version()')."
    [||]
    (fun py_args -> pystring_fromstring version_string);;

let _py_string_multifill_template_then_concat =
  python_pre_interfaced_function
    [|StringType;StringType;ListType;ListType|]
    (fun args ->
       let separator = pystring_asstring args.(0) in
       let template = pystring_asstring args.(1) in
       let patterns = py_string_list_as_array args.(2) in
       let replacements = py_string_list_list_as_array args.(3) in
       let result = string_multifill_template_then_concat ~separator template patterns replacements in
	 pystring_fromstring result)
;;

let _py_snippets_get_feature =
  python_pre_interfaced_function
    ~doc:"Retrieve 'feature' (see Snippets.mli) \nSignature: get_feature(domain,key)"
    [|StringType;StringType|]
    (fun py_args ->
       let domain = pystring_asstring py_args.(0) in
       let key = pystring_asstring py_args.(1) in
       let value = Snippets.get_feature ~domain:domain key in
	 match value with
	   | None -> pynone();
	   | Some value -> pystring_fromstring value);;

let _py_snippets_register_feature =
  python_pre_interfaced_function
    ~doc:"Register 'feature' (see Snippets.mli). If feature exists already, override it with new value.\nSignature: register_feature(domain,key,value)"
    [|StringType;StringType;StringType|]
    (fun py_args ->
       let domain = pystring_asstring py_args.(0) in
       let key = pystring_asstring py_args.(1) in
       let value = pystring_asstring py_args.(2) in
       let () = Snippets.register_feature ~domain:domain key value in
       pynone());;

(*

Data structure as returned by all_features:
- : (string * (string * string) list) list =
  #[("main", [("one", "two")]);
  ("nmesh", [("one", "two"); ("mykey", "myvalue")])]
*)


let _py_snippets_all_features =
  python_pre_interfaced_function
    ~doc:"Returns all registered features in the form [(domain_name1,[(key1,value1),...]),...]."
    [||]
    (fun py_args ->
      let all = Array.of_list (Snippets.all_features ()) in
      let all_python_content =
	Array.map
	  (fun (domain,li_key_val) ->
	    pytuple2(pystring_fromstring domain,
		     (pylist_fromarray
		       (Array.map
			  (fun (key,value) ->
			    pytuple2(pystring_fromstring key,pystring_fromstring value))
			  (Array.of_list li_key_val)))))
	  all
      in pylist_fromarray all_python_content);;

(* Note: should be re-done *)
let _py_make_mg_gendriver =
  python_pre_interfaced_function
    [|IntType;CallableType|]
    (fun args ->
       let every_nr_steps = pyint_asint args.(0) in
       let python_mesh_processor = args.(1) in
       let gendriver =
	 (fun nr_piece ->
	    do_every_n_steps_driver every_nr_steps
	      (fun n mesh ->
		 let () = mesh_grow_bookkeeping_data ~do_regions:true mesh in
		 let (point_coords,pairs,simplices_and_cc_ic,in_body) = mesh_plotinfo mesh in
		 let (pointcoords,surf_simplices_and_cc_ic)=mesh_extract_surface mesh in
		 let py_surf_elements =
		   pylist_fromarray
		     (Array.map
			(fun (sx_points_indices,((cc_mid,cc_r),(ic_mid,ic_r),region)) ->
			   let mx = Array.map ( fun pt_index -> Array.append (Array.copy point_coords.(pt_index)) [|1.|] ) sx_points_indices
			   in
			   let det = determinant (Array.length mx) mx in
			   let ordered_points_indices =
			     if det < 0.
			     then
			       let re_ordered = Array.copy sx_points_indices in
			       let () = re_ordered.(0) <- sx_points_indices.(1) in
			       let () = re_ordered.(1) <- sx_points_indices.(0) in
		       		 re_ordered
			     else
			       sx_points_indices
			   in
			     pytuple4(int_array_to_python ordered_points_indices,
				      pytuple2(float_array_to_python cc_mid,pyfloat_fromdouble cc_r),
				      pytuple2(float_array_to_python ic_mid,pyfloat_fromdouble ic_r),
				      pyint_fromint region
				     ))
			surf_simplices_and_cc_ic)
		 in
		 let pypoints = (* XXX make this mapping a pycaml function. *)
		   pylist_fromarray
		     (Array.map
			(fun coord_array ->
			   pylist_fromarray (Array.map pyfloat_fromdouble coord_array))
			point_coords)
		 in
		 let pypairs =
		   pylist_fromarray
		     (Array.map
			(fun (ix1,ix2) -> pytuple2(pyint_fromint ix1,pyint_fromint ix2))
			pairs)
		 in
		 let pybody =
		   pylist_fromarray
		     (Array.map
			(fun body_ids ->
			   pylist_fromarray (Array.of_list (List.map pyint_fromint body_ids)))
			in_body)
		 in
		 let pysimplices =
		   pylist_fromarray
		     (Array.map
			(fun (sx_points_indices,((cc_mid,cc_r),(ic_mid,ic_r),region)) ->
			   pytuple4(int_array_to_python sx_points_indices,
				    pytuple2(float_array_to_python cc_mid,pyfloat_fromdouble cc_r),
				    pytuple2(float_array_to_python ic_mid,pyfloat_fromdouble ic_r),
				    pyint_fromint region
				   ))
			simplices_and_cc_ic)
		 in
		 let pyvolumes = float_array_to_python mesh.mm_region_volumes in
		 let pymesh = pylist_fromarray
		   (Array.map
		      (fun (tag,doc,data) ->
			 pytuple3(pystring_fromstring tag,pystring_fromstring doc,data))
		      [|("COORDS","Coordinates of points",pypoints);
			("LINKS","Links in the mesh (pairs of point indices)",pypairs);
			("SIMPLICES","Simplex info (points-indices,((circumcirc center,cc radius),(ic center,ic radius),region))",pysimplices);
			("POINT-BODIES","Which bodies does the corresponding point belong to (body index list)",pybody);
			("SURFACES","Surface elements info (points-indices,((circumcirc center,cc radius),(ic center,ic radius),region))",py_surf_elements);
			("REGION-VOLUMES","Volume for every region",pyvolumes);
		      |])
		 in
		 let _ =
		   pyeval_callobject(python_mesh_processor,
				     pytuple3(pyint_fromint nr_piece,pyint_fromint n,pymesh))
		 in
		   ()))
       in
	 ocamlpill_from_mg_gendriver gendriver)
;;


let _py_mgo_extract_mesh =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let mgo = mgo_int_from_ocamlpill args.(0) in
      match mgo with
      | Mesh_Engine_Finished_Step_Limit_Reached (mesh, nr_steps)
	->
	  pytuple3(pystring_fromstring "Step Limit Reached",
		   pyint_fromint nr_steps,
		   ocamlpill_from_mesh mesh)
      | Mesh_Engine_Finished_Force_Equilibrium_Reached (mesh, residual_force)
	->
	  pytuple3(pystring_fromstring "Force Equilibrium Reached",
		   pyfloat_fromdouble residual_force,
		   ocamlpill_from_mesh mesh)
      | Mesh_Engine_Produced_Intermediate_Mesh_And_Can_Continue (mesh, _)
	->
	  pytuple3(pystring_fromstring "Intermediate Mesh",
		   args.(0),	(* Just put in the object itself -
				   this is the most appropriate value here,
				   as it tells us how to continue.
				   We might also just provide pynone()
				   or something similar.
				 *)
		   ocamlpill_from_mesh mesh)
      | _ -> pynone()
	    (* No mesh to extract! *)
	    (* XXX is that right? Should we return pynull()? *)
    )
;;

(* Geometry/Body functions *)

let _py_body_box =
  python_pre_interfaced_function
    [|ListType;ListType|]
    (fun args ->
      let corner_nw = py_float_list_as_array ~error_label:"body_box arg #1" args.(0) in
      let dim = Array.length corner_nw in
      let corner_se = py_float_list_as_array ~length:dim ~error_label:"body_box arg #2" args.(1) in
      let bc = bc_box corner_nw corner_se in
      let body = Body(body_trafo_id dim,bc)
      in ocamlpill_from_fem_body body)
;;

let _py_body_ellipsoid =
  python_pre_interfaced_function
    [|ListType|]
    (fun args ->
      let radii = py_float_list_as_array ~error_label:"body_ellipsoid arg #1" args.(0) in
      let bc = bc_ellipsoid radii in
      let dim = Array.length radii in
      let body = Body(body_trafo_id dim,bc)
      in ocamlpill_from_fem_body body)
;;




let _py_body_helix =
  python_pre_interfaced_function
    [|ListType;     (* centre1 *)
      FloatType;    (* radius_spiral *)
      ListType;     (* centre2 *)
      FloatType;    (* radius_circle *)
    |]
    (fun args ->
      let centre1 = py_float_list_as_array ~error_label:"body_helix arg #1" args.(0) in
      let dim = Array.length centre1 in
      let radius1 = pyfloat_asdouble args.(1) in
      let centre2 = py_float_list_as_array ~error_label:"body_helix arg #3" args.(2) in
      let radius2 = pyfloat_asdouble args.(3) in
      let bc = bc_helix (centre1,radius1) (centre2,radius2) in
      let body = Body(body_trafo_id dim,bc)
      in ocamlpill_from_fem_body body)
;;


let _py_body_frustum =
  python_pre_interfaced_function
    [|ListType;     (* centre1 *)
      FloatType;    (* radius1 *)
      ListType;     (* centre2 *)
      FloatType;    (* radius2 *)
    |]
    (fun args ->
      let centre1 = py_float_list_as_array ~error_label:"body_frustum arg #1" args.(0) in
      let dim = Array.length centre1 in
      let radius1 = pyfloat_asdouble args.(1) in
      let centre2 = py_float_list_as_array ~error_label:"body_frustum arg #3" args.(2) in
      let radius2 = pyfloat_asdouble args.(3) in
      let bc = bc_frustum (centre1,radius1) (centre2,radius2) in
      let body = Body(body_trafo_id dim,bc)
      in ocamlpill_from_fem_body body)
;;


let _py_body_csg csg_error_label csg =
  python_pre_interfaced_function
    [|ListType|]
    (fun args ->
      let bodies = py_homogeneous_list_as_array
	~error_label:csg_error_label
	"FEM-Body"
	(fun x -> ocamlpill_type_of x = pysym_fem_body)
	(ocamlpill_hard_unwrap:(pyobject -> body))
	args.(0)
      in
      ocamlpill_from_fem_body (csg bodies))
;;


let _py_body_difference =
  python_pre_interfaced_function
    [|CamlpillType;ListType|]
    (fun args ->
      let mother_body = fem_body_from_ocamlpill args.(0) in
      let bodies =
	py_homogeneous_list_as_array ~error_label:"body_difference"
	  "FEM-Body"
	  (fun x -> ocamlpill_type_of x = pysym_fem_body)
	  (ocamlpill_hard_unwrap:(pyobject -> body))
	  args.(1)
      in
      ocamlpill_from_fem_body (body_difference_1n mother_body bodies))
;;

(* Affine body transformations can be executed either wrt space coords,
   or wrt body coords. We will offer both variants.
 *)

let _py_body_shifted_bs error_label do_body_not_space_trafo =
  python_pre_interfaced_function
    [|CamlpillType;ListType|]
    (fun args ->
      let Body(trafo,bc) = fem_body_from_ocamlpill args.(0) in
      let dim = body_trafo_dim trafo in
      let nshift = py_float_list_as_array
	  ~error_label:error_label
	  ~length:dim
	  args.(1)
      in
      let shift = Array.map (fun x -> 0.0-.x) nshift in
      let new_trafo = body_trafo_shift shift in
      let combined_trafo =
	if do_body_not_space_trafo
	then
	  combine_body_transformations new_trafo trafo
	else
	  combine_body_transformations trafo new_trafo
      in
      ocamlpill_from_fem_body (Body(combined_trafo, bc)))
;;


let _py_body_rotated error_label do_body_not_space_trafo =
  python_pre_interfaced_function
    [|CamlpillType;  (* object *)
      IntType;   (* first axis *)
      IntType;   (* second axis *)
      FloatType|] (* rotation angle *)
    (fun args ->
      let Body(trafo,bc) = fem_body_from_ocamlpill args.(0) in
      let dim = body_trafo_dim trafo in
      let axis1 = pyint_asint args.(1) in
      let axis2 = pyint_asint args.(2) in
      let rad_angle = pyfloat_asdouble args.(3) in
      let new_trafo = body_trafo_rotate dim axis1 axis2 (-.rad_angle) in
      let combined_trafo =
	if do_body_not_space_trafo
	then
	  combine_body_transformations new_trafo trafo
	else
	  combine_body_transformations trafo new_trafo
      in
      ocamlpill_from_fem_body (Body(combined_trafo, bc)))
;;

(* XXX Note: this should have "3d" in its name! *)
let _py_body_rotated_axis error_label do_body_not_space_trafo =
  (* rotation only in 3D *)
  python_pre_interfaced_function
    [|CamlpillType;  (* object *)
      ListType;   (* rotation axis *)
      FloatType|] (* rotation angle *)
    (fun args ->
      let Body(trafo,bc) = fem_body_from_ocamlpill args.(0) in
      let dim = 3 in
      let axis = py_float_list_as_array
	  ~error_label:error_label
	  ~length:dim
	  args.(1)
      in
      let rad_angle = pyfloat_asdouble args.(2) in
      let new_trafo = body_trafo_rotate_axis dim axis (-.rad_angle) in
      let combined_trafo =
	if do_body_not_space_trafo
	then
	  combine_body_transformations new_trafo trafo
	else
	  combine_body_transformations trafo new_trafo
      in
      ocamlpill_from_fem_body (Body(combined_trafo, bc)))
;;


let _py_body_scaled error_label =
  python_pre_interfaced_function
    [|CamlpillType;  (* object *)
      ListType|] (* scaling factor *)
    (fun args ->
       let (Body(trafo,bc)) = fem_body_from_ocamlpill args.(0) in
       let dim = body_trafo_dim trafo in
       let nscale = py_float_list_as_array
	 ~error_label:error_label
	 ~length:dim
	 args.(1)
       in
       let scale = Array.map (fun x -> 1. /. x) nscale in
       let new_trafo = body_trafo_scale scale in
       let combined_trafo =
	 combine_body_transformations new_trafo trafo
       in
	 ocamlpill_from_fem_body (Body(combined_trafo, bc)))
;;

let _do_reorder_mesh mesh =
  let connectivity = mesh_connectivity mesh in
  let reordering = Mpi_petsc.proximity_improving_reordering connectivity in
  let () = mesh_do_reorder_vertices mesh (fun n -> reordering.(n)) in
    ()
;;

let _do_distribute_mesh mesh =
  let nr_pieces = Mpi_petsc.comm_size (Mpi_petsc.petsc_get_comm_world()) in
    if nr_pieces > 1 then
      let () = loginfo (Printf.sprintf "Calling ParMETIS to partition the mesh among %d processors" nr_pieces) in
      let adjacency = mesh_connectivity mesh in
      let pieces = Mpi_petsc.cluster_partitioning nr_pieces adjacency in
      let perm = array_join pieces in
      let () = mesh_do_reorder_vertices mesh (fun n -> perm.(n)) in
      let () = mesh.mm_vertex_distribution <- Array.map Array.length pieces in
      let () = Array.iteri
                 (fun nr_cpu piece ->
                    loginfo (Printf.sprintf "Processor %d: %d nodes"
                                            nr_cpu (Array.length piece)))
                 pieces
      in
        ()
    else
      (* mesh.mm_vertex_distribution is the empty array, meaning
       * that we are doing everything on one single machine.
       *)
      ()
;;

let _py_delaunay =
  python_pre_interfaced_function
    ~doc:"Delaunay-triangluate a set of points using Qhull."
    [|ListType|]
    (fun args ->
      let points =
	Array.map
	  py_number_list_as_float_array
	  (pylist_toarray args.(0))
      in
      let triangulated = Qhull.delaunay points in
      pylist_fromarray
	(Array.map
	   (fun v -> pylist_fromarray (Array.map pyint_fromint v))
	   triangulated))
;;

(* The problem with Laplacian Smoothing seems to be
   that it does not really do well for the initial
   steps of mesh generation...
*)

let _py_simple_convex_mesh =
  python_pre_interfaced_function
    ~doc:"Given points that define a convex hull, generate
    a simple comvex mesh. Arguments:
    (boundary_points,random_seed,nr_points_interior,nr_steps)"
    [|ListType;	(* Hull points *)
      IntType;  (* RNG seed *)
      IntType;  (* Nr points in the interior *)
      IntType;  (* Nr steps *)
      (* maybe add: a function that gives local weights... *)
    |]
    (fun args ->
      let v_hull_points =
	Array.map
	  py_number_list_as_float_array
	  (pylist_toarray args.(0))
      in
      let rng = Mt19937.make (pyint_asint args.(1)) in
      let nr_interior_points = pyint_asint args.(2) in
      let nr_steps = pyint_asint args.(3) in
      let nr_hull_points = Array.length v_hull_points in
      let nr_points = nr_hull_points + nr_interior_points in
      let dim = Array.length v_hull_points.(0) in
      let generate_point () =
	let indices =
	  Array.init dim
	    (fun _ -> Mt19937.int rng nr_hull_points) in
	let weights = Array.init dim
	    (fun _ -> Mt19937.float rng 1.0)
	in
	let normalized_weights =
	  let s=Array.fold_left (+.) 0.0 weights in
	  if s = 0.0
	  then weights (* Very unlikely! *)
	  else
	    Array.map (fun x -> x /. s) weights
	in
	let point = Array.make dim 0.0 in
	let () =
	  for i=0 to dim-1 do
	    for k=0 to dim-1 do
	      point.(k) <-
		point.(k)
		  +. normalized_weights.(i)*.v_hull_points.(indices.(i)).(k);
	    done
	  done
	in
	point
      in
      let v_points = Array.init nr_points
	  (fun n ->
	    if n < nr_hull_points then v_hull_points.(n)
	    else generate_point())
      in
      let f_dim = float_of_int dim in
      let point_distance ix1 ix2 =
	let p1 = v_points.(ix1)
	and p2 = v_points.(ix2)
	in
	let rec walk n sum =
	  if n = dim then sqrt(sum)
	  else walk (n+1) (sum+.(p1.(n)-.p2.(n))*.(p1.(n)-.p2.(n)))
	in walk 0 0.0
      in
      let distance_weight ix1 ix2 =
	(* 1.0 *)
	(* xxx some other complicated method, e.g. *)
	let d = point_distance ix1 ix2
	in
	tanh (d*.2.0)
      in
      let averaged_point ix neighbourhoods =
	let neighbours = neighbourhoods.(ix) in
        (* Current point also included in averaging with one vote:
	   (Note: If we did not do this, there would be a strong
	   tendency to generate meshes that crash Qhull!)
	 *)
	let weight_p = 1.0 in

	let p = Array.map (fun x -> x*.weight_p) v_points.(ix) in
	let total_weight =
	  Array.fold_left
	    (fun weight ix_neighbour ->
	      let w = distance_weight ix_neighbour ix in
	      let neighbour = v_points.(ix_neighbour) in
	      let () =
		for i=0 to dim-1 do
		  p.(i) <- p.(i) +. w*.neighbour.(i)
		done
	      in
	      (weight+.w)) weight_p
	    neighbours
	in
	let () =
	  for i=0 to dim-1 do
	    p.(i) <- p.(i) /. total_weight
	  done
	in p
      in
      let r_neighbourhoods = ref [||] in
      let () =
	for nr_step=0 to nr_steps-1 do
	  (* Re-delaunay every 4 steps or so. Note that this is not just a speed issue,
	     but this heuristics actually manages to improve the process...
	   *)
	  let () =
	    (if nr_step mod 4 = 0 then r_neighbourhoods := Qhull.neighbours v_points else ())
	  in
	  let neighbourhoods = !r_neighbourhoods in
	  let v_moved =
	    Array.init nr_interior_points
	      (fun ix_interior ->
		averaged_point
		  (nr_hull_points+ix_interior)
		  neighbourhoods)
	  in
	  for i=0 to nr_interior_points-1 do
	    (* Printf.printf "%3d: %s -> %s (d=%f)\n" i (float_array_to_string v_points.(nr_hull_points+i)) (float_array_to_string v_moved.(i)) (sqrt (euclidean_distance_sq v_points.(nr_hull_points+i) v_moved.(i))); (* DDD *) *)
	    v_points.(nr_hull_points+i) <- v_moved.(i)
	  done
	done
      in
      let triangulation = Qhull.delaunay v_points in
      let the_mesh =
	mesh_from_known_delaunay
	  v_points
	  (Array.map
	     (fun simplex -> (Mesh.Body_Nr 1,simplex))
	     triangulation)
      in
      let ddd = Printf.printf "DDD made mesh!\n%!" in
      let () = mesh_grow_bookkeeping_data ~do_connectivity:true the_mesh in
      let ddd = Printf.printf "DDD did connectivity!\n%!" in
      ocamlpill_from_mesh the_mesh)
;;

(* Note: this function has weird and ugly calling conventions. We MUST
   therefore provide a python wrapper which hides the underlying complexity.
 *)
let _py_mesh_bodies_raw =
  python_pre_interfaced_function
    [|CamlpillType;	 (* gendriver *)
      CamlpillType;         (* mesher defaults *)
      ListType;ListType; (* Bounding box nw/se *)
      IntType;	         (* Mesh exterior - 0=false,1=true XXX use boolean! *)
      ListType;		 (* Bodies *)
      FloatType;	 (* Length Scale *)
      StringType;        (* Density *)
      ListType;          (* Fixed initial points*)
      ListType;          (* Mobile initial points*)
      ListType;          (* Simply initial points*)
      ListType;          (* periodic dimensions *)
      StringType;	 (* Opt. Cache name *)
      ListType		 (* Hints *)
    |]
    (fun args ->
      let gendriver = mg_gendriver_from_ocamlpill args.(0) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(1) in
      let corner_nw = py_float_list_as_array ~error_label:"mesh_bodies (nw bounding box corner)" args.(2) in
      let corner_se = py_float_list_as_array ~error_label:"mesh_bodies (se bounding box corner)" args.(3) in
      let mesh_exterior = (pyint_asint args.(4) <> 0) in
      let bodies = py_homogeneous_list_as_array
	  ~error_label:"mesh_bodies"
	  "FEM-Body"
	  (fun x -> ocamlpill_type_of x = pysym_fem_body)
	  (ocamlpill_hard_unwrap:(pyobject -> body))
	  args.(5)
      in
      let type_error s =
	raise
	  (Pycaml_exn
	      (Pyerr_TypeError, s))
      in

      let hints =
	Array.map
	  (fun mesh_body_pytuple ->
	    if pytype mesh_body_pytuple <> ListType
	    then type_error "The hint provided doesn't match the type specification"
	    else
	      let mesh_body_tuple = pylist_toarray mesh_body_pytuple in
		try
		  (mesh_from_ocamlpill mesh_body_tuple.(0), fem_body_from_ocamlpill mesh_body_tuple.(1))
                with | _ -> type_error "The hint provided should be of the type (mesh, mesh object)"
	  )
	  (pylist_toarray args.(13))
      in
      let rod_length = pyfloat_asdouble args.(6) in
      let ccode_density = pystring_asstring args.(7) in
      let fun_density =
	if ccode_density = ""
	then (fun _ -> 1.0)
	else
	  let c_interim = Fastfields.c_register_field
	      ~in_name:"x"
	      ~out_name:"__density"
	      ~extra_defines:[("density",[],"(*__density)")]
	      ccode_density
	  in
	  let c_callable_wrapped =
	    Fastfields.c_field_evaluator_mapping_float_array_to_float_array
	      ~allocate_every_result:false
	      1 (* dim_output *)
	      c_interim
	  in
	  (fun pos -> let result = c_callable_wrapped pos in result.(0))
      in
      let fixed_points =
	py_float_list_list_as_array
	  ~error_label:"mesh_bodies" ~length_inner:(Array.length corner_nw)
	  args.(8)
      in
      let mobile_points =
	py_float_list_list_as_array
	  ~error_label:"mesh_bodies" ~length_inner:(Array.length corner_nw)
	  args.(9)
      in
      let simply_points =
	py_float_list_list_as_array
	  ~error_label:"mesh_bodies" ~length_inner:(Array.length corner_nw)
	  args.(10)
      in
      let periodic = py_float_list_as_array ~error_label:"mesh_bodies" args.(11) in
      let fem_geometry = fem_geometry_from_bodies
	  ~mesh_exterior:mesh_exterior
	  ~density:fun_density
	  (corner_nw,corner_se) bodies hints
      in
      try
	let do_the_work () =
	  let () = Printf.printf "DDD do_the_work()\n%!" in
	  mesh_boundaries_and_objects ~gendriver:gendriver fixed_points mobile_points simply_points fem_geometry mdefaults rod_length periodic
	in
	let cache_name = pystring_asstring args.(12) in
	let mesh =
	  if cache_name="" then do_the_work ()
	  else
	    maybe_use_cached_mesh cache_name do_the_work
	in
	let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
	  (* extract coordinates of periodic points to update mesh.mm_periodic after the reordering *)
	let periodic_points = Array.map (fun per_ix_arr -> Array.map (fun per_ix -> points_coords.(per_ix)) per_ix_arr  ) mesh.mm_periodic_points in
	let () = _do_reorder_mesh mesh in
	let () = Printf.printf "DDD NOTE: internal mesher not used at present. This does not set up mesh cluster distribution!\n%!" in
	let new_points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
	let () = mesh.mm_periodic_points <- Array.map ( fun per_arr -> Array.map (fun p -> array_position p new_points_coords 0 ) per_arr) periodic_points in
	  ocamlpill_from_mesh mesh
      with | _ -> pynone()
    )
;;



(* Use this function only to get timings on this process *)
let _py_mesh_grow_bookkeeping_data_all  =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), it grows all the bookkeeping data. (Use for taking timings during development)"
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
      let () = mesh_grow_bookkeeping_data ~do_connectivity:true ~do_incircle_circumcircle:true ~do_regions:true m in
	pynone()
    );;


let _py_mesh_plotinfo_points =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return the positions of nodes (list of list of floats)"
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
      let point_coords = mesh_plotinfo_points m in
	pylist_fromarray
	  (Array.map
	      (fun coord_array ->
		pylist_fromarray (Array.map pyfloat_fromdouble coord_array))
	      point_coords));;

let _py_mesh_plotinfo_pointsregions =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return the regions to which a node belongs (list of list of ints)"
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
      let point_regions = mesh_plotinfo_pointsregions m in
	pylist_fromarray
	  (Array.map
	      (fun regions_array ->
		pylist_fromarray (Array.map pyint_fromint regions_array))
	      point_regions));;


let _py_mesh_plotinfo_links =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return all links (as list of node index tuples)"
    [|CamlpillType|]
    ( fun args ->
      let m = mesh_from_ocamlpill args.(0) in
      let links = mesh_plotinfo_links m in
	pylist_fromarray
	  (Array.map
	     (fun (ix1,ix2) -> pytuple2(pyint_fromint ix1,pyint_fromint ix2))
	     links));;


let _py_mesh_plotinfo_simplices =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return the list of simplices (each simplex described by a list of point ids). I.e., returns (list of list of int)."
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
      let simplices = mesh_plotinfo_simplices m in
	pylist_fromarray
	  (Array.map
	      (fun point_index_array ->
		pylist_fromarray (Array.map pyint_fromint point_index_array))
	      simplices));;


let _py_mesh_plotinfo_simplicesregions =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return a list of region ids (one for each simplex). Returns (list of int)."
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
      let simplicesregions = mesh_plotinfo_simplicesregions m in
	pylist_fromarray (Array.map pyint_fromint simplicesregions));;

let _py_mesh_plotinfo_regionvolumes =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return a list of region volumes."
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
	float_array_to_python(mesh_plotinfo_regionvolumes m));;


let _py_mesh_plotinfo_periodic_points_indices =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return a list where each entry is a list of equivalent periodic points, expressed through their indices."
    [|CamlpillType|]
    ( fun args ->
      let m  = mesh_from_ocamlpill args.(0) in
      let periodic_indices = mesh_plotinfo_periodic_points_indices m in
	pylist_fromarray
	  (Array.map
	      (fun equiv_points_index_array ->
		pylist_fromarray (Array.map pyint_fromint equiv_points_index_array))
	      periodic_indices));;

let _py_mesh_plotinfo_surfaces_and_surfacesregions =
  python_pre_interfaced_function
    ~doc:"For a give mesh (CamlpillType), return a list of surface simplices (one for each simplex) and a list of regions they belong to. Double counts surfaces when there is a region to both sides. Returns (list of list of int, lists of int)."
    [|CamlpillType|]
    ( fun args ->
      let mesh = mesh_from_ocamlpill args.(0) in
      let (surface_simplices, surface_regions) = mesh_plotinfo_surfaces_and_surfacesregions mesh
      in
	pytuple2
	  (pylist_fromarray
	      ( Array.map
		  (fun pointidarray ->
		    pylist_fromarray (Array.map pyint_fromint pointidarray))
		  surface_simplices
	      )
	      ,
	  pylist_fromarray ( Array.map pyint_fromint surface_regions)
	  )
    )
;;

let _py_mesh_plotinfo =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let m = mesh_from_ocamlpill args.(0) in
      let the_det = determinant m.mm_dim in
      let (point_coords,pairs,simplices_and_cc_ic,in_body) = mesh_plotinfo m in
      let (pointcoords,surf_simplices_and_cc_ic)=mesh_extract_surface m in
      let py_surf_elements =
	pylist_fromarray
	  (Array.map
	     (fun (sx_points_indices,((cc_mid,cc_r),(ic_mid,ic_r),region)) ->
	       pytuple4(int_array_to_python sx_points_indices,
			pytuple2(float_array_to_python cc_mid,pyfloat_fromdouble cc_r),
			pytuple2(float_array_to_python ic_mid,pyfloat_fromdouble ic_r),
			pyint_fromint region
		       ))
	     surf_simplices_and_cc_ic)
      in
      let pypoints = (* XXX make this mapping a pycaml function. *)
	pylist_fromarray
	  (Array.map
	     (fun coord_array ->
	       pylist_fromarray (Array.map pyfloat_fromdouble coord_array))
	     point_coords)
      in
      let pypairs =
	pylist_fromarray
	  (Array.map
	     (fun (ix1,ix2) -> pytuple2(pyint_fromint ix1,pyint_fromint ix2))
	     pairs)
      in
      let pybody =
	pylist_fromarray
	  (Array.map
	     (fun body_ids ->
	       pylist_fromarray (Array.of_list (List.map pyint_fromint body_ids)))
	     in_body)
      in
      let pysimplices =
	pylist_fromarray
	  (Array.map
	     (fun (sx_points_indices,((cc_mid,cc_r),(ic_mid,ic_r),region)) ->
	       let mx = Array.map ( fun pt_index -> Array.append (Array.copy point_coords.(pt_index)) [|1.|] ) sx_points_indices
	       in
	       let det = the_det mx in
	       let ordered_points_indices =
		 if det < 0.
		 then
		   let re_ordered = Array.copy sx_points_indices in
		   let () = re_ordered.(0) <- sx_points_indices.(1) in
		   let () = re_ordered.(1) <- sx_points_indices.(0) in
		   re_ordered
		 else
		   sx_points_indices
	       in
	       pytuple4(int_array_to_python ordered_points_indices,
			pytuple2(float_array_to_python cc_mid,pyfloat_fromdouble cc_r),
			pytuple2(float_array_to_python ic_mid,pyfloat_fromdouble ic_r),
			pyint_fromint region
		       ))
	     simplices_and_cc_ic)
      in
      let pyvolumes = float_array_to_python m.mm_region_volumes in
      pylist_fromarray
	(Array.map
	   (fun (tag,doc,data) ->
	     pytuple3(pystring_fromstring tag,pystring_fromstring doc,data))
	   [|("COORDS","Coordinates of points",pypoints);
	     ("LINKS","Links in the mesh (pairs of point indices)",pypairs);
	     ("SIMPLICES","Simplex info (points-coords,((circumcirc center,cc radius),(ic center,ic radius),region))",pysimplices);
	     ("POINT-BODIES","Which bodies does the corresponding point belong to (body index list)",pybody);
	     ("SURFACES","Surface elements info (points-coords,((circumcirc center,cc radius),(ic center,ic radius),region))",py_surf_elements);
	     ("REGION-VOLUMES","Volume for every region",pyvolumes);
	   |]))
;;

let _py_mesh_writefile =
  python_pre_interfaced_function
    [|StringType; CamlpillType|]
    (fun args ->
      let filename = pystring_asstring args.(0) in
      let mesh = mesh_from_ocamlpill args.(1) in
      begin
	write_mesh filename mesh;
	pynone()
      end
    )
;;

let _py_mesh_readfile =
    python_pre_interfaced_function
    [|StringType;py_bool_type;py_bool_type|]
    (fun args ->
      let filename = pystring_asstring args.(0) in
      let do_reorder = py_is_true args.(1) in
      let do_distribute = py_is_true args.(2) in
      let some_mesh = read_mesh filename in
      let mesh =
	match some_mesh with
	| Some mesh -> mesh
	| _ -> impossible ()(*failwith "Reading mesh failed!"*)
      in
      let () =
	(if do_reorder
	 then
	   let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
	     (* extract coordinates of periodic points to update mesh.mm_periodic after the reordering *)
	   let periodic_points = Array.map (fun per_ix_arr -> Array.map (fun per_ix -> points_coords.(per_ix)) per_ix_arr  ) mesh.mm_periodic_points in
	   let () = _do_reorder_mesh mesh in
	   let new_points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
	     mesh.mm_periodic_points <- Array.map ( fun per_arr -> Array.map (fun p -> array_position p new_points_coords 0 ) per_arr) periodic_points
	 else ())
      in
      let () =
	if do_distribute then
	  _do_distribute_mesh mesh
	else ()
      in
	ocamlpill_from_mesh mesh
    )
;;



let _py_mesh_points =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let m = mesh_from_ocamlpill args.(0) in
      let the_det = determinant m.mm_dim in
      let (point_coords,pairs,simplices_and_cc_ic,in_body) = mesh_plotinfo m in
      let (pointcoords,surf_simplices_and_cc_ic)=mesh_extract_surface m in
      let py_surf_elements =
	pylist_fromarray
	  (Array.map
	     (fun (sx_points_indices,((cc_mid,cc_r),(ic_mid,ic_r),region)) ->
	       pytuple4(int_array_to_python sx_points_indices,
			pytuple2(float_array_to_python cc_mid,pyfloat_fromdouble cc_r),
			pytuple2(float_array_to_python ic_mid,pyfloat_fromdouble ic_r),
			pyint_fromint region
		       ))
	     surf_simplices_and_cc_ic)
      in
      let pypoints = (* XXX make this mapping a pycaml function. *)
	pylist_fromarray
	  (Array.map
	     (fun coord_array ->
	       pylist_fromarray (Array.map pyfloat_fromdouble coord_array))
	     point_coords)
      in
      let pypairs =
	pylist_fromarray
	  (Array.map
	     (fun (ix1,ix2) -> pytuple2(pyint_fromint ix1,pyint_fromint ix2))
	     pairs)
      in
      let pybody =
	pylist_fromarray
	  (Array.map
	     (fun body_ids ->
	       pylist_fromarray (Array.of_list (List.map pyint_fromint body_ids)))
	     in_body)
      in
      let pysimplices =
	pylist_fromarray
	  (Array.map
	     (fun (sx_points_indices,((cc_mid,cc_r),(ic_mid,ic_r),region)) ->
	       let mx = Array.map ( fun pt_index -> Array.append (Array.copy point_coords.(pt_index)) [|1.|] ) sx_points_indices
	       in
	       let det = the_det mx in
	       let ordered_points_indices =
		 if det < 0.
		 then
		   let re_ordered = Array.copy sx_points_indices in
		   let () = re_ordered.(0) <- sx_points_indices.(1) in
		   let () = re_ordered.(1) <- sx_points_indices.(0) in
		   re_ordered
		 else
		   sx_points_indices
	       in
	       pytuple4(int_array_to_python ordered_points_indices,
			pytuple2(float_array_to_python cc_mid,pyfloat_fromdouble cc_r),
			pytuple2(float_array_to_python ic_mid,pyfloat_fromdouble ic_r),
			pyint_fromint region
		       ))
	     simplices_and_cc_ic)
      in
      let pyvolumes = float_array_to_python m.mm_region_volumes in
      pylist_fromarray
	(Array.map
	   (fun (tag,doc,data) ->
	     pytuple3(pystring_fromstring tag,pystring_fromstring doc,data))
	   [|("COORDS","Coordinates of points",pypoints);
	     ("LINKS","Links in the mesh (pairs of point indices)",pypairs);
	     ("SIMPLICES","Simplex info (points-coords,((circumcirc center,cc radius),(ic center,ic radius),region))",pysimplices);
	     ("POINT-BODIES","Which bodies does the corresponding point belong to (body index list)",pybody);
	     ("SURFACES","Surface elements info (points-coords,((circumcirc center,cc radius),(ic center,ic radius),region))",py_surf_elements);
	     ("REGION-VOLUMES","Volume for every region",pyvolumes);
	   |]))
;;



let _py_mesh_from_points_and_simplices =
  python_pre_interfaced_function
    [| IntType;  (* dimension of the space *)
       ListType; (* points array *)
       ListType; (* simplices indices array *)
       ListType;  (* simplices_regions array *)
       ListType;  (* periodic points array *)
       py_bool_type; (* if true, reorder nodes using metis*)
       py_bool_type (* if true, set up distribution across cluster *)
     |]
    (fun args ->
      let () = reportmem "_py_mesh_from_points_and_simplices: beginning of function"
      in
      let dim = pyint_asint args.(0) in
      let nodes_arr = py_float_list_list_as_array ~error_label:"mesh_from_points_and_simplices: points"
	  ~length_inner:dim args.(1)
      in
      let simplices_indices_arr = py_int_list_list_as_array
	  ~error_label:"mesh_from_points_and_simplices: simplices"
	  ~length_inner:(dim+1) args.(2)
      in
      let simplices_regions_arr = py_int_list_as_array args.(3) in
      if (Array.length simplices_indices_arr) <> (Array.length simplices_regions_arr)
      then failwith "Number of simplices different from number of regions"
      else
	let periodic_points_arr = py_int_list_list_as_array args.(4) in
	let do_reorder = py_is_true args.(5) in
	let do_distribute = py_is_true args.(6) in
	let () = logdebug (Printf.sprintf "mesh_from_points_and_simplices #1\n") in
        let () = reportmem "_py_mesh_from_points_and_simplices: Before read_mesh" in
        let some_mesh =
	  read_mesh_from_points_and_simplices nodes_arr simplices_indices_arr simplices_regions_arr periodic_points_arr in
	let mesh =
	  match some_mesh with
	  | Some mesh -> mesh
	  | _ -> failwith "Building internal mesh data structure failed"
	in
	let () = reportmem "_py_mesh_from_points_and_simplices: Before optional reordering"  in
	let () =
	  (if do_reorder
	   then
	   let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
	     (* extract coordinates of periodic points to update mesh.mm_periodic after the reordering *)
	   let periodic_points = Array.map (fun per_ix_arr -> Array.map (fun per_ix -> points_coords.(per_ix)) per_ix_arr  ) mesh.mm_periodic_points in
	   let () = _do_reorder_mesh mesh in
	   let new_points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
	     mesh.mm_periodic_points <- Array.map ( fun per_arr -> Array.map (fun p -> array_position p new_points_coords 0 ) per_arr) periodic_points
	   else ())
	in
	let () = reportmem "_py_mesh_from_points_and_simplices: before do_distribute" in
	let () =
	  (if do_distribute then
	     _do_distribute_mesh mesh
	   else ())
	in
	let () = reportmem "_py_mesh_from_points_and_simplices: very end before returning ocamlpill" in
	  ocamlpill_from_mesh mesh)
;;

let _py_mesh_scale_node_positions =
  python_pre_interfaced_function
    ~doc:"mesh_scale_node_positions(raw_mesh, scalefloat) \nMultiply node positions of mesh (given as raw ocaml object) by float. (Works in-place, returns None)"
    [|CamlpillType;FloatType|]
    (fun args ->
      let mesh = mesh_from_ocamlpill args.(0) in
      let scaling_factor = pyfloat_asdouble args.(1) in
	begin
	  scale_node_positions mesh scaling_factor;
	  pynone()
	end
    )
;;

let _py_copy_mesher_defaults =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      let copy_mdefaults =
	copy_mesher_defaults
	  mdefaults
	  mdefaults.mdefault_meshgen_controller
	  mdefaults.mdefault_meshgen_controller_initial
      in
      ocamlpill_from_mesher_defaults_int copy_mdefaults)
;;

(* XXX Note: the _py_mdefaults_* functions are a very ugly cut&paste hack
   that takes up considerable space within this code module.
   This definitely should be abstracted a bit more!
 *)


let _py_mdefaults_set_movement_max_freedom =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_max_free_move = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_movement_max_freedom <- new_max_free_move;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_topology_threshold =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_threshold = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_topology_threshold <- new_threshold;
	  pynone()
	end
    )
;;

(*KKK*)
let _py_mdefaults_set_shape_force_scale =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_shape_force_scale <- new_scaling_factor;
	  pynone()
	end
    )
;;



let _py_mdefaults_set_volume_force_scale =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_volume_force_scale <- new_scaling_factor;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_neigh_force_scale =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_neigh_force_scale <- new_scaling_factor;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_irrel_elem_force_scale =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_irrel_elem_force_scale <- new_scaling_factor;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_thresh_add =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_thresh = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_thresh_add <- new_thresh;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_thresh_del =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_thresh = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_thresh_del <- new_thresh;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_initial_settling_steps =
  python_pre_interfaced_function
    [|CamlpillType;IntType|]
    (fun args ->
      let new_steps = pyint_asint args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_initial_settling_steps <- new_steps;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_sliver_correction =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_sliver_correction <- new_scaling_factor;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_time_step_scale =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_time_step_scale <- new_scaling_factor;
	  pynone()
	end
    )
;;


let _py_mdefaults_set_smallest_allowed_volume_ratio =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_smallest_allowed_volume_ratio = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_smallest_allowed_volume_ratio <- new_smallest_allowed_volume_ratio;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_tolerated_rel_movement =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_tolerated_rel_movement = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_tolerated_rel_movement <- new_tolerated_rel_movement;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_max_relaxation_steps =
  python_pre_interfaced_function
    [|CamlpillType;IntType|]
    (fun args ->
      let new_step_limit_max = pyint_asint args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_step_limit_max <- new_step_limit_max;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_initial_points_volume_ratio =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_ratio = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_initial_points_volume_ratio <- new_ratio;
	  pynone()
	end
    )
;;

let _py_mdefaults_set_splitting_connection_ratio =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_ratio = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_splitting_connection_ratio <- new_ratio;
	  pynone()
	end
    )
;;


let _py_mdefaults_set_exp_neigh_force_scale =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
      let new_scaling_factor = pyfloat_asdouble args.(1) in
      let mdefaults = mesher_defaults_int_from_ocamlpill args.(0) in
      if mdefaults == !opt_mesher_defaults (* Note: EQ comparison! *)
      then failwith "It is not permitted to modify the original mesher defaults. Please make a copy!"
      else
	begin
	  mdefaults.mdefault_controller_exp_neigh_force_scale <- new_scaling_factor;
	  pynone()
	end
    )
;;

let _py_mesh2d_ps =
  python_pre_interfaced_function
    [|CamlpillType;StringType;ListType|]
    (fun args ->
      let mesh = mesh_from_ocamlpill args.(0) in
      let filename = pystring_asstring args.(1) in
      let scale_params = py_float_list_as_array ~error_label:"mesh2d_ps: scale parameters" ~length:4 args.(2)
      in
      let scale xy = [|xy.(0)*.scale_params.(0)+.scale_params.(2);
		       xy.(1)*.scale_params.(1)+.scale_params.(3);
		     |]
      in
      if mesh.mm_dim <> 2 then failwith "mesh2d_ps only works for 2d meshes!"
      else
	let () = mesh2d_ps ~scale:scale mesh filename
	in pynone()
    )
;;

(* Note: this is overly primitive! We really should not do it in that way! *)
let fuse_points points1 points2 =
  let pts = Hashtbl.create 100 in
  let () = Array.iter (fun p -> Hashtbl.replace pts p true) points1 in
  let () = Array.iter (fun p -> Hashtbl.replace pts p true) points2 in
  hashtbl_keys pts
;;


let _py_make_element =
  python_pre_interfaced_function
    [|StringType;ListType;IntType;IntType|] (* name, max_indices, dimension, order *)
    (fun args ->
      let indices = py_int_list_as_array ~error_label:"make_element" args.(1) in
      let dof_name = (pystring_asstring args.(0),indices) in
      let dim = pyint_asint args.(2) in
      let ord = pyint_asint args.(3) in
      let elem = make_element dim dof_name ord in
      ocamlpill_from_element elem)
;;

let _py_empty_element = ocamlpill_from_element empty_element;;

let _py_fuse_elements =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|]
    (fun args ->
      let elem1 = element_from_ocamlpill args.(0) in
      let elem2 = element_from_ocamlpill args.(1) in
      let elem12 = fuse_elements elem1 elem2 in
      ocamlpill_from_element elem12)
;;

let _py_element_to_string =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
       let elem = element_from_ocamlpill args.(0) in
       let dofstr dofs = string_array_to_string (Array.map dof_name_to_string dofs) in
       let desc = Printf.sprintf "#<element '%s' subfields: %s dof_names %s abssites: %s>"
	 elem.el_name
	 (dofstr elem.el_subfields)
	 (dofstr elem.el_dof_names)
	 (string_array_to_string (Array.map int_array_to_string elem.el_abssites))
       in
	 pystring_fromstring desc)
;;

let _py_mwe_to_string =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
       let mwe = mwe_from_ocamlpill args.(0) in
       let desc = mwe_to_string mwe in
	 pystring_fromstring desc)
;;


let __py_element_associator py_body_elem =
  let v_py_body_and_element = pylist_toarray py_body_elem in
  let v_body_el =
    Array.map
      (fun py ->
	 if pytype py <> TupleType
	 then raise (Pycaml_exn(Pyerr_TypeError, "py_element_associator: Expected tuple argument!"))
	 else
	   let entry = pytuple_toarray py in
	   if Array.length entry <> 2
	   then raise (Pycaml_exn(Pyerr_TypeError, "py_element_associator: Expected pair tuple!"))
	   else
	     if pytype entry.(0) <> IntType
	        || pytype entry.(1) <> CamlpillType
		|| ocamlpill_type_of entry.(1) <> pysym_element
	     then raise (Pycaml_exn(Pyerr_TypeError, "py_element_associator: Bad tuple entries!"))
	     else
	       let region = pyint_asint entry.(0) in
	       let el:(float Fem.element) = (ocamlpill_hard_unwrap entry.(1)) in
		 (region,el))
      v_py_body_and_element
  in
  let fun_element_associator sx =
    let Body_Nr b = sx.ms_in_body in
    let (_,elem) =
      try array_find (fun (body_nr,el) -> body_nr=b) v_body_el
      with | Not_found -> (0,empty_element)
    in elem
  in fun_element_associator
;;

let __py_properties_by_region py_props =
  Array.map
    (fun py_pair ->
       let a = guarded_pytuple_toarray py_pair in
       let () = (if Array.length a <> 2
		 then failwith "Bad properties entry!"
		 else ())
       in
       let id = guarded_pyint_asint a.(0) in
       let props = py_string_list_as_array a.(1) in
	 (id,props)
    )
    (guarded_pylist_toarray py_props)
;;

(* Note: this will take element specifications
   as a Python list [(Body_Nr,Element),...] *)
let _py_make_mwe =
  python_pre_interfaced_function
    [|StringType;CamlpillType;ListType;ListType;ListType|]
    (* mwe_name, mesh, array of (body_nr,element),
       opt. outer-boundary-region fun,
       properties_by_region *)
    (fun args ->
      let mwe_name=pystring_asstring args.(0) in
      let mesh = mesh_from_ocamlpill args.(1) in
      let () = loginfo (Printf.sprintf "Associating elements to mesh ('%s')" mwe_name) in
      let () = reportmem mwe_name in
      let fun_element_associator =
	__py_element_associator args.(2)
      in
      let fun_outer_region =
	py_optionally
	  (fun c ->
	     fun nr_site pos -> pyint_asint (pyeval_callobject(c,(pytuple_fromsingle (float_array_to_python pos)))))
	  args.(3)
      in
      let properties_by_region = __py_properties_by_region args.(4) in
      let mwe = make_mwe mwe_name ?fun_outer_region ~properties_by_region fun_element_associator mesh in
      ocamlpill_from_mwe mwe)
;;

let _py_mwe_sibling =
  python_pre_interfaced_function
    [|CamlpillType;StringType;StringType;ListType|] (* mwe, new-name, renaming-prefix, dof-renamings *)
    (fun args ->
      let mwe = mwe_from_ocamlpill args.(0) in
      let new_name = pystring_asstring args.(1) in
      let rename_prefix = pystring_asstring args.(2) in
      let v_py_renaming_pairs = pylist_toarray args.(3) in
      let v_renaming_pairs =
	Array.mapi
	  (fun n py ->
	    let fail () = raise (Pycaml_exn (Pyerr_StandardError,Printf.sprintf "Bad dof-renaming specification arg Nr. %d" (1+n))) in
	    if pytype py <> TupleType then fail()
	    else
	      let v_tup = pytuple_toarray py in
	      if Array.length v_tup <> 2 then fail()
	      else
		if pytype v_tup.(0) <> StringType
	        ||  pytype v_tup.(1) <> StringType
		then fail ()
		else
		  (pystring_asstring v_tup.(0),
		   pystring_asstring v_tup.(1)))
	  v_py_renaming_pairs
      in
      let sibling = mwe_sibling new_name rename_prefix v_renaming_pairs mwe in
      ocamlpill_from_mwe sibling)
;;


let _py_field_alias =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|] (* field, new-mwe *)
    (fun args ->
      let (FEM_field (mwe_f,r,v_f) as field) = field_from_ocamlpill args.(0) in
      let mwe = mwe_from_ocamlpill args.(1) in
      if not(mwes_are_compatible mwe_f mwe)
      then failwith "field_alias: new mwe and field mwe are incompatible!"
      else
	ocamlpill_from_field (FEM_field (mwe,r,v_f)))
;;

let _py_cofield_alias =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|] (* cofield, new-mwe *)
    (fun args ->
      let (FEM_cofield (mwe_co,r,v_co)) = cofield_from_ocamlpill args.(0) in
      let mwe = mwe_from_ocamlpill args.(1) in
      if not(mwes_are_compatible mwe_co mwe)
      then failwith "cofield_alias: new mwe and cofield mwe are incompatible!"
      else
	ocamlpill_from_cofield (FEM_cofield (mwe,r,v_co)))
;;

let _py_field_cofield_inner_product =
  python_pre_interfaced_function
    [|CamlpillType;
      CamlpillType;
    |]
    (fun args ->
       let field = field_from_ocamlpill args.(0) in
       let cofield = cofield_from_ocamlpill args.(1) in
       let result = field_cofield_inner_product field cofield in
	 pyfloat_fromdouble result)
;;

let __mwe_from_mwe_or_field_or_cofield py_x =
  let ty_x = ocamlpill_type_of py_x in
    if ty_x = pysym_mwe then
      ocamlpill_hard_unwrap py_x
    else if ty_x = pysym_field then
      let (FEM_field (mwe,_,_)) = ocamlpill_hard_unwrap py_x in
	mwe
    else if ty_x = pysym_cofield then
      let (FEM_cofield (mwe,_,_)) = ocamlpill_hard_unwrap py_x in
      mwe
    else failwith (Printf.sprintf "Wanted mwe - got bad pill type: '%s'" ty_x)
;;

let _py_get_mwe =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let x = args.(0) in
      let mwe = __mwe_from_mwe_or_field_or_cofield x in
      ocamlpill_from_mwe mwe)
;;

let _py_mwe_norm_fun =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
       let mwe = mwe_from_ocamlpill args.(0) in
       let norm_fun = fun_tensor_norms mwe in
	 python_interfaced_function
	   [|CamlpillType|]
	   (fun args ->
	      let field = field_from_ocamlpill args.(0) in
	      let norms = norm_fun field in
	      let py_norms =
		pylist_fromarray
		  (Array.map
		     (fun (stem,norm1,norm2) ->
			pytuple3(pystring_fromstring stem,
				 pyfloat_fromdouble norm1,
				 pyfloat_fromdouble norm2))
		     norms)
	      in py_norms))
;;

(* Note: this only handles full vectors! *)
let _py_mwe_norm_dist_fun =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
       let mwe = mwe_from_ocamlpill args.(0) in
       let v_delta = Mpi_petsc.vector_create (Array.length mwe.mwe_dofs) (gensym "v_norm_dist_") in
       let field_delta = FEM_field(mwe,None,v_delta) in
       let () = Mpi_petsc.vector_assemble v_delta in
       let norm_fun = fun_tensor_norms mwe in
	 python_interfaced_function
	   [|CamlpillType;CamlpillType|]
	   (fun args ->
	      let field_1 = field_from_ocamlpill args.(0) in
	      let field_2 = field_from_ocamlpill args.(1) in
	      let bailout () = failwith "Problem: invalid fields passed to norm-of-difference function!" in
	      let norms =
		match (field_1,field_2) with
		  | (FEM_field (mwe1,None,v1),FEM_field (mwe2,None,v2)) ->
		      if not(mwes_are_compatible mwe1 mwe2 && mwes_are_compatible mwe mwe1) then bailout()
		      else
			begin
			  Mpi_petsc.vector_copy v1 v_delta;
			  Mpi_petsc.vector_AXPBY (-1.0) 1.0 v2 v_delta;
			  norm_fun field_delta
			end
		  | _ -> bailout()
	      in
	      let py_norms =
		pylist_fromarray
		  (Array.map
		     (fun (stem,norm1,norm2) ->
			pytuple3(pystring_fromstring stem,
				 pyfloat_fromdouble norm1,
				 pyfloat_fromdouble norm2))
		     norms)
	      in py_norms))
;;

let _py_tensor_entry py_tensor coords =
  let nr_coords = Array.length coords in
  let rec walk_tensor tensor_todo pos =
    if pos=nr_coords then tensor_todo
    else
      if pytype tensor_todo <> ListType
      then failwith
	(Printf.sprintf "Python tensor of too low rank. Expected: %d, have: %d" nr_coords pos)
      else
	let len_here = pysequence_length tensor_todo in
	  if coords.(pos) >= len_here
	  then failwith
	    (Printf.sprintf "Python tensor does not have entry %s: %d. index range is %d, wanted index: %d"
	       (int_array_to_string coords)
	       (1+pos) len_here
	       coords.(pos))
	  else
	    walk_tensor (pylist_get tensor_todo coords.(pos)) (1+pos)
  in walk_tensor py_tensor 0
;;

let want_unrestricted_field field =
  let FEM_field(_, restr, _) = field in
    (if restr <> None
     then failwith "NOTE: ocaml.set_field_at_site for now only works on \
                    unrestricted fields. XXX TODO."
     else ())
;;

let _py_set_field_at_site =
  python_pre_interfaced_function
    ~doc:"set_field_at_site(field_pill, site, subfield_name, data_tensor)\n\n
  Note: 1. if subfield_name is wrong, nothing will happen (no error, no change of data).
        2. if data_tensor is 'shorter' than actual data in subfield, we get an error message
           (something like

             OCaml exception 'Failure: Python tensor does not have entry [|2|]:
                                       1. index range is 2, wanted index: 2'
                                       (set_field_at_site)
           )
        3. if data_tensor is too long, the 'extra' components will be silently ignored
        4. if the site is wrong (i.e. doesn't exist in the mesh), nothing will happen
           (no error, no data change).

  Need to catch all these things at the Python level.
        "
    [|CamlpillType;ListType;StringType;ListType|]
    (fun args ->
       let FEM_field(mwe, restr, v_data) as field =
         field_from_ocamlpill args.(0) in
       let () = want_unrestricted_field field in
       let site = py_int_list_as_array args.(1) in
       let subfield_name = pystring_asstring args.(2) in
       let py_data = (pylist_toarray args.(3)).(0) in
       let status = ref "unset" in
       let dofs_at_site =
         try Hashtbl.find mwe.mwe_dofs_by_site site with
         | Not_found -> let () = status := "not found" in [||] in
       let () = Mpi_petsc.with_petsc_vector_as_bigarray v_data
	 (fun ba_data ->
	    Array.iter
	      (fun dof ->
		 let (dof_stem,indices) = the_dof_name mwe dof in
		   if dof_stem <> subfield_name
		   then ()
		   else
		     let data_entry = _py_tensor_entry py_data indices in
		     let v =
		       match pytype data_entry with
			 | FloatType -> pyfloat_asdouble data_entry
			 | IntType -> float_of_int(pyint_asint data_entry)
			 | _ -> failwith
			     (Printf.sprintf "Bad tensor entry for coordinates %s, expected number, got: %s"
				(int_array_to_string indices) (py_repr data_entry))
		     in
                     let () = status := "set" in
		       ba_data.{dof.dof_nr} <- v) dofs_at_site)
       in pystring_fromstring !status)
;;

let _py_set_field_uniformly =
  python_pre_interfaced_function
    ~doc:"set_field_uniformly (field_pill, subfield_name, data_tensor)\n\n\
          Similar to set_field_at_site, but does set the field with the same \
          value at all the sites where the field is defined."
    [|CamlpillType; StringType; ListType|]
    (fun args ->
       let field = field_from_ocamlpill args.(0) in
       let () = want_unrestricted_field field in
       let subfield_name = pystring_asstring args.(1) in
       let pydata = args.(2) in
       let ba = pylist_ftensor_to_ba pydata in
         try
           if set_field_uniformly subfield_name field ba
           then pyint_fromint 0 (* some components were set *)
           else pyint_fromint 1 (* the field was not set at all *)
         with _ ->
           pyint_fromint 1)
;;

let _py_mwe_subfield_metadata =
  python_pre_interfaced_function
    ~doc:"mwe_subfield_metadata(field_pill,subfield_name) -> ([site_ids],[site_positions],max_indices,site_volumes)"
    [|CamlpillType;StringType|]
    (fun args ->
       let mwe = __mwe_from_mwe_or_field_or_cofield args.(0) in
       let subfield_name = pystring_asstring args.(1) in
       let ((_,subfield_indices),sites_and_dofs) = mwe_subfield_info mwe subfield_name in
       let site_ids = Array.map (fun (site_id,_,_) -> site_id) sites_and_dofs in
       let site_posns = Array.map (fun (_,site_pos,_) -> site_pos) sites_and_dofs in
       let site_vols = Array.map (fun (_,_,dofs) -> dofs.(0).dof_volume) sites_and_dofs in
	 pytuple4(pylist_fromarray (Array.map int_array_to_python site_ids),
		  pylist_fromarray (Array.map float_array_to_python site_posns),
		  int_array_to_python subfield_indices,
		  float_array_to_python site_vols))
;;

let _py_mwe_subfield_data =
  python_pre_interfaced_function
    ~doc:"mwe_subfield_meta(field_pill,subfield_name) -> multi-array: a[site_id][index0][index1][...][indexn] -> dof value"
    [|CamlpillType;StringType|]
    (fun args ->
       let FEM_field (mwe,_,v_data) as field = field_from_ocamlpill args.(0) in
       let () = ensure_field_is_unrestricted field in (* XXX *)
       let subfield_name = pystring_asstring args.(1) in
       let ((_,subfield_indices),sites_and_dofs) = mwe_subfield_info mwe subfield_name in
       let nr_sites = Array.length sites_and_dofs in
       let rank = Array.length subfield_indices in
       let index_weights =
	 let w = Array.make rank 1 in
	 let rec walk pos =
	   if pos<0 then w
	   else
	     let () = w.(pos) <- w.(pos+1) * subfield_indices.(pos+1) in
	       walk (pos-1)
	 in walk (rank-2)
       in
       let linear_offset v_indices =
	 let rec walk pos ix =
	   if pos = rank then ix
	   else walk (1+pos) (ix+index_weights.(pos)*v_indices.(pos))
	 in
	   walk 0 0
       in
       let r_result = ref py_true in
       let () = Mpi_petsc.with_petsc_vector_as_bigarray v_data
	 (fun ba_data ->
            let pytensor, _ =
              fast_py_float_tensor
                ~init:(fun indices ->
                       let nr_site = indices.(0) in
                       let (_,_,dofs) = sites_and_dofs.(nr_site) in
                       let v_indices =
                         Array.sub indices 1 ((Array.length indices)-1) in
                       let ix = linear_offset v_indices in
                       let petsc_ix = dofs.(ix).dof_nr in
                         ba_data.{petsc_ix})
                (Array.concat [[|nr_sites|]; subfield_indices])
            in
              r_result := pytensor)
       in !r_result)
;;

(* XXX MISNOMER! SHOULD BE: py_mwe_field_nr_entries! FIXME!
   Similarly for functions below!
*)
let _py_data_length =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let x = args.(0) in
      let mwe = __mwe_from_mwe_or_field_or_cofield x in
      pyint_fromint (Array.length mwe.mwe_dofs))
;;

let _py_data_doftypes =
  python_pre_interfaced_function
   ~doc:"Given a mwe or field or cofield, this returns lots of data (to be detailed)"
    [|CamlpillType|]
    (fun args ->
      let mwe = __mwe_from_mwe_or_field_or_cofield args.(0) in
      pylist_fromarray
	(Array.map (fun (stem,indices) ->
	  pytuple2(pystring_fromstring stem,
		   int_array_to_python indices))
	   mwe.mwe_subfields))
;;


let _py_data_dofvector_from_field =
  python_pre_interfaced_function
   ~doc:"Given a mwe or field or cofield and the name of a dof, this returns all the data associated with that dof (positions, data). Unfinished; not working currently. Hans, Nov 2006"
    [|CamlpillType;StringType|]
    (fun args ->
      let mwe = __mwe_from_mwe_or_field_or_cofield args.(0) in
      let dofname = pystring_asstring args.(1) in
      let mwedofs=mwe.mwe_dofs in
      pytuple2 (pylist_fromarray [|pynone()|] ,pylist_fromarray [|pynone()|])
    )
;;


let _py_field_entry_wise =
  python_pre_interfaced_function
    [|CamlpillType;CallableType|]
    (fun args ->
      let x = args.(0) in
      let ty_x = ocamlpill_type_of x in
      let (mwe,stl,v) =
	if ty_x = pysym_field then
	  let (FEM_field (mwe,restr,v) as field) = ocamlpill_hard_unwrap x in
	  let (_,stl,_) = mwe_shortvec_info mwe restr in
	  (mwe,stl,v)
	else if ty_x = pysym_cofield then
	  let (FEM_cofield (mwe,restr,v) as cofield) = ocamlpill_hard_unwrap x in
	  let (_,stl,_) = mwe_shortvec_info mwe restr in
	  (mwe,stl,v)
	else raise (Py_Exn (Printf.sprintf "py_field_entry_wise: got pill of bad type: %s" ty_x))
      in
      let nr_dofs = Array.length stl in
      let () = Mpi_petsc.with_petsc_vector_as_bigarray v
	  (fun v_ba ->
	    for i=0 to nr_dofs-1 do
	      let dof = mwe.mwe_dofs.(stl.(i)) in
	      let entry = v_ba.{i} in
	      let (dof_name_stem,dof_indices) = the_dof_name mwe dof in
	      let py_dof_pos = float_array_to_python dof.dof_pos in
	      let py_dof_site = int_array_to_python dof.dof_site in
	      let callback_args =
		pytuple_fromarray
		  [|pyint_fromint i;
		    pytuple2(pystring_fromstring dof_name_stem,int_array_to_python dof_indices);
		    py_dof_site;
		    py_dof_pos;
		    pyfloat_fromdouble entry
		  |]
	      in
	      let _ = pyeval_callobject(args.(1),callback_args) in ()
	    done;
	  )
      in pynone())
;;



(* mf: I'm not sure this definition is not redundant. But I need this function
       to do the hysteresis loop.
 *)
let _py_set_field =
  python_pre_interfaced_function
    [|CamlpillType;CallableType|]
    (* field, function used to set it *)
    (fun args ->
       let (FEM_field (mwe,_, _) as field) = field_from_ocamlpill args.(0) in
       let () = ensure_field_is_unrestricted field in (* XXX *)
       let nr_dof = Array.length mwe.mwe_dofs in
       let f_py_sample = pycallable_asfun args.(1) in
       let fun_sampling (dof_name,dof_indices) dof =
         let dof_pos = dof.dof_pos in
         let py_sampled_val =
           f_py_sample
             [|pytuple2(pystring_fromstring dof_name,
                        pylist_fromarray
                          (Array.map
                             pyint_fromint
                             dof_indices));
               pylist_fromarray
                 (Array.map
                    pyfloat_fromdouble
                    dof_pos)
             |]
         in
           if pytype py_sampled_val <> FloatType
           then 0.0
           else pyfloat_asdouble py_sampled_val
       in
       let () = set_field field fun_sampling
       in
         pynone())
;;


(* Actually, sampling co-fields maybe does not make that much sense... *)
let _py_raw_make_field_or_cofield do_field =
  python_pre_interfaced_function
    [|CamlpillType;ListType;StringType;StringType|]
    (* mwe,opt_sampling function, field_restriction_string or "", petsc name or "" *)
    (fun args ->
       let mwe = mwe_from_ocamlpill args.(0) in
       let petsc_name =
	 let s = pystring_asstring args.(3) in
	   if s = "" then None
	   else Some s
       in
       let opt_field_restriction =
	 let s = pystring_asstring args.(2) in
	   if s = "" then None
	   else
	     parse_or_error Ddiffop_lexer.token
	       Ddiffop_parser.short_vector_restriction s
       in
       let (dof_mapping_l_to_s,dof_mapping_s_to_l,distrib_info) = mwe_shortvec_info mwe opt_field_restriction in
       let nr_dof = Array.length dof_mapping_s_to_l in
       let opt_callable = py_optionally pycallable_asfun args.(1) in
       let v_petsc =
	 match opt_callable with
	   | None ->
	       Mpi_petsc.vector_pack (Array.make nr_dof 0.0) (* XXX improve! *)
	   | Some f_py_sample ->
	       let fun_sampling (dof_name,dof_indices) dof =
		 let dof_pos = dof.dof_pos in
		 let py_sample_args =
		     [|pytuple2(pystring_fromstring dof_name,
				pylist_fromarray
				  (Array.map
				     pyint_fromint
				     dof_indices));
		       pylist_fromarray
			 (Array.map
			    pyfloat_fromdouble
			    dof_pos)
		     |]
		       (* XXX Insanely consing-excessive! *)
		 in
		 let py_sampled_val = f_py_sample py_sample_args
		 in
		 let sampled_type = pytype py_sampled_val in
		   match sampled_type with
		     | FloatType -> pyfloat_asdouble py_sampled_val
		     | IntType -> float_of_int (pyint_asint py_sampled_val)
		     | _ -> failwith "Python field/cofield sampling function returned non-number!"
	       in (* XXX Note that we do not use sample_field here, but sample directly! *)
		 Mpi_petsc.vector_pack
		   (Array.init nr_dof
		      (fun n ->
			 let dof = mwe.mwe_dofs.(dof_mapping_s_to_l.(n)) in
			 let dof_name = the_dof_name mwe dof in
			   fun_sampling dof_name dof))
       in
	 if do_field
	 then
	   ocamlpill_from_field (FEM_field (mwe,opt_field_restriction,v_petsc))
	 else
	   ocamlpill_from_cofield (FEM_cofield (mwe,opt_field_restriction,v_petsc)))
;;

(* XXX This has to be re-worked, allowing an optional target argument as well!
   NOTE: also need a "sloppy" cofield->field mapping!
*)
let _py_cofield_to_field =
  python_pre_interfaced_function
    [|ListType;CamlpillType;py_bool_type|]
    (fun args ->
       let opt_target_field = py_optionally field_from_ocamlpill args.(0) in
       let cofield = cofield_from_ocamlpill args.(1) in
       let box_method = py_is_true args.(2) in
       let field_result = cofield_to_field ?target:opt_target_field ~box_method cofield in
	 ocamlpill_from_field field_result)
;;

let _py_field_to_cofield =
  python_pre_interfaced_function
    [|ListType;CamlpillType;py_bool_type|]
    (fun args ->
       let box_method = py_is_true args.(2) in
       let field = field_from_ocamlpill args.(1) in
       let opt_target_cofield = py_optionally cofield_from_ocamlpill args.(0) in
       let cofield_result =
	 field_to_cofield ?target:opt_target_cofield ~box_method field
       in
	 ocamlpill_from_cofield cofield_result)
;;

let _py_field_push =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|]
    (fun args ->
       let field_src = field_from_ocamlpill args.(0) in
       let field_dst = field_from_ocamlpill args.(1) in
       let () = field_push field_src field_dst in
	 pynone())
;;

let _py_field_pull =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|]
    (fun args ->
       let field_src = field_from_ocamlpill args.(0) in
       let field_dst = field_from_ocamlpill args.(1) in
       let () = field_pull field_src field_dst in
	 pynone())
;;

let _py_cofield_push =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|]
    (fun args ->
       let field_src = cofield_from_ocamlpill args.(0) in
       let field_dst = cofield_from_ocamlpill args.(1) in
       let () = cofield_push field_src field_dst in
	 pynone())
;;

let _py_cofield_pull =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|]
    (fun args ->
       let field_src = cofield_from_ocamlpill args.(0) in
       let field_dst = cofield_from_ocamlpill args.(1) in
       let () = cofield_pull field_src field_dst in
	 pynone())
;;


(* Note: the applicator has args (int,field,field)
   if int<>0, then the first field is used as a target field.
   Result is returned as a field.
 *)

let __parse_if_coeffs ifc =
  let if_coeffs_1 = pylist_toarray ifc in
  Array.mapi
    (fun nr_p p ->
      if pytype p <> TupleType
      then raise (Pycaml_exn(Pyerr_TypeError,
			     Printf.sprintf "interface coeffs list: entry %d is not a tuple!"
			       (1+nr_p)))
      else
	let triplet = pytuple_toarray p in
	if    Array.length triplet <> 3
           || pytype triplet.(0) <> IntType
	   || pytype triplet.(1) <> IntType
	   || pytype triplet.(2) <> FloatType
	then
	  raise (Pycaml_exn(Pyerr_TypeError,
			    Printf.sprintf "interface coeffs list: entry %d is not a (int,int,float) triplet!"
			      (1+nr_p)))
	else
	  let n1 = pyint_asint triplet.(0)
	  and n2 = pyint_asint triplet.(1)
	  and coeff = pyfloat_asdouble triplet.(2)
	  in
	    ((if n1=(-2) then None else Some (Mesh.Body_Nr n1)),
	     (if n2=(-2) then None else Some (Mesh.Body_Nr n2)),
	     coeff))
    if_coeffs_1
;;


(* The color scheme also allows integers,
   so that "red" alternatively may be specified as [1,0,0]
   instead of having to be given as [1.0,0.0,0.0].
*)
let _py_plot_scalar_field =
  let translate_color_scheme csc =
    let contribs =
      Array.mapi
	(fun n entry ->
	   if pytype entry <> TupleType
	   then raise (Pycaml_exn (Pyerr_TypeError,Printf.sprintf "Color scheme entry #%d: expected tuple!" n))
	   else
	     let py_x_col = pytuple_toarray entry in
	       if    Array.length py_x_col <> 2
		 || pytype py_x_col.(0) <> FloatType
		 || pytype py_x_col.(1) <> ListType
	       then raise (Pycaml_exn (Pyerr_TypeError,Printf.sprintf "Color scheme entry #%d: Bad tuple!" n))
	       else
		 let x = pyfloat_asdouble py_x_col.(0)
		 and py_col = pylist_toarray py_x_col.(1) in
		   if Array.length py_col <> 3
		     || not (array_all_satisfy
			       (fun c ->
				  (pytype c = FloatType &&
				      (let cf = pyfloat_asdouble c in
					 cf >= 0.0 && cf <= 1.0))
				|| (pytype c = IntType &&
				    (let ci = pyint_asint c in
				       ci=0 || ci=1)))
			       py_col)
		   then raise (Pycaml_exn (Pyerr_TypeError,Printf.sprintf "Color scheme entry #%d: Bad color list!" n))
		   else
		     let col =
		       Array.map
			 (fun x -> if pytype x = FloatType
			  then pyfloat_asdouble x
			  else float_of_int (pyint_asint x))
			 py_col
		     in
		       (x,col))
	csc
    in contribs
  in
    python_pre_interfaced_function
      [|CamlpillType;  (* Field *)
	StringType; (* DOF Name *)
	StringType; (* Filename *)
	ListType;   (* Color Scheme: [(x0,[r,g,b])] *)
	IntType;    (* Plot Order *)
	py_bool_type; (* Plot Edges? *)
	ListType;   (* origin + scale *)
      |]
      (fun args ->
	 let field = field_from_ocamlpill args.(0) in
	 let geom =
	   py_float_list_as_array
	     ~error_label:"_py_plot_scalar_field: geometry specification vector"
	     ~length:4
	     args.(6)
	 in
	 let scale_fun coords =
	   [|(coords.(0)-.geom.(0))*.geom.(2);
	     (-.coords.(1)+.geom.(1))*.geom.(3);|]
	 in
	 let color_scheme = translate_color_scheme (pylist_toarray args.(3))
	 and plot_order=pyint_asint args.(4)
	 and plot_edges = py_is_true args.(5)
	 and filename=pystring_asstring args.(2)
	 and dof_name=(pystring_asstring args.(1),[||])
	 in
	 let () =
	   mesh2d_plot_scalar_field
	     ~scale:scale_fun
	     ~plot_order:plot_order
	     ~plot_edges:plot_edges
	     dof_name field color_scheme
	     filename
	 in
	   pynone ())
;;



let _py_integrate_field =
  python_pre_interfaced_function
    [|CamlpillType;StringType|] (* If we pass the empty string, this will mean "any dof" *)
    (fun args ->
       let ((FEM_field (mwe,_,_)) as field) = field_from_ocamlpill args.(0) in
       let () = ensure_field_is_unrestricted field in
       let subfields = mwe.mwe_subfields in
       let ht_result_tensors_and_setters =
	 Hashtbl.create (Array.length subfields)
       in
       let dof_stem = pystring_asstring args.(1) in
       let opt_dof_stem = if dof_stem="" then None else Some dof_stem in
       let integrated = integrate_field ?dof_stem:opt_dof_stem field in
       let () = Array.iter
	 (fun ((dof_stem,indices),v) ->
	    if Array.length indices = 0
	    then
	      Hashtbl.add ht_result_tensors_and_setters
		dof_stem ((pyfloat_fromdouble v),fun _ _ -> ())
	    else
	      let (entry,setter) =
		try Hashtbl.find ht_result_tensors_and_setters dof_stem
		with | Not_found ->
		  let (_,max_indices) = array_find (fun (s,_) -> s=dof_stem) subfields in
		  let (pytensor,setter) as ts = py_float_tensor max_indices in
		  let () = Hashtbl.add ht_result_tensors_and_setters dof_stem ts in
		    ts
	      in setter indices (pyfloat_fromdouble v))
	 integrated
       in
       let ml_result =
	 map_hashtbl_to_array
	   ~sorter:(fun (k1,_) (k2,_) -> compare k1 k2)
	   (fun key (py_result,_) -> (key,py_result))
	   ht_result_tensors_and_setters
       in
	 pylist_fromarray
	   (Array.map
	      (fun (k,py) -> pytuple2(pystring_fromstring k,py))
	      ml_result))
;;

(* XXX Note: this is quite unnecessarily slow,
   as we have to compute the subfields
   whenever we perform a single probe!
*)
let _py_probe_field =
  python_pre_interfaced_function
  ~doc:"Input arguments: CamlpillField; Dofname; position (list of floats). Returns the value of the dof at that position (using interpolation). "
    [|CamlpillType;StringType;ListType|]
    (fun args ->
       let ((FEM_field (mwe,_,_)) as field) = field_from_ocamlpill args.(0) in
       let () = ensure_field_is_unrestricted field in (* XXX *)
       let position = py_float_list_as_array ~error_label:"probe_field arg #3 (position)" args.(2) in
       let ht_result_tensors_and_setters = Hashtbl.create 5 in
       let dof_stem = pystring_asstring args.(1) in
       let opt_dof_stem = if dof_stem="" then None else Some dof_stem in
       let probed = probe_field ?dof_stem:opt_dof_stem field position in
       let () = Array.iter
	 (fun ((dof_stem,indices),v) ->
	    if Array.length indices = 0
	    then
	      Hashtbl.add ht_result_tensors_and_setters
		dof_stem ((pyfloat_fromdouble v),fun _ _ -> ())
	    else
	      let (entry,setter) =
		try Hashtbl.find ht_result_tensors_and_setters dof_stem
		with | Not_found ->
		  let (_,max_indices) = array_find (fun (s,_) -> s=dof_stem) mwe.mwe_subfields in
		  let (pytensor,setter) as ts = py_float_tensor max_indices in
		  let () = Hashtbl.add ht_result_tensors_and_setters dof_stem ts in
		    ts
	      in setter indices (pyfloat_fromdouble v))
	 probed
       in
       let ml_result =
	 map_hashtbl_to_array
	   ~sorter:(fun (k1,_) (k2,_) -> compare k1 k2)
	   (fun key (py_result,_) -> (key,py_result))
	   ht_result_tensors_and_setters
       in
	 pylist_fromarray
	   (Array.map
	      (fun (k,py) -> pytuple2(pystring_fromstring k,py))
	      ml_result))

(* XXX NOTE: it would be gorgeous if we could just start the field_scope from within python
   like this, but unfortunately, pycaml does not (yet?) cooperate too well with ocaml threads.

   The effect is: in native-code pyfem, the sheer presence of ddd_field_scope
   will make our system crash, while if we start in interpreted ocaml as follows:

export PYTHONPATH=/home/tf/ocaml/interface:$PYTHONPATH
ocaml
#use "topfind";;
#thread;;
#require "pyfem";;
Pycaml.python();;
execfile(<example>)

   then it "sort of" works - but gtk will only react interactively
   as soon as we quit the python interpreter.

   Therefore, as it stands, there is no way to reconcile
   the python interface with the graphical field scope.
   (There is an alternative ocaml<->python interface
   which is by far not as extensive as pycaml, but which claims to
   also handle threads well. But migrating for now is not an option...)

   So, we will at present not pursue this any further. What we actually may do
   would be to get a "demonstration system" working which is ocaml-bytecode based
   and temporarily leaves python whenever the field scope is to be used.

   Somewhat sad, isn't that?

let _py_ddd_field_scope =
  python_pre_interfaced_function
    [|CamlpillType;|] (* the M field *)
    (fun args ->
       ocamlpill_applicator_for_field args.(0)
	 (fun field_M ->
	    let () = Field_scope.interactive_field_scope [|field_M|] in
	      pynone()))
;;
*)

(* This does virtually everything that can be done to fields in terms of
   simple linear algebra... We may want to provide faster more specialized
   variants, but for now, there is little reason why set_field or copy_field
   should not be implemented on top of this as well...
*)

let __for_field_or_cofield_vector want_field pill fun_mwe_vec =
  if want_field
  then
    let (FEM_field (mwe,_,v) as field) = field_from_ocamlpill pill in
    let () = ensure_field_is_unrestricted field in (* XXX *)
      fun_mwe_vec mwe v
  else
    let (FEM_cofield (mwe,_,v) as cofield) = cofield_from_ocamlpill pill in
    let () = ensure_cofield_is_unrestricted cofield in (* XXX *)
      fun_mwe_vec mwe v
;;

let _py_fcfield_zero fc =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
       __for_field_or_cofield_vector fc args.(0)
	 (fun mwe v ->
	    let () = Mpi_petsc.vector_scale v 0.0 in
	      pynone()))
;;

let _py_fcfield_scale fc =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
       __for_field_or_cofield_vector fc args.(0)
	 (fun mwe v ->
	    let () = Mpi_petsc.vector_scale v (pyfloat_asdouble args.(1)) in
	      pynone()))
;;

let _py_fcfield_copy_into fc =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType|]
    (fun args ->
       __for_field_or_cofield_vector fc args.(0)
	 (fun mwe_src v_src ->
	    __for_field_or_cofield_vector fc args.(1)
	      (fun mwe_dst v_dst ->
		 if not(mwes_are_compatible mwe_src mwe_dst)
		 then raise (Pycaml_exn(Pyerr_StandardError,"Copying (co)field data requires compatible mwes!"))
		 else
		   let () = Mpi_petsc.vector_copy v_src v_dst in
		     pynone())))
;;

let _py_fcfield_axpby fc =
  python_pre_interfaced_function
    [|FloatType;FloatType;CamlpillType;CamlpillType|]
    (fun args ->
       __for_field_or_cofield_vector fc args.(2)
	 (fun mwe_src v_src ->
	    __for_field_or_cofield_vector fc args.(3)
	      (fun mwe_dst v_dst ->
		 if not(mwes_are_compatible mwe_src mwe_dst)
		 then failwith "Performing Y=a*X+b*Y on (co)field data requires compatible mwes!"
		 else
		   let () =
		     Mpi_petsc.vector_AXPBY
		       (pyfloat_asdouble args.(0))
		       (pyfloat_asdouble args.(1))
		       v_src v_dst
		   in
		     pynone())))
;;

let _py_site_wise_applicator =
  python_pre_interfaced_function
    [|ListType;	(* all the field mwes *)
      ListType;	(* all the cofield mwes *)
      ListType;	(* Names of auxiliary arguments *)
      StringType; (* The C code *)
      py_bool_type; (* Be strict with MWE checks? *)
    |]
    (fun args ->
       let strict_mwe_check = py_is_true args.(4) in
       let py_field_mwes = pylist_toarray args.(0) in
       let () = Array.iteri
	 (fun nr_field f ->
	    if not(
		    pytype f = CamlpillType && ocamlpill_type_of f = pysym_mwe
		  ) then
	      raise (Pycaml_exn(Pyerr_TypeError,(Printf.sprintf "site_wise_applicator field-mwe argument #%d: Not a mwe!" nr_field)))
	    else ())
	 py_field_mwes in
       let field_mwes =
	 Array.map ocamlpill_hard_unwrap py_field_mwes
       in
	  let py_cofield_mwes = pylist_toarray args.(1) in
	  let () = Array.iteri
	    (fun nr_cofield f ->
	       if not(
		       pytype f = CamlpillType && ocamlpill_type_of f = pysym_mwe
		     ) then
		 raise (Pycaml_exn(Pyerr_TypeError,(Printf.sprintf "site_wise_applicator cofield-mwe argument #%d: Not a mwe!" nr_cofield)))
	       else ())
	    py_cofield_mwes in
	  let cofield_mwes =
	    Array.map ocamlpill_hard_unwrap py_cofield_mwes
	  in
	  let aux_args = Array.map
	    (fun a -> if pytype a <> StringType
	     then raise (Pycaml_exn(Pyerr_StandardError,"site_wise_applicator: Bad aux args names!"))
	     else pystring_asstring a) (pylist_toarray args.(2))
	  in
	  let nr_aux_args = Array.length aux_args in
	  let c_code = pystring_asstring args.(3) in
	  let applicator =
	    site_wise_execute ~strict_mwe_check field_mwes cofield_mwes aux_args c_code
	  in
	  let py_applicator =
	    python_interfaced_function
	      ~name:"ANON_site_wise_applicator"
	      [|ListType;ListType;ListType|]
	      (fun args ->
		 let aux_args =
		   py_float_list_as_array
		     ~length:nr_aux_args
		     ~error_label:"Site-wise applicator " args.(0)
		 in
		 let v_fields =
		   py_homogeneous_list_as_array
		     ~error_label:"site_wise_applicator"
		     "FEM-field"
		     (fun x -> pytype x = CamlpillType && ocamlpill_type_of x = pysym_field)
		     (ocamlpill_hard_unwrap:(pyobject -> 'a fem_field))
		     args.(1)
		 in
		 let v_cofields =
		   py_homogeneous_list_as_array
		     ~error_label:"site_wise_applicator"
		     "FEM-cofield"
		     (fun x -> pytype x = CamlpillType && ocamlpill_type_of x = pysym_cofield)
		     (ocamlpill_hard_unwrap:(pyobject -> 'a fem_cofield))
		     args.(2)
		 in
		 let result = applicator aux_args v_fields v_cofields in
		   if result
		   then float_array_to_python aux_args
		     (* We may be interested in these values as they might have been
			modified by C code *)
		   else (* C code execution stopped somewhere *)
		     py_false)
	  in
	    py_applicator)
;;

let _py_finer_mesh_from_coarse_mesh =
  python_pre_interfaced_function
    [|IntType;	(* Refinement level *)
      CamlpillType; (* Coarse mesh *)
    |]
    (fun args ->
       let coarse_mesh = mesh_from_ocamlpill args.(1) in
       let refinement = mesh_simplex_refinement coarse_mesh.mm_dim (pyint_asint args.(0))
       in
       let (fine_mesh,point_info,simplex_info) =
	 finer_mesh_from_coarse_mesh refinement coarse_mesh
       in
	 pytuple2(ocamlpill_from_mesh fine_mesh,ocamlpill_from_mesh_fca (point_info,simplex_info)))
;;


let _py_make_coarse_fine_mwe =
  (* returns: [coarse_mwe,fine_mwe,fun_fine_to_coarse,fun_coarse_to_fine] *)
  python_pre_interfaced_function
    [|CamlpillType; (* The coarse mesh *)
      CamlpillType; (* The fine mesh *)
      StringType;StringType; (* coarse and fine mwe names *)
      ListType;	(* Array of (body_nr,element) *)
      CamlpillType; (* opaque fine simplex index -> coarse simplex index array *)
      StringType; (* Petsc Name or "" for default *)
    |]
    (fun args ->
       let py_coarse_mesh = args.(0)
       and py_fine_mesh = args.(1)
       and coarse_name = pystring_asstring args.(2)
       and fine_name = pystring_asstring args.(3)
       and py_body_nr_elem = args.(4)
       and py_mesh_fca = args.(5)
       and opt_petsc_name =
	    let s = pystring_asstring args.(6) in
	      if s = "" then None else Some s
       in
       let fun_elem_associator = __py_element_associator py_body_nr_elem in
       let coarse_mesh = mesh_from_ocamlpill py_coarse_mesh in
       let fine_mesh = mesh_from_ocamlpill py_fine_mesh in
       let (point_info,simplex_info) = mesh_fca_from_ocamlpill py_mesh_fca in
       let (coarse_mwe,fine_mwe,
	    fun_fine_field_to_coarse_cofield,
	    fun_coarse_field_to_fine_cofield)
	   =
	 make_coarse_fine_mwe
	   ?petsc_name:opt_petsc_name
	   (coarse_name,fine_name)
	   fun_elem_associator
	   (coarse_mesh,fine_mesh)
	   simplex_info
       in
       let pywrap_field_translator f =
	 python_interfaced_function
	   ~name:"ANON_coarse_fine_field_translator"
	   [|ListType; (* opt. target cofield *)
	     StringType; (* petsc name or "" *)
	     CamlpillType; (* source field (fine) *)
	   |]
	   (fun args ->
	      let opt_petsc_name =
		let s = pystring_asstring args.(1) in
		  if s = "" then None else Some s
	      in
	      let opt_target = py_optionally cofield_from_ocamlpill args.(0) in
	      let field = field_from_ocamlpill args.(2) in
	      let result=
		f ?petsc_name:opt_petsc_name
		  ?target:opt_target
		  field
	      in
		ocamlpill_from_cofield result)
       in
	 pytuple_fromarray
	   [|ocamlpill_from_mwe coarse_mwe;
	     ocamlpill_from_mwe fine_mwe;
	     pywrap_field_translator fun_fine_field_to_coarse_cofield;
	     pywrap_field_translator fun_coarse_field_to_fine_cofield
	   |]
    )
;;

let _py_lam_get_ccpla_resources =
  python_pre_interfaced_function
    [|CamlpillType; (* the linalg_machine *)
    |]
    (fun args ->
       let lam = linalg_machine_from_ocamlpill args.(0) in
       let res = lam.internal_ccpla_resources () in
	 pylist_fromarray (Array.map pystring_fromstring res)
    )
;;

let _py_cvode_set_max_num_steps =
  python_pre_interfaced_function
    [|CamlpillType; (* the cvode *)
      IntType;	    (* max steps *)
    |]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let nr_steps = pyint_asint args.(1) in
       let () = Sundials_sp.cvode_set_max_num_steps cvode nr_steps in
	 pynone())
;;

let _py_cvode_set_init_step =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let step = pyfloat_asdouble args.(1) in
       let () = Sundials_sp.cvode_set_init_step cvode step in
	 pynone())
;;

let _py_cvode_set_min_step =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let step = pyfloat_asdouble args.(1) in
       let () = Sundials_sp.cvode_set_min_step cvode step in
	 pynone())
;;

let _py_cvode_set_max_step =
  python_pre_interfaced_function
    [|CamlpillType;FloatType|]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let step = pyfloat_asdouble args.(1) in
       let () = Sundials_sp.cvode_set_max_step cvode step in
	 pynone())
;;

let _py_cvode_get_num_steps =
  python_pre_interfaced_function
    [|CamlpillType|] (* the cvode *)
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let nr_steps = Sundials_sp.cvode_get_num_steps cvode in
	 pyfloat_fromdouble nr_steps)
;;

let _py_cvode_set_tolerances =
  python_pre_interfaced_function
    [|CamlpillType; (* the cvode *)
      FloatType;    (* rel_tol *)
      FloatType;    (* abs_tol *)
    |]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let rel_tol = pyfloat_asdouble args.(1) in
       let abs_tol = pyfloat_asdouble args.(2) in
       let () = Sundials_sp.cvode_set_tolerances cvode rel_tol abs_tol in
	 pynone())
;;

let _py_cvode_reinit =
  python_pre_interfaced_function
    [|CamlpillType; (* the cvode *)
      FloatType;    (* Initial time *)
      FloatType;    (* Relative tolerance *)
      FloatType;    (* Absolute tolerance *)

    |]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let () =
	 Sundials_sp.cvode_reinit
	   ~initial_time:(pyfloat_asdouble args.(1))
	   ~rel_tolerance:(pyfloat_asdouble args.(2))
	   ~abs_tolerance:(pyfloat_asdouble args.(3))
	   cvode
       in
	 pynone())
;;

(*let _py_raw_cvode_advance =
  python_pre_interfaced_function
    [|CamlpillType; (* the cvode *)
      CamlpillType; (* the result vector *)
      FloatType;    (* target time *)
      IntType;    (* max nr_steps, -1 for "no limit" *)
    |]
    (fun args ->
       let cvode = cvode_from_ocamlpill args.(0) in
       let FEM_field (_,_,v_result) = field_from_ocamlpill args.(1) in
       let target_time = pyfloat_asdouble args.(2) in
       let max_steps = pyint_asint args.(3) in
       let () = logdebug (Printf.sprintf "DDD cvode_advance call") in
       let result_time =
	 Sundials.cvode_advance cvode target_time v_result max_steps
       in
       let () = logdebug (Printf.sprintf "DDD cvode_advanve finished") in
	 pyfloat_fromdouble result_time)
;;*)

let _py_cvode_get_step_info =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let py_cvode = args.(0) in
      let cvode = cvode_from_ocamlpill py_cvode in
      let (last_step,current_step,actual_init_step) = Sundials_sp.cvode_get_step_info cvode in
        pytuple3(pyfloat_fromdouble last_step, pyfloat_fromdouble current_step, pyfloat_fromdouble actual_init_step))
;;

let _py_cvode_get_stats =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let py_cvode = args.(0) in
      let cvode = cvode_from_ocamlpill py_cvode in
      let stats = Sundials_sp.cvode_get_stats cvode
      in
        pylist_fromarray
          (Array.map
            (fun (desc_string, float_value) ->
              pytuple2(pystring_fromstring desc_string,
                       pyfloat_fromdouble float_value))
            stats))
;;

let _py_cvode_get_current_time =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun args ->
      let py_cvode = args.(0) in
      let cvode = cvode_from_ocamlpill py_cvode in
        pyfloat_fromdouble (Sundials_sp.cvode_get_current_time cvode))
;;

let _py_parse_physical_dimensions =
  python_pre_interfaced_function
    ~doc:"parse_physical_dimensions(\"m^3/kg s^2\",[\"kg\",\"m\",\"s\",\"A\"]) would return [-1,3,2,0]"
    [|StringType;ListType|]
    (fun args ->
       let str = pystring_asstring args.(0) in
       let allowed_dimensions =
	 Array.map
	   (fun py_s -> if pytype py_s <> StringType
	    then failwith "Expected list of known dimensions!"
	    else pystring_asstring py_s)
	   (pylist_toarray args.(1))
       in
       let parsed = parse_or_error Dimensions_lexer.token Dimensions_parser.parse_dimension str in
       let ht_powers = Hashtbl.create 1 in
       let () = array_foreach_do parsed
	 (fun (name,numer,denom) ->
	    let p = (float_of_int numer)/.(float_of_int denom) in
	      (* Note that we are not really that picky about non-float-representable
		 powers like 1/3. Maybe we should be more careful about this.
	      *)
	      hashtbl_increase (+.) ht_powers name p)
       in
       let result =
	 pylist_fromarray
	   (Array.map
	      (fun name ->
		 pyfloat_fromdouble
		   (try
		      let p = Hashtbl.find ht_powers name in
		      let () = Hashtbl.remove ht_powers name in
			p
		    with | Not_found -> 0.0))
	      allowed_dimensions)
       in
	 if Hashtbl.length ht_powers>0
	 then failwith
	     (Printf.sprintf "Encountered unknown physical dimensions: %s"
		(string_array_to_string (hashtbl_keys ~sorter:compare ht_powers)))
	 else result)
;;


(* NOTE: this function has to be generalized and moved to some other package!

   Function to work out the maximum angle between the vector data on
   neighbouring sites. This (as far as we know) is only useful
   information for magnetisation vectors described by first order basis
   functions.

   T.F. Note: A number of variables in that code do have misleading names!
*)

let _vector_field_max_angle field =
  let () = ensure_field_is_unrestricted field in
  let (FEM_field (mwe,_, data)) = field in
  let () = mwe_ensure_has_neighbour_pairs mwe in
  let subfields = mwe.mwe_subfields in
  let dofnames = Array.map fst subfields in
  let nr_dofnames = Array.length dofnames in

  let max_angle_by_dofname =
    Array.init nr_dofnames (fun i -> (dofnames.(i),0.0)) in  (* array to store the field_name+max_angle *)

  let compute_cosine v1 v2 =
    let len = Array.length v1 in
      if Array.length v2 <> len
      then failwith "Tried to calculate scalar product for vectors of different dimension"
      else
	let (scalar_pr,sq_v1,sq_v2) =
	  let rec walk n scalar_prod_sf v1_square_sf v2_square_sf =
	    if n = len
	    then (scalar_prod_sf,v1_square_sf,v2_square_sf)
	    else
	      walk (n+1) (scalar_prod_sf+.v1.(n)*.v2.(n)) (v1_square_sf+.v1.(n)*.v1.(n)) (v2_square_sf+.v2.(n)*.v2.(n))
	  in walk 0 0.0 0.0 0.0
	in
	  scalar_pr/.((sqrt sq_v1)*.(sqrt sq_v2))
  in

  let compute_angle v1 v2 =
    let angle =
      try
	acos (max (-1.0) (min 1.0 (compute_cosine v1 v2)))      (* angle in radiants *)
      with
      | _ -> failwith "fem.ml - vector_field_max_angle - : division by zero"
    in
      angle
  in
  let () =
    for name_ix = 0 to nr_dofnames-1 do
      let vec_len = (snd subfields.(name_ix)).(0)   in
      let v1 = Array.make vec_len 0.0 in
      let v2 = Array.make vec_len 0.0 in

      let (ixs_dofs_with_field_le,ixs_dofs_with_field_ri) =
	match !(mwe.mwe_dof_neighbour_pairs) with
	  | None -> ([||],[||])
	  | Some dofs_with_all_fields ->
	      dofs_with_all_fields.(name_ix)
      in
      let len_data = Array.length ixs_dofs_with_field_le in
      let max_angle_ref = ref 0.0 in
	Mpi_petsc.with_petsc_vector_as_bigarray data
	  (fun ba_field ->
	     let rec walk n =
	       if n = len_data
	       then
		 let (field_name,initial_val) = max_angle_by_dofname.(name_ix) in
		   max_angle_by_dofname.(name_ix) <- (field_name,!max_angle_ref)
	       else
		 let initial_ix_dof_a = ixs_dofs_with_field_le.(n)
		 and initial_ix_dof_b = ixs_dofs_with_field_ri.(n)
		 in
		 let () =
		   for comp_ix=0 to vec_len-1 do
		     let dof_ix_a = initial_ix_dof_a + comp_ix in
		     let dof_ix_b = initial_ix_dof_b + comp_ix in
		     let () = v1.(comp_ix) <- (ba_field.{dof_ix_a}) in
		       v2.(comp_ix) <- (ba_field.{dof_ix_b})
		   done
		 in
		 let new_angle = compute_angle v1 v2 in
		 let () =
		   if new_angle > !max_angle_ref
		   then max_angle_ref := new_angle
		   else ()
		 in
		   walk (n+1)
	     in
	       walk 0
	  )
    done
  in max_angle_by_dofname;;


let _py_mumag_vector_field_max_angles =
  python_pre_interfaced_function
    ~doc:"
 Given a field, this function returns a list of 2-tuples. The first element
 in the tuple is the degree of freedom name, and the second element contains
 the maximum angle between this degree of freedom data on neighbouring mesh sites.

 This is important for micromagnetic calculations to check the 'max angle' stays small.

 This function only works for 1st order basis functions and for vector data
 (i.e. the degree of freedom has to be a vector such as the magnetisation).

 (not implemented as yet)
"
    [|CamlpillType|]
    (* field *)
    (fun args ->
       let field = field_from_ocamlpill args.(0) in
       let dofname_maxangle_array = _vector_field_max_angle field in
	 pylist_fromarray
	   (Array.map
	      (fun (dofname,maxangle) -> pytuple2(pystring_fromstring dofname,pyfloat_fromdouble maxangle))
	      dofname_maxangle_array));;

(* --- BEGIN NSIM MODULE --- *)

let __parse_matoption_type str =
  match str with
    | "MAT_SYMMETRIC" -> Mpi_petsc.MAT_SYMMETRIC
    | "MAT_HERMITIAN" -> Mpi_petsc.MAT_HERMITIAN
    | "MAT_STRUCTURALLY_SYMMETRIC" -> Mpi_petsc.MAT_STRUCTURALLY_SYMMETRIC
    | "MAT_NOT_SYMMETRIC" -> Mpi_petsc.MAT_NOT_SYMMETRIC
    | "MAT_NOT_HERMITIAN" -> Mpi_petsc.MAT_NOT_HERMITIAN
    | "MAT_NOT_STRUCTURALLY_SYMMETRIC" -> Mpi_petsc.MAT_NOT_STRUCTURALLY_SYMMETRIC
    | "MAT_SYMMETRY_ETERNAL" -> Mpi_petsc.MAT_SYMMETRY_ETERNAL
    | _ -> failwith (Printf.sprintf "Unknown PETSc matrix matoption: '%s'" str)
;;

(* Note that this will have to be wrapped up with quite a dramatic
   amount of python sugar!
*)
let _py_raw_make_lam	= (* XXX TO BE EXTENDED QUITE DRAMATICALLY IN THE FUTURE! XXX *)
  python_pre_interfaced_function
    [|ListType;	(* mwes *)
      ListType;	(* internal buffers *)
      ListType;	(* operator matrices *)
      ListType;	(* dense matrices *)
      ListType;	(* KSPs *)
      ListType;	(* SWEXs *)
      ListType; (* Jacobi plans *)
      ListType;	(* CommandSequences *)
      ListType;	(* Internal Params *)
      ListType;	(* Timesteppers *)
      (* --- args to make_linalg_machine --- *)
      StringType; (* name prefix for petsc objects *)
    |]
    (fun args ->
       let parsed_matoptions x =
	 Array.map
	   (fun py -> __parse_matoption_type (guarded_pystring_asstring py))
	   (guarded_pylist_toarray x)
       in
       let () = logdebug (Printf.sprintf "raw_make_lam") in
       let mwes =
	 py_homogeneous_list_as_array
	   "Mesh-With-Elements"
	   (fun x -> ocamlpill_type_of x = pysym_mwe)
	   mwe_from_ocamlpill
	   args.(0)
       in
       let () = logdebug (Printf.sprintf "raw_make_lam mwes") in
       let ensure_has_enough_args name how_many a =
	 if Array.length a < how_many
	 then failwith (Printf.sprintf "Missing args (%s): %d of %d" name (Array.length a) how_many)
	 else ()
       in
       let spec_ibuffers =
	 Array.map
	   (fun spec ->
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_ibuffers" 4 data in
	      let buffer_name = guarded_pystring_asstring data.(0) in
	      let mwe_name = guarded_pystring_asstring data.(1) in
	      let field_restr_spec = guarded_pystring_asstring data.(2) in
	      let restr =
		if field_restr_spec = ""
		then None
		else
		  parse_or_error
		    Ddiffop_lexer.token
		    Ddiffop_parser.short_vector_restriction
		    field_restr_spec
	      in
	      let is_field = py_is_true data.(3) in
	      let initial_value = pyfloat_asdouble data.(4) in
		(buffer_name,mwe_name,restr,is_field,initial_value))
	   (pylist_toarray args.(1))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam ibuffers") in
       let spec_op_matrices =
	 Array.map
	   (fun spec ->
	      (* let ddd = Printf.printf "DDD SPEC OP MATRIX: %s\n%!" (py_repr spec) in *)
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_op_matrices" 6 data in
	      let name = guarded_pystring_asstring data.(0) in
	      let mwe_name_le = guarded_pystring_asstring data.(1) in
	      let mwe_name_ri = guarded_pystring_asstring data.(2) in
	      let op = guarded_pystring_asstring data.(3) in
	      let matoptions = parsed_matoptions data.(4) in
	      let opt_mwe_name_mid =
		py_optionally guarded_pystring_asstring data.(5) in
		{loms_name=name;
		 loms_mwe_name_le = mwe_name_le;
		 loms_mwe_name_ri = mwe_name_ri;
		 loms_mwe_name_mid = opt_mwe_name_mid;
		 loms_symbolic_operator = op;
		 loms_matoptions=matoptions;
		})
	   (pylist_toarray args.(2))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam op_matrices") in
       let spec_dense_matrices =
	 Array.map
	   (fun spec ->
	      (* let ddd = Printf.printf "DDD SPEC LDMS %s\n%!" (py_repr spec) in *)
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_dense_matrices" 8 data in
	      let name = guarded_pystring_asstring data.(0) in
	      let is_hlib = py_is_true data.(1) in
	      let mwe_name = guarded_pystring_asstring data.(2) in
	      let dof_name = (* Note: we encode the dof name as ["name",ix1,ix2,ix3,...] -
				most often, this will just be [phi]! *)
		let dn = pylist_toarray data.(3) in
		let dof_stem = pystring_asstring dn.(0) in
		let dof_indices = Array.map guarded_pyint_asint (Array.sub dn 1 ((Array.length dn)-1)) in
		  (dof_stem,dof_indices)
	      in
	      let boundary_spec = pystring_asstring data.(4) in
	      let lattice_info =
		py_optionally
		  (fun py_linfo ->
		     let a = guarded_pytuple_toarray py_linfo in
		     let v_py_ortho_trans = guarded_pylist_toarray a.(0)
		     and v_py_ortho_gf_disp = guarded_pylist_toarray a.(1)
		     in
		     let ortho_trans =
		       Array.map
			 (fun py_ortho ->
			    Array.init 3
			      (fun row ->
				 Array.init 3
				   (fun col ->
				      let e = _py_tensor_entry py_ortho [|row;col|] in
					guarded_pynumber_asfloat e)))
			 v_py_ortho_trans
		     in
		     let v_o_gf_disp = Array.map
		       (fun py_o_gf_disp ->
			  let a = guarded_pytuple_toarray py_o_gf_disp in
			    (guarded_pyint_asint a.(0),
			     guarded_pynumber_asfloat a.(1),
			     py_number_list_as_float_array a.(2)))
		       v_py_ortho_gf_disp
		     in
		       (ortho_trans,v_o_gf_disp))
		  data.(5)
	      in
	      let hlib_params = (pylist_toarray data.(7)) in
	      let () = ensure_has_enough_args "hlib_params" 8 data in
	      let algorithm = guarded_pyint_asint hlib_params.(0) in
	      let nfdeg = guarded_pyint_asint hlib_params.(1) in
	      let nmin = guarded_pyint_asint hlib_params.(2) in
	      let eta = guarded_pyfloat_asfloat hlib_params.(3) in
	      let eps_aca = guarded_pyfloat_asfloat hlib_params.(4) in
	      let eps = guarded_pyfloat_asfloat hlib_params.(5) in
	      let p = guarded_pyint_asint hlib_params.(6) in
	      let kmax = guarded_pyint_asint hlib_params.(7) in
	      let hlib_params_tuple = (algorithm, nfdeg, nmin, eta, eps_aca, eps, p, kmax) in
	      let matoptions = parsed_matoptions data.(6) in
		{
		  ldms_name=name;
		  ldms_hlib=is_hlib;
		  ldms_hlib_params=hlib_params_tuple;
		  ldms_mwe_name=mwe_name;
		  ldms_dof_name=dof_name;
		  ldms_boundary_spec=boundary_spec;
		  ldms_lattice_info=lattice_info;
		  ldms_matoptions=matoptions;
		})
	   (pylist_toarray args.(3))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam dense_matrices") in
       let spec_ksps =
	 Array.map
	   (fun spec ->
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_ksps" 3 data in
	      let name = guarded_pystring_asstring data.(0) in
	      let matrix_name = guarded_pystring_asstring data.(1) in
	      let precond_name = guarded_pystring_asstring data.(2) in
	      let ksp_type = py_optionally guarded_pystring_asstring data.(3) in
	      let pc_type = py_optionally guarded_pystring_asstring data.(4) in
	      let ignz = py_optionally py_is_true data.(5) in
	      let rtol = py_optionally guarded_pyfloat_asfloat data.(6) in
	      let atol = py_optionally guarded_pyfloat_asfloat data.(7) in
	      let dtol = py_optionally guarded_pyfloat_asfloat data.(8) in
	      let maxits = py_optionally guarded_pyint_asint data.(9) in
	      let nullspace =
		py_optionally
		  (fun py_nsdata ->
		     (let nsdata = guarded_pylist_toarray py_nsdata in
		      let () = ensure_has_enough_args "nsdata" 2 nsdata in
			(py_is_true nsdata.(0),
			 Array.map guarded_pystring_asstring (guarded_pylist_toarray nsdata.(1)))))
		  data.(10) in
		{
		  lks_name=name;
		  lks_matrix_name=matrix_name;
		  lks_precond_name=precond_name;
		  lks_ksp_type=ksp_type;
		  lks_pc_type=pc_type;
		  lks_nullspace=nullspace;
		  lks_initial_guess_nonzero=ignz;
		  lks_rtol=rtol;
		  lks_atol=atol;
		  lks_dtol=dtol;
		  lks_maxits=maxits;
		})
	   (pylist_toarray args.(4))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam ksps") in
       let spec_swexs =
	 Array.map
	   (fun spec ->
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_swexs" 6 data in
	      let name = guarded_pystring_asstring data.(0) in
	      let () = logdebug (Printf.sprintf "DDD processing SWEX name='%s'" name) in
	      let code = guarded_pystring_asstring data.(1) in
	      let aux_args = Array.map guarded_pystring_asstring (guarded_pylist_toarray data.(2)) in
	      let field_mwes = Array.map guarded_pystring_asstring (guarded_pylist_toarray data.(3)) in
	      let cofield_mwes = Array.map guarded_pystring_asstring (guarded_pylist_toarray data.(4)) in
	      let must_parse_eqns = py_is_true data.(5) in
	      let () = logdebug (Printf.sprintf "DDD making swex: name='%s' field_mwes=%s" name (string_array_to_string field_mwes)) in
	      let ccode = if must_parse_eqns
	      then localeqn_ccode code
	      else code
	      in
		(* let () = Printf.printf "DDD SWEX mwes=%s\n code:\n===>\n%s\n<===\n ccode:\n===>\n%s\n<===\n\n%!" (string_array_to_string field_mwes) code ccode in (* DDD *) *)
		{
		  lss_name=name;
		  lss_c_code=ccode;
		  lss_aux_args=aux_args;
		  lss_field_mwes=field_mwes;
		  lss_cofield_mwes=cofield_mwes;
		})
	   (pylist_toarray args.(5))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam swexs") in
       let spec_jplans =
	 Array.map
	   (fun spec ->
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_jplan" 6 data in
	      let name = guarded_pystring_asstring data.(0) in
	      let mwe_lhs_name = guarded_pystring_asstring data.(1) in
	      let mwe_names = Array.map guarded_pystring_asstring (guarded_pylist_toarray data.(2)) in
	      let derive_me =
		Array.map
		  (fun py_contrib_spec_pieces ->
		     let contrib_spec_pieces = guarded_pylist_toarray py_contrib_spec_pieces in
		       Array.map
			 (fun py_contrib_spec ->
			    let contrib_spec = guarded_pytuple_toarray py_contrib_spec in
			    let () = (if Array.length contrib_spec <> 2 then failwith (Printf.sprintf "Bad Jacobian derivative contrib spec: %s" (py_repr py_contrib_spec)) else ()) in
			    let contrib_type = guarded_pystring_asstring contrib_spec.(0)
			    and contrib_content = guarded_pystring_asstring contrib_spec.(1)
			    in
			      match contrib_type with
				| "operator" -> Nsim.JPLAN_OPERATOR contrib_content
				| "equation" -> Nsim.JPLAN_EQUATION contrib_content
				| _ -> failwith (Printf.sprintf "Bad Jacobian contrib type: '%s'" contrib_type))
			 contrib_spec_pieces)
		  (guarded_pylist_toarray data.(3))
	      in
	      let eom_str = guarded_pystring_asstring data.(4) in
	      let debugprint = py_is_true data.(5) in
	      let (_,eqns) = parse_localeqn eom_str in
		{
		  ljps_name=name;
		  ljps_debugprint=debugprint;
		  ljps_mwe_lhs=mwe_lhs_name;
		  ljps_mwe_names=mwe_names;
		  ljps_derive_me=derive_me;
		  ljps_eqns=array_join eqns;
		}
	   )
	   (pylist_toarray args.(6))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam jplans") in
       let spec_commseqs =
	 Array.map
	   (fun spec ->
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_commseqs" 4 data in
	      let cseq_name = guarded_pystring_asstring data.(0) in
	      let parse_args_fields_cofields py_a =
		let a = guarded_pylist_toarray py_a in
		  Array.map
		    (fun py_names ->
		       let names = guarded_pylist_toarray py_names in
			 (guarded_pystring_asstring names.(0),
			  guarded_pystring_asstring names.(1))) a
	      in
	      let args_fields = parse_args_fields_cofields data.(1) in
	      let args_cofields = parse_args_fields_cofields data.(2) in
	      let ops =
		Array.map
		  (fun py_data ->
		     let data = guarded_pylist_toarray py_data in
		     let op = guarded_pystring_asstring data.(0) in
		     let () = logdebug (Printf.sprintf "DDD linalg_machine op=%s" op) in
		       match op with
			 | "DISTRIB" -> LC_vec_distribute (guarded_pystring_asstring data.(1),
							   guarded_pystring_asstring data.(2))
			 | "COLLECT" -> LC_vec_collect (guarded_pystring_asstring data.(1),
							guarded_pystring_asstring data.(2))
			 | "SCALE" -> LC_pvec_scale (guarded_pystring_asstring data.(1),
						     guarded_pyfloat_asfloat data.(2))
			     (* Note: the opcode below is somewhat weird, but we use it to implement
				thermal effects...
			     *)
			 | "PW-SQRT" -> LC_pvec_pointwise (guarded_pystring_asstring data.(1),
							   (fun _ x -> sqrt(x)))
			 | "AXPBY" -> LC_pvec_axpby
			     (
			       (if pytype data.(1) = FloatType
				then CCCOEFF_float (pyfloat_asdouble data.(1))
				else CCCOEFF_var (guarded_pystring_asstring data.(1))),
			       guarded_pystring_asstring data.(2),
			       (if pytype data.(3) = FloatType
				then CCCOEFF_float (pyfloat_asdouble data.(3))
				else CCCOEFF_var (guarded_pystring_asstring data.(3))),
			       guarded_pystring_asstring data.(4)
			     )
			 | "AXPBY-IPARAMS" -> LC_iparams_axpby
			     (
			       (if pytype data.(1) = FloatType
			        then CCCOEFF_float (pyfloat_asdouble data.(1))
				else CCCOEFF_var (guarded_pystring_asstring data.(1))),
			       guarded_pystring_asstring data.(2),
			       (if pytype data.(3) = FloatType
			        then CCCOEFF_float (pyfloat_asdouble data.(3))
				else CCCOEFF_var (guarded_pystring_asstring data.(3))),
			       guarded_pystring_asstring data.(4)
			     )
			 | "PULL" -> failwith "NOTE: op-code PULL not implemented yet!"
			     (* The problem is the range specificator! *)
			 | "PUSH" -> failwith "NOTE: op-code PUSH not implemented yet!"
			     (* The problem is the range specificator! *)
			 | "PULL-FEM" -> LC_pvec_pull_fem (guarded_pystring_asstring data.(1),
							   guarded_pystring_asstring data.(2),
							   guarded_pystring_asstring data.(3),
							   guarded_pystring_asstring data.(4))
			 | "PUSH-FEM" -> LC_pvec_push_fem (guarded_pystring_asstring data.(1),
							   guarded_pystring_asstring data.(2),
							   guarded_pystring_asstring data.(3),
							   guarded_pystring_asstring data.(4))
			 | "SM*V" -> LC_smx_x_pvec (guarded_pystring_asstring data.(1),
						    guarded_pystring_asstring data.(2),
						    guarded_pystring_asstring data.(3))
			 | "DM*V" -> LC_dmx_x_pvec (guarded_pystring_asstring data.(1),
						    guarded_pystring_asstring data.(2),
						    guarded_pystring_asstring data.(3))
			 | "V*V" -> LC_vec_pointwise_mult (guarded_pystring_asstring data.(1),
							   guarded_pystring_asstring data.(2),
							   guarded_pystring_asstring data.(3))
			 | "CFBOX" -> LC_cofield_to_field (guarded_pystring_asstring data.(1),
							   guarded_pystring_asstring data.(2))
			 | "SOLVE" -> LC_psolve (guarded_pystring_asstring data.(1),
						 guarded_pystring_asstring data.(2),
						 guarded_pystring_asstring data.(3))
			 | "SITE-WISE-IPARAMS" -> LC_psite_wise_iparams (guarded_pystring_asstring data.(1),
									 Array.map guarded_pystring_asstring
									   (guarded_pylist_toarray data.(2)),
									 Array.map guarded_pystring_asstring
									   (guarded_pylist_toarray data.(3)))
			     (*
			       | "SITE-WISE" -> LC_psite_wise (guarded_pystring_asstring data.(1),
			       Array.map guarded_pystring_asstring
			       (guarded_pylist_toarray data.(2)),
			       Array.map guarded_pystring_asstring
			       (guarded_pylist_toarray data.(3)),
			       Array.map guarded_pyfloat_asfloat
			       (guarded_pylist_toarray data.(4)))
			     *)
			 | "GOSUB" -> LC_gosub (guarded_pystring_asstring data.(1))
                         | "CALLPY" ->
                           LC_callext
                             (fun time ->
                                let py_time = pytuple_fromsingle (pyfloat_fromdouble time) in
                                  Array.iter
                                    (fun py_fn -> ignore (pyeval_callobject (py_fn, py_time)))
                                    (guarded_pylist_toarray data.(1)))
                           (* Note: we want to keep a reference to the Python
                            * list so that it can be updated from Python later
                            *)
			 | "JPLAN" -> LC_jplan_execute ((guarded_pystring_asstring data.(1)),
							Array.map guarded_pystring_asstring
							  (guarded_pylist_toarray data.(2)))
			 | "DEBUG" -> LC_debug_printvec_1cpu (guarded_pystring_asstring data.(1),
							      guarded_pystring_asstring data.(2),
							      guarded_pyint_asint data.(3)
							     )
			 | "TSTART" -> LC_start_timer (guarded_pystring_asstring data.(1))
			 | "TSTOP" -> LC_stop_timer (guarded_pystring_asstring data.(1))
			 | "TREPORT" -> LC_report_timers (Array.map guarded_pystring_asstring (Array.sub data 1 (Array.length data-1)))
			 | "TCLEAR" -> LC_clear_timers (Array.map guarded_pystring_asstring (Array.sub data 1 (Array.length data-1)))
			 | other_op -> failwith (Printf.sprintf "Unknown opcode: '%s'" other_op))
		  (guarded_pylist_toarray data.(3)) in
		(cseq_name,args_fields,args_cofields,ops))
	   (pylist_toarray args.(7))
       in
       let intensive_params = py_string_list_as_array args.(8) in
       let spec_timesteppers =
	 Array.map
	   (fun spec ->
	      let data = guarded_pylist_toarray spec in
	      let () = ensure_has_enough_args "spec_timestepper" 17 data in
	      let name = guarded_pystring_asstring data.(0) in
	      let name_jacobian = guarded_pystring_asstring data.(1) in
	      let nr_primary_fields = guarded_pyint_asint data.(2) in
	      let jacobi_prealloc_diagonal = guarded_pyint_asint data.(3) in
	      let jacobi_prealloc_off_diagonal = guarded_pyint_asint data.(4) in
	      let use_jacobian = py_is_true data.(5) in
	      let pc_same_nonzero_pattern = py_is_true data.(6) in
	      let pc_opt_rtol = py_optionally guarded_pyfloat_asfloat data.(7) in
	      let pc_opt_atol = py_optionally guarded_pyfloat_asfloat data.(8) in
	      let pc_opt_dtol = py_optionally guarded_pyfloat_asfloat data.(9) in
	      let pc_opt_maxit = py_optionally guarded_pyint_asint data.(10) in
	      let names_phys_field_buffers =
		Array.map guarded_pystring_asstring (guarded_pylist_toarray data.(11))
	      in
	      let max_order = guarded_pyint_asint data.(12) in
	      let krylov_max = guarded_pyint_asint data.(13) in
	      let name_seq_velocities = guarded_pystring_asstring data.(14) in
	      let names_phys_field_mwes = Array.map guarded_pystring_asstring (guarded_pylist_toarray data.(15)) in
	      let jacobi_eqns =
		let (_,x) = parse_localeqn (guarded_pystring_asstring data.(16)) in array_join x
	      in
	      let phys_field_derivs =
		Array.map
		  (fun py_type_and_name ->
		     let z = guarded_pytuple_toarray py_type_and_name in
		       (guarded_pystring_asstring z.(0),
			guarded_pystring_asstring z.(1)))
		  (guarded_pylist_toarray data.(17))
	      in
		{
		  (* lts=Linalgmachine Timestepper Specification *)
		  Nsim.lts_name = name;
		  Nsim.lts_jacobian = name_jacobian;
		  Nsim.lts_name_seq_velocities = name_seq_velocities;
		  Nsim.lts_nr_primary_fields = nr_primary_fields;
		  Nsim.lts_names_phys_field_mwes = names_phys_field_mwes;
		  Nsim.lts_names_phys_field_buffers = names_phys_field_buffers;
		  Nsim.lts_jacobi_prealloc_diagonal = jacobi_prealloc_diagonal;
		  Nsim.lts_jacobi_prealloc_off_diagonal = jacobi_prealloc_off_diagonal;
		  Nsim.lts_use_jacobian = use_jacobian;
		  Nsim.lts_jacobi_equations = jacobi_eqns;
		  Nsim.lts_phys_field_derivs = phys_field_derivs;
		  Nsim.lts_pc_same_nonzero_pattern = pc_same_nonzero_pattern;
		  Nsim.lts_pc_opt_rtol = pc_opt_rtol;
		  Nsim.lts_pc_opt_atol = pc_opt_atol;
		  Nsim.lts_pc_opt_dtol = pc_opt_dtol;
		  Nsim.lts_pc_opt_maxit = pc_opt_maxit;
		  Nsim.lts_krylov_max = krylov_max;
		  Nsim.lts_max_order = max_order;
		}
	   )
	 (pylist_toarray args.(9))
       in
       let () = logdebug (Printf.sprintf "raw_make_lam commseqs") in
       let linalg_script =
	 {
	  las_intensive_params = intensive_params;
	  las_mwes=Array.map (fun mwe -> mwe.mwe_name) mwes;
	  las_internal_buffers=spec_ibuffers;
	  las_op_matrices=spec_op_matrices;
	  las_dense_matrices=spec_dense_matrices;
	  las_ksps=spec_ksps;
	  las_swexs=spec_swexs;
	  las_jplans=spec_jplans;
	  las_command_sequences=spec_commseqs;
	  las_timesteppers=spec_timesteppers;
	 }
       in
       let () = logdebug (Printf.sprintf "raw_make_lam before make_linalg_machine") in
       let machine =
	 make_linalg_machine
	   ?ccpla:(!pyfem_ccpla)
	   ~prefix:(pystring_asstring args.(10))
	   ~relevant_mwes:mwes
	   linalg_script
       in
       let () = logdebug (Printf.sprintf "raw_make_lam before returning OCamlPill")
       in
	 ocamlpill_from_linalg_machine machine
    )
;;

let _py_execute_linalg_machine	=
  python_pre_interfaced_function
    [|CamlpillType;StringType;ListType;ListType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let seq_name = pystring_asstring args.(1) in
       let fields =
	 py_homogeneous_list_as_array
	   "FEM field" (fun x -> ocamlpill_type_of x = pysym_field)
	   field_from_ocamlpill args.(2)
       in
       let cofields =
	 py_homogeneous_list_as_array
	   "FEM cofield" (fun x -> ocamlpill_type_of x = pysym_cofield)
	   field_from_ocamlpill args.(3)
       in
       let () = linalg_machine.execute_on seq_name fields cofields in
	 pynone()
    )
;;

let _py_lam_get_timers =
  python_pre_interfaced_function
    [|CamlpillType;ListType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let names = py_string_list_as_array args.(1) in
       let result = linalg_machine.get_timers names in
	 pylist_fromarray
	   (Array.map
	      (fun (name,time) ->
		 pytuple2(pystring_fromstring name,
			  pyfloat_fromdouble time))
	      result))
;;

let _py_lam_ts_init =
  python_pre_interfaced_function
    ~doc:"Initialise timestepper. Arguments: (linalgmachine:CamlPill,timesteppername:string,t_initial:float,rel_tol:float,abs_tol). rel_tol and abs_tol are sundials tolerance parameters."
    [|CamlpillType;StringType;FloatType;FloatType;FloatType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let name = pystring_asstring args.(1) in
       let t_initial = pyfloat_asdouble args.(2) in
       let rel_tol = pyfloat_asdouble args.(3) in
       let abs_tol = pyfloat_asdouble args.(4) in
       let () = linalg_machine.timestepper_initialize_from_physics name t_initial rel_tol abs_tol in
	 pynone())
;;

let _py_lam_ts_advance =
  python_pre_interfaced_function
    ~doc:"Advance timestepper. Arguments: (linalgmachine:CamlPill,timesteppername:string,t_final:float,max_steps:int). max_steps=-1 means no step limit. Returns time reached (float)"
    [|CamlpillType; StringType; BoolType; FloatType; IntType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let name = pystring_asstring args.(1) in
       let exact_tstop = pybool_asbool args.(2) in
       let t_final = pyfloat_asdouble args.(3) in
       let maxsteps = pyint_asint args.(4) in
       let t_reached = linalg_machine.timestepper_advance name exact_tstop t_final maxsteps in
	 pyfloat_fromdouble t_reached)
;;

let _py_lam_get_ts_cvode =
  python_pre_interfaced_function
    ~doc:"Advance timestepper. Arguments: (linalgmachine:CamlPill,timesteppername:string). Returns Ocamlpill of cvode."
    [|CamlpillType;StringType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let name = pystring_asstring args.(1) in
       let cvode = linalg_machine.timestepper_get_cvode name in
	 ocamlpill_from_cvode cvode)
;;

let _py_lam_get_ts_timings =
  python_pre_interfaced_function
    ~doc:"Get timestepper timings. Arguments: (linalgmachine:CamlPill,timesteppername:string)."
    [|CamlpillType;StringType;|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let name = pystring_asstring args.(1) in
       let result = linalg_machine.timestepper_timings name "REPORT" in
	 pylist_fromarray
	   (Array.map
	      (fun (name,n,t) ->
		 pytuple3(pystring_fromstring name,
			  pyfloat_fromdouble n,
			  pyfloat_fromdouble t))
	      result))
;;

let _py_lam_set_ksp_tolerances =
  python_pre_interfaced_function
    ~doc:"Set ksp tolerances for kspsolver. Arguments: (cvode:CamlPill,kspname:string,rel_tol: List of float, abs_tol: list of float, dtol: list of float, maxits: list of int ). Lists can be empty if corresponding value should not be modified."
    [|CamlpillType; (* the cvode *)
      StringType;  (* ksp name *)
      ListType;    (* opt rel_tol *)
      ListType;    (* opt abs_tol *)
      ListType;    (* opt dtol *)
      ListType;    (* opt maxits *)
    |]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let ksp_name = pystring_asstring args.(1) in
       let rtol = py_optionally guarded_pyfloat_asfloat args.(2) in
       let atol = py_optionally guarded_pyfloat_asfloat args.(3) in
       let dtol = py_optionally guarded_pyfloat_asfloat args.(4) in
       let maxits = py_optionally guarded_pyint_asint args.(5) in
       let () = linalg_machine.set_ksp_tolerances ksp_name (rtol,atol,dtol,maxits) in
	 pynone())
;;

let _py_lam_get_ksp_tolerances =
  python_pre_interfaced_function
    ~doc:"Get ksp tolerances from kspsolver. Arguments: (cvode:CamlPill,kspname:string). Returns 4-tuple (rtol,atol,dtol,maxits)."
    [|CamlpillType; (* the cvode *)
      StringType;  (* ksp name *)
    |]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let ksp_name = pystring_asstring args.(1) in
       let (rtol,atol,dtol,maxits) = linalg_machine.get_ksp_tolerances ksp_name in
	 pytuple4 (pyfloat_fromdouble rtol,
		   pyfloat_fromdouble atol,
		   pyfloat_fromdouble dtol,
		   pyint_fromint maxits))
;;



let _py_lam_bem_field_strength =
  python_pre_interfaced_function
    [|FloatType; (* epsilon *)
      CamlpillType; (* linalg_machine *)
      StringType;   (* name of lam-vector mwe (e.g. "H_demag") *)
      StringType;   (* name of lam-vector field (e.g. "v_H_demag") *)
      StringType;   (* name of lam-vector dof *)
      StringType;   (* bem name *)
      StringType;   (* boundary vec name *)
      ListType;	    (* position *)
    |]
    (fun args ->
       let epsilon = pyfloat_asdouble args.(0) in
       let linalg_machine = linalg_machine_from_ocamlpill args.(1) in
       let mwe_name = pystring_asstring args.(2) in
       let lam_vector_field_name = pystring_asstring args.(3) in
       let dof_stem = pystring_asstring args.(4) in
       let bem_name = pystring_asstring args.(5) in
       let boundary_vec_name = pystring_asstring args.(6) in
       let pos = py_float_list_as_array args.(7) in
       let mwe = linalg_machine.get_mwe mwe_name in
       let buffer = make_field mwe in
	 (* ^ XXX wasteful allocation of buffer! Need linalg_machine-internal probe_field! *)
       let () = linalg_machine.get_field lam_vector_field_name buffer in
       let probed = probe_field buffer ~dof_stem pos in
	 if Array.length probed>0
	 then
	   float_array_to_python (Array.map (fun (_,x) -> x) probed)
	     (* probe_field returns a lexicographically sorted (dof_name*float) array *)
	 else
	   let grad =
	     Nsim.bem_field_strength
	       ~epsilon linalg_machine ~bem_name ~boundary_vec_name pos
	   in
	     float_array_to_python grad
    )
;;

let _py_lam_get_set_field_cofield do_get do_field =
  python_pre_interfaced_function
    [|CamlpillType;CamlpillType;StringType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let buffer_name = pystring_asstring args.(2) in
       let () =
	 (if do_field
	  then
	    let buffer = field_from_ocamlpill args.(1) in
	      (if do_get then linalg_machine.get_field else linalg_machine.set_field)
		buffer_name buffer
	  else
	    let buffer = cofield_from_ocamlpill args.(1) in
	      (if do_get then linalg_machine.get_cofield else linalg_machine.set_cofield)
		buffer_name buffer)
       in
	 pynone())
;;

let _py_lam_get_iparam =
  python_pre_interfaced_function
    [|CamlpillType;StringType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let name = pystring_asstring args.(1) in
       let result = linalg_machine.get_iparam name in
	 pyfloat_fromdouble result)
;;

let _py_lam_set_iparam =
  python_pre_interfaced_function
    [|CamlpillType;StringType;FloatType|]
    (fun args ->
       let linalg_machine = linalg_machine_from_ocamlpill args.(0) in
       let name = pystring_asstring args.(1) in
       let z = pyfloat_asdouble args.(2) in
       let () = linalg_machine.set_iparam name z in
	 pynone())
;;



let _py_nsim_anisotropy_equations =
  python_pre_interfaced_function
    [|StringType;StringType;StringType; (* name_E, name_m, name_H *)
      IntType; IntType;			(* dim, order *)
      (* TODO: Not provided: random number generator! *)
      CallableType;
      FloatType;			(* correction factor (mu0) *)
    |]
    (fun args ->
       let name_E = pystring_asstring args.(0)
       and name_m = pystring_asstring args.(1)
       and name_H = pystring_asstring args.(2)
       and dim = pyint_asint args.(3)
       and order = pyint_asint args.(4)
       and py_fun_E = pycallable_asfun args.(5)
       and factor_mu0 = pyfloat_asdouble args.(6)
       in
       let fun_E coords =
	 let py_coords = float_array_to_python coords in
	 let energy = py_fun_E [|py_coords|] in
	   guarded_pynumber_asfloat energy
       in
       let (eq_E,eq_H) = Nsim_anisotropy.anisotropy_equations ~factor_mu0 ~dim ~name_E ~name_m ~name_H order fun_E
       in
	 pytuple2 (pystring_fromstring eq_E, pystring_fromstring eq_H))
;;

let _py_oommf_interpolator =
  python_pre_interfaced_function
    ~doc:"oommf_interpolator(oommf_omf_filename) -> function: [coords] -> [vector]\n\nNote: this function is a temporary fix only. There are better ways to achieve such results. "
    [|StringType|]
    (fun args ->
       let filename = pystring_asstring args.(0) in
       let interpolator = interpolated_oommf_data filename in
	 python_interfaced_function
	   ~doc:(Printf.sprintf "Numerical data interpolated from OOMMF file '%s'" filename)
	   [|ListType|]
	   (fun args ->
	      let pos = py_number_list_as_float_array args.(0) in
	      let data = interpolator pos in
		float_array_to_python data))
;;

let _py_time_vmem_rss =
  python_pre_interfaced_function
    ~doc:"Returns a triple containing three double numbers (time, vmem, rss): the time since this function was called for the first time, the memory usage (vmem and rss)."
    [||]
    (fun args ->
       let (time, vmem, rss) = time_vmem_rss ()
       in
	 pytuple3 ((pyfloat_fromdouble time),
	           (pyfloat_fromdouble vmem),
	           (pyfloat_fromdouble rss)))
;;

let _py_memory_footprint =
  python_pre_interfaced_function
    ~doc:"Returns a triple containing three double numbers (size_data, size_headers, depth) that describe the size of a given OCamlpill. The size is expressed in bytes."
    [|CamlpillType|]
    (fun args ->
       let x = args.(0) in
       let value = Snippets.get_feature ~domain:"debug" "do_ocaml_pill_memory_reports" in
       let inactive = (-1.,-1.,-1.) in
       let (sz_data,sz_headers,sz_depth) =
	 match value with
	   | None -> inactive
	   | Some value ->
	       if value = "True" || value = "true" then
		   memory_footprint(ocamlpill_hard_unwrap args.(0))
	       else
		 inactive
     in
      pytuple3 ((pyfloat_fromdouble sz_data),
	        (pyfloat_fromdouble sz_headers),
	        (pyfloat_fromdouble sz_depth)))
;;


let _py_ml_heap_footprint =
  python_pre_interfaced_function
    ~doc:"Returns a string describing the state of the ml heap."
    [||]
    (fun args ->
       pystring_fromstring (gc_info true));;



let _py_debug_absorb_list =
  python_pre_interfaced_function
    ~doc:"Debug function that takes a python list and does nothing with it. Returns None"
    [|ListType|]
    (fun args -> pynone());;

(* Print sundials library for info (debug): *)
let get_nsim_sundials_library_path () =
  sundials_path (Printf.sprintf "%s/" Nsimconf.sundials_lib_path);;

let _py_get_nsim_sundials_library_path =
  python_pre_interfaced_function
    ~doc:"Returns search path for sundials libraries."
    [||]
    (fun args ->
       pystring_fromstring (get_nsim_sundials_library_path ())
    )
;;

let _py_reset_signal_handler =
  python_pre_interfaced_function
    ~doc:"Reset handler for given signal number to default (e.g. to overcome petsc's SEGV handler)."
    [|IntType|]
    (fun args ->
       begin
	 reset_signal_handler (pyint_asint args.(0));
	 pynone();
       end
    )
;;

let _py_get_numpy_dbl_array =
  python_pre_interfaced_function
    ~doc:"Test passing of NumPy array (generated from OCaml) to Python"
    [|IntType|]
    (fun args ->
        pyobject_of_pytensor
          (pytensor_init double_items
             [|(pyint_asint args.(0)); 3|]
             (fun [|i; j|] -> (float_of_int i) +. (float_of_int j)*.0.1)))
;;

(* --- END NSIM MODULE --- *)

let _ =
  register_for_python
    [|("mesher_defaults",ocamlpill_from_mesher_defaults_int !opt_mesher_defaults);
      ("mesher_default_gendriver",ocamlpill_from_mg_gendriver default_gendriver);
      ("empty_element",_py_empty_element);
    |]
;;

let _ =
  register_pre_functions_for_python
    [|(* The version *)
      ("version", _py_version);
      (* Test of NumPy support *)
      ("get_dbl_arr", _py_get_numpy_dbl_array);
      (* Nlog *)
      ("nlog_register_handler", _py_nlog_register_handler);
      ("nlog_setupLogger", _py_nlog_setupLogger);
      ("nlog_setLogLevel", _py_nlog_setLogLevel);
      ("nlog_printLoggerInfo", _py_nlog_printLoggerInfo);
      ("nlog_printLoggerInfo_mpi", _py_nlog_printLoggerInfo_mpi);
      ("nlog_log", _py_nlog_logmsg);
      ("nlog_log_mpi", _py_nlog_logmsg_mpi);
      (* Useful snippets functions: *)
      ("string_multifill_template_then_concat", _py_string_multifill_template_then_concat);
      ("reset_signal_handler", _py_reset_signal_handler);
      (* Snippets features: *)
      ("snippets_get_feature", _py_snippets_get_feature);
      ("snippets_register_feature", _py_snippets_register_feature);
      ("snippets_all_features", _py_snippets_all_features);
      (* PETSc intelligence *)
      ("petsc_cpu_cycles", _py_petsc_cpu_cycles);
      ("petsc_mpi_nr_nodes", _py_petsc_mpi_nr_nodes);
      (* ("getcwd",_py_getcwd); *)
      (* ("get_initial_working_directory",_py_get_initial_working_directory); *)
      ("mpi_status", _py_mpi_status);
      ("mpi_hello", _py_ccpla_hello);
      ("mpi_execute_on_all_nodes_hello",_py_execute_on_all_nodes_hello);
      ("petsc_is_mpi", _py_petsc_is_mpi);
      (* HLib setup *)
      ("init_hlib",_py_init_hlib);
      (* Other helpers *)
      ("delaunay",_py_delaunay);
      ("simple_convex_mesh", _py_simple_convex_mesh);
      (* The mesh *)
      ("mesh_dim", _py_mesh_dim);
      ("mesh_nr_points", _py_mesh_nr_points);
      ("mesh_nr_simplices", _py_mesh_nr_simplices);
      ("mesh_set_vertex_distribution", _py_mesh_set_vertex_distribution);
      ("mesh_plotinfo",_py_mesh_plotinfo);
      ("mesh_plotinfo_points",_py_mesh_plotinfo_points);
      ("mesh_plotinfo_pointsregions",_py_mesh_plotinfo_pointsregions);
      ("mesh_plotinfo_simplices",_py_mesh_plotinfo_simplices);
      ("mesh_plotinfo_simplicesregions",_py_mesh_plotinfo_simplicesregions);
      ("mesh_plotinfo_links",_py_mesh_plotinfo_links);
      ("mesh_plotinfo_surfaces_and_surfacesregions",_py_mesh_plotinfo_surfaces_and_surfacesregions);
      ("mesh_plotinfo_regionvolumes",_py_mesh_plotinfo_regionvolumes);
      ("mesh_plotinfo_periodic_points_indices",_py_mesh_plotinfo_periodic_points_indices);
      ("mesh_writefile",_py_mesh_writefile);
      ("mesh_readfile",_py_mesh_readfile);
      ("mesh_from_points_and_simplices",_py_mesh_from_points_and_simplices);
      ("mesh_scale_node_positions",_py_mesh_scale_node_positions);
      (* Only to get timings: *)
      ("mesh_grow_bookkeeping_data_all",_py_mesh_grow_bookkeeping_data_all);
      (* -- *)
      ("copy_mesher_defaults", _py_copy_mesher_defaults);

      (* functions used in the sole persson-strang mesher *)
      ("mesher_defaults_set_movement_max_freedom", _py_mdefaults_set_movement_max_freedom);
      ("mesher_defaults_set_shape_force_scale", _py_mdefaults_set_shape_force_scale);
      ("mesher_defaults_set_volume_force_scale", _py_mdefaults_set_volume_force_scale);
      ("mesher_defaults_set_neigh_force_scale", _py_mdefaults_set_neigh_force_scale);
      ("mesher_defaults_set_irrel_elem_force_scale", _py_mdefaults_set_irrel_elem_force_scale);

      ("mesher_defaults_set_thresh_add", _py_mdefaults_set_thresh_add);
      ("mesher_defaults_set_thresh_del", _py_mdefaults_set_thresh_del);
      ("mesher_defaults_set_initial_settling_steps", _py_mdefaults_set_initial_settling_steps);
      ("mesher_defaults_set_sliver_correction", _py_mdefaults_set_sliver_correction);
      ("mesher_defaults_set_max_relaxation_steps", _py_mdefaults_set_max_relaxation_steps);

      (* functions used in the lennard-jones and persson-strang meshers *)
      ("mesher_defaults_set_topology_threshold", _py_mdefaults_set_topology_threshold);
      ("mesher_defaults_set_smallest_allowed_volume_ratio", _py_mdefaults_set_smallest_allowed_volume_ratio);
      ("mesher_defaults_set_time_step_scale", _py_mdefaults_set_time_step_scale);
      ("mesher_defaults_set_tolerated_rel_movement", _py_mdefaults_set_tolerated_rel_movement);

      (* functions used in the sole lennard-jones mesher *)
      ("mesher_defaults_set_initial_points_volume_ratio", _py_mdefaults_set_initial_points_volume_ratio);
      ("mesher_defaults_set_splitting_connection_ratio", _py_mdefaults_set_splitting_connection_ratio);
      ("mesher_defaults_set_exp_neigh_force_scale", _py_mdefaults_set_exp_neigh_force_scale);
      (* -- *)

      ("make_mg_gendriver",_py_make_mg_gendriver); (* XXX needs a better name! *)
      ("mesher_extract_mesh",_py_mgo_extract_mesh); (* XXX presumably, also obsolete, just as all mgo stuff! *)
      ("mesh_bodies_raw",_py_mesh_bodies_raw); (* XXX Needs a python-side wrapper "mesh_bodies"! *)
      (* -- *)
      ("mesh2d_ps",_py_mesh2d_ps);
      (* -- *)
      ("body_box",_py_body_box);
      ("body_ellipsoid",_py_body_ellipsoid);
      ("body_helix",_py_body_helix);
      ("body_frustum",_py_body_frustum);
      ("body_union",_py_body_csg "body_union" body_union_n);
      ("body_intersection",_py_body_csg "body_intersection" body_intersection_n);
      ("body_difference",_py_body_difference);
      ("body_shifted_bc",_py_body_shifted_bs "body_shifted_bc" true);
      ("body_shifted_sc",_py_body_shifted_bs "body_shifted_sc" false);
      ("body_rotated_bc",_py_body_rotated "_py_body_rotated_bc" true);
      ("body_rotated_sc",_py_body_rotated "_py_body_rotated_sc" false);
      ("body_rotated_axis_bc",_py_body_rotated_axis "_py_body_rotated_axis_bc" true);
      ("body_rotated_axis_sc",_py_body_rotated_axis "_py_body_rotated_axis_sc" false);
      ("body_scaled",_py_body_scaled "_py_body_scaled");
      (* ^ NOTE: one may think about wrapping this up as a python class method...
	 But then again, maybe not: a geometry specification conceptually
	 isn't really a class.

	 Note that we provide shifting both wrt body coords and wrt space coords!
       *)
      (* -- *)
      ("make_element",_py_make_element); (* name, max_indices, dimension, order *)
      ("fuse_elements",_py_fuse_elements);
      ("element_to_string",_py_element_to_string);
      ("mwe_to_string",_py_mwe_to_string);
      ("make_mwe",_py_make_mwe); (* name, mesh, [(body_nr,elem),...] *)
      ("mwe_sibling",_py_mwe_sibling);
      ("field_alias",_py_field_alias);
      ("cofield_alias",_py_cofield_alias);
      ("field_cofield_inner_product",_py_field_cofield_inner_product);
      ("get_mwe",_py_get_mwe);
      ("mwe_norm_fun",_py_mwe_norm_fun);
      ("mwe_norm_dist_fun",_py_mwe_norm_dist_fun);
      ("data_length",_py_data_length);
      ("data_doftypes",_py_data_doftypes);
      ("data_dofvector_from_field",_py_data_dofvector_from_field);
      ("set_field_at_site",_py_set_field_at_site);
      ("set_field_uniformly", _py_set_field_uniformly);
      ("mwe_subfield_metadata",_py_mwe_subfield_metadata);
      ("mwe_subfield_data",_py_mwe_subfield_data);
      ("field_entry_wise",_py_field_entry_wise);

      ("plot_scalar_field",_py_plot_scalar_field); (* field dof_stem filename color_scheme plot_order offset+scale *)
      ("raw_make_field",_py_raw_make_field_or_cofield true);
      ("raw_make_cofield",_py_raw_make_field_or_cofield false);
      ("cofield_to_field",_py_cofield_to_field);
      ("field_to_cofield",_py_field_to_cofield);
      ("field_push",_py_field_push);
      ("field_pull",_py_field_pull);
      ("cofield_push",_py_cofield_push);
      ("cofield_pull",_py_cofield_pull);
      ("set_field",_py_set_field);
      ("integrate_field",_py_integrate_field);
      ("probe_field",_py_probe_field);
      (* -- *)
      ("site_wise_applicator", _py_site_wise_applicator);
      ("field_zero", _py_fcfield_zero true);
      ("cofield_zero", _py_fcfield_zero false);
      ("field_scale", _py_fcfield_scale true);
      ("cofield_scale", _py_fcfield_scale false);
      ("field_copy_into", _py_fcfield_copy_into true);
      ("cofield_copy_into", _py_fcfield_copy_into false);
      ("field_axpby", _py_fcfield_axpby true);
      ("cofield_axpby", _py_fcfield_axpby false);
      ("parse_physical_dimensions",_py_parse_physical_dimensions);
      (* -- *)
      (* ("anisotropy_ccode", _py_anisotropy_ccode); XXX obsolete! *)
      (* -- Mesh and MWE refinement -- *)
      ("finer_mesh_from_coarse_mesh",_py_finer_mesh_from_coarse_mesh);
      ("make_coarse_fine_mwe",_py_make_coarse_fine_mwe);
      ("lam_get_ccpla_resources",_py_lam_get_ccpla_resources);
      ("cvode_set_max_num_steps",_py_cvode_set_max_num_steps);
      ("cvode_set_init_step",_py_cvode_set_init_step);
      ("cvode_set_min_step",_py_cvode_set_min_step);
      ("cvode_set_max_step",_py_cvode_set_max_step);
      ("cvode_get_num_steps",_py_cvode_get_num_steps);
      ("cvode_set_tolerances",_py_cvode_set_tolerances);
      ("cvode_reinit",_py_cvode_reinit);
      (*("raw_cvode_advance",_py_raw_cvode_advance);*)
      ("cvode_get_step_info",_py_cvode_get_step_info);
      ("cvode_get_stats",_py_cvode_get_stats);
      ("cvode_get_current_time", _py_cvode_get_current_time);
      ("get_nsim_sundials_library_path", _py_get_nsim_sundials_library_path);
      (* -- DDD -- *)
      ("mumag_vector_field_max_angles",_py_mumag_vector_field_max_angles);
      (* --- The new linalg_machine --- *)
      ("raw_make_lam",_py_raw_make_lam);
      ("lam_execute",_py_execute_linalg_machine);
      ("lam_get_timers",_py_lam_get_timers);
      ("lam_ts_init",_py_lam_ts_init);
      ("lam_ts_advance",_py_lam_ts_advance);
      ("lam_get_ts_cvode",_py_lam_get_ts_cvode);
      ("lam_get_ts_timings",_py_lam_get_ts_timings);
      ("lam_set_ksp_tolerances",_py_lam_set_ksp_tolerances);
      ("lam_get_ksp_tolerances",_py_lam_get_ksp_tolerances);
      ("lam_bem_field_strength",_py_lam_bem_field_strength);
      ("lam_get_field",_py_lam_get_set_field_cofield true true);
      ("lam_set_field",_py_lam_get_set_field_cofield false true);
      ("lam_get_cofield",_py_lam_get_set_field_cofield true false);
      ("lam_set_cofield",_py_lam_get_set_field_cofield false false);
      ("lam_get_iparam",_py_lam_get_iparam);
      ("lam_set_iparam",_py_lam_set_iparam);
      (* --- deprecated lam functions provided for compatibility --- *)
      ("linalg_machine_set_field", _py_lam_get_set_field_cofield false true);
      ("linalg_machine_get_field", _py_lam_get_set_field_cofield true true);
      ("linalg_machine_initialize_timestepper", _py_lam_ts_init);
      ("linalg_machine_advance_timestepper", _py_lam_ts_advance);
      (* --- --- *)
      ("nsim_anisotropy_equations",_py_nsim_anisotropy_equations);
      ("oommf_interpolator",_py_oommf_interpolator);
      ("time_vmem_rss", _py_time_vmem_rss);
      ("memory_footprint", _py_memory_footprint);
      ("ml_heap_footprint", _py_ml_heap_footprint);
      (* --- --- *)
      ("debug_absorb_list", _py_debug_absorb_list);
    |]
;;


(* If given a file to run, we run that. Otherwise,
   we treat argv[0] as magical: if we are called as "pyfem", we
   run python, if we are called as "ipyfem", we run ipython.
   Otherwise, we do not start a python interpreter.
 *)




(*****************************************************************************
 * HERE THE MAIN PROGRAM STARTS                                              *
 *****************************************************************************)

(*
let () = Gc.set
  {Gc.minor_heap_size = 2097152; (* was: 32768 *)
   Gc.major_heap_increment = 524288; (* was: 61440 *)
   Gc.space_overhead = 80; Gc.verbose = 0; Gc.max_overhead = 500;
   Gc.stack_limit = 262144}
;;
*)

let () =
  let argv = !effective_argv in
  (* let () = Printf.printf "DDD effective_argv=%s\n%!" (string_array_to_string argv) in *)
  let () = set_python_argv (array_one_shorter argv 0) in
  if Array.length argv>1
  then python_load(argv.(1))
  else
    if
      (try
	let _ = Str.search_forward (Str.regexp "/ipyfem") argv.(0) 0 in true
      with | Not_found -> false)
    then ignore(Pycaml.ipython())
    else if
      (try
	let _ = Str.search_forward (Str.regexp "/pyfem") argv.(0) 0 in true
      with | Not_found -> false)
    then ignore(Pycaml.python())
    else ()
;;





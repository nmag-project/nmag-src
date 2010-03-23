(* (C) 2009 Dr. Thomas Fischbacher

   Python interface to the dependency-driven parallel
   linear algebra module.

*)

(* We need Python access to the building blocks of ddpla opcodes.
   Also, we need pills for some of the things to be found within
   those opcodes, such as distributed resource handles.
 *)

let pysym_drh="DDPLA: Distributed Resource Handle";;
let pysym_oa="DDPLA: Opcode Argument";;

open Pycaml;;
open Ddpla;;

let () = register_ocamlpill_types
    [|pysym_drh;
      pysym_oa
    |]
;;

let (ocamlpill_from_drh,drh_from_ocamlpill) = 
  make_ocamlpill_wrapper_unwrapper
    pysym_drh
    (DRH ("",ref None))
;;

let (ocamlpill_from_drh,drh_from_ocamlpill) = 
  make_ocamlpill_wrapper_unwrapper
    pysym_drh
    (DRH ("",ref None))
;;

let (ocamlpill_from_oa,oa_from_ocamlpill) = 
  make_ocamlpill_wrapper_unwrapper
    pysym_oa
    (OA_bool true)
;;

let _ht_oa_special_translators = Hashtbl.create 17;;

let add_oa_special_translator name f =
  Hashtbl.replace _ht_oa_special_translators name f
;;

let rec _populate_ht_oa_special_translators () =
  begin
    add_oa_special_translator ":REBUILD"
      (fun name data ->
	try
	  let OA_array cmd_array = oa_from_python data in
	  let dscript = 
	    Array.map
	      (fun (OA_array operation) ->
		let OA_string opcode = operation.(0) in
		let args =
		  Array.map
		    (fun (OA_array arg_val) ->
		      let OA_string argname = arg_val.(0) in
		      let v = arg_val.(1) in
		      (argname,v))
		      (Array.sub operation 1 (Array.length operation-1))
		in
		{dc_opcode=opcode; dc_args=args})
	      cmd_array
	  in OA_array [|OA_string name;OA_buildscript dscript|]
	with
	| _ -> failwith
	      (Printf.sprintf
		 "Could not translate :REBUILD script %s"
		 (py_repr data)));
    (* ---------- *)
    (* We may also want to introduce special handling for :OPTARGS,
       so that we then can also pass [(name,value),(name,value)]
       lists and arrays, rather than just dictionaries.
       
       | ":OPTARGS" -> ...
     *)
    (* This is extremely wacky... *)
    add_oa_special_translator ":PYTHON"
      (fun name data ->
	let fail () =
	  failwith "Usage of :PYTHON args: (\":PYTHON\",{\"VALUE\":xyz,\"DEPS\":[drh1,drh2,...]})"
	in
	let () = (if pytype data <> DictType then fail () else ()) in
	let a_keys = pylist_toarray (pydict_keys data) in
	let s_keys =
	  Array.map
	    (fun k -> if pytype k <> StringType then fail() else pystring_asstring k)
	    a_keys
	in
	let () = Array.sort compare s_keys in
	let () = (if s_keys <> [|"DEPS";"VALUE"|] then fail() else ()) in
	let the_deps = pydict_getitem(data,pystring_fromstring "DEPS") in
	let the_val = pydict_getitem(data,pystring_fromstring "VALUE") in
	let () = (if pytype the_deps <> ListType then fail() else ()) in
	let v_deps =
	  Array.map 
	    (fun dep -> 
	      if pytype dep <> CamlpillType
	      then fail ()
	      else drh_from_ocamlpill dep)
	    (pylist_toarray the_deps)
	in
	OA_array [|OA_string name;
		   OA_complex_data (Obj.magic the_val, "PYTHON", v_deps)
		 |]
      );
  end
and oa_from_python_special_named_pair name data =
  let xlator =
    try
      Hashtbl.find _ht_oa_special_translators name 
    with
    | Not_found ->
	(fun name data ->
	  let arg:opcode_argument = oa_from_python data in
	  OA_array [|OA_string name;arg|])
  in xlator name data
and process_list_tuple py_z =
  let arr =
    match pytype py_z with
    | ListType -> pylist_toarray py_z
    | TupleType -> pytuple_toarray py_z
    | _ -> failwith
	  (Printf.sprintf "Expected list/tuple, got: %s" (pytype_name (pytype py_z)))
  in
  OA_array (Array.map oa_from_python arr)
and oa_from_python py_x = 
  let pt = pytype py_x in
  let ddd = Printf.printf "DDD obj[%s] %s\n%!" (pytype_name pt) (py_repr py_x) in
  match pt with
  | BoolType -> OA_bool (pybool_asbool py_x)
    (* Note: There still is some issue with pycaml's
       bool/int handling - to be addressed! *)
  | IntType -> OA_int (pyint_asint py_x)
  | FloatType -> OA_float (pyfloat_asdouble py_x)
  | StringType -> OA_string (pystring_asstring py_x)
  | CamlpillType ->
      let pt = ocamlpill_type_of py_x in
      (
       match pt with
       | pysym_drh ->
	   let drh = drh_from_ocamlpill py_x in
	   OA_resource drh
       | _ ->
	   failwith 
	     (Printf.sprintf "oa_from_python: don't know how to handle pill for '%s'." pt))
  (* We face a slight issue here: in some respects, Python's type universe is
     less fine-grained than Ddpla's. How do we deal with that?

     Idea: in the translation, we treat "special named pair" tuples as special.
     A "special named pair" is a 2-tuple whose left entry is a string that
     starts with a colon ":".

     Here, some keywords are "hot" in the sense that they translate
     the whole tuple by a specialist function, rather than the
     default way.
   *)
  | ListType -> process_list_tuple py_x
  | TupleType -> 
      let ddd = Printf.printf "DDD translating tuple %s\n%!" (py_repr py_x) in
      if (pytuple_size py_x = 2) 
      then
	let arr = pytuple_toarray py_x in
	if pytype arr.(0) = StringType
	then
	  let n = pystring_asstring arr.(0) in
	  let ddd = Printf.printf "DDD tuple name='%s'\n%!" n in
	  if (String.length(n)>0) && (n.[0] = ':')
	  then
	    oa_from_python_special_named_pair n arr.(1)
	  else process_list_tuple py_x
	else process_list_tuple py_x
      else process_list_tuple py_x
  | DictType -> 
      let a_kv = 
	Array.map 
	  (fun py_kv -> 
	    let kv = pytuple_toarray py_kv in
	    let k = pystring_asstring kv.(0) in
	    let v = oa_from_python kv.(1) in
	    (k,v))
	  (pylist_toarray (pydict_items py_x))
      in
      OA_assoc_array a_kv
  | _ -> failwith
	(Printf.sprintf
	   "oa_from_python: don't know how to handle '%s' (type '%s')"
	   (py_repr py_x) (pytype_name (pytype py_x)))
;;

let rec oa_to_python oa = 
  match oa with
  | OA_bool x -> if x then py_true else py_false
  | OA_int x -> pyint_fromint x
  | OA_float x -> pyfloat_fromdouble x
  | OA_string x -> pystring_fromstring x
  | OA_resource x -> ocamlpill_from_drh x
  | OA_array x -> pylist_fromarray (Array.map oa_to_python x)
  | _ -> pystring_fromstring
	(Printf.sprintf "[TODO: ARG NOT MAPPED - was: %s]"
	   (oa_to_string oa))
;;
	   

let dscript_from_python py_x =
  let translate arr =
    let translated = 
      Array.mapi
	(fun nr_cmd py_op ->
	  let () =
	    (
	     if (
	         (pytype py_op <> TupleType) || (pytuple_size py_op < 1)
	         || (pytype (pytuple_getitem(py_op,0)) <> StringType)
	      )
	     then
	       (failwith 
		  (Printf.sprintf 
		     "Build-script Instruction #%d: Not an Op-Code: %s"
		     (1+nr_cmd) (py_repr py_op)))
	     else ())
	  in
	  let arr = pytuple_toarray py_op in
	  let opcode = pystring_asstring arr.(0) in
	  let op_args = Array.sub arr 1 ((Array.length arr)-1) in
	  let args = 
	    Array.mapi
	      (fun nr_arg py_kv ->
		try
		  let OA_array [|OA_string key;oa_arg|] = oa_from_python py_kv in
		  (key,oa_arg)
		with
		| _ ->
		    (failwith 
		       (Printf.sprintf
			  "Build-script Instruction #%d: Op-Code '%s': broken parameter #%d: '%s'"
			  (1+nr_cmd) opcode (1+nr_arg) (py_repr py_kv))))
	      op_args
	  in
	  {dc_opcode=opcode;
	   dc_args=args})
	arr
    in 
    translated
  in
  match pytype py_x with
  | ListType -> translate (pylist_toarray py_x)
  | TupleType -> translate (pytuple_toarray py_x)
  | _ -> failwith
	(Printf.sprintf "dscript_from_python: arg is not a dscript: '%s'"
	   (py_repr py_x))
;;

let pyddpla_add_opcodes ddpla =
  begin
    (* -------------------- *)
    (Printf.printf "DDD Defining pyddpla extension opcodes!\n%!");
    _def_opcode
      ~ddpla
      ~opcode:"DEF-PYTHON-OPCODE"
      ~args:[|":NAME";":ARGS";":MODULE";":MODULEFUNCTION"|]
      (* XXX TODO: :EXEC parameter must be a module-path, e.g.
	 "nmag.opcodes.op_inspect_M" - only this way, we can 
	 guarantee that we get what we want. After all, we cannot
	 send Python closures over the net!
       *)
      (fun ~report_error () ->
	fun args ->
	  let newop_name =
	    oa_string ~report_error ~opcode_name:"DEF-PYTHON-OPCODE" ~param_name:":NAME" args.(0)
	  in
	  let newop_args =
	    oa_string_array ~report_error ~opcode_name:"DEF-PYTHON-OPCODE" ~param_name:":ARGS" args.(1)
	  in
	  let newop_module =
	    oa_string ~report_error ~opcode_name:"DEF-PYTHON-OPCODE" ~param_name:":MODULE" args.(2)
	  in
	  let newop_modulefun =
	    oa_string ~report_error ~opcode_name:"DEF-PYTHON-OPCODE" ~param_name:":MODULEFUNCTION" args.(3)
	  in
	  let py_fun = python_object_from_module newop_module newop_modulefun in
	  if pytype py_fun <> CallableType
	  then 
	    (* XXX TODO: use report_error! *)
	    Printf.fprintf stderr "PROBLEM: py_fun not a callable - have: %s\n%!" 
	      (py_repr py_fun)
	  else
	    let newop_exec = 
	      (fun ~report_error () ->
		fun args ->
		  let py_args = Array.map oa_to_python args in
		  let _ =
		    pyeval_callobject(py_fun,
				      pytuple_fromarray py_args
				     )
		  in
		  ())
	    in
	    let _ =
	      report_error
		~level:"DEBUG"
		(Printf.sprintf "Registering PYTHON opcode '%s'." newop_name)
	    in
	    _def_opcode
	      ~ddpla
	      ~opcode:newop_name
	      ~args:newop_args
	      newop_exec
      );
    (* -------------------- *)
    (
     let all_opcodes = Hashtbl.fold (fun k v sf -> k::sf) ddpla.ddpla_opcodes [] in
     let () = Printf.printf "=== ALL KNOWN OP-CODES ===\n %s\n==========\n%!"
	 (String.concat "\n " all_opcodes)
     in ()
    )
  end
;;

(* As this will eventually supersede pyfem3, it has to come
   with its own startup code.

   This code is not brilliant, basically just a copy of the old pyfem3
   start-up code. Does the job, for now, but needs some serious
   re-design.
*)

let _pyddpla_was_initialized = ref false;;

let init = 
  let the_ddpla = ref None in
  fun () ->
    if !_pyddpla_was_initialized
    then (match !the_ddpla with | Some x -> x | None -> failwith "Impossible!")
    else
      (
       let () = _populate_ht_oa_special_translators () in
       let initial_working_directory = Unix.getcwd() in
       let mpi_prefix = "-p4" in
       let args = Sys.argv in
       let (mpi_args,non_mpi_args) =
	 (* function from snippets - duplicated here to make this module self-contained *)
	 let string_compare_n n str1 str2 =
	   let len1 = String.length str1
	   and len2 = String.length str2
	   in
	   let rec walk pos =
	     if pos = n then true
	     else 
	       if pos = len1
	       then (pos=len2)
	       else
		 if pos = len2
		 then false
		 else
		   if str1.[pos] = str2.[pos]
		   then walk (1+pos)
		   else false
	   in walk 0
	 in
	 let rec scan pos mpi non_mpi =
	   if pos=Array.length args 
	   then (List.rev mpi,List.rev non_mpi)
	   else
	     let arg = args.(pos) in
	     if (string_compare_n
		   (String.length mpi_prefix)
		   mpi_prefix arg) && pos<Array.length args-1
	     then scan (2+pos) ((args.(pos),args.(pos+1))::mpi) non_mpi
	     else scan (1+pos) mpi (args.(pos)::non_mpi)
	 in
	 scan 1 [] []
       in
       let effective_argv=Array.of_list (Sys.argv.(0)::non_mpi_args)
       in
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
       in
       let _ = Mpi_petsc.mpi_init args in
       let p = Mpi_petsc.petsc_init args petscrc_file "help-message" true in (* XXX! *)
       (* Upon initialization, PETSc (stupidly!) seems to change our cwd! Bug! *)
       let () = Sys.chdir initial_working_directory in
       let parent_communicator = Mpi_petsc.comm_dup Mpi_petsc.comm_world
	   (* XXX Use this instead? (Mpi_petsc.petsc_get_comm_world()) *) 
       in
       let ddd =
	 Printf.fprintf stderr "MPI/PETSc initialized [%d/%d]\n%!" 
	   (Mpi_petsc.comm_rank parent_communicator) (Mpi_petsc.comm_size parent_communicator)
       in
       let ddpla =
	 Ddpla.ddpla_create
	   (* Args:
	      ?(fun_error=default_ddpla_error)
	      ?(name="the_ddpla")
	      ~parent_communicator
	      ~fun_quit ()
	    *)
	   ~parent_communicator
	   ~fun_quit:
	   (fun () ->
	     begin
	       Printf.fprintf stderr
		 "NOTE: pyddpla.ml fun_quit() should do a clean shut-down of the ddpla and MPI/PETSC!\n%!";
	       exit 0;
	     end
	   ) ()
       in
       let myrank = Mpi_petsc.comm_rank parent_communicator in
       let () = 
	 (* Here, slave nodes enter slave mode as they execute the start-up code: *)
	 (if myrank >0 then slave_mode ddpla else ())
       in
       let () = pyddpla_add_opcodes ddpla in
       let () = the_ddpla := Some ddpla in
       let () = _pyddpla_was_initialized := true in
       ddpla)
;;

let _pyddpla_exec =
  python_pre_interfaced_function
    ~doc:"Exec a command sequence across all cluster nodes"
    [|ListType|]
    (fun args ->
      let dscript = dscript_from_python args.(0) in
      let ddpla = init() in
      let _ = ddpla_exec ddpla dscript in
      pynone())
;;

let _pyddpla_snatchable =
  python_pre_interfaced_function
    ~doc:"Produce a list of newly constructed resources that wait to be collected."
    [||]
    (fun args ->
      let ddpla = init() in
      let x = Array.of_list (ddpla_names_of_unsnatched_resources ddpla) in
      pylist_fromarray
	(Array.map pystring_fromstring x))
;;


let _pyddpla_snatch_new =
  python_pre_interfaced_function
    ~doc:"Snatch a newly constructed ddpla resource that waits to be collected."
    [|StringType|]
    (fun args ->
      let res_name = pystring_asstring args.(0) in
      let ddpla = init() in
      let drh = ddpla_snatch_new ddpla res_name in
      ocamlpill_from_drh drh)
;;

let _pyddpla_drh_name =
  python_pre_interfaced_function
    ~doc:"Given a distributed resource handle (DRH), get the resource's name."
    [|CamlpillType|]
    (fun args ->
      let (DRH (name,_)) = drh_from_ocamlpill args.(0) in
      pystring_fromstring name)
;;

let _pyddpla_drh_ensure_uptodate =
  python_pre_interfaced_function
    ~doc:"Given a distributed resource handle (DRH), ensure it is up-to-date (across all nodes)."
    [|CamlpillType|]
    (fun args ->
      let ddpla = init() in
      let drh = drh_from_ocamlpill args.(0) in
      let dscript = 
	[|
	  {dc_opcode="CLEAN";
	   dc_args=[|(":RESOURCES",OA_array [|OA_resource drh|])|];
	 }
	|]
      in
      let _ = ddpla_exec ddpla dscript in
      pynone())
;;
  
let _pyddpla_drh_spoil_uptodate =
  python_pre_interfaced_function
    ~doc:"Given a distributed resource handle (DRH), spoil its up-to-date status (across all nodes)."
    [|CamlpillType|]
    (fun args ->
      let ddpla = init() in
      let drh = drh_from_ocamlpill args.(0) in
      let dscript = 
	[|
	  {dc_opcode="DIRTY";
	   dc_args=[|(":RESOURCES",OA_array [|OA_resource drh|])|];
	 }
	|]
      in
      let _ = ddpla_exec ddpla dscript in
      pynone())
;;

let _pyddpla_dependencies =
  python_pre_interfaced_function
    ~doc:"Produce all the inter-dependency relations of DDPLA resources."
    [||]
    (fun args ->
      let ddpla = init() in
      let deps = known_dependencies ddpla in
      pylist_fromarray
	(Array.map
	   (fun (parent,childs) ->
	     pytuple2(pystring_fromstring parent,
		      pylist_fromarray (Array.map pystring_fromstring childs)))
	   deps))
;;

let _ddpla_pyeval_on_drh_msvec =
  python_pre_interfaced_function
    ~doc:"Eval a Python function which takes a numarray on the vector of a DRH"
    [|CallableType;
      CamlpillType
    |]
    (fun args ->
      let ddpla = init() in
      let py_fun = args.(0) in
      let drh = drh_from_ocamlpill args.(1) in
      let vec = get_msvec ddpla drh in
      let r_result = ref None in
      let () = Mpi_petsc.with_petsc_vector_as_bigarray vec
	  (fun ba ->
	    let dim = Bigarray.Array1.dim ba in
	    let py_result = 
	      pyeval_on_ba_vector py_fun ba
	    in
	    r_result := Some py_result)
      in
      match !r_result with
      | None -> failwith "Impossible!"
      | Some x -> x)
;;
  
  
let _ = 
  register_pre_functions_for_python
    [|
      ("ddpla_exec",_pyddpla_exec);
      ("ddpla_snatchable",_pyddpla_snatchable);
      ("ddpla_snatch_new",_pyddpla_snatch_new);
      ("ddpla_drh_name",_pyddpla_drh_name);
      ("ddpla_drh_ensure_uptodate",_pyddpla_drh_ensure_uptodate);
      ("ddpla_drh_spoil_uptodate",_pyddpla_drh_spoil_uptodate);
      ("ddpla_dependencies",_pyddpla_dependencies);
      (* --- *)
      ("ddpla_pyeval_on_drh_msvec",_ddpla_pyeval_on_drh_msvec);
    |]
;;

(*
export PYTHONPATH=$PYTHONPATH:`pwd`
ocaml

#use "topfind";;
#require "pycaml";;
#require "nsim_ddpla";;
#require "pyddpla";;
Pycaml.python();;

# === Python ===
# ocaml.ddpla_exec([("HELLO",)])

ocaml.ddpla_exec([("HELLO",),("HELLO",)])

ocaml.ddpla_exec([("VEC-CREATE", (":NAME","vec1"), (":SIZES",[100]), (":REBUILD",[]))])
ocaml.ddpla_snatchable()
vec1_ms=ocaml.ddpla_snatch_new("vec1.ms")
print ocaml.ddpla_drh_name(vec1_ms)

ocaml.ddpla_exec([("VEC-CREATE", (":NAME","vec2"), (":SIZES",[100]), (":REBUILD",[]))])
print ocaml.ddpla_dependencies()

# GIVES:
# [('vec1.ms', ['vec1.ms', 'vec1.p']), ('vec1.p', ['vec1.p']), ('vec2.ms', ['vec2.ms', 'vec2.p']), ('vec2.p', ['vec2.p'])]
#
# This, then, basically is just what we want. (For each parent, all
# childs are listed. Just what we need for graphviz plotting!)

ocaml.ddpla_exec([("DEF-PYTHON-OPCODE",
                   (":NAME","PY-GREETING"),
                   (":ARGS",[":MESSAGE"]),
                   (":MODULE","pyddpla_opcodes"),
                   (":MODULEFUNCTION","op_greeting"),
                  )])

ocaml.ddpla_exec([("DEF-PYTHON-OPCODE",
                   (":NAME","PY-INSPECT-MSVEC"),
                   (":ARGS",[":TAG",":VEC"]),
                   (":MODULE","pyddpla_opcodes"),
                   (":MODULEFUNCTION","op_inspect_msvec"),
                  )])


ocaml.ddpla_exec([("PY-GREETING",(":MESSAGE","Good day, Sir!"))])

ocaml.ddpla_exec([("PY-INSPECT-MSVEC",
                   (":TAG","Inspecting vec1"),
                   (":VEC",vec1_ms))])

ocaml.ddpla_exec([("CREATE-MESH",
                    (":NAME","the_mesh"),
                    (":FILENAME-ON-MASTER","../nsim_ddpla/example.nmesh"),
                    (":RE-ORDER",0))])

mesh=ocaml.ddpla_snatch_new("the_mesh")

*)

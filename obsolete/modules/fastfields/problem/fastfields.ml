(* (C) 2006 Dr. Thomas Fischbacher *)

open Bigarray;;

exception Fastfields_eval_problem;;

type c_funptr;;
type dl_handle;;

type c_field = string * string * (c_funptr array);;
(* symbol, code, c pointer *)
(* Exported as an opaque type. Note that the array has length 1,
   and will just store a single pointer.
 *)

type c_manipulator = string * string * (c_funptr array);;
(* Basically, this is just the same, but with double** arguments that
   are supposed to be used ONLY through special #define macros!
 *)

module HashedFundesc =
  struct
    type t = c_field (* symbol, code, c pointer *)
    let equal (x,_,_) (y,_,_) = (x=y)
    let hash (x,_,_) =
      let len = String.length x in
      let q3 = String.make 3 '\000' in
      let rec walk pos =
	if pos=len then
	  ((Char.code q3.[0]) lsl 24)
	    + ((Char.code q3.[1]) lsl 24)
	    + ((Char.code q3.[2]) lsl 24)
	else
	  let n = pos mod 3 in
	  let old_val = Char.code q3.[n] in
	  let () = q3.[n] <- Char.chr (Char.code x.[pos] lxor old_val) in
	  walk (pos+1)
      in 
      let result = walk 0 in
      let () = Printf.printf "Hashed string '%s' to %d\n%!" x result
      in result
  end
;;

module WeakFundescs = Weak.Make(HashedFundesc);;

let _cc_debug_handle = ref (None:(out_channel option));;

let opt_directory=ref "/tmp/ocaml-fastfields.XXXXXX";;

let the_directory=ref None;;
 (* Also serves to check if we are initialized *)

let the_lib_handle = ref None;;

let need_relink = ref false;;
 (* Will be set once we define our first function *)

let opt_cc = ref "gcc";;
let opt_cc_opts = ref "-O3 -fomit-frame-pointer";;
let opt_includes = ref ["#include <stdio.h>\n";"#include <math.h>\n"];;
(* stdio and math will be needed virtually always... *)

let active_functions = WeakFundescs.create 10;; (* A weak hash table *)

external c_null_funpointer: unit -> c_funptr = "ocaml_c_fastfield_null_funpointer";;

external dl_open: string -> dl_handle = "ocaml_c_fastfield_dlopen";;
external dl_close: dl_handle -> unit = "ocaml_c_fastfield_dlclose";;
external dl_sym: dl_handle -> string -> c_funptr = "ocaml_c_fastfield_dlsym";;

external c_fastfield_eval:
c_funptr -> float array -> float array -> bool =
  "ocaml_c_fastfield_eval";;

external c_fastfield_eval_bigarray:
c_funptr ->
  (float, float64_elt, c_layout) Array1.t ->
    (float, float64_elt, c_layout) Array1.t ->
      bool =
      "ocaml_c_fastfield_eval_bigarray";;

external gcc_flags_shlibs: unit -> string = "gcc_flags_shlibs";;

let get_it x = 
  match x with
  | None -> failwith "Inconceivable!"
  | Some z -> z
;;

(* For conventions, see mkdtemp(3) *)

(* === Note: these directory functions should go into snippets.ml === *)

external mkdtemp: string -> string = "ocaml_c_wrapped_mkdtemp";;

type inode =
  | INO_Directory of string * inode array
  | INO_File of string
  | INO_Other of string;;

let try_and_cleanup f c =
  let f_value=ref None in
  try
    begin
      f_value := Some (f());
      c();
      !f_value
    end
  with
  | x ->
      begin
	(if !f_value = None
	then 
	  c ()
	else (* Exception was raised in cleanup! *)
	  raise x);
	!f_value
      end
;;

let dir_tree start_directory =
  let opt_readdir dir_handle =
    try
      Some (Unix.readdir dir_handle)
    with
    | End_of_file -> None
  in
  let rec walk_dir path dir_handle read_so_far =
    let next = opt_readdir dir_handle in
    match next with
    | None ->
	  Array.of_list (List.rev read_so_far)
    | Some name ->
	if (name = "." || name = "..")
	then
	  walk_dir path dir_handle read_so_far
	else
	  try
	    let extended_path=Printf.sprintf "%s/%s" path name in
	    let stat_data = Unix.lstat extended_path in
	    let this_ino =
	      match stat_data.Unix.st_kind with
	      | Unix.S_REG -> INO_File extended_path
	      | Unix.S_DIR -> get_dir extended_path
	      | _ -> INO_Other extended_path
	    in
	    walk_dir path dir_handle (this_ino::read_so_far)
	  with
	  | _ -> walk_dir path dir_handle read_so_far
		(* Just ignore errors *)
  and get_dir path =
    let d = Unix.opendir path in
    let some_subtree =
      try_and_cleanup
	(fun () ->
	  let d_tree = walk_dir path d [] in
	  INO_Directory (path,d_tree))
	(fun () -> Unix.closedir d)
    in
    (match some_subtree with
    | None -> failwith "Directory Problem"
    | Some x -> x)
  in
  get_dir start_directory
;;

let do_dir_tree_from_bottom dir_tree f =
  let rec walk subtree =
    match subtree with
    | INO_File x -> f false x
    | INO_Other x -> f false x
    | INO_Directory (x,contents) ->
	begin
	  Array.iter walk contents;
	  f true x
	end
  in
  walk dir_tree
;;

let rmdir_recursive path =
  let dt = dir_tree path in
  do_dir_tree_from_bottom dt
    (fun is_dir subpath -> 
      if is_dir
      then Unix.rmdir subpath
      else Unix.unlink subpath)
;;

let quick_write filename content =
  let oh = open_out filename in
  let () = Printf.fprintf oh "%s" content in
  close_out oh
;;

(* === === *)

(* Note that initialization and finishing functions are idempotent! *)

let rec
 init () =
  if !the_directory == None
  then
    begin
      the_directory:=Some (mkdtemp !opt_directory);
      at_exit finish;
    end
  else ()
and
 finish () =
  match !the_directory with
  | None -> ()
  | Some x ->
      begin
	rmdir_recursive x;
	the_directory:=None;
	match !the_lib_handle with
	| Some x -> 
	    begin
	      dl_close x;
	      the_lib_handle:=None
	    end
	| _ -> ()
      end
;;

let set_directory dir = opt_directory:=dir;;
let set_cc cc = opt_cc:=cc;;
let set_cc_opts cc_opts = opt_cc_opts:=cc_opts;;

let set_cc_debug_handle new_handle =
  let old_handle = !_cc_debug_handle in
  let () = _cc_debug_handle := new_handle
  in old_handle
;;

let register_include ?(library_file=true) name =
  let directive =
    if library_file
    then Printf.sprintf "#include <%s>\n" name
    else Printf.sprintf "#include \"%s\"\n" name
  in
  try
    let _ = List.find (fun x -> x = directive) !opt_includes
    in ()
  with
  | Not_found ->
      begin
	opt_includes:= directive :: !opt_includes;
	()
      end
;;

let do_compile () =
  begin
    let () = Printf.printf "DDD fastfields.ml - do_compile()\n%!" in
    let wd = Unix.getcwd () in
    let _ = 
      try_and_cleanup
	(fun () ->
	let cc_invocation =
	  Printf.sprintf 
	    "%s %s %s -o fastfields_dynlib.so fastfields_ccode.c"
	    !opt_cc (gcc_flags_shlibs ()) !opt_cc_opts
	in
	begin
	  Unix.chdir (get_it !the_directory);
	  (match !_cc_debug_handle with
	     | None -> ()
	     | Some h ->
		 begin
		   Printf.fprintf h "%s\n" cc_invocation;
		   flush h;
		 end);
	  ignore(Unix.system cc_invocation);
	end
	)
	(fun () -> Unix.chdir wd)
    in ()
  end
;;

let do_relink_if_necessary () =
  if not(!need_relink) then ()
  else
    let header = String.concat "" !opt_includes in
    let c_funs = ref [] in
    begin
      Gc.full_major ();
      (* This way we ensure we really cleanse any no longer used functions.
	 This is not just for tidiness or optimization, but also
	 matters semantically:
	 
	 When the user provides a C function which contains a
	 syntactical error, and then makes the system forget all
	 references to that errorneous definition, the system would
	 nevertheless fail to recompile the dynamic code unless
	 a GC run really removed that bogus entry.
	 
	 Hence, we simply place the Gc call here, to make sure this is
	 done automatically.
       *)
      Printf.printf "do_relink_if_necessary(). Have now %d C-functions\n%!" (WeakFundescs.count active_functions);
      WeakFundescs.iter
	(fun (sym,code,_) ->
	  c_funs:= code:: !c_funs)
	active_functions;
      let c_source = 
	String.concat "\n" (header:: "\n" :: !c_funs)
      in
      let d = get_it !the_directory in
      let c_name = Printf.sprintf "%s/fastfields_ccode.c" d in
      let so_name = Printf.sprintf "%s/fastfields_dynlib.so" d in
      begin
	quick_write c_name c_source;
	Printf.printf "DDD fastfields.ml - do_relink_if_necessary() - wrote C source\n===>\n%s\n<===\n\n%!" c_source;
	WeakFundescs.iter
	  (fun (sym,_,a_c_ptr) ->
	     let () = Printf.printf "DDD fastfields.ml - invalidating fun '%s'\n%!" sym in
	       a_c_ptr.(0) <- c_null_funpointer())
	  active_functions;
	(match !the_lib_handle with
	| None -> ()
	| Some x -> dl_close x);
	do_compile();
	the_lib_handle := Some (dl_open so_name);
	WeakFundescs.iter
	  (fun (sym,_,a_c_ptr) ->
	     let () = Printf.printf "DDD fastfields.ml - re-validating fun '%s'\n%!" sym in
	    a_c_ptr.(0) <- dl_sym (get_it !the_lib_handle) sym)
	  active_functions;
	need_relink := false;
      end
    end
;;

(* Note that we explicitly allow the user to
   (1) specify the names used for in and out arguments,
   and
   (2) specify #define directives
   (which MUST NOT conflict with any other
   #defines that are currently in use,
   say, from stdio.h) which are wrapped around the code.

   This way, it is possible to use the fastfields mechanism as a basis
   for more elaborate schemes to provide means to use fast
   run-time-compiled code to applications. 
 *)

let rec fun_counter = ref 0
and c_define_directives defines =
  String.concat ""
    (List.map
       (fun (name,args,def) ->
	 match args with
	 | [] ->
	     Printf.sprintf "#define %s %s\n" name def
	 | list_args ->
	     let str_args = String.concat "," list_args in
	     Printf.sprintf "#define %s(%s) %s\n" name str_args def
       ) defines)
and c_undef_directives defines =
  String.concat ""
    (List.map
       (fun (name,_,_) -> Printf.sprintf "#undef %s\n" name)
       defines)
and c_register_field
    ?(in_name="position")
    ?(out_name="result")
    ?(extra_defines=[])
    c_code =
  let () = init () in
  let nr = !fun_counter in
  let () = fun_counter:= nr+1 in
  let sym = Printf.sprintf "ocaml_fastfield_field_%d" nr in
  let () = Gc.finalise (fun s -> Printf.printf "DDD losing C sym '%s'\n%!" sym) sym in
  let define_directives = c_define_directives extra_defines in
  let undef_directives = c_undef_directives extra_defines in
  (* Note that we include a "return 1" at the end - the default is
     to finally announce that the calculation was successful.
     If one wants to early-abort, this can be done via "return 0;".
   *)
  let full_c_code =
    Printf.sprintf
      "%s\n\nint %s(double *%s, double *%s){\n%s\nreturn 1;\n}\n\n%s\n\n"
      define_directives
      sym in_name out_name c_code
      undef_directives
  in
  let () = need_relink := true in
  let the_c_field = (sym,full_c_code, [|c_null_funpointer()|]) in
  let () = WeakFundescs.add active_functions the_c_field in
  let () = Printf.printf "Added manipulator. Have now %d C-functions\n%!" (WeakFundescs.count active_functions) in
    the_c_field
and c_register_field_manipulator
    ?(buffer_name="__refbuf")
    ?(posbuffer_name="__position")
    ?(extra_defines=[])
    c_code =
  let () = Printf.printf "c_register_field_manipulator\nCODE\n%s\nEND CODE\n%!" c_code in
  let () = init () in
  let nr = !fun_counter in
  let () = fun_counter:= nr+1 in
  let sym = Printf.sprintf "ocaml_fastfield_manipulator_%d" nr in
  let () = Gc.finalise (fun s -> Printf.printf "DDD losing C sym '%s'\n%!" sym) sym in
  let define_directives = c_define_directives extra_defines in
  let undef_directives = c_undef_directives extra_defines in
  let full_c_code =
    Printf.sprintf
      "%s\n\nint %s(double **%s, double *%s){\n%s\nreturn 1;\n}\n\n%s\n\n"
      define_directives
      sym buffer_name posbuffer_name c_code
      undef_directives
  in
  let () = need_relink := true in
  let the_c_manipulator = (sym,full_c_code, [|c_null_funpointer()|]) in
  let () = WeakFundescs.add active_functions the_c_manipulator in
  let () = Printf.printf "Added manipulator. Have now %d C-functions\n%!" (WeakFundescs.count active_functions) in
  (* IMPORTANT: Note that it is up to the user to call do_relink_if_necessary()
     before using a manipulator!
  *)
  the_c_manipulator
;;

(* XXX semi-internal! This is to be used by libraries only! *)
let _c_manipulator_funptr ((_,_,a):c_manipulator) = a.(0)
;;

let c_field_evaluator_mapping_float_array_to_float_array
    ?(allocate_every_result=true) 
    dim_output
    c_field =
  let the_result =
    if allocate_every_result
    then None
    else Some (Array.make dim_output 0.0)
  in
  let fun_compute_me position =
    let output =
      if allocate_every_result
      then (Array.make dim_output 0.0)
      else
	match the_result with
	| Some x -> x
	| None -> failwith "Impossible!"
    in
    let c_funptr = 
      let () = do_relink_if_necessary () in
      let (_,_,x) = c_field in x.(0)
    in
    let success = c_fastfield_eval c_funptr position output in
    if success then output else raise Fastfields_eval_problem
  in
  fun_compute_me
;;


let c_field_evaluator_modifying_float_array
    c_field =
  let fun_compute_me ~position ~result =
    let c_funptr = 
      let () = do_relink_if_necessary () in
      let (_,_,x) = c_field in x.(0)
    in
    let success = c_fastfield_eval c_funptr position result in
    if success then () else raise Fastfields_eval_problem
  in
  fun_compute_me
;;

let c_field_evaluator_modifying_bigarray
    c_field =
  let fun_compute_me ~position ~result =
    let c_funptr = 
      let () = do_relink_if_necessary () in
      let (_,_,x) = c_field in x.(0)
    in
    let success = c_fastfield_eval_bigarray c_funptr position result in
    if success then () else raise Fastfields_eval_problem
  in
  fun_compute_me
;;

let version () = "$Id$";;

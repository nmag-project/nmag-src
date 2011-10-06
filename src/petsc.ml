(* === petsc interface (C) 2005, 2006, 2007 Dr. Thomas Fischbacher, Giuliano Bordignon === *)

open Bigarray;;
open Nsimconf;;

type simple_float_bigarray = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;;
type simple_nativeint_bigarray = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t;;

type vector;;

type matrix;;

type matstructure;;

type ksp;;

type matnullspace;;

type viewer;;

type timestepper;;

type matfdcoloring;;

type ts_problem_type = TS_LINEAR | TS_NONLINEAR;;

type ts_type = TS_EULER | TS_BEULER | TS_PSEUDO | TS_PVODE;;

type insert_mode_type = INSERT_VALUES | ADD_VALUES
;;

type matstructure_type =
    SAME_NONZERO_PATTERN | DIFFERENT_NONZERO_PATTERN | SAME_PRECONDITIONER | SUBSET_NONZERO_PATTERN
;;

let int_from_matstructure mst =
  match mst with
    DIFFERENT_NONZERO_PATTERN -> 0
  | SAME_NONZERO_PATTERN -> 1
  | SAME_PRECONDITIONER -> 2
;;

(* mf *)
type matoption_type =
    MAT_SYMMETRIC | MAT_HERMITIAN | MAT_STRUCTURALLY_SYMMETRIC
  | MAT_NOT_SYMMETRIC | MAT_NOT_HERMITIAN | MAT_NOT_STRUCTURALLY_SYMMETRIC
  | MAT_SYMMETRY_ETERNAL
;;

type matinfo_type = MAT_LOCAL | MAT_GLOBAL_MAX | MAT_GLOBAL_SUM;;

type ts_rhs_function = (float -> vector -> vector -> petsc_error);;
type ts_rhs_jacobian = (float -> vector -> matrix -> matrix -> matstructure_type -> petsc_error);;


external petsc_init: string array -> string -> string -> bool -> bool = "caml_petsc_init"

(* Types and functions to interface to the Petsc logging facilities *)
type logstage = int;;
external petsc_log_stage_register: string -> logstage
         = "caml_petsc_log_stage_register"
external petsc_log_stage_push: logstage -> unit = "caml_petsc_log_stage_push"
external petsc_log_stage_pop: unit -> unit = "caml_petsc_log_stage_pop"

external petsc_reset_cpu_cycle_counters: unit -> unit = "caml_petsc_reset_cpu_cycle_counters"

external petsc_get_cpu_cycle_counters: unit -> (float * float * float) = "caml_petsc_get_cpu_cycle_counters"

external petsc_finalize: unit -> unit = "caml_petsc_finalize";;

external petsc_check_initialized: unit -> bool = "caml_petsc_check_initialized";;

external petsc_get_comm_world: unit -> communicator = "caml_petsc_get_comm_world";;
external petsc_get_comm_self: unit -> communicator = "caml_petsc_get_comm_self";;

external petsc_vector_get_name: vector -> string = "caml_petsc_obj_name";;
external petsc_matrix_get_name: matrix -> string = "caml_petsc_obj_name";;
external petsc_ksp_get_name: ksp -> string = "caml_petsc_obj_name";;


external matrix_set_option_raw: matrix -> int -> unit = "caml_petsc_mat_set_option";; (* reintroduced and modified by mf *)

external petsc_options_set_value: string -> string -> unit = "caml_petsc_options_set_value";; (* mf *)

external ksp_set_initial_guess_nonzero: ksp -> bool -> unit = "caml_petsc_ksp_set_initial_guess_nonzero";; (* mf *)

external ksp_get_initial_guess_nonzero: ksp -> bool = "caml_petsc_ksp_get_initial_guess_nonzero";; (* mf *)

external ksp_get_converged_reason: ksp -> int = "caml_petsc_ksp_get_converged_reason";; (* mf *)

external ksp_set_pc_type: ksp -> string -> unit = "caml_petsc_ksp_set_pc_type";;
external ksp_set_type: ksp -> string -> unit = "caml_petsc_ksp_set_type";;

(* The way we wrap up things here is a bit different from how petsc
   likes to think about them at the elementary level:
*)

external ksp_set_pc_bjacobi_sub_type: ksp -> string -> unit = "caml_petsc_ksp_set_pc_bjacobi_subtype";;


external ksp_set_up: ksp -> unit = "caml_petsc_ksp_set_up";; (* mf *)

external vector_create: int -> string -> vector = "caml_petsc_vec_create_v1";;
external vector_create_mpi_raw: communicator -> int -> int -> string -> vector = "caml_petsc_vec_create_mpi";;

let vector_create_mpi ?(communicator=petsc_get_comm_world()) global_size local_size name =
  vector_create_mpi_raw communicator global_size local_size name
;;


external vector_dummy: unit -> vector = "caml_petsc_vec_dummy"
external matrix_dummy: unit -> matrix = "caml_petsc_mat_dummy"
external ksp_dummy: unit -> ksp = "caml_petsc_ksp_dummy"

external vector_set: vector -> int -> float -> unit = "caml_petsc_vec_set1";;

external vector_inc: vector -> int -> float -> unit = "caml_petsc_vec_inc1";;

external vector_zero: vector -> unit = "caml_petsc_vec_zero";;
external vector_scale: vector -> float -> unit = "caml_petsc_vec_scale";;


external vector_assemble: vector -> unit = "caml_petsc_vec_assemble";;
external vector_assembly_begin: vector -> unit = "caml_petsc_vec_assembly_begin";;
external vector_assembly_end: vector -> unit = "caml_petsc_vec_assembly_end";;

external vector_global_size: vector -> int = "caml_petsc_vec_global_size";;


(* XXX NOTE: it should be part of the specification that the duplicate is initially empty (is it?) *)
external vector_duplicate: vector -> vector = "caml_petsc_vec_duplicate";;

external vector_copy: vector -> vector -> unit = "caml_petsc_vec_copy";;

external vector_AXPBY: float -> float -> vector -> vector -> unit = "caml_petsc_vec_AXPBY";;

external vector_pointwise_mult: vector -> vector -> vector -> unit = "caml_petsc_vec_pointwise_mult";;
external vector_pointwise_divide: vector -> vector -> vector -> unit = "caml_petsc_vec_pointwise_divide";;

external vector_extract: vector -> float array = "caml_petsc_vec_extract";;

external vector_get_own_range: vector -> int * int = "caml_petsc_vec_get_own_range";;


(* The following are internal only: *)
external _vector_as_bigarray_open_raw: vector -> (float, float64_elt, c_layout) Array1.t
    = "caml_petsc_vec_as_bigarray_open_raw";;

external _vector_as_bigarray_close_raw: vector -> (float, float64_elt, c_layout) Array1.t -> unit
    = "caml_petsc_vec_as_bigarray_close_raw";;

external _bigarray_as_vector_open_raw: (float, float64_elt, c_layout) Array1.t -> vector
    = "caml_petsc_bigarray_as_vec_open_raw";;

external _bigarray_as_vector_close_raw: vector -> unit
  = "caml_petsc_bigarray_as_vec_close_raw";;

external matrix_create_raw: communicator -> string -> string -> (int*int*int*int) -> matrix = "caml_petsc_mat_create_raw";;

external matrix_info_raw: matrix -> int -> float array = "caml_petsc_matinfo_raw";;

external matrix_get_row_raw: matrix -> int -> ((int array) * (float array)) = "caml_petsc_mat_get_row_raw";;

external matrix_set_prealloc: matrix -> int -> int -> unit =  "caml_petsc_mat_set_prealloc";;

external matrix_set: matrix -> int -> int -> float -> unit = "caml_petsc_mat_set"
external matrix_inc: matrix -> int -> int -> float -> unit = "caml_petsc_mat_inc"

external matrix_set_timing_dummy1: matrix -> int -> int -> float -> unit = "caml_petsc_mat_set_timing_dummy1"


external matrix_set_fast_no_safety_belt: matrix -> int -> int -> float -> unit = "caml_petsc_mat_set_fast"
external matrix_inc_fast_no_safety_belt: matrix -> int -> int -> float -> unit = "caml_petsc_mat_inc_fast"


external matrix_assemble: matrix -> bool -> unit = "caml_petsc_mat_assemble";;

external matrix_assembly_begin: matrix -> bool -> unit = "caml_petsc_mat_assembly_begin";;
external matrix_assembly_end: matrix -> bool -> unit = "caml_petsc_mat_assembly_end";;

external matrix_mult_add: matrix -> vector -> vector -> vector -> unit = "caml_petsc_mat_mult_add";;

external matrix_times_vector: matrix -> vector -> vector -> unit = "caml_petsc_mat_mult";;

external matrix_transpose_times_vector: matrix -> vector -> vector -> unit = "caml_petsc_mat_mult_transpose";;

external matrix_zero_entries: matrix -> unit = "caml_petsc_mat_zero_entries";;

external matrix_reincarnate: matrix -> unit = "caml_petsc_mat_reincarnate_also_known_as_duplicate_and_destroy";;

external matrix_duplicate: bool -> matrix -> matrix = "caml_petsc_mat_duplicate";;
external matrix_copy: bool -> matrix -> matrix -> unit = "caml_petsc_mat_copy";;

external matrix_scale: matrix -> float -> unit = "caml_petsc_mat_scale";;
external matrix_add_identity: matrix -> float -> unit = "caml_petsc_mat_shift";;

external matrix_write_on_file: matrix -> string -> unit = "caml_petsc_write_mat";;
external matrix_read_from_file: string -> matrix = "caml_petsc_read_mat";;


let matrix_create
    ?communicator
    ?(local_rows=(-1)) ?(local_cols=(-1))
    ?(matrix_type="seqaij")
    ?prealloc_diagonal
    ?(prealloc_off_diagonal=0)
    ?(auto_assembly=true)
    global_rows global_cols
    name =
  let comm =
    match communicator with
      | Some c -> c
      | None -> petsc_get_comm_self ()
  in
  let mx = matrix_create_raw comm matrix_type name (global_rows,global_cols,local_rows,local_cols) in
  let () =
    match prealloc_diagonal with
      | None -> ()
      | Some d -> matrix_set_prealloc mx d prealloc_off_diagonal
  in
  let () =
    (if auto_assembly
     then matrix_assemble mx true
     else ())
  in
    mx
;;

let matrix_info ?(info_type=MAT_LOCAL) mx =
  let data =
    matrix_info_raw mx
      (match info_type with
	 | MAT_LOCAL -> 0
	 | MAT_GLOBAL_MAX -> 1
	 | MAT_GLOBAL_SUM -> 2)
  in
  let names =
    [|
      "rows_global";"columns_global";"rows_local";"columns_local";"block_size";
      "nz_allocated";"nz_used";"nz_unneeded";"memory";"assemblies";"mallocs";
      "fill_ratio_given";"fill_ratio_needed";"factor_mallocs";
    |]
  in
    Array.init (Array.length names) (fun n -> (names.(n),data.(n)))
;;

(* Note: can do this in a better way now, using petsc_matrix_call_on_rows() *)
let matrix_get_row mx nr_row =
  matrix_get_row_raw mx nr_row
;;

let matrix_info_str ?info_type mx =
  let minfo = matrix_info ?info_type mx in
    String.concat "" (Array.to_list (Array.map (fun (k,v) -> Printf.sprintf "%-20s: %8.0f\n" k v) minfo))
;;

external matrix_get: matrix -> int -> int -> float = "caml_petsc_mat_get";;

external matrix_get_size: matrix -> int * int = "caml_petsc_mat_get_size";;


(* This is slow, and hence presumably just for debugging... *)
let matrix_nr_entries_per_row mx =
  let (rows,cols) = matrix_get_size mx in
    Array.init rows
      (fun n -> Array.length (let (x,_) = matrix_get_row mx n in x))
;;


external matrix_setinc_vals:
    matrix ->
      (int, int32_elt, c_layout) Array1.t ->
      (int, int32_elt, c_layout) Array1.t ->
      (float, float64_elt, c_layout) Array2.t ->
      bool -> unit =
	"caml_petsc_mat_setinc_vals"
;;



external raw_matrix_diagonal_scale_L: matrix -> vector -> unit -> unit = "caml_petsc_mat_diagonal_scale";;
external raw_matrix_diagonal_scale_R: matrix -> unit -> vector -> unit = "caml_petsc_mat_diagonal_scale";;
external matrix_diagonal_scale_LR: matrix -> vector -> vector -> unit = "caml_petsc_mat_diagonal_scale";;

let matrix_diagonal_scale_L m v = raw_matrix_diagonal_scale_L m v ();;
let matrix_diagonal_scale_R m v = raw_matrix_diagonal_scale_R m () v;;


external matrix_get_ownership_range: matrix -> int * int = "caml_petsc_mat_get_ownership_range";;

(* XXX Had to remove this, as wrapping up MatSetOption in such a way does not make sense!
external matrix_set_option: matrix -> unit = "caml_petsc_mat_set_option"
*)

(* Note: ksp_create takes the system and preconditioner matrices as args.
   It calls KSPCreate() as well as KSPSetOperators().
   XXX For now, KSPSetOperators has flag set to DIFFERENT_NONZERO_PATTERN.
   We may want to change that.
 *)
external ksp_create_raw: communicator -> string -> matrix -> matrix -> int -> ksp = "caml_petsc_ksp_create_raw" (* modified by mf *)

external matnullspace_create_raw: communicator -> string -> bool -> vector array -> matnullspace = "caml_petsc_matnullspace_create_raw"


external ksp_set_operators_raw: ksp -> matrix -> matrix -> int -> unit = "caml_petsc_ksp_set_operators"

let ksp_set_operators
      ?(matrix_structure=DIFFERENT_NONZERO_PATTERN) ksp mx prec_mx =
  ksp_set_operators_raw
    ksp mx prec_mx (int_from_matstructure matrix_structure);;

external ksp_set_matnullspace: ksp -> matnullspace -> unit = "caml_petsc_ksp_set_null_space"

external ksp_solve_raw: ksp -> vector -> vector -> int = "caml_petsc_ksp_solve_raw";;

external ksp_get_tolerances: ksp -> float * float * float * int = "caml_petsc_ksp_get_tolerances";;
external ksp_set_tolerances: ksp -> float -> float -> float -> int -> unit = "caml_petsc_ksp_set_tolerances";;


external _raw_matrix_execute_polyplan_noassemble:
  ((int * int) * (float * string * int array) array) array array ->
  matrix ->
  vector array -> int -> unit = "caml_petsc_matrix_execute_polyplan_noassemble"
;;

(* Note that matrix_execute_polyplan can only be used in somewhat strange context:
   First of all, we have to take care ourselves of zeroing the matrix first, and second,
   when in parallel execution, the master executes a polyplan, the slaves have to
   synchronously call MatAssemblyBegin/End to match the flushes on the master!
*)

let matrix_execute_polyplan ?(nr_flushes=20) plan mx v_data =
  (* let ddd = Printf.printf "DDD matrix_execute_polyplan mx='%s' len_pplan=%6d\n%!" (petsc_matrix_get_name mx) (Array.length plan) in *)
  let () = _raw_matrix_execute_polyplan_noassemble plan mx v_data nr_flushes in
    ()
;;


external ts_create: unit -> timestepper = "caml_petsc_ts_create";;

external ts_set_problem_type: timestepper -> ts_problem_type -> unit = "caml_petsc_ts_set_problem_type";;

external ts_get_problem_type: timestepper -> ts_problem_type = "caml_petsc_ts_get_problem_type"

external ts_set_type: timestepper -> ts_type -> unit = "caml_petsc_ts_set_type";;

external ts_set_initial_time_step: timestepper -> float -> float -> unit = "caml_petsc_ts_set_initial_time_step";;

external ts_set_time_step: timestepper -> float -> float = "caml_petsc_ts_set_time_step";;

external ts_get_time_step: timestepper -> float = "caml_petsc_ts_get_time_step";;

external ts_set_duration: timestepper -> int -> float -> unit = "caml_petsc_ts_set_duration";;

external ts_set_solution: timestepper -> vector -> unit = "caml_petsc_ts_set_solution";;

external ts_set_up: timestepper -> unit = "caml_petsc_ts_set_up";;

external ts_step: timestepper -> int*float = "caml_petsc_ts_step";;

(* NOTE: ts_set_rhs_function will allow the RHS function access to vectors,
   but these MUST NOT in any way be passed out of that function!
 *)

external ts_set_rhs_function: timestepper -> ts_rhs_function -> unit = "caml_petsc_ts_set_rhs_function";;

external ts_set_rhs_jacobian: timestepper -> matrix -> matrix -> ts_rhs_jacobian -> unit = "caml_petsc_ts_set_rhs_jacobian";;

external ts_set_rhs_simple_jacobian: timestepper -> int -> unit = "caml_petsc_ts_set_rhs_simple_jacobian";;

external ts_trivial_matfdcoloring_size_n: int -> matfdcoloring = "caml_petsc_trivial_fdcoloring_size_n"


external vec_destroy: vector -> unit = "caml_petsc_vec_destroy";;
external mat_destroy: matrix -> unit = "caml_petsc_mat_destroy";;
external ksp_destroy: ksp -> unit = "caml_petsc_ksp_destroy";;
external matnullspace_destroy: matnullspace -> unit = "caml_petsc_matnullspace_destroy";;
external ts_destroy: timestepper -> unit = "caml_petsc_ts_destroy";;

external vec_describe: vector -> string = "caml_petsc_vec_describe";;
external mat_describe: matrix -> string = "caml_petsc_mat_describe";;
external mat_view: matrix -> unit = "caml_petsc_mat_view";;

external vec_scatter: communicator -> vector -> vector ->
  Nsimconf.c_int_bigarray1 -> (* lengths *)
  Nsimconf.c_int_bigarray1 -> (* displacements *)
  unit = "caml_petsc_mpi_scatter_vec"
;;

external vec_gather: communicator -> vector -> vector ->
  Nsimconf.c_int_bigarray1 -> (* lengths *)
  Nsimconf.c_int_bigarray1 -> (* displacements *)
  unit = "caml_petsc_mpi_gather_vec"
;;

external vec_allgather: communicator -> vector -> vector ->
  Nsimconf.c_int_bigarray1 -> (* lengths *)
  Nsimconf.c_int_bigarray1 -> (* displacements *)
  unit = "caml_petsc_mpi_allgather_vec"
;;


(* Note: unfortunately, we cannot use Snippets.gensym here,
   as this should not depend on snippets!

   Nevertheless, we do need an own gensym - and it should *not* be exported! (XXX TODO!)
 *)

let _petsc_gensym =
  let ht_sym_count = Hashtbl.create 10 in
  fun name ->
    let n =
      try
	Hashtbl.find ht_sym_count name
      with
      | Not_found -> 0
    in
    let () = Hashtbl.replace ht_sym_count name (1+n) in
    let sym = Printf.sprintf "%s-%d" name (1+n)
    in sym
;;

(* Same goes for list_iteri: *)

let _list_iteri f li =
  let rec walk n rest_li =
    match rest_li with
    | [] -> ()
    | (hd::tl) ->
	let () = f n hd in
	walk (1+n) tl
  in walk 0 li
;;



(* XXX this should support a target optional argument! *)
let vector_pack ?(name=_petsc_gensym "vec") v_ml =
  let dim = Array.length v_ml in
  let v = vector_create dim name in
  begin
    for i=0 to dim-1 do
      vector_set v i v_ml.(i);
    done;
    vector_assemble v;
    v
  end
;;

let matrix_extract mx =
  let (nr_rows,nr_cols) = matrix_get_size mx in
  Array.init nr_rows
    (fun nr_row ->
      (Array.init nr_cols
	 (fun nr_col -> matrix_get mx nr_row nr_col)))
;;

let matrix_extract_transpose mx =
  let (nr_rows,nr_cols) = matrix_get_size mx in
  Array.init nr_cols
    (fun nr_col ->
      (Array.init nr_rows
	 (fun nr_row -> matrix_get mx nr_row nr_col)))
;;


let matrix_pack mx =
  let nr_rows = Array.length mx in
  let nr_cols = Array.length mx.(0) in
  let mx_petsc = matrix_create nr_rows nr_cols (_petsc_gensym "packed-") in
    begin
      for r=0 to nr_rows-1 do
	for c=0 to nr_cols-1 do
	  matrix_set mx_petsc r c mx.(r).(c)
	done;
      done;
      matrix_assemble mx_petsc true;
      mx_petsc
    end
;;


external petsc_matrix_ownership_range:
  matrix -> (int*int) = "caml_petsc_matrix_ownership_range"
;;


external petsc_matrix_call_on_rows_raw:
  matrix ->
  (int -> simple_nativeint_bigarray -> simple_float_bigarray -> unit) ->
  int -> int ->
  unit = (* "caml_petsc_matrix_call_on_rows_raw" *) "caml_petsc_matrix_call_on_rows_raw_defensive"
;;

let petsc_matrix_call_on_rows mx ?(start_row=(-1)) ?(end_row=(-1)) f =
  petsc_matrix_call_on_rows_raw mx f start_row end_row
;;

let matrix_seq_to_ht mx =
  let ht = Hashtbl.create 97 in
  let () = petsc_matrix_call_on_rows mx
    (fun nr_row ba_ix_cols ba_vals ->
       for i=0 to Bigarray.Array1.dim ba_ix_cols-1 do
	 Hashtbl.add ht [|nr_row;Nativeint.to_int ba_ix_cols.{i}|] ba_vals.{i}
       done
    )
  in ht
;;

(* end new code (fangohr 17/01/2008)*)

external petsc_put_double_into_string: string -> float -> unit = "caml_petsc_put_double_into_string";;


let matrix_debugdump_to_file filename mx =
  let ml_mx = matrix_extract mx in
  let nr_rows = Array.length ml_mx in
  let nr_cols = Array.length ml_mx.(0) in
  let fh = open_out filename in
  let str = String.make 8 '\000' in
  let write_int x =
    let () = petsc_put_double_into_string str (float_of_int x) in
      Printf.fprintf fh "%s" str
  in
  let write_float x =
    let () = petsc_put_double_into_string str x in
      Printf.fprintf fh "%s" str
  in
    begin
      write_int nr_rows;
      write_int nr_cols;
      for i=0 to nr_rows-1 do
	for j=0 to nr_cols-1 do
	  write_float ml_mx.(i).(j)
	done;
      done;
      close_out fh
    end
;;

(* NOTE: f operates on a bigarray, but MUST NOT store this or pass it around
   in a way that it can leave the execution scope of f!
*)

let with_petsc_vector_as_bigarray petsc_vec (f:((float, float64_elt, c_layout) Array1.t -> unit)) =
  let biga = _vector_as_bigarray_open_raw petsc_vec in
  try
    begin
      f biga;
      _vector_as_bigarray_close_raw petsc_vec biga;
    end
  with
  | any_exception ->
      begin
	_vector_as_bigarray_close_raw petsc_vec biga;
	raise any_exception
      end
;;

let with_petsc_vectors_as_bigarrays petsc_vecs f =
  let v_biga = Array.map _vector_as_bigarray_open_raw petsc_vecs in
    try
      begin
	f v_biga;
	for i=0 to Array.length v_biga-1 do
	  let ix=Array.length v_biga-1-i in
	    _vector_as_bigarray_close_raw petsc_vecs.(ix) v_biga.(ix);
	done
      end
  with
  | any_exception ->
      begin
	for i=0 to Array.length v_biga-1 do
	  let ix=Array.length v_biga-1-i in
	    _vector_as_bigarray_close_raw petsc_vecs.(ix) v_biga.(ix);
	done;
	raise any_exception
      end
;;


let with_bigarray_as_petsc_vector biga (f: vector -> unit) =
  let vec = _bigarray_as_vector_open_raw biga in
  try
    begin
      f vec;
      _bigarray_as_vector_close_raw vec;
    end
  with
  | any_exception ->
      begin
	_bigarray_as_vector_close_raw vec;
	raise any_exception
      end
;;


let matrix_set_option mx mat_option =
  let mo =
    match mat_option with
      | MAT_SYMMETRIC -> 1
      | MAT_HERMITIAN -> 2
      | MAT_STRUCTURALLY_SYMMETRIC -> 3
      | MAT_NOT_SYMMETRIC -> 4
      | MAT_NOT_HERMITIAN -> 5
      | MAT_NOT_STRUCTURALLY_SYMMETRIC -> 6
      | MAT_SYMMETRY_ETERNAL -> 7
  in
    matrix_set_option_raw mx mo
;;

let ksp_create
    ?(communicator=petsc_get_comm_self())
    ?(name=_petsc_gensym "ksp")
    ?(matrix_structure=DIFFERENT_NONZERO_PATTERN)
    m1 m2 =
  ksp_create_raw
    communicator name m1 m2 (int_from_matstructure matrix_structure)
;;

let matnullspace_create
    ?(communicator=petsc_get_comm_self())
    ?(name=_petsc_gensym "matnullspace")
    ?(has_constant=true) vecs =
  matnullspace_create_raw communicator name has_constant vecs
;;

let ksp_set_tolerances_opt ksp opt_rtol opt_atol opt_dtol opt_maxit =
  let (rtol,atol,dtol,maxit) = ksp_get_tolerances ksp in
  let rtol = match opt_rtol with | None -> rtol | Some x -> x in
  let atol = match opt_atol with | None -> atol | Some x -> x in
  let dtol = match opt_dtol with | None -> dtol | Some x -> x in
  let maxit = match opt_maxit with | None -> maxit | Some x -> x in
    ksp_set_tolerances ksp rtol atol dtol maxit
;;

(* === Time Stepping === *)

let setup_simple_timestepping
    ?(ts_method=TS_PVODE)
    ?(initial_time=0.0)
    ?(initial_timestep=1.0e-10)
    ~vec ~fun_rhs
    =
  let ts = ts_create () in
  let () = ts_set_problem_type ts TS_NONLINEAR in (* XXX is that right? Do I have to set that? *)
  let () = ts_set_type ts ts_method in
  let () = ts_set_rhs_function ts fun_rhs in
  let (mybase,myend) = vector_get_own_range vec in
  let vec_len = myend-mybase in
  let () = ts_set_rhs_simple_jacobian ts vec_len in
  let () = ts_set_initial_time_step ts initial_time initial_timestep in
  let () = ts_set_solution ts vec in
  let () = ts_set_up ts in
  fun maxsteps maxtime ->
    let () = ts_set_duration ts maxsteps maxtime in
    ts_step ts
;;


(* === Parmetis === *)

(* Some support code for parmetis *)


external parmetis_v3_node_nd_raw:
  Nsimconf.c_int_bigarray1 -> (* cpu by site (starts at 0) *)
  Nsimconf.c_int_bigarray1 -> (* csr offsets *)
  Nsimconf.c_int_bigarray1 -> (* csr neighbours *)
  Nsimconf.c_int_bigarray1 -> (* result permutation *)
  Nsimconf.c_int_bigarray1 -> (* result sizes *)
    unit
      = "caml_parmetis_v3_node_nd_raw"
;;

external metis_node_nd_raw:
  Nsimconf.c_int_bigarray1 -> (* csr offsets *)
  Nsimconf.c_int_bigarray1 -> (* csr neighbours *)
  Nsimconf.c_int_bigarray1 -> (* result permutation *)
  Nsimconf.c_int_bigarray1 -> (* result iperm *)
    unit
      = "caml_metis_node_nd_raw"
;;

external parmetis_v3_part_kway_raw:
  (  Nsimconf.c_int_bigarray1 * (* csr offsets *)
     Nsimconf.c_int_bigarray1 * (* csr neighbours *)
     Nsimconf.c_int_bigarray1 * (* result permutation *)
     int * (* number of parts *) int (* number of nodes *) )
  -> unit = "caml_parmetis_v3_part_kway_raw"
;;



type adjacency_structure = int list array;;
 (* For every point, we provide a list of neighbours.

    This is a list array (and not an array array),
    because we may want to update the adjacency
    of individual points.

    This makes it especially easy to translate e.g. a hash of entries
    to an adjacency structure.
  *)

(* The serial CSR format (Compressed Storage) is very popular
   among libraries dealing with sparse numerical structures.
 *)

let adjacency_to_serial_csr (adjacency:adjacency_structure) =
  let nr_sites = Array.length adjacency in
  let nr_neighbours_total = Array.fold_left (fun so_far x -> so_far+List.length x) 0 adjacency in
  let arr_length_adjacency_by_site =
    Nsimconf.c_int_bigarray1_create (1+nr_sites) in
  let arr_all_adjacencies =
    Nsimconf.c_int_bigarray1_create nr_neighbours_total in
  let r_offset = ref 0 in
  begin
    for i=0 to nr_sites-1 do
      let adj_here = adjacency.(i) in
      let len_adj_here = List.length adj_here in
      begin
	let offset = !r_offset in
	arr_length_adjacency_by_site.{i} <- Nsimconf.c_int_of_int offset;
	r_offset := offset + len_adj_here;
	_list_iteri
          (fun n x ->
            arr_all_adjacencies.{offset+n} <- Nsimconf.c_int_of_int x)
          adj_here;
      end
    done;
    arr_length_adjacency_by_site.{nr_sites} <- Nsimconf.c_int_of_int !r_offset;
    (arr_length_adjacency_by_site,arr_all_adjacencies)
  end
;;


let proximity_improving_reordering adjacency =
  let nr_sites = Array.length adjacency in
  let (csr_lengths,csr_neighbours)=adjacency_to_serial_csr adjacency in
  let arr_sites_per_cpu =
    let x = Nsimconf.c_int_bigarray1_create 2 in
    begin
      x.{0} <- Nsimconf.c_int_of_int 0;
      x.{1} <- Nsimconf.c_int_of_int nr_sites;
      x
    end
  in
  let arr_result_order = Nsimconf.c_int_bigarray1_create nr_sites in
  let arr_result_sizes = Nsimconf.c_int_bigarray1_create 2
      (* XXX THIS HAS TO BE CHANGED ONCE WE GO MULTIPROCESSOR
	 Presumably, this would even segfault if we run
	 a single-cpu application on multiprocessors.
	 Have to fetch the size from the MPI communicator?!?
       *)
  in
  let arr_result_iperm = Nsimconf.c_int_bigarray1_create nr_sites in
  let () =
    (*
    parmetis_v3_node_nd_raw
      arr_sites_per_cpu csr_lengths csr_neighbours
      arr_result_order
      arr_result_sizes
     *)
    metis_node_nd_raw
      csr_lengths
      csr_neighbours
      arr_result_order
      arr_result_iperm
  in
    Array.init nr_sites (fun n -> Nsimconf.c_int_to_int arr_result_order.{n})
      (* For now, we do not care about per-cpu and separator sizes *)
;;

let cluster_partitioning nr_pieces adjacency =
  let nr_sites = Array.length adjacency in
  let (csr_lengths,csr_neighbours)=adjacency_to_serial_csr adjacency in
  let arr_result_clusternodes = Nsimconf.c_int_bigarray1_create nr_sites in
  let () = parmetis_v3_part_kway_raw (csr_lengths, csr_neighbours,
                                      arr_result_clusternodes,
                                      nr_pieces, nr_sites)
  in
  let piece_sizes = Array.make nr_pieces 0 in
  let piece_offsets = Array.make nr_pieces 0 in
  (* First we find the number of nodes for each piece
     and put it into piece_sizes *)
  let () =
    for i=0 to nr_sites-1 do
      let piece = Nsimconf.c_int_to_int arr_result_clusternodes.{i} in
	piece_sizes.(piece) <- 1+piece_sizes.(piece)
    done
  in
  (* Now we create an array of arrays. The n-th array contains the indices
     of the nodes which belong to the n-th partition *)
  let pieces = Array.init nr_pieces (fun n -> Array.make piece_sizes.(n) (-1)) in
  let () =
    for i=0 to nr_sites-1 do
      let piece = Nsimconf.c_int_to_int arr_result_clusternodes.{i} in
      let offset = piece_offsets.(piece) in
      let () = pieces.(piece).(offset) <- i in
      let () = piece_offsets.(piece) <- 1+piece_offsets.(piece) in
	()
    done
  in
    pieces
;;

let version () = "$Id$";;


(*
Look here:

#use "topfind";;
#require "mpi_petsc";;

open Mpi_petsc;;

petsc_init [|"app"|] "foo" "foo";;
let v1 = vector_create 100 "Some Vector";;
vector_set v1 10 1.0;;
vector_set v1 20 1.0;;
vector_set v1 30 1.0;;
vector_set v1 40 1.0;;
vector_set v1 50 3.0;;
vector_set v1 60 1.0;;
vector_set v1 70 1.0;;
vector_set v1 80 (-5.0);;

vector_assemble v1;;
(*
vector_extract v1;;
- : float array =
[|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
  0.; 4.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 8.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
  0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
  0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
  0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
  0.; 0.; 0.; 0.; 1.|]
# Array.length (vector_extract v1);;
- : int = 100
*)
let m1 = matrix_create 100 100 "Matrix M";;
for i=0 to 100-1 do matrix_set m1 i i 2.0 done;;
for i=0 to 100-1-1 do matrix_set m1 i (i+1) (-1.0) done;;
for i=0 to 100-1-1 do matrix_set m1 (i+1) i (-1.0) done;;
matrix_set m1 5 7 3.0;;
matrix_assemble m1;;
let ksp1 = ksp_create m1 m1;;
let solution = vector_create 100 "Solution";;
vector_assemble solution;;
ksp_solve_raw ksp1 solution v1;;
let ml_solution = vector_extract solution;;

Array.iter (fun x -> Printf.printf "%f\n" x) ml_solution;;

*)

#use "topfind";;
#require "mpi_petsc";;

open Mpi_petsc;;
open Bigarray;;

let checkheap str =
  begin
    Printf.printf "Checking heap integrity [%s]...%!" str;
    Gc.full_major();
    Printf.printf "OK!\n%!";
  end
;;

petsc_init [|"foo"|] "foo" "foo";;

let comm = petsc_get_comm_world ();;

checkheap "1";;

let problem_size = 60;;
let h = 1.0/.(float_of_int(problem_size-1));;
let dt = h/.2.0;;
let initial_t = 0.0;;
let time_step_max = 10;;
let time_total_max = 100.0;;


let the_da = da_create_1d DA_NONPERIODIC problem_size 1 1;;
let (dim, bigM, bigN, bigP, m, n, p, dof, datype) = da_get_info the_da;;
let u = da_create_global_vector the_da;;

let u_local = da_create_local_vector the_da;;

let solution = da_create_global_vector the_da;;
let localwork = da_create_local_vector the_da;;

vector_duplicate u_local localwork;;
vector_duplicate u solution;;

checkheap "2";;

let ts = ts_create ();;

ts_set_problem_type ts TS_NONLINEAR;;

let rhs_fun time_now vec_in vec_out =
  begin
    Printf.printf("boooooo #1\n%!");
    da_global_to_local_begin the_da vec_in INSERT_VALUES u_local;
    checkheap "X1";
    da_global_to_local_end the_da vec_in INSERT_VALUES u_local;
    let rank = comm_rank comm in
    let size = comm_size comm in
    begin
      with_petsc_vector_as_bigarray u_local
	(fun biga_local ->
	  with_petsc_vector_as_bigarray localwork
	    (fun biga_copy ->
	      let localsize = Array1.dim biga_copy in
	      let sc = 1.0/.(h*.h*.2.0*.(1.0+.time_now)*.(1.0+.time_now)) in
	      let () = 
		begin
		  (if rank=0 then biga_copy.{0} <- 1.0);
		  Printf.printf("boooooo\n%!");
		  (if rank=size-1 then biga_copy.{localsize-1} <- 2.0);
		  for i=1 to localsize-1-1 do
		    biga_copy.{i} <- biga_local.{i} *. sc *.
			(biga_local.{i+1} +. biga_local.{i-1} -. 2.0*.biga_local.{i});
		  done;
 		end 
 	      in ();
	    )
	);
    end;
    da_local_to_global the_da localwork INSERT_VALUES vec_out;
    Petsc_error_none;
  end;
;;

checkheap "3";;

ts_set_rhs_function ts rhs_fun;;

checkheap "4";;

let rhs_jacobian ts time_now vec_in jacobian preconditioner arg_same_nonzero_pattern =
  begin
    da_global_to_local_begin the_da vec_in INSERT_VALUES u_local;
    da_global_to_local_end the_da vec_in INSERT_VALUES u_local;
    with_petsc_vector_as_bigarray u_local
      (fun biga_local ->
	let mstart, mend = matrix_get_own_range jacobian in
	let v = Array.init 3 (fun x -> 0.0) in 
	let idx = Array.init 3 (fun x -> 0) 
	in 
	if mstart=0 
	then begin
	  v.(0) <- 0.0;
	  matrix_set_vals jacobian 1 [|mstart|] 1 [|mstart|] v;
	end; 
	if mend=problem_size 
	then begin 
	  v.(0) <- 0.0;
	  matrix_set_vals jacobian 1 [|mend-1|] 1 [|mend-1|] v;
	end;
	let sc = 1.0/.(h*.h*.2.0*.(1.0+.time_now)*.(1.0+.time_now)) in
	let () = 
	  begin
	    for i=mstart to mend-1 do
	      idx.(0) <- i-1;
	      idx.(1) <- i;
	      idx.(2) <- i+1;
	      let is = i-mstart+1 in
	      v.(0) <- sc *. biga_local.{is};
	      v.(1) <- sc *. (biga_local.{is+1}+.biga_local.{is-1}-.4.0*.biga_local.{is});
	      v.(2) <- sc *. biga_local.{is};
	      matrix_set_vals jacobian 1 [|i|] 3 idx v;
	    done;
 	  end 
 	in ();
      );
    matrix_assemble jacobian;
    matrix_set_option jacobian;
    Petsc_error_none;
  end
;;

let jacob = matrix_create problem_size problem_size "Jacobian";;
let (rows, cols) = matrix_get_size jacob;;

checkheap "5";;

ts_set_rhs_jacobian ts jacob jacob rhs_jacobian;;

ts_set_initial_time_step ts initial_t dt;;
ts_set_solution ts u;;

checkheap "6";;

let tstype = TS_BEULER;;
ts_set_type ts tstype;;

ts_set_duration ts time_step_max time_total_max;; 
ts_set_from_options ts;;

checkheap "7";;

let initial_conditions vec_in h =
  begin
    da_global_to_local_begin the_da vec_in INSERT_VALUES u_local;
    da_global_to_local_end the_da vec_in INSERT_VALUES u_local;
    Printf.printf "OK 2\n%!";
    let mybase, myend = vector_get_own_range vec_in in
    Printf.printf "%d, %d %!" mybase myend;
    with_petsc_vector_as_bigarray u_local
      (fun biga_local ->
	begin
	  Printf.printf "Inside with_petsc_vector_as_bigarray!\n%!";
	  for i=mybase to myend-1 do
	    let x = h*.float_of_int(i) in
	    biga_local.{i-mybase} <- 1.0 +. x*.x
	  done
	end
      );
  end;
;;


checkheap "8";;

initial_conditions u h;;

checkheap "9";;

ts_step ts;;
petsc_finalize();;


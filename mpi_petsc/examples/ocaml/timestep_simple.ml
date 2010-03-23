
(* (C) 2006 Dr. Thomas Fischbacher
   
   Testing the simple timestepper interface...
 *)
  
#use "topfind";;
#require "mpi_petsc";;
#require "snippets";;
#load "unix.cma";;

open Snippets;;

let _ = Mpi_petsc.petsc_init [|"foo";"-log_all"|]
  (let home = Unix.getenv "HOME" in
     Printf.sprintf "%s/.petscrc" home)
  "[help message]"
    true
;;


let fun_rhs t_now v_config v_velocity =
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v_config
      (fun ba_config ->
	Mpi_petsc.with_petsc_vector_as_bigarray v_velocity
	  (fun ba_velocity ->
	    let len = Bigarray.Array1.dim ba_config in
	    for i=0 to len-1 do
	      let i_left = if i=0 then len-1 else i-1
	      and i_right = if i=len-1 then 0 else i+1
	      in
	      let c_i = ba_config.{i}
	      and c_iL = ba_config.{i_left}
	      and c_iR = ba_config.{i_right}
	      in
	      ba_velocity.{i} <- 1.2*.(c_iL+.c_iR-.2.0*.c_i)
	    done
	  ))
  in Mpi_petsc.Petsc_error_none
;;


let config_initial =
  let nr_sites=200 in
  (Array.init 200
     (fun n ->
       let x = (float_of_int n)/.(float_of_int nr_sites) in
       let x = (x-.0.5)*.10.0 in
       let y = exp(-.x*.x*.0.5) in
       y))
;;

let v_config=Mpi_petsc.vector_pack config_initial;;

let do_ts =
  Mpi_petsc.setup_simple_timestepping
    ~ts_method:Mpi_petsc.TS_EULER
    ~initial_timestep:1e-1
    (* Note: The Euler method will keep this initial timestep forever
       and never change it! *)
    ~vec:v_config 
    ~fun_rhs:fun_rhs
;;

let evolved delta_t = 
  let _ = do_ts 100000000 delta_t in
  Mpi_petsc.vector_extract v_config
;;
  

let evolving_data = Array.init 10 (fun _ -> evolved 10.0);;

for i=0 to 200-1 do
  begin  
    for k=0 to 10-1 do
      Printf.printf "%6.3f " evolving_data.(k).(i)
    done;
    Printf.printf "\n"
  end
done
;;


(* (C) 2009 Dr. Thomas Fischbacher
   Vanilla sundials CVODE time integration:

   Setting up the heat equation:

   y,t = c*y,xx
*)

#use "topfind";;
#require "bigarray";;
#require "sundials_sp";;
#require "mpi_petsc";;
#require "graphics";;
#require "unix";;

let time_integrator ?(c_heat=1.0) ~delta_x y_initial = 
  let len = Array.length y_initial in
  let inv_step2 = 1.0/.(delta_x*.delta_x) in
  let fun_rhs ~time ~y ~ydot =
    let ddd = Printf.printf "fun_rhs T=%8.3f\n%!" time in
    let () =
      for i=0 to len-1 do
	let il = if i=0 then len-1 else i-1
	and ir = if i=len-1 then 0 else i+1
	in
	let d2y_by_dx2 = (-2.0*.y.{i}+.y.{ir}+.y.{il})*.inv_step2 in
	  ydot.{i} <- c_heat*.d2y_by_dx2
      done
    in
      0
  in
  let fun_fill_jacobian ~jacobian ~y ~ydot ~time =
    let () =
      for i=0 to len-1 do
	let il = if i=0 then len-1 else i-1
	and ir = if i=len-1 then 0 else i+1
	in
	  begin
	    Mpi_petsc.matrix_inc jacobian i i  (-2.0*.c_heat*.inv_step2);
	    Mpi_petsc.matrix_inc jacobian i il  (1.0*.c_heat*.inv_step2);
	    Mpi_petsc.matrix_inc jacobian i ir  (1.0*.c_heat*.inv_step2);
	  end
      done
    in
      ()
  in
    Sundials_sp.cvode_setup_vanilla 
      ~name:"Heat"
      ~fun_fill_jacobian
      ~y_initial
      ~fun_rhs ()
;;

let visualize ?(ysize=400) v_result =
  let xsize = Bigarray.Array1.dim v_result.(0) in
  let delay s =
    let _ = Unix.select [] [] [] s in ()
  in
  let () = Graphics.open_graph "" in
  let () = Graphics.set_window_title "Evolution" in
  let () = Graphics.resize_window xsize ysize in
  let ymin_max = [|1e307;-1e307|] in
  let () = Array.iter
    (fun ba_y ->
       let len = Bigarray.Array1.dim ba_y in
	 for i=0 to len-1 do
	   let y=ba_y.{i} in
	   let () = (if y > ymin_max.(1) then ymin_max.(1) <- y else ()) in
	   let () = (if y < ymin_max.(0) then ymin_max.(0) <- y else ()) in
	     ()
	 done
    ) v_result
  in
  let yrange = ymin_max.(1)-.ymin_max.(0) in
  let draw_frame n =
    let () = delay 0.2 in
    let () = Graphics.clear_graph () in
    let v = v_result.(n) in
    let ypos y = int_of_float(((y-.ymin_max.(0))/.yrange)*.(float_of_int ysize))
    in
      for i=0 to (Bigarray.Array1.dim v)-1-1 do
	Graphics.moveto i (ypos v.{i});
	Graphics.lineto i (ypos v.{i+1});
      done
  in
  let () =
    for n=0 to Array.length v_result-1 do
      draw_frame n
    done
  in
  let () = delay 5.0 in
    Graphics.close_graph()
;;

let ti_demo () =
  let pi = 4.0*.atan 1.0 in
  let delta_x=0.01 in
  let nr_steps = 1000 in
  let x_len = delta_x*.(float_of_int nr_steps) in
  let ti =
    time_integrator
      ~delta_x
      (* Let us use a simple sine wave: *)
      (Array.init nr_steps
	 (fun n ->
	    let x = delta_x*.(float_of_int n) in
	      sin(2.0*.pi*.x/.x_len)))
  in
  let nr_time_steps=200 in
  let dt = 0.01 in
  let v_result =
    Array.init nr_time_steps 
      (fun _ -> Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout nr_steps)
  in
  let () =
    for n = 0 to nr_time_steps-1 do
      Printf.printf "TIME STEP %4d...\n%!" n;
      ignore(Sundials_sp.cvode_advance ti (dt*.(float_of_int n)) v_result.(n) 100000)
    done
  in
    visualize v_result
;;


let () = ti_demo();;

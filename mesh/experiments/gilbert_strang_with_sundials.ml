(* (C) 2009 Dr. Thomas Fischbacher

   An exeriment: (Mostly done as I need this to test out a few things
   considering MPI prifiling strategies...)

   Combining Gilbert Strang's mesh generating techniques with sundials
   to do the time integration of the stiff system (Well, this
   certainly is a prototypical stiff system), and reverse mode
   "autodifferentiation" sensitivity analysis to obtain the forces.

*)

(* For now, let's just use the interpreter... *)

#use "topfind";;
#require "sundials_sp";;
#require "qhull";;
#require "snippets";;
#require "sundials_sp";;
#require "mpi_petsc";;
#require "bigarray";;

open Snippets;;

let () = Mpi_petsc.mpi_init();;
let comm_world = Mpi_petsc.comm_world;;

let () =
  Sundials_sp.sundials_init
    ~path_cvode:"/usr/local/lib/libsundials_cvode.so"
    ~path_nvec_serial:"/usr/local/lib/libsundials_nvecserial.so"
    ~path_nvec_parallel:"/usr/local/lib/libsundials_nvecparallel.so"
    ()
;;

let () = Random.init 1;; (* Guarantee reproducibility for this test case *)

let default_debugmessage level message =
  Printf.fprintf stderr "%2d: %s%!" level message
;;

let default_rnd = Random.float;;


let mesh_interior_of_convex_surface_3d
    (* ?fun_debugmessage *)
    ?(fun_debugmessage=default_debugmessage)
    ?(fun_rnd=default_rnd)
    ~nr_volume_points
    v_points_surface =
  let debug =
    match fun_debugmsg with
      | None -> (fun ~level message -> ())
      | Some f -> f
  in
  let fun_vol = volume_p_simplex_in_q_dimensions 3 3 in
  let simplices = Qhull.delaunay v_points_surface in
  let total_volume = 
    Array.fold_left (+.) 0.0
      (Array.map
	 (fun sx ->
	    let points = Array.map (fun ix -> v_points_surface.(ix)) sx in
	      fun_vol points)
	 simplices)
  in
  let nr_surface_points = Array.length v_points_surface in
    (* Assuming a completely regular mesh, and also assuming that
       the number of "edge" and "corner" vertices is negligible,
       the number of cells a vertex is part of should be 24 for
       volume and 12 for surface vertices.
    *)
  let estimated_nr_cells_f = 
    float_of_int(nr_surface_points)/.12.0 +. float_of_int(nr_volume_points)/.24.0
  in
  let avg_cell_volume = total_volume /. estimated_nr_cells_f in
  let guessed_rod_length =
    let vol_reg = volume_regular_simplex 3 in
      ((avg_cell_volume/.vol_reg)**(1.0/.3.0))*.1.2 
	(* The 1.2 is just a guessed "fudge factor" here, to make up
	   for the fact that we are not able to fill space with
	   regular tetrahedra. There is a more sound way to do that,
	   but for now, I don't really care. It is, after all, a
	   constant that should be between 1 and 1.5, so I'll just
	   guess it to be 1.2.
	*)
  in
  let () = 
    debug 10
      (Printf.sprintf
	 "Total volume to be meshed: %f Estimated Nr. cells. %f Est. rod length: %f\n"
	 total_volume estimated_nr_cells_f guessed_rod_length)
  in
  let v_random_initial_positions =
    (* We just start by taking random weighted linear combinations of the surface vertices *)
    Array.init
      nr_volume_points
      (fun n ->
	 let v_pos = Array.make 3 0.0 in
	 let () =
	   for i=0 to nr_surface_points-1 do
	     let weight = fun_rnd 1.0 in
	       for k=0 to 3-1 do
		 v_pos.(k) <- v_pos.(k) +. v_points_surface.(i).(k)*.weight
	       done
	   done
	 in
	   v_pos)
  in
  let v_config_initial =
    Array.init
      (3*nr_volume_points)
      (fun n -> v_random_initial_positions.(n/3).(n mod 3))
  in
    (* Problem here is: When do we re-triangulate? We may want to cache triangulations
       "if they don't change much", but what exactly is that supposed to mean then?
       For now, I will just re-triangulate at every step and postpone using more
       sophisticated methods until later...
    *)
  let dist v_config ix1 ix2 =
    let rec walk n sum =
      if n=3 then sqrt sum else
	let d = v_config.(ix1*3+n) -. v_config.(ix2*3+n) in
	  walk (n+1) (sum+.d*.d)
    in walk 0 0.0
  in
  let fun_config_dot v_config =
    (* Note: this conses - unnecessarily so. But that is not so much an issue here, 
       for before we start to address this, we should consider the more pressing
       issue of caching Qhull.delaunay calls...
    *)
    let v_points_vol = Array.init nr_volume_points (fun n -> Array.sub v_config (n*3) 3) in
    let v_points_all = Array.append v_points_vol v_points_surface in
    let simplices = Qhull.delaunay v_points_all in
    let v_forces = Array.make (3*nr_volume_points) 0.0 in
    let () = 
      for ix_sx = 0 to Array.length simplices-1 do
	let sx = simplices.(ix_sx) in
	  for ix1 = 0 to 4-1 do
	    for ix2 = ix1+1 to 4-1 do
	      (* Walk all six edge pairs *)
	      let d = dist v_config ix1 ix2 in
		if d > guessed_rod_length
		then () (* Do nothing if rod is not compressed *)
		else
		  (* Add force contribution to both points *)
		  let compression = (d /. guessed_rod_length)-.1.0 in
		  let force_factor = compression /. guessed_rod_length in
		    (* Scaling things this way gives us a notion of "time"
		       which is related to absolute coordinates.
		    *)
		  let v12 = Array.make 3 0.0 in
		    (* This points 1->2 *)
		  let () =
		    for i=0 to 3-1 do
		      v12.(i) <- (v_points_all.(ix2).(i) - v_points_all.(ix1).(i))*.force_factor
		    done
		  in
		  let () =
		    (if ix1 < nr_volume_points then
		       for i=0 to 3-1 do
			 v_forces.(3*ix1+i) <- v_forces.(3*ix1+i) -. v12.(i);
		       done
		     else ())
		  in
		  let () =
		    (if ix2 < nr_volume_points then
		       for i=0 to 3-1 do
			 v_forces.(3*ix2+i) <- v_forces.(3*ix2+i) +. v12.(i);
		       done
		     else ())
		  in
		    ()
	    done
	  done
      done
    in ()
  in
  let make_fun_config_ddot v_config =
    (* We first have to know the triangulation before we can work out
       how forces change with a configuration change...
       Note that
       
         d[force]/dxj = d [(R/L-1)/L] / dxj
       = 1/L^2 dR/dxj = 1/L^2 e_j,
       where e_j = j-th component of the unit vector
       from point A to point B.

       Signs are chosen such that x-force increases as
       we squeeze an x-directed rod.
    *)
    (* Note: this conses - unnecessarily so. But that is not so much an issue here, 
       for before we start to address this, we should consider the more pressing
       issue of caching Qhull.delaunay calls...
    *)
    let the_config = Array.copy v_config in
      (* Let's just make very sure no one stomps on that configuration vector. *)
    let v_points_vol = Array.init nr_volume_points (fun n -> Array.sub v_config (n*3) 3) in
    let v_points_all = Array.append v_points_vol v_points_surface in
    let simplices = Qhull.delaunay v_points_all in
    let jacobi_times_vec v =
      let v_out = Array.make (3*nr_volume_points) 0.0 in
      let () = 
	for ix_sx = 0 to Array.length simplices-1 do
	  let sx = simplices.(ix_sx) in
	    for ix1 = 0 to 4-1 do
	      for ix2 = ix1+1 to 4-1 do
		(* Walk all six edge pairs *)
		let d = dist v_config ix1 ix2 in
		  if d > guessed_rod_length
		  then () (* Do nothing if rod is not compressed *)
		  else
		    let v12 = Array.make 3 0.0 in
		      (* This points 1->2 *)
		    let () =
		      for i=0 to 3-1 do
			v.(i) <- (v_points_all.(ix2).(i) - v_points_all.(ix1).(i))/.d
		      done
		    in
		    let () =
		      (if ix1 < nr_volume_points then
			 for i=0 to 3-1 do
			   v_out.(3*ix1+i) <- v_out.(3*ix1+i) -. v.(i);
			 done
		       else ())
		    in
		    let () =
		      (if ix2 < nr_volume_points then
			 for i=0 to 3-1 do
			   v_out.(3*ix2+i) <- v_out.(3*ix2+i) +. v.(i);
			 done
		       else ())
		    in
		      ()
	      done
	    done
	done
      in v_out
    in
      jacobi_times_vec
  in
  let cvode_fun_rhs (time,ba_y,ba_ydot) () =
    let v_in = Array.init (3*nr_volume_points) in
    let v_out = fun_config_dot v_in in
    let () = Array.iteri (fun ix v -> ba_ydot.{ix} <- v) v_out in
      ()
  in
  let ba_initial =
    let ba = Bigarray.Array1.Array1.create Bigarray.float64 Bigarray.c_layout (3*nr_volume_points) in
    let () = Array.iteri (fun ix v -> ba.{ix} <- v) v_config_initial in
      ba
  in
  let cvode =
    Sundials_sp.cvode_make_raw_ba
      (comm_world,
       false, (* Non-parallel (for now) *)
       cvode_fun_rhs,
       3*nr_volume_points,
       ba_initial,
       0.0, (* initial time *)
       1e-6, (* rel_tol *)
       1e-2, (* abs_tol *)
      )
  in

  (* ------------ *)
  (* XXX The following code has not been adopted. So far, it's just
     copied hard from nsim.ml *)

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
    let t0 = Unix.gettimeofday() in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #2\n%!" myrank in *)
    let (ksp,mx) = get_precond gamma in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #3\n%!" myrank in *)
    let () =
      begin
	(* Maybe we should matrix_zero mx at this point? TF HF 24/01/2008 *)
	Mpi_petsc.matrix_copy lts.lts_pc_same_nonzero_pattern jacobian mx;
	Mpi_petsc.matrix_scale mx (-.gamma);
	Mpi_petsc.matrix_add_identity mx 1.0;
	Mpi_petsc.ksp_set_operators ksp mx mx; (* XXX Q: is this actually necessary? Suppose so... *)
	Mpi_petsc.ksp_set_up ksp;
      end
    in
    let t1 = Unix.gettimeofday() in
    let () = pts_timings.ptt_pc_setup_n <- 1.0 +. pts_timings.ptt_pc_setup_n in
    let () = pts_timings.ptt_pc_setup_t <- pts_timings.ptt_pc_setup_t +. (t1-.t0) in
      (* let () = Printf.printf "DDD [Node=%d] PC-Setup #4\n%!" myrank in *)
      (true,0)
  in
    (* === CVODE PC-Solve === *)
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
    let nr_iterations = Mpi_petsc.ksp_solve_raw ksp y_pcsolve_out_pv y_pcsolve_in_pv in
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
    
  (* ------------ *)
  let () =
    Sundials_sp.cvode_setup_cvspgmr_raw
      cvode
      300 (* krylov_max - let's just set this to something high, as for nmag *) 
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
    

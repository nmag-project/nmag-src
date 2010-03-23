(* (C) 2006 Dr. Thomas Fischbacher *)

let lattice_vectors_d_n dim =
  let rec walk_i i have =
    if i = dim then have else
      let rec walk_j j have =
	if j = dim then walk_i (1+i) have else
	  let have_next =
	    (let a1 = Array.make dim 0
	     and a2 = Array.make dim 0
	     and a3 = Array.make dim 0
	     and a4 = Array.make dim 0
	     in
	       begin
		 a1.(i) <- 1;
		 a1.(j) <- 1;
		 a2.(i) <- 1;
		 a2.(j) <- -1;
		 a3.(i) <- -1;
		 a3.(j) <- 1;
		 a4.(i) <- -1;
		 a4.(j) <- -1;
		 a1::a2::a3::a4::have
	       end)
	  in
	    walk_j (1+j) have_next
      in
	walk_j (1+i) have
  in Array.of_list (walk_i 0 [])
;;

let vvi_to_vvf vvi = Array.map (fun v -> Array.map float_of_int v) vvi;;

(* Look for the highest position that does not violate a given property *)

let binary_search_max_still_satisfying ?(max_nr_bisections=20) pos_low pos_high property =
  let delta = pos_high-.pos_low in
  let rec walk n step x =
    if n = max_nr_bisections
    then x
    else
      let new_step = step*.0.5 in
      let x_plus = x +. new_step in
	walk (1+n) new_step (if property x_plus then x_plus else x)
  in walk 0 delta pos_low
;;

(*
# binary_search_max_still_satisfying 0.0 2.0 (fun x -> x*.x<2.0);;
- : float = 1.41421318054199219
*)

let make_raw_star_mesh
    start_pos rod_length
    fun_constraint_soft fun_constraint_hard
    =
  let dim = Array.length start_pos in
  let lattice_vectors = vvi_to_vvf (lattice_vectors_d_n dim) in
    (* Note that these will be exact, even though we do floatingpoint. *)
  let ht_all_points = Hashtbl.create 1000 in
  let v_add v1 coeff v2 =
    let v3 = Array.make dim 0.0 in
      begin
	for i=0 to dim-1 do
	  v3.(i) <- v1.(i) +. coeff*.v2.(i);
	done;
	v3
      end
  in
  let rec do_shell li_points_to_look_at ht_points_in_next_shell =
    match li_points_to_look_at with
      | [] ->
	  let next_shell = Array.to_list (hashtbl_keys ht_points_in_next_shell) in
	  let () = Printf.printf "Doing shell of size %d\n%!" (List.length next_shell) in
	    if next_shell = []
	    then
	      let raw_points = Array.make (Hashtbl.length ht_all_points) start_pos in
	      let r_n = ref 0 in
	      let () = Hashtbl.iter
		(fun pos_rel pos_abs ->
		   let the_point =
		     if (fun_constraint_soft pos_abs > 0.0)
		     then pos_abs
		     else
		       (* Soft constraint was satisfied, but not hard constraint. *)
		       let scale_factor =
			 binary_search_max_still_satisfying 0.0 1.0
			   (fun x -> fun_constraint_soft (v_add start_pos x pos_rel) > 0.0)
		       in
			 v_add start_pos scale_factor pos_rel
		   in
		   let () = raw_points.(!r_n) <- the_point in
		     r_n:= !r_n+1)
		ht_all_points
	      in
		raw_points
	    else do_shell next_shell (Hashtbl.create 100)
      | (p0::rest) ->
	  begin
	    Array.iter
	      (fun v ->
		 let v_new = v_add p0 1.0 v in
		   try 
		     let _ = Hashtbl.find ht_all_points v_new in ()
		   with 
		     | Not_found ->
			 let v_new_absolute = v_add start_pos rod_length v_new in
			   if fun_constraint_hard v_new_absolute < 0.0 then ()
			   else
			     (* In any case, we record this point as interesting final point. *)
			     let _ = Hashtbl.replace ht_all_points v_new v_new_absolute in
			       if fun_constraint_soft v_new_absolute < 0.0
			       then ()
			       else (* This point also is a starting point for the next shell *)
				 Hashtbl.replace ht_points_in_next_shell v_new true)
	      lattice_vectors;
	    do_shell rest ht_points_in_next_shell
	  end
  in
  let p0_relative = Array.make dim 0.0 in
  let () = Hashtbl.add ht_all_points p0_relative start_pos in
    do_shell [p0_relative] (Hashtbl.create 100)
;;

(*      
let test_sphere_mesh =
  let p0 = [|1.0;0.0;0.0|] in
    make_raw_star_mesh p0 0.3
      (fun x -> 2.0 -. sqrt (euclidean_distance_sq p0 x))
      (fun x -> 2.2 -. sqrt (euclidean_distance_sq p0 x))
;;
*)

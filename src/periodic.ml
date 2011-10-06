open Snippets;;
open Printf;;


let nr_true arr =
  Array.fold_left (fun sf x -> if x then 1+sf else sf) 0 arr
;;

let list_filteri f li =
  let len = List.length li in
  let rec check_entry ix good_sf =
    if ix = len
    then Array.of_list (List.rev good_sf)
    else
      let entry = List.nth li ix in
      if (f ix entry)
      then check_entry (1+ix) (entry::good_sf)
      else check_entry (1+ix) good_sf 
  in
  check_entry 0 []
;;

let all_combinations len =
  let comb = 
    Array.init (1 lsl len)
      (fun n -> Array.init len (fun p -> 0 <> (1 lsl p) land n))
  in
  let () = Array.sort
    (fun x1 x2 -> compare (nr_true x1) (nr_true x2))
    comb
  in comb
;;

let reduce_array arr =                                             (* function to create an array taking each entry of the  *)
  let len = Array.length arr in                                    (* given array only once (in case they appear more than once) *)
  let rec add_el ix elem_sf =
    if ix = len
    then Array.of_list elem_sf
    else
      let new_elem = arr.(ix) in
      if List.mem new_elem elem_sf (* already in the list *)
      then add_el (1+ix) elem_sf
      else add_el (1+ix) (new_elem::elem_sf)
  in
  add_el 0 []
    
;;

let bc_box corner_nw corner_se =
  (* function to define a rectangular box *)
  let dim = Array.length corner_nw in
  let midpoint = array_pointwise (fun x y -> (x+.y)*.0.5) corner_nw corner_se in
  let edge_lengths = array_pointwise (-.) corner_nw corner_se in
  let inv_half_edge_lengths = Array.map (fun x -> 2.0/.x) edge_lengths in
    (fun pos ->
       let rec walk n so_far =
	 if n = dim then so_far
	 else
	   let x_n = abs_float ((pos.(n) -. midpoint.(n)) *. inv_half_edge_lengths.(n)) in
	     walk (n+1) (max x_n so_far)
       in
       let max_rel_dist_center = walk 0 0.0 in
       let v0 = 1.0 -. max_rel_dist_center in
	 v0)
;;

let components_from_directions directions =
  (* function that given a boolean vector, extracts all the 
  "unit vectors" associated to the base e1,e2,..,en *)
  let len = Array.length directions in
  let rec extract_uv ix uv_sf =
    if ix = len
    then uv_sf
    else
      if directions.(ix) 
      then 
	let new_uv = Array.init len (fun i -> if i=ix then true else false) in
	  extract_uv (1+ix) (new_uv::uv_sf)
      else 
	extract_uv (1+ix) uv_sf
  in 
    extract_uv 0 []
;;

let all_directions_but direction =
  (* function that given a boolean vector, returns a vector
     whose components are the opposite of the components in
     the original one  *)
  let len = Array.length direction in
  let inverse_arr = Array.init len (fun i -> if direction.(i) then false else true) in
    inverse_arr
;;

let periodicity_from_direction direction =  
  (* function that given a boolean vector (where only one component is true)
  returns a vector containing its inverse and all the components of the inverse *)
  let inverse = all_directions_but direction in
  let components = components_from_directions inverse in
    Array.of_list (inverse::components) 
;;

let periodic_directions directions =
  (* function that given a boolean vector representing the
  periodic directions of the mesh, it returns all the vectors
  necessary to create the periodicity along the given directions *)
  let components = components_from_directions directions in
  let len = List.length components in
  let rec add_component ix comp_sf =
    if ix = len 
    then 
      let reduced_comp = reduce_array (array_uniqify (Array.of_list comp_sf)) in
      let () = Array.sort
	(fun x1 x2 -> compare (nr_true x1) (nr_true x2))
	reduced_comp
      in reduced_comp
    else add_component (1+ix) ((periodicity_from_direction (List.nth components ix))::comp_sf)
  in
    add_component 0 []
;;

    
let mask_coords mask coord1 coord2 point_to_mask =
  (* function used to mask the immobile points from partial periodic meshes:
     the components of point_to_mask associated to true in the mask are kept, 
     the ones associated to false are removed if they have the same value of 
     coord1.(0) or coord2.(0), or the point is ignored if the components 
     associated to false have values different from coord1 or coord2
  *)
  let len = Array.length point_to_mask in
  let rec walk so_far point_masked =
    if so_far = len 
    then Array.of_list (List.rev point_masked)
    else
      if mask.(so_far)
      then walk (so_far+1) (point_to_mask.(so_far)::point_masked)
      else
	if (point_to_mask.(so_far) = coord1.(so_far)) || (point_to_mask.(so_far) = coord2.(so_far))
	then walk (so_far+1) point_masked
	else [||]
  in
    walk 0 []
;;

let maskarr mask coord1 coord2 arr =
  (* function to apply mask_coords to an array of arrays *)
  let len = Array.length arr in
  let rec walk so_far list_so_far =
    if so_far = len
    then Array.of_list(List.rev list_so_far) 
    else
      let mask_candidate = mask_coords mask coord1 coord2 arr.(so_far) in
	if mask_candidate <> [||]
	then walk (so_far+1) (mask_candidate::list_so_far)
	else walk (so_far+1) list_so_far
  in
    walk 0 []
;;

let corner_points bbox = 
  
  let (bbox_a,bbox_b) = bbox in
  let dim = Array.length bbox_a in
  let pox_combinations = all_combinations dim in
    Array.map (
      fun arr -> 
	Array.mapi ( 
	  fun arr_i arr_val -> 
	    if arr_val then 
	      bbox_a.(arr_i)
	    else bbox_b.(arr_i) 
	) arr
    ) pox_combinations 
;;

let unmask_coords mask point coord1 coord2 =
  (* function that given a mask and a point, creates
     as many points as the number of "periodic copies" 
     needed for the required periodic mesh (along the 
     remaining directions of the bounding box)
  *)
  let len = Array.length mask in
  let len_of_filling_vec = len - (nr_true mask) in
  let pox_combinations = all_combinations len_of_filling_vec in
    
    Array.map (
      fun comb ->
	let new_point = Array.make len 0.0 in
	let rec fill_point i j k =
	  if i = len
	  then new_point
	  else
	    if mask.(i) 
	    then 
	      let () = new_point.(i) <- point.(k) in
		fill_point (1+i) j (1+k)
	    else
	      let () = 
		if comb.(j)
		then new_point.(i) <- coord1.(j)
		else new_point.(i) <- coord2.(j)
	      in
		fill_point (1+i) (1+j) k	
	in
	  fill_point 0 0 0
    ) pox_combinations
;;


let merge_periodic_seed seed unmerged_arr =
  (* take all the points in the unmerged_arr (periodically) equivalent to the seed *)
  let len = Array.length unmerged_arr in
  let rec merge_next ix merged_sf=
    if ix=len 
    then 
      let merged_arr = Array.of_list merged_sf in
      let () = Array.sort compare merged_arr in
	merged_arr
    else 
      let [|p1;p2|] = unmerged_arr.(ix) in
 	if List.mem p1 merged_sf    
	then merge_next (1+ix) (p2::merged_sf)
	else if List.mem p2 merged_sf    
	then merge_next (1+ix) (p1::merged_sf)
	else
	  merge_next (1+ix) merged_sf 
  in 
    merge_next 0 [seed]
;;

let merge_periodic_pts ht_periodic_pts = 
  let periodic_arr = hashtbl_to_array ht_periodic_pts in
  let periodic_couples = Array.map (fun (k,v) -> v) periodic_arr in
  let all_periodic_points = reduce_array (array_uniqify periodic_couples) in 
  let merged_all_periodic_points = Array.map (fun p -> merge_periodic_seed p periodic_couples) all_periodic_points in
  (* we merged the periodic points and use again the hashtbl to store them *)
  let () = Hashtbl.clear ht_periodic_pts in 
  let () = Array.iter (fun per_pt -> Hashtbl.add ht_periodic_pts (Hashtbl.length ht_periodic_pts) per_pt) (reduce_array merged_all_periodic_points) in
   ht_periodic_pts 
;;


open Snippets;;
open Printf;;

let find_interface_points_to_add_between_mesh_pieces v_bodies_surfaces =                         (* this function takes the list of all the surfaces of the previous *)
  let numbered_v_bodies_surfaces =                                                               (* meshed objects and checks if there are faces on the common surface *)
												 (* which appear on one side of the surface but not on the other: for those *)
    Array.mapi (fun n v -> (n,v)) v_bodies_surfaces                                              (* count the number of times they appear and if twice divide them *)
												 (* by adding the midpoint. *)
  in
  let to_add = Hashtbl.create 10 in
  let rec do_for_all_pairs_of v f =                                                              (* function which applies f to all the couples of surfaces between *)
												 (* two objects *)
    let n = Array.length v in
      begin
	for i=0 to n-1 do
	  for k=i+1 to n-1 do
	    f v.(i) v.(k)
	  done
	  done
      end
  in
  let process_two_bodies (body_nr_1,surfaces_b1) (body_nr_2,surfaces_b2) =
    let ht_interface_points =
      let h = Hashtbl.create 100 in
      let h_body_1 = Hashtbl.create 100 in
      let () =
	Array.iter 
	  (fun face ->
	     Array.iter
	       (fun p -> Hashtbl.replace h_body_1 p true) face)                                 (* the vertices of each simplex in surface 1 is stored *)
	  surfaces_b1                                                                           (* in the h_body_1 hashtable *)
      in
	
      let () =
	Array.iter 
	  (fun face ->
	     Array.iter
	       (fun p ->                                                                        (* if a vertex appears in body 1 and body 2 then replace it *)
		  try                                                                           (* in h (associated to body 2) hashtable (that is keep track *)
		    let _ = Hashtbl.find h_body_1 p in                                          (* of all the points which appear in both surfaces) *)
		      Hashtbl.replace h p true
		  with
		    | Not_found -> ())
	       face)
	  surfaces_b2
      in h                                                                                      (* this is ht_interface_points now *)
    in

    let all_points_on_interface points =
      array_all_satisfy
	(fun p ->
	   try let _ = Hashtbl.find ht_interface_points p in
		 true
	   with | Not_found -> false)
	points
    in
    let faces_of_common_interface_1 =                                                           (* simplices on the common interface from both perspectives *)
      array_filter all_points_on_interface surfaces_b1
    and faces_of_common_interface_2 =
      array_filter all_points_on_interface surfaces_b2
    in

    let bad_faces_1 =
      let h = Hashtbl.create 10 in
      let () = Array.iter (fun f -> 
			     let sorted_face = Array.copy f in
			     let () = Array.sort compare sorted_face in 
			       Hashtbl.add h sorted_face true)                                  (* store all the common surfaces of interfaces2 in h *)
	faces_of_common_interface_2
      in
	array_filter
	  (fun f -> 
	     let sorted_face = Array.copy f in
	     let () = Array.sort compare sorted_face in 
	       try let _ = Hashtbl.find h sorted_face in                                        (* filter all the faces of body1 which appear to be on the common *)
		  false                                                                         (* interface but don't appear in both of them *)
	       with | Not_found -> true) 
	  faces_of_common_interface_1
    in                                                                                          (* bad_faces_1 now contains the problematic interfaces as *)
	                                                                                        (* triangulated on the surface of body 1 *)
    let bad_faces_2 =
      let h = Hashtbl.create 10 in
      let () = Array.iter (fun f -> 
			     let sorted_face = Array.copy f in
			     let () = Array.sort compare sorted_face in 
			       Hashtbl.add h sorted_face true)                                  (* store all the common surfaces of interfaces1 in h *)
	faces_of_common_interface_1
      in
	array_filter
	  (fun f -> 
	     let sorted_face = Array.copy f in
	     let () = Array.sort compare sorted_face in 
	     try let _ = 
	       Hashtbl.find h sorted_face in                                                    (* filter all the faces of body1 which appear to be on the common *)
	       false                                                                            (* interface but don't appear in both of them *)
	     with | Not_found -> true) 
	  faces_of_common_interface_2
    in                                                                                          (* bad_faces_2 now contains the problematic interfaces as *)
                                                                                                (* triangulated on the surface of body 2 *)

    let ht_bad_edges_on_body2 = Hashtbl.create 10 in                                            (* these edges appear on the bad surfaces of body 2 *)
    let () = Array.iter                                                                               
      (fun bad_face ->
	let edges = array_all_one_shorter bad_face in
	  Array.iter
	    (fun e ->
	      Hashtbl.add ht_bad_edges_on_body2 e true)                                         (* they are already sorted (from the implementation of bad_faces_2) *) 
	    edges)                                                                         
      bad_faces_2
    in
    let ht_edges = Hashtbl.create 10 in
    let () = 
       Array.iter
	(fun bad_face ->
	   let edges = array_all_one_shorter bad_face in
	     Array.iter
	       (fun e ->
		  try
		    let _ = Hashtbl.find ht_edges e in
		      if Hashtbl.mem ht_bad_edges_on_body2 e
		      then ()                                                                   (* this edges appear on both sides of the broken surface, *)
		      else                                                                      (* so they cannot be the cause of the bad triangulation *) 
			                                                                        (* (case of two contiguous faces being "bad simplices") *)
		                                                   
                                                                                                (* We already encountered this edge before! *)
												(* Hence, its midpoint will be a new point which *)
												(* we have to add. Think of a square surface in 3D: *)
			Hashtbl.replace to_add e ([||],(body_nr_1,body_nr_2))                          (* if on body1 it is meshed in one way and in body2 *) 
		  with | Not_found -> Hashtbl.add ht_edges e true)                              (* in the other way, the bad faces are two triangles *)
	       edges)                                                                           (* touching along the diagonals, and the diagonal, which *)
	bad_faces_1                                                                             (* appears twice if we count all the edges, is the edge to divide *) 
    in                                                               

    Array.iter
      (fun e ->                                                                                 (* for each broken edge from body1 save the related broken *)
	 let middle_point = float_arrays_avg e in                                               (* from body2 plus the region numbers of the two bodies *)
	 let len_edge = euclidean_distance_sq e.(0) e.(1) in
	 let edges_from_body1 = hashtbl_keys to_add in 
	   for i=0 to (Array.length edges_from_body1)-1 do
	     let point_from_body1 = float_arrays_avg edges_from_body1.(i) in
	     if (euclidean_distance_sq middle_point point_from_body1) < 0.01*.len_edge
	     then 
               let regions = snd (Hashtbl.find to_add edges_from_body1.(i)) in
	       Hashtbl.replace to_add edges_from_body1.(i) (e,regions) 
	     else ()
	   done
      )
      (hashtbl_keys ht_bad_edges_on_body2)
  in                                                               
    
  
                            
  let () = do_for_all_pairs_of numbered_v_bodies_surfaces process_two_bodies   in                 
  hashtbl_to_array to_add                                                                   

;;

let find_simplices_sharing_edge points simplices =
  let nr_pts = Array.length points in
  let () = printf "nr points to avoid a broken mesh: %d\n%!" nr_pts in
  let () = 
    if nr_pts > 0 
    then Array.iter (fun p -> printf "sharing edge points %s %s\n%!" (float_array_to_string p.(0)) (float_array_to_string p.(1)) ) points
    else () 
  in
  if (Array.length simplices) = 0 then [||]
  else
  let dim_sx = Array.length simplices.(0) in
  let sx_sharing_edge = Hashtbl.create (2*nr_pts) in (* there are at least 2 simplices sharing an edge *)
  let () = Array.iteri
    (fun sx_ix sx -> 
       let () = printf "%d simplex -  %!" sx_ix in
       let () = Array.iter (fun p -> printf "%s %!" (float_array_to_string p)) sx in
       let () = printf "\n%!" in
       for i = 0 to dim_sx-2 do
	 for j = i+1 to dim_sx-1 do
	   let edge = [|sx.(i);sx.(j)|] in
	   let () = Array.sort compare edge in       (* the "broken points" (expressed as couple of vertices of the mesh) are sorted *)
	     for k=0 to nr_pts-1 do
(*	       let () = printf "compare edge in simplex: %s-%s  %s-%s\n%!" (float_array_to_string edge.(0))(float_array_to_string edge.(1)) (float_array_to_string points.(k).(0))(float_array_to_string points.(k).(1)) in
*)	       if edge = points.(k)
	       then 
		 try 
		   let () = printf "try... %d\n%!" sx_ix in
		   let sx_sf = Hashtbl.find sx_sharing_edge points.(k)
		   in 
		   let () = printf "another one: %d\n%!" sx_ix in
		     Hashtbl.replace sx_sharing_edge points.(k) (sx_ix::sx_sf)
		 with | Not_found -> 
		   let () = printf "add %d\n%!" sx_ix in
		   Hashtbl.add sx_sharing_edge points.(k) [sx_ix]
	     done
	 done
       done
    )
    simplices
  in
    hashtbl_to_array sx_sharing_edge
;;



let find_simplices_sharing_vertices points simplices =
  let nr_pts = Array.length points in
  let () = printf "nr points to avoid a broken mesh: %d\n%!" nr_pts in
  let () = 
    if nr_pts > 0 
    then Array.iter (fun p -> printf "sharing vertices points %s %s\n%!" (float_array_to_string p.(0)) (float_array_to_string p.(1)) ) points
    else () 
  in
  if (Array.length simplices) = 0 then [||]
  else
  let dim_sx = Array.length simplices.(0) in
  let sx_sharing_vertex = Hashtbl.create (3*nr_pts) in (* there are at least 2 simplices sharing an vertex *)
  let () = Array.iteri
    (fun sx_ix sx -> 
       let () = printf "%d simplex -  %!" sx_ix in
       let () = Array.iter (fun p -> printf "%s %!" (float_array_to_string p)) sx in
       let () = printf "\n%!" in
       for i = 0 to dim_sx-1 do
	 
	 for k=0 to nr_pts-1 do
(*	       let () = printf "compare vertex in simplex: %s-%s  %s-%s\n%!" (float_array_to_string vertex.(0))(float_array_to_string vertex.(1)) (float_array_to_string points.(k).(0))(float_array_to_string points.(k).(1)) in
 *)	       if sx.(i) = points.(k).(0) || sx.(i) = points.(k).(1)
	       then 
		 try 
		   let () = printf "try... %d\n%!" sx_ix in
		   let sx_sf = Hashtbl.find sx_sharing_vertex points.(k)
		   in 
		   let () = printf "another one: %d\n%!" sx_ix in
		     Hashtbl.replace sx_sharing_vertex points.(k) (sx_ix::sx_sf)
		 with | Not_found -> 
		   let () = printf "add %d\n%!" sx_ix in
		   Hashtbl.add sx_sharing_vertex points.(k) [sx_ix]
	     done
       done
    )
    simplices
  in
    hashtbl_to_array sx_sharing_vertex
;;



















let remove_surfaces_with_edge target_edge surfaces =  (* the target edge is sorted *)
  let len = Array.length surfaces in
  let sorted_surfaces = Array.copy surfaces in  (* so we sort also the edges to check *)
  let () = Array.iter (fun surf -> Array.sort compare surf) sorted_surfaces in
  let rec find_surf ix surf_sf =
    if ix = len
    then surf_sf
    else
      let pox_edges = array_all_one_shorter sorted_surfaces.(ix) in
	if array_all_satisfy (fun e -> e <> target_edge) pox_edges 
	then
	 find_surf (1+ix) (sorted_surfaces.(ix)::surf_sf) 
	else
          find_surf (1+ix) surf_sf
  in
  find_surf 0 [] 
;;

let extract_surfaces_with_edge target_edge surfaces =  (* the target edge is sorted *)
  let len = Array.length surfaces in
  let sorted_surfaces = Array.copy surfaces in  (* so we sort also the edges to check *)
  let () = Array.iter (fun surf -> Array.sort compare surf) sorted_surfaces in
  let rec find_surf ix surf_sf =
    if ix = len
    then Array.of_list surf_sf
    else
      let pox_edges = array_all_one_shorter sorted_surfaces.(ix) in
	if array_all_satisfy (fun e -> e <> target_edge) pox_edges 
	then
	 find_surf (1+ix) surf_sf 
	else
          find_surf (1+ix) (sorted_surfaces.(ix)::surf_sf)
  in
  find_surf 0 [] 
;;


let common_elements_arrays arr1 arr2 =
  let len1 = Array.length arr1 in
  let list2 = Array.to_list arr2 in
  let rec find_elem1_in_list2 ix common_sf =
    if ix=len1 
    then Array.of_list common_sf
    else 
      let e1 = arr1.(ix) in
      if  List.exists (fun e2 -> e2 = e1) list2 
      then find_elem1_in_list2 (1+ix) (e1::common_sf)
      else find_elem1_in_list2 (1+ix) common_sf
  in
  find_elem1_in_list2 0 []
;;

let difference_elements_arrays arr1 arr2 =
  let len1 = Array.length arr1 in
  let list2 = Array.to_list arr2 in
  let rec find_elem1_in_list2 ix differences_sf =
    if ix=len1 
    then Array.of_list differences_sf
    else 
      let e1 = arr1.(ix) in
      if  List.exists (fun e2 -> e2 = e1) list2 
      then find_elem1_in_list2 (1+ix) differences_sf
      else find_elem1_in_list2 (1+ix) (e1::differences_sf)
  in
  find_elem1_in_list2 0 []
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

let find_external_edges simplices = 
  let len = Array.length simplices in
  if len = 0 then 
    [||]
  else 

    (* we need first to find all the external faces ... (the ones which appear only once) *)

    let ht_all_faces = Hashtbl.create (3*len) in
    let () = 
      Array.iteri (fun sx_ix sx ->
	let faces = array_all_one_shorter sx in
	for face_i = 0 to (Array.length faces)-1 do
	  let face = faces.(face_i) in
	  let () = Array.sort compare face in
	  if Hashtbl.mem ht_all_faces face 
	  then
	    let nr_times = Hashtbl.find ht_all_faces face in
	    Hashtbl.replace ht_all_faces face (1+nr_times) 
	  else
	    Hashtbl.add ht_all_faces face 1
	done) simplices 
    in	  
    let outer_faces_from_ht = array_filter (fun (face,nr_times) -> nr_times = 1) (hashtbl_to_array ht_all_faces) in
    let outer_faces = Array.map (fun (face,nr_times) -> face) outer_faces_from_ht in
    let ht_outer_edges = Hashtbl.create (2*(Array.length outer_faces)) in
    (* now extract all the edges from the outer faces *)
    let () = Array.iter (
      fun face ->
	let edges = array_all_one_shorter face in
	for edge_i = 0 to (Array.length edges)-1 do
	  let edge = edges.(edge_i) in
	   let () = Array.sort compare edge in 
	   if Hashtbl.mem ht_outer_edges edge    
	   then ()
	   else Hashtbl.add ht_outer_edges edge true   (* they are stored in the sorted form *)
	done
     ) outer_faces
    in
    let outer_edges = hashtbl_keys ht_outer_edges in
    let () = Array.sort compare outer_edges in
    outer_edges
;;
    
let extract_couples arr = 
  let len = Array.length arr in
  let ref_li_couples = ref [] in
  let () = for i= 0 to len-2 do
    for j=i+1 to len-1 do
      let couple1 = [|arr.(i);arr.(j)|] in
      let couple2 = [|arr.(j);arr.(i)|] in
      ref_li_couples := couple1 :: couple2::!ref_li_couples
    done
  done
  in Array.of_list (!ref_li_couples)
;;


(* (C) 2006 Dr. Thomas Fischbacher, SES

   An abstract implementation of the algorithm to identify and remove
   bad points.
*)

#use "topfind";;
#require "snippets";;

open Snippets;;

(*
val find_interface_points_to_add_between_mesh_pieces
  'a array array array -> ('a array * (int * int)) array

Map an array of vectors of surface points (one such vector per body)
to an array of (point array * (<index of body 1> * <index of body 2>)).

Every entry describes one additional point to be introduced at the
interface between two of the bodies in order to get rid of mesh
mis-alignments across interfaces. The point array will contain the points
of the "edge" (or hyper-edge") simplex whose midpoint is to be introduced
as an additional point.

*)

let find_interface_points_to_add_between_mesh_pieces v_bodies_surfaces =
  let numbered_v_bodies_surfaces = 
    Array.mapi (fun n v -> (n,v)) v_bodies_surfaces
  in
  let nr_bodies = Array.length v_bodies_surfaces in
  let to_add = Hashtbl.create 10 in
  let rec do_for_all_pairs_of v f =
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
	       (fun p -> Hashtbl.replace h_body_1 p true) face)
	  surfaces_b1
      in
      let () =
	Array.iter 
	  (fun face ->
	     Array.iter
	       (fun p -> 
		  try 
		    let _ = Hashtbl.find h_body_1 p in ()
		  with
		    | Not_found -> Hashtbl.replace h p true)
	       face)
	  surfaces_b2
      in h (* this is ht_interface_points now *)
    in
    let all_points_on_interface points =
      array_all_satisfy
	(fun p ->
	   try let _ = Hashtbl.find ht_interface_points p in true
	   with | Not_found -> false)
	points
    in
    let faces_of_common_interface_1 =
      array_filter all_points_on_interface surfaces_b1
    and faces_of_common_interface_2 =
      array_filter all_points_on_interface surfaces_b1
    in
    let bad_faces_1 =
      let h = Hashtbl.create 10 in
      let () = Array.iter (fun f -> Hashtbl.add h f true)
	faces_of_common_interface_2
      in
	array_filter
	  (fun f -> try let _ = Hashtbl.find h f in false
	   with | Not_found -> true) faces_of_common_interface_1
    in (* bad_faces_1 now contains the problematic interfaces as
	  triangulated on the surface of body 1.
       *)
    let ht_edges = Hashtbl.create 10 in
      Array.iter
	(fun bad_face ->
	   let edges = array_all_one_shorter bad_face in
	     Array.iter
	       (fun e ->
		  try
		    let _ = Hashtbl.find ht_edges e in
		      (* We already encountered this edge before!
			 Hence, its midpoint will be a new point which
			 we have to add.
		      *)
		      Hashtbl.replace to_add e (body_nr_1,body_nr_2)
		  with | Not_found -> Hashtbl.add ht_edges e true)
	       edges)
	bad_faces_1
  in
  let () = do_for_all_pairs_of numbered_v_bodies_surfaces process_two_bodies
  in hashtbl_to_array to_add
;;


(* 
   Flexible proximity search

   (C) 2005 Dr. Thomas Fischbacher

   Idea: we keep a priority queue of reached elements by distance.

   For distance function d(x,y), we just assume that it defines a
   metric.

   In every step, we pop from that priority queue,
   generate new neighbours, filter them, throw out seen ones,
   and re-add them to the priority queue.
   
   Note 1: when we reach a neighbour for the second (, third, ...) time,
   it must be via a longer path, due to our metric!

   Note 2: in order to find out whether we did reach a neighbour a
   second time, the user has to specify a naming function that maps
   neighbours to unique names. (We might also think about a variant
   which can work with == testing for being the same, maybe...? But
   after all, there are no EQ hash tables in OCaml, or are there?)

   XXX STATUS: did not try to compile it yet.

 *)

open Pqueue

(*
  Output: hash table mapping names to (obj, distance, name_of_prev option)
  
  Where name_of_prev option = None for the root elements.
  
  Ad arguments:
  
  fun_obj_to_name: obj -> obj_name
  
  fun_do_all_neighbours: obj f -> ()
  
  and calls f neigh_dist neigh_obj for every neighbour of this object.
  
  fun_check takes 4 args:
  
  the neighbour N,
  its distance to the closest root,
  the object P from which we come,
  the length of the step P->N.
  
  and may do whatever it wants in terms of side effects,
  before returning true or false. Only neighbours
  for which the check returns "true" are invetigated any further.
  
*)

let scan
    fun_obj_to_name fun_dist_add
    fun_do_all_neighbours fun_check list_of_roots
    =
  let ht_seen_to_tup3_obj_dist_prevname = Hashtbl.create 12 in
  let pq_dist = Pqueue.make fun_obj_to_name in
  let rec walk () =
    if Pqueue.is_empty pq_dist
    then ht_seen_to_tup3_obj_dist_prevname
    else
      let (dist_now,obj_now) = Pqueue.pop pq_dist in
      let obj_name = fun_obj_to_name obj_now in
      let process_neighbour dist_now_neigh obj_neigh =
	let neigh_name = fun_obj_to_name obj_neigh in
	let have_seen_this =
	  Hashtbl.mem ht_seen_to_tup3_obj_dist_prevname neigh_name
	in
	  if have_seen_this
	  then ()
	  else
	    let dist_neigh = fun_dist_add dist_now dist_now_neigh in
	    let is_ok =
	      fun_check obj_neigh dist_neigh obj_now dist_now_neigh in
	      if (not is_ok)
	      then () (* Just ignore that neighbour *)
	      else
		begin
		  Hashtbl.replace ht_seen_to_tup3_obj_dist_prevname
		    neigh_name (obj_neigh,dist_neigh,obj_name);
		  Pqueue.push pq_dist dist_neigh obj_neigh;
		end;
      in
	begin
	  fun_do_all_neighbours obj_now process_neighbour;
	  walk ();
	end
  in walk ()
;;


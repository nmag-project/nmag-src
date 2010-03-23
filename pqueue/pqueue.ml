(* === Priority Queues in OCaml that also support dynamic re-scheduling by name ===

   2005-2008 Dr. Thomas Fischbacher

   Note: if one does not want to use the by-name features, then it is
   valid to provide something like (fun _ -> 0) as a naming function
   that gives all the elements the same name.
   
   STATUS: compiles, seems to work. Not extensively tested yet though.
 *)

type heap_posn = int;;

type pq_exn = Impossible | Too_Small | Unknown_Name
;;

exception Exn of pq_exn;;

(*
   Priority queue entries could be just triples, if it were not for
   re-scheduling updates that just change the priority.
   Those should work with little consing, hence pqe_pri must be mutable.
*)

type ('pri_t,'data_t) pqueue_entry =
    {
     mutable pqe_pri: 'pri_t;
     pqe_rpos: heap_posn ref;
     pqe_data: 'data_t;
   };;

type ('pri_t,'name_t,'data_t) pqueue =
    {
     mutable pq_heap:
       ('pri_t, 'data_t) pqueue_entry option array;
     mutable pq_heap_nr_entries: int;
     pq_heap_resize_factor: float;
     pq_fun_name_from_data: ('data_t -> 'name_t);
     pq_names_to_posns: ('name_t, heap_posn ref) Hashtbl.t;
   };;

let _failure () =
  raise (Exn Impossible)
;;


let pq_make f_name_from_data =
  {
   pq_heap = Array.make 12 None;
   pq_heap_nr_entries = 0;
   pq_heap_resize_factor = 1.5;
   pq_fun_name_from_data = f_name_from_data;
   pq_names_to_posns = Hashtbl.create 12;
 };;

let pq_is_empty pq =
  pq.pq_heap_nr_entries = 0
;;

let pq_nr_entries pq =
  pq.pq_heap_nr_entries
;;


let _reheap_upward pq pos =
  if is_empty pq
  then ()
  else
    let heap = pq.pq_heap in
    match heap.(pos) with
    | None -> _failure ()
    | Some {pqe_pri=pri_here;pqe_rpos=ixref_here;pqe_data=data_here}
	as entry_here ->
	  let rec walk pos_now =
	    if pos_now<=1
	    then ()			(*  finished *)
	    else
	      let pos_parent = pos_now/2 in
	      match heap.(pos_parent) with
	      | None -> _failure ()
	      | Some {pqe_pri=pri_parent;
		      pqe_rpos=ixref_parent;
		      pqe_data=data_parent}
		  as entry_parent ->
		    if pri_here >= pri_parent
		    then ()	(* do nothing, finished *)
		    else
		      begin
			heap.(pos_now) <- entry_parent;
			heap.(pos_parent) <- entry_here;
			ixref_parent := pos_now;
			ixref_here := pos_parent;
			walk pos_parent
		      end
	  in walk pos
;;


let _reheap_downward pq pos =
  if is_empty pq
  then ()
  else
    let heap = pq.pq_heap in
    let max_pos = pq.pq_heap_nr_entries in
    let rec walk pos_here =
      match heap.(pos_here) with
      | None -> _failure ()
      | Some {pqe_pri=pri_here;
	      pqe_rpos=ixref_here;
	      pqe_data=data_here}
	  as entry_here ->
	  let pos_left = pos_here+pos_here in
	  if pos_left > max_pos
	  then () (* Have no left sibling - Finished *)
	  else
	    match heap.(pos_left) with
	    | None -> _failure ()		
	    | Some {pqe_pri=pri_left;pqe_rpos=ixref_left;pqe_data=data_left}
		as entry_left ->
		let pos_right = 1+pos_left in
		if pos_right > max_pos
		then
		  (* Have no right, but may have to swap left/up *)
		  if pri_here <= pri_left
		  then ()		(* Do nothing *)
		  else
		    begin
		      heap.(pos_here) <- entry_left;
		      heap.(pos_left) <- entry_here;
		      ixref_here := pos_left;
		      ixref_left := pos_here;
		      walk pos_left;
		    end
		else
		  (* Have left and right sibling *)
		  match heap.(pos_right) with
		  | None -> _failure ()		
		  | Some {pqe_pri=pri_right;
			  pqe_rpos=ixref_right;
			  pqe_data=data_right}
		      as entry_right ->
		      if pri_left < pri_here
		      then
			if pri_right < pri_here 
			then (* Trouble: left and right are smaller than we are. *)
			  if pri_left < pri_right
			  then (* Left smallest - bring that one up. *)
			    begin
			      heap.(pos_here) <- entry_left;
			      heap.(pos_left) <- entry_here;
			      ixref_here := pos_left;
			      ixref_left := pos_here;
			      walk pos_left;
			    end
			  else (* right at least as small as we are - bring right up *)
			    begin
			      heap.(pos_here) <- entry_right;
			      heap.(pos_right) <- entry_here;
			      ixref_here := pos_right;
			      ixref_right := pos_here;
			      walk pos_right;
			    end
			else (* left < here <= right *)
			  begin
			    heap.(pos_here) <- entry_left;
			    heap.(pos_left) <- entry_here;
			    ixref_here := pos_left;
			    ixref_left := pos_here;
			    walk pos_left;
			  end
		      else (* here <= left *)
			if pri_right < pri_here
			then
			  begin
			    heap.(pos_here) <- entry_right;
			    heap.(pos_right) <- entry_here;
			    ixref_here := pos_right;
			    ixref_right := pos_here;
			    walk pos_right;
			  end
			else
			  ()
    in
    walk pos
;;

let pq_push pq pri data =
  let name = pq.pq_fun_name_from_data data in
  let heap = pq.pq_heap in
  let nr_entries = pq.pq_heap_nr_entries in
    (* We might have to grow our heap *)
  let () =
    if (Array.length heap)-1 > nr_entries
    then ()
    else
      let new_size = 
	int_of_float(float_of_int(Array.length heap)*.pq.pq_heap_resize_factor)
      in
      let new_heap = Array.make new_size None in
      begin
	for i = 1 to nr_entries do
	  new_heap.(i) <- heap.(i);
	done;
	pq.pq_heap<- new_heap;
      end
  in
  (* First, place the new element at the end of the array,
     then, reheap upward. *)
  let new_pos = nr_entries+1 in
  let pos_ref = ref new_pos in
  begin
    pq.pq_heap.(new_pos)<- Some {pqe_pri=pri;pqe_rpos=pos_ref;pqe_data=data};
    pq.pq_heap_nr_entries<-new_pos;
    Hashtbl.replace pq.pq_names_to_posns name pos_ref;
    _reheap_upward pq new_pos;
  end
;;

(* Popping a given position is an internal function,
   from which we derive popping the top element
   as well as popping by name.
 *)
 
let _pop_pos pq pos =
let nr_entries = pq.pq_heap_nr_entries in
  if pos > nr_entries 
  then raise (Exn Too_Small)
  else
    let heap = pq.pq_heap in
    match heap.(pos) with
    | None -> _failure ()
    | Some {pqe_pri=pri;pqe_rpos=ref_pos;pqe_data=data} as entry_here ->
	match heap.(nr_entries) with
	| None -> _failure ()
	| Some {pqe_pri=pri_last;pqe_rpos=ref_pos_last;pqe_data=data_last}
	    as entry_last ->
	    (* Note: entry_last and entry_here may be the same.
	       This is not a problem!
	     *)
	    begin
	      Hashtbl.remove
		pq.pq_names_to_posns
		(pq.pq_fun_name_from_data data);
	      heap.(pos) <- entry_last;
	      ref_pos_last := pos;
	      heap.(nr_entries) <- None;
	      pq.pq_heap_nr_entries <- nr_entries-1;
	      (if pri_last <= pri then _reheap_upward else _reheap_downward)
		pq pos;
	      (pri,data)
	    end
;;



let _peek_pos pq pos =
  if pq.pq_heap_nr_entries < pos
  then raise (Exn Too_Small)
  else
    match pq.pq_heap.(pos)
    with 
    | None -> _failure ()
    | Some {pqe_pri=pri;pqe_rpos=pos_ref;pqe_data=data}
      -> (pri,data)
;;
      

let pq_pop pq = _pop_pos pq 1;;

let pq_peek pq = _peek_pos pq 1;;

let _name_to_pos pq name =
  let ht = pq.pq_names_to_posns in
  let ref_pos =
    try
      Hashtbl.find ht name
    with
    | Not_found -> raise (Exn Unknown_Name)
  in
  !ref_pos
;;

let pq_pop_by_name pq name = _pop_pos pq (_name_to_pos pq name);;

let pq_peek_by_name pq name = _peek_pos pq (_name_to_pos pq name);;

let pq_reschedule_by_name pq name new_pri =
  let pos = _name_to_pos pq name in
  let entry = pq.pq_heap.(pos) in
  match entry with
  | None -> _failure ()
  | Some pqe ->
      let old_pri = pqe.pqe_pri in
      begin
	pqe.pqe_pri <- new_pri;
	(if new_pri <= old_pri then _reheap_upward else _reheap_downward)
	  pq pos
      end
;;

let pq_iter pq f =
  let nr_entries = pq.pq_heap_nr_entries in
  let heap = pq.pq_heap in
    for i = 1 to nr_entries do
      match heap.(i) with
	| None -> _failure()
	| Some {pqe_pri=pri;pqe_rpos=rpos;pqe_data=data} ->
	    let () = f pri data in ()
    done
;;

let pq_check pq =
  let nr_entries = pq.pq_heap_nr_entries in
  let heap = pq.pq_heap in
    for i = 2 to nr_entries do
      match heap.(i) with
	| None -> _failure()
	| Some {pqe_pri=pri;pqe_rpos=rpos;pqe_data=data} ->
	    let i2 = i/2 in
	      match heap.(i2) with
		| None -> _failure()
		| Some {pqe_pri=pri2;pqe_rpos=rpos2;pqe_data=data2} ->
		    if pri2 <= pri then () else 
		      failwith
			(Printf.sprintf "Heap violation: Entry #%d (%f) < Entry #%d (%f)!" i2 pri2 i pri)
    done
;;

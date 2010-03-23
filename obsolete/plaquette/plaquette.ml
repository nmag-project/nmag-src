(* (C) 2006 Dr. Thomas Fischbacher

   Plaquette handling code to bunch up far-distance effects and get
   O(N log N) behaviour for approximate integrals over space where
   everything interacts with everything, but far-away regions
   contribute only very little, and position dependency of the effect
   decreases with increasing distance.

   Note: quite a few things that are parametrized functionally here
   may (for efficiency reasons) be lifted up to the module level.
   For now, we do not do this.
*)


#use "topfind";;
#require "snippets";;
open Snippets;;


type ('a, 'b, 'c) plaquette =
    {
     pq_center: float array;
     pq_xysize: float;
     pq_sub: ('a, 'b, 'c) plaquette array;
     (* Some of the later entries of this array may be PQP_none, hence we need: *)
     mutable pq_nr_childs: int;
     mutable pq_parent: ('a, 'b, 'c) plaquette option;
     pq_point: 'a option;
     (* At the very lowest level, plaquettes just carry one point.
	While it may seem wasteful to have that much wrapping around every point
	(in terms of memory), it does greatly simplify the coding later on, as we
	only have to uniformly deal with plaquettes.
      *)
     pq_source: 'b;
     pq_effect: 'c;
   }
;;

type 'a accum_manipulators =
    {
     am_create: unit -> 'a; (* It is understood that this will create a zeroed accumulator. *)
     am_clear: 'a -> unit;
     am_increase: 'a -> 'a -> unit;
   }
;;

type ('a, 'b, 'c) pq_hierarchy = 
    {
     pqh_center: float array;
     pqh_am_source: 'b accum_manipulators;
     pqh_am_effect: 'c accum_manipulators;
     pqh_pq_nil: ('a,'b,'c) plaquette;
     (* This is used as a "nil value of type plaquette" for that particular hierarchy.
	We need this to give sensible defaults to pq_sub contents.
      *)
     mutable pqh_coarsest_scale: float;
     mutable pqh_levels: (int * int array, ('a, 'b, 'c) plaquette) Hashtbl.t list;
     (* ^ XXX do we actually really have to export this?
	It does not hurt us too much and may be nice for debugging purposes, but
	do we need it?
      *)
     mutable pqh_top_plaquette: ('a, 'b, 'c) plaquette option;
   };;

let pq_tag level center fundamental_xysize coords =
  let f_level = float_of_int level in
  let inv_xysize = 1.0/.(fundamental_xysize*.(3.0**f_level)) in
  let reduced_coords =
    Array.mapi
      (fun n x -> 
	let x0 = x -. center.(n) in
	let f_nr_steps0 = x0*.inv_xysize in
	let nr_steps0=int_of_float f_nr_steps0 in
        (* Here, we use the property that OCaml's int_of_float
	   rounds 3.2 and 3.7 both to 3, and -3.2 and -3.7 both to -3.
	 *)
	if nr_steps0 > 0
	then (1+nr_steps0) lsr 1
	else if nr_steps0 < 0
	then -((1-nr_steps0) lsr 1)
	else 0)
      coords
  in
  (level,reduced_coords) 
(* Maybe omit the level here?
   It may be useful for debugging purposes,
   but maybe only for debugging...
 *)
;;

let pq_coarsen_tag (level,reduced_coords) =
  (1+level,
   Array.map
     (fun x ->
	if x>1 then (x+1)/3
	else if x<(-1) then -((1-x)/3)
	else 0) reduced_coords)
;;

(* Internal. This is used to create the structure at the lowest level. *)
let _pqh_from_points
    ~point_position
    ~am_source
    ~am_effect
    center xysize xs =
  let sub_none=[||] in
  let pq_nil =
    {pq_center=[||];
     pq_xysize=0.0;
     pq_sub=sub_none;
     pq_nr_childs=0;
     pq_parent=None;
     pq_point=None;
     pq_source=am_source.am_create();
     pq_effect=am_effect.am_create();
   }
  in
  let ht_point_plaquettes = Hashtbl.create 5 in
  let () = Array.iter
      (fun x ->
	let pt = point_position x in
	let tag = pq_tag 0 center xysize pt in
	hashtbl_push ht_point_plaquettes tag
	  {pq_center=pt;
	   pq_xysize=0.0;
	   pq_sub=sub_none;
	   pq_nr_childs=0;
	   pq_parent=None;
	   pq_point=Some x;
	   pq_source=am_source.am_create();
	   pq_effect=am_effect.am_create();
	 })
      xs
  in
    {pqh_center=center;
     pqh_am_source=am_source;
     pqh_am_effect=am_effect;
     pqh_pq_nil=pq_nil;
     pqh_coarsest_scale=xysize;
     pqh_levels=
     [hashtbl_map_values 
	(fun (lvl,tagcoords) li_pq_pt ->
	  let v_pq_pt = Array.of_list li_pq_pt in
	  {pq_center=array_pointwise
	     (fun cp offset -> cp+.(float_of_int offset)*.xysize)
	     center tagcoords;
	   pq_xysize=xysize;       
	   pq_sub=v_pq_pt;
	   pq_nr_childs=Array.length v_pq_pt;
	   pq_parent=None;
	   pq_point=None;
	   pq_source=am_source.am_create();
	   pq_effect=am_effect.am_create();
	 })
	ht_point_plaquettes];
     pqh_top_plaquette=None
   }
;;

(* Internal also:

   Add the next coarsening level to a pqh.
   Return true, if the next-most level only contains
   one plaquette, false otherwise.
*)
let _pqh_do_coarsen pqh =
  let xysize_prev = pqh.pqh_coarsest_scale in
  let xysize_new = xysize_prev*.3.0 in
  let dim = Array.length pqh.pqh_center in
  let nr_subcells = int_of_float (3.0**(float_of_int dim)) in
  let ht_new = Hashtbl.create 5 in
  let ht_old =
    match pqh.pqh_levels with
      | [] -> impossible()
      | (x::_) -> x
  in
    if Hashtbl.length ht_old=1 then true (* We actually are done already! *)
    else
      let () = Hashtbl.iter
	(fun old_tag pq_old ->
	   let (nt_lvl, nt_tagcoords) as new_tag = pq_coarsen_tag old_tag in
	   let pq_new =
	     try
	       Hashtbl.find ht_new new_tag
	     with
	     | Not_found ->
		 let e = 
		   {
		    pq_center=array_pointwise
		      (fun cp offset -> cp+.(float_of_int offset)*.xysize_new)
		      pqh.pqh_center nt_tagcoords;
		    pq_xysize=xysize_new;
		    pq_sub=Array.make nr_subcells pqh.pqh_pq_nil;
		    pq_nr_childs=0;
		    pq_parent=None;
		    pq_point=None;
		    pq_source=pqh.pqh_am_source.am_create();
		    pq_effect=pqh.pqh_am_effect.am_create();
		  }
		 in
		 let () = Hashtbl.add ht_new new_tag e in
		 e
	   in
	   begin
	     pq_old.pq_parent <- Some pq_new;
	     pq_new.pq_sub.(pq_new.pq_nr_childs) <- pq_old;
	     pq_new.pq_nr_childs <- 1+pq_new.pq_nr_childs;
	     (* We already made the entry in ht_new! *)
	   end)
	  ht_old
      in
      let () = pqh.pqh_levels <- ht_new :: pqh.pqh_levels in
      let () = pqh.pqh_coarsest_scale <- xysize_new in
      let finished = Hashtbl.length ht_new = 1 in
      let () = 
	(if finished 
	then
	  let r = ref None in
	  let () = Hashtbl.iter (fun k v -> r:=Some v) ht_new in
	  pqh.pqh_top_plaquette <- !r
	else ())
      in finished
;;

(* pqh_from_points now uses _pqh_from_points to make the first-level structure,
   and automatically refines until we do have the full tower.
*)
let pqh_from_points ~point_position ~am_source ~am_effect center xysize xs =
  let pqh = _pqh_from_points ~point_position ~am_source ~am_effect center xysize xs in
  let rec coarsen () = 
    if _pqh_do_coarsen pqh
    then pqh (* done *)
    else coarsen()
  in coarsen()
;;

let pqh_fill_source_accums pqh fun_point_source =
  match pqh.pqh_top_plaquette with
  | None -> impossible()
  | Some pq_top ->
      let rec process_plaquette pq =
	let () = pqh.pqh_am_source.am_clear pq.pq_source in
	match pq.pq_point with
	| Some pt ->
	    pqh.pqh_am_source.am_increase pq.pq_source (fun_point_source pt)
	| None ->
	    for i=0 to pq.pq_nr_childs-1 do
	      process_plaquette pq.pq_sub.(i);
	      pqh.pqh_am_source.am_increase pq.pq_source pq.pq_sub.(i).pq_source
	    done
      in process_plaquette pq_top
;;

let _pqh_clear_effects pqh =
  match pqh.pqh_top_plaquette with
  | None -> impossible()
  | Some pq_top ->
      let rec process_plaquette pq =
	begin
	  pqh.pqh_am_effect.am_clear pq.pq_effect;
	  for i=0 to pq.pq_nr_childs-1 do
	    process_plaquette pq.pq_sub.(i)
	  done;
	end
      in
      process_plaquette pq_top
;;

(* Note: for this module, we do not require/use petsc at all! *)

let pqh_compute_effects
    ?fun_pointpoint_override (* point_source -> source -> point_effect -> effect_contrib *)
    ~fun_split (* pq_source -> pq_effect -> bool *)
    ~fun_kernel (* pos_src -> src -> pos_effect -> effect_contribution *)
    ~fun_process_point_result (* point -> effect -> unit *)
    pqh
    =
  let the x =
    match x with
    | None -> impossible()
    | Some z -> z
  in
  let () = _pqh_clear_effects pqh in
  let rec traverse_plaquettes pq_source pq_effect =
    if fun_split pq_source pq_effect
    then
      (* We have to split - and we do split the larger plaquette.
	 When in doubt, we rather first split the source plaquette.

	 Note that fun_split must never tell us to split
	 if both plaquettes have point size!
       *)
      (* Usually when we come here, source and effect plaquette have the same size. 
	 When in doubt, we split the one with fewer children.
       *)
      if     pq_source.pq_xysize > pq_effect.pq_xysize
         || (pq_source.pq_xysize = pq_effect.pq_xysize
	       && pq_source.pq_nr_childs <= pq_effect.pq_nr_childs)
      then
	let source_nr_childs = pq_source.pq_nr_childs in
	for i=0 to source_nr_childs-1 do
	  traverse_plaquettes pq_source.pq_sub.(i) pq_effect
	done
      else (* source plaquette is smaller than effect plaquette,
	      or they are the same in size and effect pq has
	      fewer children *)
	let effect_nr_childs = pq_effect.pq_nr_childs in
	for i=0 to effect_nr_childs-1 do
	  traverse_plaquettes pq_source pq_effect.pq_sub.(i)
	done
    else (* No split! *)
      if pq_source.pq_point <> None
	  && pq_effect.pq_point <> None
	  && fun_pointpoint_override <> None
      then (* close-by point/point interaction *)
	let contrib =
	  (the fun_pointpoint_override)
	    (the pq_source.pq_point)
	    pq_source.pq_source
	    (the pq_effect.pq_point)
	in
	pqh.pqh_am_effect.am_increase pq_effect.pq_effect contrib
      else
	(* harmless plaquette/plaquette interaction -
	   this is where the bulk of all the work is being done... *)
	let contrib = fun_kernel pq_source.pq_center pq_source.pq_source pq_effect.pq_center in
	pqh.pqh_am_effect.am_increase pq_effect.pq_effect contrib
	  (* ...and do not recurse, as we fully accounted for this contribution of the source
	     to this other region of space now.
	   *)
  in
  let pq_top = the pqh.pqh_top_plaquette in
  let () = traverse_plaquettes pq_top pq_top
  in
  (* Now we have done all the hard work. We still have to see that we sum
     the large-scale effects into the lower levels.
   *)
  let rec percolate_effect_down effect_received pq =
    (* First, add the effect which we received from the higher level into
       the more regional effect contribution that is present locally: *)
    let () = pqh.pqh_am_effect.am_increase pq.pq_effect effect_received in
    match pq.pq_point with
    | Some pt -> fun_process_point_result pt pq.pq_effect
    | None -> (* This plaquette is not a point plaquette,
		 but does have children of its own *)
	for i=0 to pq.pq_nr_childs-1 do
	  percolate_effect_down pq.pq_effect pq.pq_sub.(i)
	done
  in percolate_effect_down (pqh.pqh_am_effect.am_create ()) pq_top
;;

(* ---- Testing the plaquette stuff so far... ---- *)

let am_float = 
  {am_create= (fun () -> ref 0.0);
   am_clear= (fun r -> r:=0.0);
   am_increase= (fun r1 r2 -> r1:= !r1+. !r2)
 }
;;

let am_vec dim = 
  {am_create= (fun () -> Array.make dim 0.0);
   am_clear= (fun a -> for i=0 to dim-1 do a.(i) <- 0.0 done);
   am_increase= (fun a1 a2 -> for i=0 to dim-1 do a1.(i) <- a1.(i)+.a2.(i) done)
 }
;;

(* Test example 1: take a "sphere" made of points in 3d
   on a regular cubic lattice (for now), consider all of them
   to carry equal charge, and compute the total potential.
   Should increase quadratically with distance from the origin.
 *)

let test_sphere_points =
  let r = 5.0 in
  let r2 = r*.r in
  let accept_point coords =
    (coords.(0)*.coords.(0)+.coords.(1)*.coords.(1)+.coords.(2)*.coords.(2) <= r2)
  in
  let r_points = ref [] in
  begin
    for x0=0 to 20 do
      for x1=0 to 20 do
	for x2=0 to 20 do
	  let coords=[|0.793*.float_of_int (x0-10); (* Just to make sure our coords are somewhat crooky... *)
		       0.793*.float_of_int (x1-10);
		       0.793*.float_of_int (x2-10);
		     |] in
	  if accept_point coords then
	    r_points:= coords :: !r_points
	  else ()
	done; done; done;
    Array.mapi (fun n coords -> (n,coords)) (Array.of_list !r_points)
  end
;;

let pqh_test_sphere =
  pqh_from_points
    ~point_position:(fun (n,coords) -> coords)
    ~am_source:am_float
    ~am_effect:am_float
    [|0.0;0.0;0.0|]	(* later on, also test with a different origin *)
    1.2	(* Initial cell spacing *)
    test_sphere_points
;;

let test_sphere_potential coarseness =
  let v_result = Array.make (Array.length test_sphere_points) 0.0 in
  let r0 = ref 0.0 in
  let () = pqh_fill_source_accums pqh_test_sphere (fun (n,coords) -> ref 1.0) in
  let () =
    pqh_compute_effects
      ~fun_split:
      (fun pq1 pq2 ->
	let dist = sqrt(euclidean_distance_sq pq1.pq_center pq2.pq_center) in
	let sum_radius = sqrt(2.0)*.(pq1.pq_xysize+.pq2.pq_xysize) in
	dist < coarseness*.sum_radius)
      ~fun_kernel:
      (fun pos_src src pos_effect ->
	let dist = sqrt(euclidean_distance_sq pos_src pos_effect) in
	if dist = 0.0 then r0 (* deal with self-interaction *)
	else ref (!src /. dist))
      ~fun_process_point_result:
      (fun (n,coords) effect ->
	v_result.(n) <- !effect)
      pqh_test_sphere
  in
  v_result
;;

let test_sphere c =
  let result = ref [] in
  let _ =
    array_pointwise
      (fun (n,coords) potential ->
	let d = 1e-6*.(float_of_int n)+.sqrt(euclidean_len_sq coords) in
	result := (d,potential) :: !result
      )
      test_sphere_points (timing test_sphere_potential c)
  in
  let sorted = List.sort (fun (r1,_) (r2,_) -> compare r1 r2) !result in
  List.iter (fun (d,phi) -> Printf.printf "%10.6f %8.4f\n" d phi) sorted
;;

let test_random_points =
  Array.init 100 (fun _ -> Array.init 2 (fun _ -> Random.float 10.0 -. 5.0));;

let test_pqh =
  pqh_from_points
    ~point_position:identity
    ~am_source:(am_vec 3)
    ~am_effect:am_float
    [|0.0;0.0|] 1.0 test_random_points;;



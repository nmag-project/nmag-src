
(* Simple function for higher-dimensional integration. Can be improved.

   Strategy: Given an integrand f and a simplicial decomposition of an
   integration domain D (points and vertex coordinates), as well as an
   accuracy limit, work out the integrals over all simplices. Put
   those into a priority queue. Get an estimate on the total error.

   Next, start from the highest-error simplex, remove it from the
   pqueue, subdivide the longest side in half, and add the halves to
   the pqueue.

   We estimate errors in a very naive way: evaluate the function at
   the simplex midpoint and take the volume of the "tent" (i.e. 
   convex hull in R^d x R extended "space x value" space) spawned by
   (mid,f(mid)), {(vertex,f(vertex))}.

   It makes sense to start to develop this as an application of the
   pqueue module. Later on, we may consider shifting this to snippets.

*)

#use "pqueue.ml";;
#use "topfind";;
#require "snippets";;

let callwrap name f =
  fun x ->
    (* let () = Printf.printf "DDD '%s'\n%!" name in *)
      f x
;;

let pq_make = (* Pqueue. *)callwrap "pq_make"  make;;
let pq_push = (* Pqueue. *)callwrap "pq_push" push;;
let pq_pop = (* Pqueue. *) callwrap "pq_pop" pop;;
let pq_iter = (* Pqueue. *)callwrap "pq_iter" iter;;
let pq_check = (* Pqueue. *)check;;

open Snippets;;


(* We use a very primitive method to estimate errors: 
   (f(x)-f(x)[interpolated])*[simplex-volume]/dim, i.e. 
   the volume of a tent in d+1-dimensional space.

   We try multiple intermediate points (as defined
   by the stencil) and just choose the worst tent.
*)

let default_fun_make_stencil dim =
  let nr_vertices = 1+dim in
  let w = 1.0/.(float_of_int nr_vertices) in
  let center = Array.make nr_vertices w in
    [|center|]
;;


let default_fun_make_stencil_2 dim =
  let nr_vertices = 1+dim in
  let w = 1.0/.(float_of_int nr_vertices) in
  let center = Array.make nr_vertices w in
    Array.init
      (1+nr_vertices)
      (fun k ->
	 if k=nr_vertices 
	 then center
	 else (* Use midpoint center-vertex *)
	   Array.init nr_vertices (fun n -> 0.5*.(if n=k then (1.0+.w) else w)))
;;

	   

let default_make_error_estimator ?(fun_make_stencil=default_fun_make_stencil) dim =
  let stencil = fun_make_stencil dim in
  let xdim = 1+dim in
  let inv_dim = 1.0/.(float_of_int dim) in
  let buf = Array.make xdim 0.0 in
  let weighted_average weights vals =
    let rec walk n x =
      if n = xdim then x
      else walk (1+n) (x+.weights.(n)*.vals.(n))
    in walk 0 0.0
  in
  let set_buf_to_weighted_pos weights point_coords =
    let rec wpos j n x =
      if n = xdim then buf.(j) <- x
      else wpos j (1+n) (x+.weights.(n)*.point_coords.(n).(j))
    in
      for j=0 to xdim-1 do
	wpos j 0 0.0
      done
  in
    fun ~volume ~point_coords ~f_vals ~f ->
      let tent_error weights =
	let weighted_f_at_vertices = weighted_average weights f_vals in
	let f_mid = 
	  let () = set_buf_to_weighted_pos weights point_coords in
	    f buf
	in
	  abs_float((f_mid-.weighted_f_at_vertices)*.inv_dim*.volume)
      in
      let err = Array.fold_left (fun sf x -> max sf (tent_error x)) 0.0 stencil in
	(* We have strong reason to believe that there is no bug in the error computation
	   that comes from our use of buffers, etc. But does the method work as it should?
	*)
	err
;;

let d_dimensional_integrator
    dim
    ?(max_evaluations=0)
    ?(make_error_estimator=default_make_error_estimator) () =
  let error_estimator = make_error_estimator dim in
  let det_inv = det_and_inv (1+dim) in
  let det x = 
    try
      let (z,_) = det_inv x in z
    with | Division_by_zero -> 0.0	(* DDD *)
  in
  let average v =
    let rescale = 1.0/.(float_of_int (Array.length v)) in
    let rec walk n x =
      if n = Array.length v
      then x*.rescale
      else walk (1+n) (x+.v.(n))
    in walk 0 0.0
  in
  let fun_simplex_volume =
    let rec fac n = if n=0 then 1 else n*fac(n-1) in (* good enough for us *)
    let coeff = 1.0/.(float_of_int (fac dim)) in
      fun points -> abs_float(coeff*.(det points))
  in
  let longest_edge_indices simplex_point_coords =
    let rec walk ix1 ix2 best12 best_len_sq =
      if ix1 > dim then best12
      else if ix2 > dim
      then walk (1+ix1) 0 best12 best_len_sq
      else
	let v1 = simplex_point_coords.(ix1)
	and v2 = simplex_point_coords.(ix2)
	in
	let len_sq =
	  let rec sum sf n = 
	    if n=dim then sf
	    else let d = v2.(n) -. v1.(n) in
	      sum (sf +. d*.d) (1+n)
	  in sum 0.0 0
	in
	  if len_sq > best_len_sq
	  then walk ix1 (1+ix2) (ix1,ix2) len_sq
	  else walk ix1 (1+ix2) best12 best_len_sq
    in
      walk 0 0 (-1,-1) (-1.0)
  in
    (*
      let ddd_longest_edge simplex_point_coords =
      let (e1,e2) = longest_edge_indices simplex_point_coords in
      sqrt(euclidean_distance_sq simplex_point_coords.(e1) simplex_point_coords.(e2))
      in
    *)
  let split_simplex point_coords f_vals vertex1 vertex2 xf =
    (* produce two sub-simplices from given simplex by halving longest side *)
    let mid =
      Array.init (1+dim)
	(fun n -> 0.5*.(point_coords.(vertex1).(n) +. point_coords.(vertex2).(n)))
    in
    let f_mid = xf mid in
    let points1 = Array.mapi (fun n p -> if n = vertex2 then mid else p) point_coords 
    and points2 = Array.mapi (fun n p -> if n = vertex1 then mid else p) point_coords 
    and f_vals1 = Array.mapi (fun n v -> if n = vertex2 then f_mid else v) f_vals
    and f_vals2 = Array.mapi (fun n v -> if n = vertex1 then f_mid else v) f_vals
    in
      (points1,f_vals1,points2,f_vals2)
  in
  let buf = Array.make dim 0.0 in
    fun ~f ~points ~simplices tolerance ->
      let nr_evaluations = ref 0 in
      let xpoints =
	(* Translate point coordinates to projective geometry representation
	   (needed for taking determinants later)
	*)
	Array.map
	  (fun p -> Array.init (1+dim) (fun n -> if n=dim then 1.0 else p.(n)))
	  points
      in
      let xf =
	(* Also lift the function f to projective coordinates *)
	fun xp ->
	  begin
	    for i=0 to dim-1 do
	      buf.(i) <- xp.(i);
	    done;
	    (if max_evaluations>0 then nr_evaluations := 1+ !nr_evaluations else ());
	    f buf
	  end
      in
      let new_simplex_id =
	let nr_simplices_generated = ref 0 in
	  fun () ->
	    let n = !nr_simplices_generated in
	    let () = nr_simplices_generated := 1+n in
	      n
      in
      let simplices_by_error = 
	(* Priority queue of simplices ordered by error.
	   Simplex data structure: 
	   (simplex_id, (* int *)
	    contrib,    (* float *)
	    vol,        (* float *)
            [|[|point coordinates|]|],
 	    [|integrand_values_at_points|])
	*)
	let fun_name_from_data (x,_,_,_,_) = x in
	  pq_make fun_name_from_data
      in
      let add_simplex_return_error point_coords f_vals =
	let volume = fun_simplex_volume point_coords in
	let contrib = volume*.(average f_vals) in
	let sx = (new_simplex_id(),contrib,volume,point_coords,f_vals) in
	let err = error_estimator ~volume ~point_coords ~f_vals ~f:xf in
	(* let ddd = Printf.printf "DDD pushing %s pri=%g vol=%g\nxdim=%g rel=%g\n%!" (string_array_to_string (Array.map float_array_to_string point_coords)) (-.err) volume (ddd_longest_edge point_coords) (err/.volume) in *)
	let () = pq_push simplices_by_error (-.err) sx in
	(* let ddd = Printf.printf "DDD HEAP %s\n%!" (string_array_to_string (Array.map (fun x -> match x with | None -> "[] " | Some {pqe_pri=p;pqe_rpos=rp;pqe_data=d} -> Printf.sprintf " [%.5f]" p) simplices_by_error.pq_heap)) in *)
	  err
      in
      let f_points = Array.map f points in
      let err_start =
	Array.fold_left
	  (fun sf vertices ->
	     let point_coords = Array.map (fun n -> xpoints.(n)) vertices in
	     let f_vals = Array.map (fun n -> f_points.(n)) vertices in
	     let err = add_simplex_return_error point_coords f_vals in
	       sf+.err) 0.0 simplices
      in
      let total_error = Array.make 2 err_start
	(* Leading entry: total error (as we keep track of it)
	   Second entry: total error at the time it last was updated
	*)
      in
      let improve_error d_err =
	let redo_error () =
	  (* Recompute estimated error from individual contributions in order
	     to get rid of numerical fuzz
	  *)
	  let sum = ref 0.0 in
	  let () =
	    pq_iter
	      simplices_by_error
	      (fun pri _ -> let err = -.pri in sum := !sum +. err) 
	  in
	  let () = total_error.(0) <- !sum in
	  let () = total_error.(1) <- !sum in
	    ()
	in
	let inv_total_error0 = let x = total_error.(0) in if x <= 0.0 then 0.0 else 1.0/.x in
	let inv_total_error1 = let x = total_error.(1) in if x <= 0.0 then 0.0 else 1.0/.x in
	let rel_improvement0 = d_err *. inv_total_error0 in
	let rel_improvement1 = d_err *. inv_total_error1 in
	let () = total_error.(0) <- total_error.(0) -. d_err in
	  if (* true ||	*)		(* DDD do this every time! *)
	    rel_improvement0 > 0.5	(* More than a factor 2 error improvement in one step... *)
	    || rel_improvement1 > 0.9   (* Or more than a factor 10 error improvement
					   since we last summed errors *)
	  then redo_error ()
	  else ()
      in
      let rec improve nr_iter =
	(* let ddd = Printf.printf "DDD iter=%6d err=%g\n%!" nr_iter total_error.(0) in *)
	if total_error.(0) <= tolerance	
	then (* Done *)
	  let integral =
	    let sum = ref 0.0 in
	    let () = pq_iter simplices_by_error (fun _ (_,x,_,_,_) -> sum := !sum +. x) in
	      !sum
	  in
	    (integral,total_error.(0),nr_iter)
	else if max_evaluations > 0 && !nr_evaluations > max_evaluations
	then failwith (Printf.sprintf "Integrator exceeded limit on the number of function evaluations (%d > %d)" !nr_evaluations max_evaluations)
	else
	  let (pri,worst_simplex) = pq_pop simplices_by_error in
	  let err = -.pri in
	  let (_,_,_,p,f_p) = worst_simplex in
	  let (vertex1,vertex2) = longest_edge_indices p in
	  let (p1,v1,p2,v2) = split_simplex p f_p vertex1 vertex2 xf in
	  let err1 = add_simplex_return_error p1 v1 in
	  let err2 = add_simplex_return_error p2 v2 in
	  let err_improvement = err-.(err1+.err2) in
	  let () = improve_error err_improvement in
	    improve (1+nr_iter)
      in
	improve 0
;;

let int2d_square ?(tolerance=1e-8) x0 y0 x1 y1 =
  let points = [|[|x0;y0|];
		 [|x0;y1|];
		 [|x1;y1|];
		 [|x1;y0|];|]
  in 
  let simplices = [|[|0;1;2|];[|0;2;3|];|] in
  let integrator = d_dimensional_integrator 2 () in
    fun f -> integrator ~f ~points ~simplices tolerance
;;

let x0 = 
  (* Determine the volume of a sphere by integrating the half-sphere function over R^2: *)
  int2d_square ~tolerance:5e-3 (-1.2) (-1.2) 1.2 1.2
    (fun xy ->
       let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	 if r2 > 1.0 then 0.0
	 else 2.0*.sqrt(1.0-.r2))
;;

(* Now this basically seems to work:

val x0 : float * float = (4.18458169423444293, 0.00499952964885422291)
# 4.0*.4.0*.(atan 1.0)/.3.0;;
- : float = 4.18879020478639053
# 4.18458169423444293-.4.18879020478639053;;
- : float = -0.00420851055194759738

   ...but somehow, this method is still quite slow... Can we improve it
   through (1) removing cruft, (2) more intelligent splitting, and (3)
   2nd order interpolation?

*)

let x1 = 
  (* Determine the volume of a sphere by integrating the half-sphere function over R^2: *)
  int2d_square ~tolerance:1e-3 (-1.2) (-1.2) 1.2 1.2
    (fun xy ->
       let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	 if r2 > 1.0 then 0.0
	 else 2.0*.sqrt(1.0-.r2))
;;

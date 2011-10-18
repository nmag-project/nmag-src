
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

open Snippets;;

(*
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
*)

open Snippets;;

(* We use the symmetric 7-point and 9-point formula which are correct to order 5
   given in "GAUSSIAN QUADRATURE FORMULAS FOR TRIANGLES" by G.R. Cowper in 1972
*)

let gauss_weights_coords_ord5_7points =
  [|[|0.225000000000000;0.333333333333333;0.333333333333333;0.333333333333333|];
    [|0.125939180544827;0.797426985353087;0.101286507323456;0.101286507323456|];
    [|0.125939180544827;0.101286507323456;0.797426985353087;0.101286507323456|];
    [|0.125939180544827;0.101286507323456;0.101286507323456;0.797426985353087|];
    [|0.132394152788506;0.470142064105115;0.470142064105115;0.059715871789770|];
    [|0.132394152788506;0.470142064105115;0.059715871789770;0.470142064105115|];
    [|0.132394152788506;0.059715871789770;0.470142064105115;0.470142064105115|];
  |]
;;

let gauss_weights_coords_ord5_9points =
  [|[|0.205950504760887;0.124949503233232;0.437525248383384;0.437525248383384|];
    [|0.205950504760887;0.437525248383384;0.124949503233232;0.437525248383384|];
    [|0.205950504760887;0.437525248383384;0.437525248383384;0.124949503233232|];
    [|0.063691414286223;0.797112651860071;0.165409927389841;0.037477420750088|];
    [|0.063691414286223;0.037477420750088;0.797112651860071;0.165409927389841|];
    [|0.063691414286223;0.165409927389841;0.037477420750088;0.797112651860071|];
    [|0.063691414286223;0.165409927389841;0.797112651860071;0.037477420750088|];
    [|0.063691414286223;0.797112651860071;0.037477420750088;0.165409927389841|];
    [|0.063691414286223;0.037477420750088;0.165409927389841;0.797112651860071|];
  |]
;;

let gauss_2d = 
  let buf_pos = Array.make 2 0.0 in
    fun ~weights_coords ~f ~points ~volume ->
      let nr_wc = Array.length weights_coords in
      let rec walk nr_pt sum =
	if nr_pt = nr_wc then sum *. volume
	else
	  let () =
	    begin
	      buf_pos.(0) <-    weights_coords.(nr_pt).(1)*.points.(0).(0)
                             +. weights_coords.(nr_pt).(2)*.points.(1).(0)
                             +. weights_coords.(nr_pt).(3)*.points.(2).(0);
	      buf_pos.(1) <-    weights_coords.(nr_pt).(1)*.points.(0).(1)
                             +. weights_coords.(nr_pt).(2)*.points.(1).(1)
                             +. weights_coords.(nr_pt).(3)*.points.(2).(1);
	    end
	  in
	  let val_f = f buf_pos in
	  (* let ddd = Printf.printf "pos: %s f: %f weight: %f\n%!" (float_array_to_string buf_pos) val_f weights_coords.(nr_pt).(0) in *)
	    walk (1+nr_pt) (sum+.weights_coords.(nr_pt).(0)*.val_f)
      in walk 0 0.0
;;


let integrate2d ?(max_evaluations=(-1)) ?(tolerance=1e-8) ~points ~simplices f =
  (* Parts of this function work in arbitrary dimension, other parts
     are specific to D=2.  More generic high-dimensional code would lift
     points to projective coordinates so that determinants can be
     employed directly to compute areas. We do not do that here. 
  *)
  let dim=2 in
  let nr_eval = ref 0 in
  let xf pos =
    (* D=2 specific:
       for arbitrary dimension, we would also include a buffer that makes xf 
       operate on normalized projective coordinates where f only takes
       affine coordinates. 
       
       We also dress up f in such a way that we count the number of function evaluations. 
    *)
    let result = f pos in
    let () = nr_eval := 1+ !nr_eval in
    let () = (if max_evaluations > 0 &&
		!nr_eval > max_evaluations
	      then
		failwith (Printf.sprintf "integrate2d: maximum number of evaluations (%d) exceeded."
			    max_evaluations)
	      else ())
    in
      result
  in
  let average v =
    (* Works in arbitrary dimension *)
    let rescale = 1.0/.(float_of_int (Array.length v)) in
    let rec walk n x =
      if n = Array.length v
      then x*.rescale
      else walk (1+n) (x+.v.(n))
    in walk 0 0.0
  in
  let simplex_volume points =
    (* D=2 specific:
          (p0x p0y 1)
     det  (p1x p1y 1)  = p0x*p1y
          (p2x p2y 1)
    *)
    abs_float(((points.(0).(0)*.points.(1).(1)-.points.(0).(1)*.points.(1).(0))
	       +. (points.(1).(0)*.points.(2).(1)-.points.(1).(1)*.points.(2).(0))
	       +. (points.(2).(0)*.points.(0).(1)-.points.(2).(1)*.points.(0).(0))
	      ))/.2.0
  in
  let longest_edge_indices simplex_point_coords =
    (* Works in arbitrary dimension *)
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
  let split_simplex point_coords vertex1 vertex2 =
    (* produce two sub-simplices from given simplex by halving longest side *)
    let mid =
      Array.init dim
	(fun n -> 0.5*.(point_coords.(vertex1).(n) +. point_coords.(vertex2).(n)))
    in
    let points1 = Array.mapi (fun n p -> if n = vertex2 then mid else p) point_coords 
    and points2 = Array.mapi (fun n p -> if n = vertex1 then mid else p) point_coords 
    in
      (points1,points2)
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
       [|[|point coordinates|]|])
    *)
    let fun_name_from_data (x,_,_,_) = x in
      pq_make fun_name_from_data
  in
  let add_simplex_return_error point_coords volume =
    (* let ddd = Printf.printf "add_simplex_return_error vol=%f points=%s\n%!" volume (float_array2_to_string point_coords) in *)
    let contrib1 =
      gauss_2d ~weights_coords:gauss_weights_coords_ord5_7points
	~f:xf ~points:point_coords ~volume
    in
    let contrib2 =
      gauss_2d ~weights_coords:gauss_weights_coords_ord5_9points
	~f:xf ~points:point_coords ~volume
    in
    (* let ddd = Printf.printf "C1: %10.6f C2: %10.6f vol=%10.6f\n%!" contrib1 contrib2 volume in *)
    let contrib = 0.5*.(contrib1+.contrib2) in
    let sx = (new_simplex_id(),contrib,volume,point_coords) in
    let err = abs_float(contrib1-.contrib2) in
    let () = pq_push simplices_by_error (-.err) sx in
      (* let ddd = Printf.printf "DDD HEAP %s\n%!" (string_array_to_string (Array.map (fun x -> match x with | None -> "[] " | Some {pqe_pri=p;pqe_rpos=rp;pqe_data=d} -> Printf.sprintf " [%.5f]" p) simplices_by_error.pq_heap)) in *)
      err
  in
  let err_start =
    Array.fold_left
      (fun sf vertices ->
	 let point_coords = Array.map (fun n -> points.(n)) vertices in
	 let err = add_simplex_return_error point_coords (simplex_volume point_coords) in
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
	let () = pq_iter simplices_by_error (fun _ (_,x,_,_) -> sum := !sum +. x) in
	  !sum
      in
	(integral,total_error.(0),nr_iter,!nr_eval)
    else
      let (pri,worst_simplex) = pq_pop simplices_by_error in
      let err = -.pri in
      let (_,_,v,p) = worst_simplex in
      let (vertex1,vertex2) = longest_edge_indices p in
      let (p1,p2) = split_simplex p vertex1 vertex2 in
      let err1 = add_simplex_return_error p1 (v*.0.5) in
      let err2 = add_simplex_return_error p2 (v*.0.5) in
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
    integrate2d ~tolerance ~points ~simplices
;;

let x0 = 
  (* Determine the volume of a sphere by integrating the half-sphere function over R^2: *)
  let z =
    int2d_square ~tolerance:1e-3 (-1.2) (-1.2) 1.2 1.2
      (fun xy ->
	 let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	   if r2 > 1.0 then 0.0
	   else 2.0*.sqrt(1.0-.r2))
  in (("INT",z),("EXACT",16.0*.(atan 1.0)/.3.0))
;;

(*

=== 1e-7 ===

let x0 = 
  (* Determine the volume of a sphere by integrating the half-sphere function over R^2: *)
  let z =
    int2d_square ~tolerance:1e-7 (-1.2) (-1.2) 1.2 1.2
      (fun xy ->
	 let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	   if r2 > 1.0 then 0.0
	   else 2.0*.sqrt(1.0-.r2))
  in (("INT",z),("EXACT",16.0*.(atan 1.0)/.3.0))
;;

val x0 : (string * (float * float * int)) * (string * float) =
  (("INT", (4.18578762009691197, 9.99987681025421525e-08, 110666)),
   ("EXACT", 4.18879020478639053))

=== 1e-6 ===

#   let x0 = 
  (* Determine the volume of a sphere by integrating the half-sphere function over R^2: *)
  let z =
    int2d_square ~tolerance:1e-6 (-1.2) (-1.2) 1.2 1.2
      (fun xy ->
	 let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	   if r2 > 1.0 then 0.0
	   else 2.0*.sqrt(1.0-.r2))
  in (("INT",z),("EXACT",16.0*.(atan 1.0)/.3.0))
;;

val x0 : (string * (float * float * int)) * (string * float) =
  (("INT", (4.18578981091738545, 9.9999859653526378e-07, 25468)),
   ("EXACT", 4.18879020478639053))

=== 1e-4 ===

let x0 = 
  (* Determine the volume of a sphere by integrating the half-sphere function over R^2: *)
  let z =
    int2d_square ~tolerance:1e-4 (-1.2) (-1.2) 1.2 1.2
      (fun xy ->
	 let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	   if r2 > 1.0 then 0.0
	   else 2.0*.sqrt(1.0-.r2))
  in (("INT",z),("EXACT",16.0*.(atan 1.0)/.3.0))
;;

                  val x0 : (string * (float * float * int)) * (string * float) =
  (("INT", (4.1878823105072982, 9.97138943464009588e-05, 1044)),
   ("EXACT", 4.18879020478639053))

*)

let x1 = 
  (* Here, we indeed do expect to immediately get the correct value,
     as our Gaussian integrator will handle polynomials of order up to 
     5 correctly.
  *)
  let z =
    int2d_square ~tolerance:1e-8 (-1.0) (-1.0) 1.0 1.0
      (fun xy ->
	 let r2 = xy.(0)*.xy.(0)+.xy.(1)*.xy.(1) in
	   r2)
  in (("INT",z),("EXACT",8.0/.3.0))
;;


let x2 = 
  let z =
    int2d_square ~tolerance:1e-8 (-1.0) (2.0*.atan (-1.0)) 1.0 (2.0*.atan 1.0)
      (fun xy -> cos(xy.(1)))
  in (("INT",z),("EXACT",4.0))
;;


(*  *)

(*

What we will work with:

df'/dk=-0.5*f'''/(k+f'')^2

So, (using variables x,t):

dZ/dt = -0.5*Z''/(k+Z')^2

  Hence, we need good first and second order derivatives of scalar
  functions. Let us again aim for correctness of our partial derivatives
  up to order 5.

For a start, let's do it the naive way:
*)

let dgl_violation ?(xstep=1e-6) ?(tstep=1e-6) f =
  fun t0 x0 ->
    (* This has to be improved so that this becomes correct to higher order: 
       we evaluate f at some points close to (t0,x0) and then form linear 
       combinations. Conventions: 
       f_t = f at (t0,x0) minus one t-step
       f_T = f at (t0,x0) plus  one t-step
       f_x = f at (t0,x0) minus one x-step
       f_X = f at (t0,x0) plus  one x-step
       f_xxT = f at (t0,x0) minus two x-steps plus one T-step,
       etc.
    *)
    let f_t = f (t0-.tstep) x0
    and f_T = f (t0+.tstep) x0
    and f_x = f t0 (x0-.xstep)
    and f_X = f t0 (x0+.xstep)
    and f_ = f t0 x0
    in
    let approx_dfdt = (f_T-.f_t)/.(2.0*.tstep)
    and approx_dfdx = (f_X-.f_x)/.(2.0*.xstep)
    and approx_d2fdx2 = (f_X+.f_x-.2.0*.f_)/.(xstep*.xstep)
    in
    let rhs = -0.5*.approx_d2fdx2/.(t0+.approx_dfdx)**2.0 in
    let lhs = approx_dfdt in
      lhs-.rhs
;;

(* Example: *)

let fun_delta_f = dgl_violation (fun t0 x0 -> x0**3.0 +. x0**2.0 -.x0 +. 5.0);;

(*
val fun_delta_f : float -> float -> float = <fun>
# fun_delta_f 2.0 3.0;;
- : float = 0.00865436134496467765
# fun_delta_f 1.0 3.0;;
- : float = 0.00918681516495504537
# fun_delta_f 0.1 3.0;;
- : float = 0.00970918538688445368
# fun_delta_f 0.01 3.0;;
- : float = 0.00976385924627149251
*)

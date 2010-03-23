#use "topfind";;
#require "snippets";;

(* This experimental piece of code tries to derive anisotropy coefficients from a given function.

   Observations are: (1) it is dog slow - far too slow to be of any
   good use - but this may be improved by a factor 10 perhaps, using
   only code optimization, and another factor 10 using better
   methodology -

   (2) furthermore, the results are not promising at all. I think what
   happens is that the first refinement level of the octahedron does
   not have its points distributed suitably to properly probe all
   fourth-order contributions... In particular, contributions of the
   form xy^2z are zero on all the vertices. So, we would at the very
   least need a different point distribution scheme...
 *)

open Snippets;;

let independent_so3_scalars order =
  let rec walk this_order have =
    if this_order > order then Array.concat have
    else 
      let all_contribs = all_distributions_to_buckets 3 this_order in
      let reduced_contribs =
	array_filter (fun v -> v.(2)<2) all_contribs
	  (* Not all the tensorial quantities are independent:
	     we also have to take into account the condition
	     M_x^2+M_y^2+M_z^2 = constant. So, whenever we encounter
	     a contribution carrying a factor of M_z^2, we may
	     regard this as dependent, as it can be re-written in terms
	     containing M_x^2 and M_y^2 instead. Hence, we remove
	     all such terms.
	   *)
      in
      walk (2+this_order) (reduced_contribs::have)
  in
  walk 0 []
;;

let vivify_multinomial_coefficient_vector v_powers =
  let n = Array.length v_powers in
  let dim = Array.length v_powers.(0) in
  let multinom powers v =
    let rec walk n have =
      if n=dim then have
      else walk (1+n) (have*.(int_power powers.(n) v.(n)))
    in
    walk 0 1.0
  in
  fun coeffs v ->
    if Array.length coeffs <> n then failwith "Bad coefficient vector!"
    else
      let rec walk j have =
	if j = n then have
	else
	  walk (1+j) (have+.coeffs.(j)*.(multinom v_powers.(j) v))
      in walk 0 0.0
;;

let rec random_point_on_d_sphere ?(fun_rng  = fun x -> Random.float x) dim =
  let p = Array.init dim (fun _ -> gauss_random ~fun_rng 0.0 1.0) in
  let len = sqrt(euclidean_len_sq p) in
    if len < 1e-8
    then random_point_on_d_sphere ~fun_rng dim
      (* redo if we had really exceptionally bad luck.
	 Not that this would ever happen... *)
    else
      let factor = 1.0/.len in
	begin
	  for i=0 to dim-1 do
	    p.(i) <- factor*.p.(i);
	  done;
	  p
	end
;;

let reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients
    ?(fun_rng  = fun x -> Random.float x)
    ?(iterations=10)
    terms =
  let nr_terms = Array.length terms in
  let dim = Array.length terms.(0) in
  let point_contribs point =
    Array.init nr_terms
      (fun n ->
	 vivify_multinomial_coefficient_vector
	   (Array.sub terms n 1) [|1.0|] point)
  in
  let weakest_point points_and_term_values =
    let nr_points = Array.length points_and_term_values in
    let lengths = Array.map (fun (_,x) -> sqrt(euclidean_len_sq x)) points_and_term_values in
    let strengths = 
      Array.mapi
	(fun n_here (pt_here,tv_here) -> 
	   let rel_strengths = 
	     Array.mapi
	       (fun m (pt,tv) ->
		  if m=n_here then infinity
		  else 
		    let cos_angle =
		      let x =
			(scalar_product tv_here tv)/.(lengths.(m)*.lengths.(n_here))
		      in if classify_float x = FP_normal then x else 1.0
		    in
		      1.0-.(abs_float cos_angle))
	       points_and_term_values
	   in
	   let (min_strength,_,min_index) =
	     Array.fold_left
	       (fun (sf_s,n,sf_ix) x ->
		  if x < sf_s then (x,n+1,n) else (sf_s,n+1,sf_ix))
	       (infinity,0,0) rel_strengths
	   in min_strength)
	points_and_term_values
    in
    let (min_strength,_,min_index) =
      Array.fold_left
	(fun (sf_s,n,sf_ix) x ->
	   if x < sf_s then (x,n+1,n) else (sf_s,n+1,sf_ix))
	(infinity,0,0) strengths
    in min_index
  in
  let weed_out_weakest_point a =
    let ix = weakest_point a in
      array_one_shorter a ix
  in
  let getpoint () =
    let p = random_point_on_d_sphere ~fun_rng dim in
      (p,point_contribs p)
  in
  let rec iterate_weed_out n have =
      if n = iterations 
      then have 
      else
	let extra_point = getpoint() in
	let have_extended =
	  Array.append [|extra_point|] have
	in iterate_weed_out (1+n) (weed_out_weakest_point have_extended)
  in
    iterate_weed_out 0 (Array.init nr_terms (fun _ -> getpoint()))
;;
(* Tests show this would easily allow us to go up to order 14 or so... *)

let taylor_coefficients_of_spherical_function
    ?(fun_rng  = fun x -> Random.float x)
    order f =
  let terms = independent_so3_scalars order in
  let det_inv = det_and_inv (Array.length terms) in
  let v_points_and_values = reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients terms in
  let (_,mx) = det_inv (Array.map (fun (_,v) -> v) v_points_and_values) in
  let f_values = Array.map (fun (p,_) -> f p) v_points_and_values in
  let all_coeffs = array_pointwise (fun x y -> (x,y)) (mx_x_vec mx f_values) terms in
    array_filter (fun (c,_) -> abs_float c > 1e-10) all_coeffs
;;

let spherical_derivative_ccode_from_coeffs
    ?(deriv_name="h") ?(field_name="m") coeffs_and_powers =
  let nr_coeffs = Array.length coeffs_and_powers in
    if nr_coeffs = 0 then ""
    else
      let format_term powers coeff =
	let expanded_powers =
	  array_join (Array.mapi (fun n p -> Array.make p n) powers)
	in
	let formatted_powers =
	  String.concat "*"
	    (Array.to_list
	       (Array.map
		  (fun n -> Printf.sprintf "%s[%d]" field_name n) expanded_powers))
	in Printf.sprintf "(%12.8f)*%s" coeff formatted_powers
      in
      let (_,powers0) = coeffs_and_powers.(0) in
      let dim = Array.length powers0 in
      let directional_ccode = Array.init dim
	(fun dir ->
	   let contribs = Hashtbl.create 10 in
	   let () =
	     Array.iter 
	       (fun (coeff,powers) ->
		  if powers.(dir)=0 then ()
		  else
		    let new_powers =
		      Array.init dim
			(fun n -> if n = dir then powers.(n)-1 else powers.(n))
		    in
		    let new_coeff = coeff*.(float_of_int powers.(dir)) in
		      hashtbl_increase (+.) contribs new_powers new_coeff)
	       coeffs_and_powers
	   in
	     Printf.sprintf "%s[%d]=%s;" deriv_name dir
	       (String.concat "+"
		  ("0"::(Array.to_list
			   (map_hashtbl_to_array format_term contribs)))))
      in String.concat "\n" (Array.to_list directional_ccode)
;;

(* (C) 2007 Dr. Thomas Fischbacher, SES *)

(* ocamlc -I ../snippets -i nsim_anisotropy.ml

 *)

(* Ad anisotropy energies:

   These are additional energy contributions which are polynomial in
   the direction of some vector. As we usually take the length of that
   vector as being fixed, we have to deal with x^2+y^2+z^2=1 
   over-determined-ness.

   In principle, we could eliminate this by requesting that the
   highest power of z that occurs in our formulae is 1 (because we
   always can substitute z^2=1-x^2-y^2). Actually, it may be a
   somewhat nicer scheme to instead require that our energy functions
   are homogeneous polynomials. So, if we look at 4th order anisotropy,
   we would give E(x,y,z) = x^2 in the form 
   E(x,y,z) = x^4 + x^2y^2 + x^2z^2.

*)

open Snippets;;

let independent_so_n_scalars dim order =
  all_distributions_to_buckets dim order
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


let reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients
    ?(fun_rng = Random.float)
    ?(dim=3)
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
    ?(fun_rng = Random.float)
    ?(dim=3)
    order f =
  let terms = independent_so_n_scalars dim order in
  let det_inv = det_and_inv (Array.length terms) in
  let v_points_and_values = 
    reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients
      ~fun_rng ~dim terms
  in
  let (_,mx) = det_inv (Array.map (fun (_,v) -> v) v_points_and_values) in
  let f_values = Array.map (fun (p,_) -> f p) v_points_and_values in
  let all_coeffs = array_pointwise (fun x y -> (x,y)) (mx_x_vec mx f_values) terms in
    array_filter (fun (c,_) -> abs_float c > 1e-10) all_coeffs
;;

(* (* Example: *)
let tc = taylor_coefficients_of_spherical_function 4
   (fun [|x;y;z|] -> x*.x+.0.8*.y*.y-.0.1*.z*.z*.z*.z);;
*)

let _ht_to_poly =
  map_hashtbl_to_array
    ~sorter:(fun (_,p1) (_,p2) -> compare p1 p2)
    (fun p c -> (c,p))
;;

let polypotential_gradient ?(dim=3) contribs =
  let nr_contribs = Array.length contribs in
    if nr_contribs = 0 
    then Array.make dim [||]
    else
      let d_dn direction =
	let ht = Hashtbl.create 17 in
	let () = Array.iter
	  (fun (coeff,powers) ->
	     let p = powers.(direction) in
	       if p = 0 then ()
	     else
	       let new_powers = Array.copy powers in
	       let new_coeff = coeff*.(float_of_int p) in
	       let () = new_powers.(direction) <- p-1 in
		 hashtbl_increase (+.) ht new_powers new_coeff)
	  contribs
	in _ht_to_poly ht
      in Array.init dim d_dn
;;

let unit_sphere_tangential_polypotential_gradient ?(dim=3) contribs =
  let raw_gradient = polypotential_gradient ~dim contribs in
  let ht_sprod = Hashtbl.create 17 in
  let () = Array.iteri
    (fun dir poly ->
       Array.iter
	 (fun (c,p) ->
	    let rp = Array.copy p in
	    let () = rp.(dir) <- p.(dir)+1 in
	      hashtbl_increase (+.) ht_sprod rp c
	 ) poly
    ) raw_gradient
  in
    Array.init dim
      (fun dir ->
	 let ht = Hashtbl.create 17 in
	 let () = Array.iter
	   (fun (c,p) -> hashtbl_increase (+.) ht p c) raw_gradient.(dir)
	 in
	 let () = Hashtbl.iter
	   (fun p c ->
	      let rp = Array.copy p in
	      let () = rp.(dir) <- p.(dir) + 1 in
		hashtbl_increase (+.) ht rp (-.c))
	   ht_sprod
	 in
	   _ht_to_poly ht)
;;

let polypotential_equation_rhs ?(correction_factor=1.0) name poly =
  let contribs = Array.map
    (fun (c,p) ->
       let nr_factors = Array.fold_left (+) 0 p in
       let a = Array.make nr_factors "" in
       let rec walk pos_a pos_p power_todo =
	 if pos_a = nr_factors then a
	 else if power_todo = 0
	 then walk pos_a (1+pos_p) p.(1+pos_p)
	 else
	   let () = a.(pos_a) <- Printf.sprintf "%s(%d)" name pos_p in
	     walk (1+pos_a) pos_p (power_todo-1)
       in
       let str_factors = walk 0 0 p.(0) in
	 String.concat "*"
	   ((Printf.sprintf "(%f)" (c*.correction_factor))::(Array.to_list str_factors)))
    poly
  in String.concat " + " (Array.to_list contribs)
;;
    
let anisotropy_equations
    ?factor_mu0
    ?(fun_rng=Random.float) ?(dim=3)
    ?(name_E="E") ?(name_m="m") ?(name_H="H")
    order f =
  let tc = taylor_coefficients_of_spherical_function ~fun_rng ~dim order f in
  let grad = unit_sphere_tangential_polypotential_gradient ~dim tc in
  let neg_grad = Array.map (Array.map (fun (c,v) -> (-.c,v))) grad in
  let eq_E = Printf.sprintf "%s <- %s;" name_E (polypotential_equation_rhs name_m tc) in
  let eq_H =
    String.concat ""
      (Array.to_list
	 (Array.mapi
	    (fun dir contribs ->
	       Printf.sprintf "%s(%d) <- %s;\n" name_H dir
		 (polypotential_equation_rhs ?correction_factor:factor_mu0 name_m contribs)
	    ) neg_grad))
  in (eq_E, eq_H)
;;
    
(* Example:    
let ae = anisotropy_equations 4 (fun [|x;y;z|] -> x*.x);;
*)

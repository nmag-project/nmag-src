
#use "topfind";;
#require "nsim_grammars";;
#require "snippets";;
open Snippets;;
open Localeqn_parser;;

let parsed_ex1 = parse_or_error
  Localeqn_lexer.token
  Localeqn_parser.parse_localeqn
"%range i:3, j:3, k:3;
dMdt(i) <- -5.3*eps(i,j,k)*m(j)*H_total(k);"
;;

let parsed_ex2 = parse_or_error
  Localeqn_lexer.token
  Localeqn_parser.parse_localeqn
"%range i:3, j:3, k:3;
dMdt(i) <- delta(i,j)*M(j)+M(i);
"
;;

(* NOTE: could be generalized to also allow
   delta(a,b, i,j) = 1/2*eps(a,b,p)*eps(i,j,p)
   or delta(a,b,c, i,j,k)
*)

let special_tensor_delta2 ~ix_ranges indices =
  let range v =
    let (_,r) = array_find (fun (n,_) -> n=v) ix_ranges
    in r
  in
  let nr_indices = Array.length indices in
    if nr_indices <> 2
    then failwith "delta tensor can only carry 2 indices!"
    else
      try
	let process_nv n v =
	  ([v],Array.init (range v) (fun n -> (1.0,[n])))
	in
	  match indices with
	    | [|IX_int n1;IX_int n2|] ->
		if n1=n2 then ([],[| (1.0,[]) |]) else ([],[||])
	    | [|IX_var v1;IX_var v2|] ->
		let r1 = range v1 and r2 = range v2 in
		  if r1<>r2
		  then failwith "delta tensor used with different-range indices!"
		    (* May want to modify/adjust this later on! *)
		  else
		    if v1=v2
		      (* same name, hence this is just a combinatorical factor 
			 (subtleties arise when the same index occurs yet
			 another time in a product... We should prevent this,
			 but actually neither can nor do at the moment...
			 ) *)
		    then
		      ([],[| (float_of_int r1,[]) |])
		    else
		      ([v1;v2],Array.init r1 (fun n -> (1.0,[n;n])))
	    | [|IX_var v1; IX_int n1|] ->
		process_nv n1 v1
	    | [|IX_int n1; IX_var v1|] ->
		process_nv n1 v1
	    | _ -> failwith "delta tensor carries strange indices!"
      with | Not_found -> failwith "delta tensor carries index of unspecified range!"
;;


let special_tensor_epsilon ~ix_ranges indices =
  (* Note: we silently assume we are not fed with junk like eps(1,2,3,8)! *)
  let range v =
    let (_,r) = array_find (fun (n,_) -> n=v) ix_ranges
    in r
  in
  let nr_indices = Array.length indices in
  let sorted_indices = array_sorted compare indices in
    if sorted_array_has_duplicate (=) sorted_indices
    then (* epsilon reduces to zero! *)
      ([],[||])
    else
      let named_indices =
	array_mapfilter
	  (fun ix -> match ix with | IX_int _ -> None | IX_var n -> Some n) sorted_indices
      in
      let nr_named_indices = Array.length named_indices in
      let possible_choices = 
	let a = Array.make nr_named_indices (-1) in
	let rec walk pos_sorted_indices pos_result nr_candidate =
	    if pos_result = nr_named_indices then a 
	    else if pos_sorted_indices=nr_indices
	  then
	    let () = a.(pos_result) <- nr_candidate in
	      walk pos_sorted_indices (1+pos_result) (1+nr_candidate)
	  else
	    match sorted_indices.(pos_sorted_indices) with
	      | IX_var _ -> walk (1+pos_sorted_indices) pos_result nr_candidate
	      | IX_int j ->
		  if nr_candidate < j then 
		    let () = a.(pos_result) <- nr_candidate in
		      walk (1+pos_sorted_indices) (1+pos_result) (1+nr_candidate)
		  else walk (1+pos_sorted_indices) pos_result nr_candidate
	in walk 0 0 0
      in
      let named_index_positions =
	Array.map (fun n -> array_position_if (fun ix -> ix = IX_var n) indices 0) named_indices
      in
      let r_result= ref [] in
      let full_perm = Array.map (fun ix -> match ix with | IX_var _ -> (-1) | IX_int n -> n) indices in
      let () = for_permutations nr_named_indices
	(fun partial_perm ->
	   begin
	     for i=0 to nr_named_indices-1 do
	       full_perm.(named_index_positions.(i)) <- possible_choices.(partial_perm.(i));
	     done;
	     let sign = float_of_int (permutation_sign full_perm) in
	       r_result:= (sign,Array.to_list (Array.map (fun n -> possible_choices.(n)) partial_perm))::(!r_result)
	   end)
      in
	(Array.to_list named_indices,Array.of_list (List.rev !r_result))
;;

let default_special_tensors=[("delta",special_tensor_delta2);("eps",special_tensor_epsilon)];;

(* XXX NOTE: this code is not really good, and quite ad-hoc! *)
let local_equation_normal_form ?(special_tensors=default_special_tensors) ~ix_ranges ~lhs ~rhs () =
  let rec nf1 term =
    match term with
      | Tensor_func _ ->
	  failwith "Non-polynomial tensor functions are not handled yet!"
      | Tensor_float x -> [|(x,[||])|]
      | Tensor_varindexed tensor -> [|(1.0,[|tensor|])|]
      | Tensor_sum summands ->
	  array_join (Array.map nf1 (Array.of_list summands))
      | Tensor_product factors ->
	  let n_factors = Array.map nf1 (Array.of_list factors) in
	  let factor_nr_contribs = Array.map Array.length n_factors in
	  let ht_result = Hashtbl.create 17 in
	  let () = multifor factor_nr_contribs
	    (fun _ selection ->
	       let factors = Array.mapi (fun n j -> n_factors.(n).(j)) selection
	       in
	       let coeff = Array.fold_left (fun sf (c,_) -> sf*.c) 1.0 factors in
	       let tensors = array_join (Array.map (fun (_,x) -> x) factors) in
	       let () = Array.sort compare tensors in
		 hashtbl_increase (+.) ht_result tensors coeff)
	  in
	    map_hashtbl_to_array ~sorter:(fun (_,x) (_,y) -> compare x y)
	      (fun k c -> (c,k))
	      ht_result
  in
  let fix_indices fixed_indices these_indices =
    Array.map
      (fun ix ->
	 match ix with
	   | IX_int _ -> ix
	   | IX_var name ->
	       try
		 let (_,resolved) =
		   List.find (fun (n,_) -> n=name) fixed_indices
		 in IX_int resolved
	       with | Not_found -> ix)
      these_indices
  in
  let sorted_array_of_list li =
    let a = Array.of_list li in
    let () = Array.sort compare a in
      a
  in
  let ht_special_tensors =
    map_array_to_hashtbl ~mapper:identity (Array.of_list special_tensors)
  in
  let open_indices indices =
    array_mapfilter
      (fun ix -> match ix with | IX_var n -> Some n | _ -> None)
      indices
  in
  let ranges_of indices_to_fix = 
    try
      Array.map
	(fun name ->
	   let (_,range) = array_find (fun (n,r) -> n=name) ix_ranges in range)
	indices_to_fix
    with | Not_found ->
      failwith "Encountered named index with unknown range!"
  in
  let extend_index_fixings indices_to_fix fix_ranges fixed_so_far =
    let extra_fixings =
      array_map2 (fun x y -> (x,y))
	indices_to_fix fix_ranges
    in
      List.append (Array.to_list extra_fixings) fixed_so_far
  in
  let resolve_running_indices fixed_indices nf_term =
    let ht_explicit_contribs = Hashtbl.create 17 in
    let rec walk_contrib coeff factors_done factors_todo fixed_indices =
      match factors_todo with
	| [] ->
	    hashtbl_increase (+.)
	      ht_explicit_contribs
	      (sorted_array_of_list factors_done) coeff
	| (stem,raw_indices)::rest_factors_todo ->
	    let indices = fix_indices fixed_indices raw_indices in
	      if Hashtbl.mem ht_special_tensors stem
	      then
		let fun_special = Hashtbl.find ht_special_tensors stem in
		let (extra_varindices,v_relevant_entries) =
		  fun_special ~ix_ranges indices
		in
		  Array.iter 
		    (fun (tensor_coeff,further_fixings) ->
		       walk_contrib (coeff*.tensor_coeff)
			 factors_done (* constant tensor is eliminated here! *)
			 rest_factors_todo
			 (List.append (List.map2 (fun x y -> (x,y)) extra_varindices further_fixings)
			    fixed_indices))
		    v_relevant_entries
	      else (* non-special (i.e. non-constant) tensor *)
		let indices_to_fix = open_indices indices in
		let fix_ranges = ranges_of indices_to_fix in
		let () = multifor fix_ranges
		  (fun _ instantiation ->
		     let new_fixed_indices = extend_index_fixings indices_to_fix instantiation fixed_indices in
		     let fixed_here = fix_indices new_fixed_indices raw_indices in
		       walk_contrib
			 coeff
			 ((stem,fixed_here)::factors_done)
			 rest_factors_todo
			 new_fixed_indices)
		in ()
    in
    let (coeff,factors) = nf_term in
    let () = walk_contrib coeff [] (Array.to_list factors) fixed_indices in
      map_hashtbl_to_array ~sorter:compare (fun factors coeff -> (coeff,factors)) ht_explicit_contribs 
  in
  let nf1_rhs = nf1 rhs in
  let ((lhs_name:string),lhs_indices) = lhs in
  let lhs_open_indices = open_indices lhs_indices in
  let lhs_index_ranges = ranges_of lhs_open_indices in
  let r_result = ref [] in
  let () = multifor lhs_index_ranges 
    (fun _ lhs_index_values ->
       let lhs_fixings = extend_index_fixings lhs_open_indices lhs_index_values [] in
       let lhs_fixed = fix_indices lhs_fixings lhs_indices
       in
       let rhs_fixed = array_join (Array.map (resolve_running_indices lhs_fixings) nf1_rhs) in
	 r_result:= ((lhs_name,lhs_fixed),rhs_fixed)::(!r_result)
    )
  in (* nf1_rhs DDD *)
   (Array.of_list (List.rev !r_result))
;;


let parse_localeqn str =
  let parsed = parse_or_error Localeqn_lexer.token Localeqn_parser.parse_localeqn str in
  let (local_tensors,ix_ranges,v_lhs_rhs) = parsed in
    (local_tensors,Array.map (fun (lhs,rhs) -> local_equation_normal_form ~ix_ranges ~lhs ~rhs ()) v_lhs_rhs)
;;

(* XXX NOTE: the grammar does need to be sorted out to some degree here!
   The +(-1.0) actually is necessary at present!
*)


let parsed_eqn_ccode (local_tensors,vv_lhs_rhs) =
  try
    let aconcat separator a = String.concat separator (Array.to_list a) in
    let local_tensor_alloc_def_undef (stem,indices) =
      let nr_indices = Array.length indices in
      let index_weights =
	let a = Array.make nr_indices 1 in
	  begin
	    for i=0 to nr_indices-2 do
	      a.(nr_indices-2-i) <- indices.(nr_indices-2-i)*a.(nr_indices-1-i)
	    done;
	    a
	  end
      in
      let range = Array.fold_left ( * ) 1 indices in
	(Printf.sprintf " static double _%s[%d];\n" stem range,
	 Printf.sprintf "#define %s(%s) _%s[%s]\n" stem
	   (aconcat "," (Array.init nr_indices (fun n -> Printf.sprintf "ix%d" n)))
	   stem
	   (aconcat "+" (Array.init nr_indices (fun n -> Printf.sprintf "%d*ix%d" index_weights.(n) n))),
	 Printf.sprintf "#undef %s\n" stem)
    in
    let lt_allocs_defs_undefs = Array.map local_tensor_alloc_def_undef local_tensors in
    let tensor_accessor (stem,indices) =
      if Array.length indices=0
      then stem
      else
	Printf.sprintf "%s(%s)" stem
	  (aconcat "," (Array.map (fun (IX_int ix) -> string_of_int ix) indices))
    in
    let xlate_lhs_rhs ((lhs_stem,lhs_indices) as lhs,rhs) =
      Printf.sprintf "   %s = %s+0.0;\n"
	(* The "+0.0" is a dirty hack to deal with empty products... *)
	(tensor_accessor lhs)
	(aconcat "\n            +"
	   (Array.map
	      (fun (coeff,factors) ->
		 Printf.sprintf "(%f)%s" coeff
		   (aconcat ""
		      (Array.map
			 (fun factor ->
			    Printf.sprintf "*%s" (tensor_accessor factor))
			 factors)))
	      rhs))
    in
    let xlate_v_lhs_rhs v_lhs_rhs =
      if Array.length v_lhs_rhs=0 then ""
      else
	let body = aconcat "" (Array.map xlate_lhs_rhs v_lhs_rhs) in
	let ((lhs_stem,_),_) = v_lhs_rhs.(0) in
	  if(-1<>array_position_if (fun (stem,_) -> stem=lhs_stem) local_tensors 0)
	  then body
	  else
	    Printf.sprintf " if(have_%s){\n%s\n }\n" lhs_stem body
    in
      Printf.sprintf
"
{
%s

%s

%s

%s
}
"
	(aconcat "" (Array.map (fun (x,_,_) -> x) lt_allocs_defs_undefs))
	(aconcat "" (Array.map (fun (_,x,_) -> x) lt_allocs_defs_undefs))
	(aconcat "" (Array.map xlate_v_lhs_rhs vv_lhs_rhs))
	(aconcat "" (Array.map (fun (_,_,x) -> x) lt_allocs_defs_undefs))
  with | _ -> failwith "parsed_eqn_ccode failed!"
;;


let ex0 = parse_localeqn
"
%range i:3, j:3, k:3,p:3,q:3;
%local foo(3,3,3);

dMdt(i) <-  10.0*eps(i,j,k)*M(j)*H_total(k);
";;


let ex1 = parse_localeqn
"
%range i:3, j:3, k:3,p:3,q:3;
%local foo(3,3,3);

dMdt(i) <-  10.0*eps(i,j,k)*M(j)*H_total(k)
           +20.0*eps(i,j,k)*M(j)*eps(k,p,q)*M(p)*H_total(q)
           +0.1*M(i)*(M(j)*M(j)+(-1.0));
";;


Printf.printf "%s\n" (parsed_eqn_ccode ex1);;

let ex2 = parse_localeqn
"
%range i:3, j:3, k:3,p:3,q:3;
%local foo(3,3,3);


H_total(j) <- H_exch(j)+H_anis(j)+H_demag(j)+H_ext(j)+J_electric(k)*grad_M(k,j);

dMdt(i) <-  10.0*eps(i,j,k)*M(j)*H_total(k)
           +20.0*eps(i,j,k)*M(j)*eps(k,p,q)*M(p)*H_total(q)
           +0.1*M(i)*(M(j)*M(j)+(-1.0));

M(i) <- M(i)+dt*dMdt(i);
";;

Printf.printf "%s\n" (parsed_eqn_ccode ex2);;


token_stream Localeqn_lexer.token Localeqn_parser.EOF "%range j:3, k:3;
dM(j) <- eps(2,j,k)*M(k);
";;

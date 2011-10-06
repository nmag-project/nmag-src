
type ix = IX_int of int | IX_var of string

type tensor_term = 
  | Tensor_sum of tensor_term list
  | Tensor_product of tensor_term list
  | Tensor_float of float
  | Tensor_varindexed of (string * (ix array))
  | Tensor_func of (string * tensor_term)

type tensor_lvalue = string * (ix array)
type assignment = tensor_lvalue * tensor_term
type range = string * int
type local_spec = string * (int array)

type local_eqn = local_spec array * range array * assignment array

(* from snippets: *)
  let multifor v_max_indices f =
    let nr_indices = Array.length v_max_indices in
    let nim1 = nr_indices-1 in
    let v_count = Array.make nr_indices 0 in
    let rec walk n =
      let rec inc incpos =
	if incpos = (-1) then false
	else
	  let increased = v_count.(incpos)+1 in
	    if increased = v_max_indices.(incpos)
	    then (* Increase to the left *)
	      begin
		v_count.(incpos) <- 0;
		inc (incpos-1);
	      end
	    else
	      begin
		v_count.(incpos) <- increased;
		true
	      end
      in
      let () = f n v_count in
      let go_on = inc nim1 in
	if go_on then walk (n+1) else ()
    in walk 0
  ;;

let forall_index_instantiations v_all_index_vars sum_specs f =
  let v_all_index_ranges =
    Array.map
      (fun n -> let (_,range) = List.find (fun (name,_) -> n=name) sum_specs in range)
      v_all_index_vars
  in
    multifor v_all_index_ranges
      (fun _ v_index_set ->
	 let ix_value ix =
	   match ix with
	     | IX_int v -> v
	     | IX_var name ->
		 let rec walk n =
		   if v_all_index_vars.(n) = name then v_index_set.(n)
		   else walk (1+n)
		 in walk 0
	 in
	   f ~ix_value)
;;

let new_tensor_product ?(extra_sign=1.0) sign_list_pair =
  let s1, fl = sign_list_pair in
  let s = s1*.extra_sign in
    Tensor_product
      (if s == 1.0
       then fl
       else (Tensor_float s)::fl)
;;

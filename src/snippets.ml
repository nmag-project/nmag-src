(*
   (C) 2005 Dr. Thomas Fischbacher, Giuliano Bordignon, Dr. Hans Fangohr,
   SES, University of Southampton

   A collection of small handy snippets.
   Subject to change a lot over time.
 *)

let version () = "$Id$";;

let __features = ref [];;

let register_feature ?(domain="main") name str =
  let assoc_replace assoc key fun_new_value val_empty =
    let rec walk a =
      match a with
	| [] -> [(key,fun_new_value val_empty)]
	| (((hk,hv) as h)::tl) ->
	    if key = hk then (hk,fun_new_value hv)::tl else h::(walk tl)
    in walk assoc
  in
  let new_features =
    assoc_replace (!__features) domain
      (fun old_entries ->
	 assoc_replace old_entries name
	   (fun old_entry -> str) "")
      []
  in
    __features:=new_features
  ;;

let get_feature ?(domain="main") name =
  try
    let (_,domain_entry) = List.find (fun (x,_) -> x=domain) !__features in
    let (_,value) = List.find (fun (x,_) -> x=name) domain_entry
    in
      Some value
  with | Not_found -> None
;;

let all_features () = !__features;;

(* This is just convenient sometimes: *)

type 'a array2 = 'a array array;;

let identity x = x;;
let do_nothing () = ();;
let constantly x y = x;;
let evaluate_at x f = f x;;


let pi=3.1415926535897932384626;;

let deg_rad x = x *. (pi /. 180.0);;

let rad_deg x = x *. (180.0 /. pi);;

let round_to_n_digits ?(base=10) n nr =
  let f_base = float_of_int base in
  let abs_nr = abs_float nr in
  let the_log = log abs_nr /. (log f_base) in
  let digit_shift = 1.0+.(floor the_log) in
  let shift_power = f_base ** (digit_shift-.(float_of_int n)) in
  shift_power*.(floor ((nr/.shift_power)+.0.5))
;;

(* Sometimes, we need this to make the exhaustiveness-of-cases check happy: *)
let impossible () = failwith "Something thought impossible just happened";;

let not_implemented_yet message =
  fun _ ->
    failwith (Printf.sprintf "This function has not been implemented yet: %s" message)
;;

let missing_param () = failwith "Required user parameter not provided";;

let is_obj_nan x = (* XXX improve me, using built-in "float type", see Pervasives *)
  Obj.tag (Obj.repr x) = Obj.double_tag &&
  (let f = (Obj.magic x : float) in not (f = f))
;;

let iterate n f zero =
  let rec walk n_now v_now =
    if n_now = n then v_now
    else walk (1+n_now) (f v_now)
  in walk 0 zero
;;

let church = iterate;;


let pointwise comp f g x = comp (f x) (g x)
;;

let optionally f opt_x =
  match opt_x with
    | None -> None
    | Some x -> Some (f x)
;;

let array_pointwise comp a1 a2 =
  let len = min (Array.length a1) (Array.length a2) in
  Array.init len (fun n -> comp a1.(n) a2.(n))
;;

let array_map2 = array_pointwise
;;

let list_foreach_do a f = List.iter f a;;
let array_foreach_do a f = Array.iter f a;;
let array_foreach_do_n a f = Array.iteri f a;;
let hashtbl_foreach_do h f = Hashtbl.iter f h;;


let array_compare_pointwise f a1 a2 =
  let len1 = Array.length a1
  and len2 = Array.length a2 in
  if len1 <> len2 then false
  else
    let rec walk n =
      if n = len1 then true else
      if f a1.(n) a2.(n) then walk (1+n) else false
    in walk 0
;;


let path_and_filename_of_fullpath fullpath =
  let filename_index =
    try String.rindex fullpath '/' with | Not_found -> (-1)
  in
  (Str.string_before fullpath (max filename_index 0),
   Str.string_after fullpath (filename_index+1)
  );;


let memoized ht f arg =
  try
    Hashtbl.find ht arg
  with
  | Not_found ->
      let result = f arg in
      let () = Hashtbl.add ht arg result in
      result
;;


let uniq fun_generator =
  let ht_uniq = Hashtbl.create 10 in
  let rec work () =
    match fun_generator () with
      | None -> ht_uniq
      | Some x ->
	  let () =
	    try
	      let _ = Hashtbl.find ht_uniq x in
		()
	    with
	      | Not_found ->
		  Hashtbl.add ht_uniq x x
	  in work()
  in work()
;;

(* Fast floatingpoint factorials with memoization. *)

(* Interestingly, this is even reentrant! *)
let float_factorial =
  let _known_factorials = ref [|1.0;1.0;2.0;6.0;24.0;120.0;720.0|] in
  (fun n ->
    let known_factorials = !_known_factorials in
    let nr_known = Array.length known_factorials in
    if n < nr_known
    then
      known_factorials.(n)
    else
      let new_known_factorials = Array.make (n+1) 0.0 in
      begin
	for i=0 to nr_known-1 do
	  new_known_factorials.(i) <- known_factorials.(i)
	done;
	(let rec fill f_pos pos =
	  if pos > n then ()
	  else
	    let () = new_known_factorials.(pos) <- f_pos in
	    fill (f_pos *. (float_of_int (pos+1))) (pos+1)
	in
	fill (known_factorials.(nr_known-1)*.(float_of_int nr_known)) nr_known);
	_known_factorials := new_known_factorials;
	new_known_factorials.(n)
      end)
;;

let int_gcd a b =
  let rec int_gcd0 a b =
    if b=1 then 1 else
    let amb = a mod b in
    if amb = 0 then b else int_gcd0 b amb
  in int_gcd0 (abs a) (abs b)
;;

let int_lcm a b = b*(a/(int_gcd a b));;

let rec int_power n x =
  if n < 0 then 1.0/.(int_power (-n) x)
  else
    let rec walk x_power n_todo have =
      if n_todo = 0 then have
      else
	walk
	  (x_power*.x_power)
	  (n_todo lsr 1)
	  (if (n_todo land 1) <> 0 then (have*.x_power) else have)
    in walk x n 1.0
;;

let int_length n =
  let rec walk j n_rest =
    if n_rest=0 then j
    else walk (1+j) (n_rest lsr 1)
  in walk 0 n
;;

let do_for_any_two_of arr f =
  let len = Array.length arr in
  for i1 = 0 to len-1 do
    for i2 = i1+1 to len-1 do
        f arr.(i1) arr.(i2)
    done;
  done
;;

(* Some combinatorics (also in the tf-spellbook lisp library)... *)

let do_for_all_distributions nr_to_distribute v_max f_todo =
  let nr_bins = Array.length v_max in
  let highest_bin = nr_bins-1 in
  let v_accum_max =
    (* v_accum_max will hold the number of elements all bins
       to the left of (and including) a given one can hold *)
    let a = Array.make nr_bins 0 in
    let rec walk j sum =
      if j < 0 then a
      else
	let new_sum = v_max.(j)+sum in
	let () = a.(j) <- new_sum in
	  walk (j-1) new_sum
    in walk highest_bin 0
  in
  let v_count = Array.make nr_bins 0 in
  let rec count count_pos rest_i_have =
    let count_val = v_count.(count_pos) in
      if 0=rest_i_have
      then
	let () = f_todo v_count in
	  v_count.(count_pos) <- 0
      else if count_pos=highest_bin
      then
	let () = v_count.(count_pos) <- count_val+rest_i_have in
	let () = f_todo v_count in
	  v_count.(count_pos) <- 0
      else
	let max_here = min rest_i_have v_max.(count_pos)
	and min_here = max 0 (rest_i_have-v_accum_max.(1+count_pos))
	in
	  begin
	    for nj=min_here to max_here do
	      v_count.(count_pos) <- nj;
	      count (1+count_pos) (rest_i_have-nj)
	    done;
	    v_count.(count_pos) <- 0
	  end
  in count 0 nr_to_distribute
;;

(* Example:
do_for_all_distributions 4 [|4;4;4;4|] (fun a -> Printf.printf "%s\n" (int_array_to_string a));;
*)


(* Note: elements never vanish when coefficients go to zero. *)
let hashtbl_increase add_fun ht key delta =
  let new_entry =
    try add_fun delta (Hashtbl.find ht key)
    with
      | Not_found -> delta
  in Hashtbl.replace ht key new_entry
;;

let hashtbl_push ht key value =
  let old_entry =
    try Hashtbl.find ht key
    with
      | Not_found -> []
  in Hashtbl.replace ht key (value::old_entry)
;;

let hashtbl_pushn ht key values =
  let old_entry =
    try Hashtbl.find ht key
    with
      | Not_found -> []
  in Hashtbl.replace ht key (List.append values old_entry)
;;


let hashtbl_sum add_fun ht1 ht2 =
  let sum = Hashtbl.copy ht1 in
  let () = Hashtbl.iter (fun k v -> hashtbl_increase add_fun sum k v) ht2 in
    sum;;

let hashtbl_reduce_map f_reduce f_map ht =
  let ht_new = Hashtbl.create (Hashtbl.length ht) in
  let () =Hashtbl.iter
    (fun k v ->
       let (fk,fv) = f_map k v in
       try
	 let old_entry = Hashtbl.find ht_new fk in
	 Hashtbl.replace ht_new fk (f_reduce fk fv old_entry)
       with
       | Not_found ->
	   Hashtbl.add ht_new fk fv) ht
  in ht_new
;;

let hashtbl_iteri f ht =
  let n = ref 0 in
  Hashtbl.iter
    (fun k v ->
      let () = f (!n) k v in
      n:= !n+1) ht
;;

let hashtbl_delete_if property ht =
  let to_delete = ref [] in
  let () =
    Hashtbl.iter
      (fun k v ->
	if property k v
	then
	  to_delete:= k::(!to_delete)
	else ())
      ht
  in
  let rec walk rest =
    match rest with
    | [] -> ()
    | (next::new_rest) ->
	let () = Hashtbl.remove ht next
	in walk new_rest
  in walk (!to_delete)
;;

let hashtbl_map_values f ht =
  let ht_new = Hashtbl.create (Hashtbl.length ht) in
  let () = Hashtbl.iter
    (fun k v -> Hashtbl.add ht_new k (f k v)) ht in
    ht_new
;;

let hashtbl_find_or ?(make_new_entry=false) ht key fun_new_entry =
  try Hashtbl.find ht key
  with
    | Not_found ->
	let z = fun_new_entry () in
	let () = (if make_new_entry then Hashtbl.add ht key z else ()) in
	  z
;;

exception Stop_it;;
(* This is local and used only in the function below. *)

let hashtbl_arbitrary_element ht =
  let have_it = ref None in
  let () =
    try
      Hashtbl.iter
	(fun k v ->
	   begin
	     have_it := Some (k,v);
	     raise Stop_it;
	   end)
	ht
    with
      | Stop_it -> ()
  in !have_it;;

let map_hashtbl_to_array ?sorter mapper ht =
  let nr_entries = Hashtbl.length ht in
    if nr_entries = 0
    then [||]
    else
      let opt_kv = hashtbl_arbitrary_element ht in
      let result =
	match opt_kv with
	  | None -> impossible ()
	  | Some (k,v) -> Array.make nr_entries (mapper k v)
      in
      let ref_index = ref 0 in
      let () = Hashtbl.iter
	(fun k v ->
	   begin
	     result.(!ref_index) <- mapper k v;
	     ref_index := (1+ !ref_index);
	   end) ht
      in
	match sorter with
	  | Some s ->
	      let () = Array.sort s result in result
	  | _ -> result
;;

let hashtbl_to_array ?sorter ht =
  match sorter with
  | None ->
      map_hashtbl_to_array (fun k v -> (k,v)) ht
  | Some s ->
      map_hashtbl_to_array ~sorter:s (fun k v -> (k,v)) ht
;;

let map_array_to_hashtbl ~mapper arr =
  let h = Hashtbl.create (Array.length arr) in
  let () = Array.iter (fun (k,v) -> Hashtbl.replace h k (mapper v)) arr in
    h;;

let hashtbl_keys ?sorter ht =
  let nr_entries = Hashtbl.length ht in
    if nr_entries = 0
    then [||]
    else
      let opt_kv = hashtbl_arbitrary_element ht in
      let result =
	match opt_kv with
	  | None -> impossible ()
	  | Some (k,_) -> Array.make nr_entries k
      in
      let ref_index = ref 0 in
      let () = Hashtbl.iter
	(fun k _ ->
	   begin
	     result.(!ref_index) <- k;
	     ref_index := (1+ !ref_index);
	   end) ht
      in
	match sorter with
	  | Some s ->
	      let () = Array.sort s result in result
	  | _ -> result
;;

let array_all_satisfy p arr =
  let len = Array.length arr in
  let rec walk n =
    if n = len then true
    else if p (arr.(n)) then walk (n+1)
    else false
  in walk 0
;;

let array_sorted sorter arr =
  let a2 = Array.copy arr in
  let () = Array.sort sorter a2 in
  a2;;

let sorted_array_has_duplicate compare a =
  let len = Array.length a in
    if len <= 1 then false
    else
      let rec walk pos =
	if pos=len then false
	else if compare a.(pos) a.(pos-1) then true
	else walk (1+pos)
      in walk 1
;;

let array_rev arr =
  let max_ix = (Array.length arr)-1 in
  Array.init (1+max_ix) (fun ix -> arr.(max_ix-ix));;

let array_repeat_ntimes n arr =
  let len = Array.length arr in
  if len=0 then arr
  else
    Array.init (n*len) (fun ix -> arr.(ix mod len))
;;


let array_reducing_mapi f arr start =
  let len = Array.length arr in
  if len = 0
  then [||]
  else
    let (entry0,next) = f start arr.(0) 0 in
    (* AAH! Unnecessary consing pains! Where is my multiple-value-bind? *)
    let result = Array.make len entry0 in
    let rec walk so_far pos =
      if pos = len
      then result
      else
	let (entry,next) = f so_far arr.(pos) pos in
	let () = result.(pos) <- entry in
	walk next (pos+1)
    in walk next 1
;;

let array_find p arr =
  let len = Array.length arr in
  let rec walk pos =
    if pos = len then raise Not_found
    else if p arr.(pos) then arr.(pos)
    else walk (1+pos)
  in walk 0
;;

let array_filter p arr =
  (* One problem here is that we cannot easily record filter results in a
     bit-vector, as OCaml does not provide anything like (simple-array bit ( * ) )

     Hence, we have to do more consing than we would like to.
  *)
  let pre_result = Array.copy arr in
  let len = Array.length arr in
  let rec walk nr_good pos_src =
    if pos_src = len
    then Array.sub pre_result 0 nr_good
    else
      if p arr.(pos_src)
      then
	let () = pre_result.(nr_good) <- arr.(pos_src) in
	  walk (1+ nr_good) (1+ pos_src)
      else
	walk nr_good (1+ pos_src)
  in walk 0 0
;;

let array_mapfilter p arr =
  let len = Array.length arr in
  let rec pos_and_val_first_good n =
    if n = len
    then None
    else
      (let p_n = p arr.(n) in
      match p_n with
      | None -> pos_and_val_first_good (1+n)
      | Some x -> Some (n,x))
	(* Note: may want to store n to a ref to reduce consing? *)
  in
  let rec gather pos_now have_now =
    match pos_and_val_first_good pos_now
	with
    | None -> Array.of_list (List.rev have_now)
    | Some (pos_next,val_next) ->
	gather (1+pos_next) (val_next::have_now)
  in gather 0 []
;;

let array_mapfilteri p arr =
  let len = Array.length arr in
  let rec pos_and_val_first_good n =
    if n = len
    then None
    else
      (let p_n = p n arr.(n) in
      match p_n with
      | None -> pos_and_val_first_good (1+n)
      | Some x -> Some (n,x))
	(* Note: may want to store n to a ref to reduce consing? *)
  in
  let rec gather pos_now have_now =
    match pos_and_val_first_good pos_now
	with
    | None -> Array.of_list (List.rev have_now)
    | Some (pos_next,val_next) ->
	gather (1+pos_next) (val_next::have_now)
  in gather 0 []
;;

(* Same as before, but not only provide the original index, but also the new index *)
let array_mapfilterij p arr =
  let len = Array.length arr in
  let rec pos_and_val_first_good nr_have n =
    if n = len
    then None
    else
      (let p_n = p n nr_have arr.(n) in
      match p_n with
      | None -> pos_and_val_first_good nr_have (1+n)
      | Some x -> Some (n,nr_have,x))
	(* Note: may want to store n to a ref to reduce consing? *)
  in
  let rec gather pos_now have_now nr_have_now =
    match pos_and_val_first_good nr_have_now pos_now
	with
    | None -> Array.of_list (List.rev have_now)
    | Some (pos_next_src,pos_next_dst,val_next) ->
	gather (1+pos_next_src) (val_next::have_now) (1+nr_have_now)
  in gather 0 [] 0
;;



let array_one_shorter arr pos =
  let len = Array.length arr in
  let c = Array.make (len-1) arr.(0) in
  begin
    for i=0 to pos-1 do
      c.(i) <- arr.(i);
    done;
    for i=pos to len-1-1 do
      c.(i) <- arr.(i+1);
    done;
    c
  end
;;

let array_one_shorter_mapped f arr pos =
  let len = Array.length arr in
  let c = Array.make (len-1) (f arr.(0)) in
  begin
    for i=0 to pos-1 do
      c.(i) <- f arr.(i);
    done;
    for i=pos to len-1-1 do
      c.(i) <- f arr.(i+1);
    done;
    c
  end
;;

let array_all_one_shorter arr =
  Array.init (Array.length arr) (fun n -> array_one_shorter arr n);;

let array_all_one_shorter_mapped f arr =
  Array.init (Array.length arr) (fun n -> array_one_shorter_mapped f arr n);;


let array_outer_product combine_fun arr1 arr2 =
  Array.map
    (fun a1 ->
       (Array.map (fun a2 -> combine_fun a1 a2) arr2))
    arr1
;;

let array_uniqify arrs =
  let arrs = array_filter (fun x -> Array.length x > 0) arrs in
  if Array.length arrs = 0 then [||] else
  let nr = Array.length arrs.(0) in
  let ht = Hashtbl.create nr in
  let () =
    Array.iter
      (fun a -> (Array.iter (fun x -> Hashtbl.replace ht x true) a)) arrs
  in
  let result = Array.make (Hashtbl.length ht) arrs.(0).(0) in
  let r_pos = ref 0 in
  let () =
    Hashtbl.iter
      (fun k _ ->
	let () = result.(!r_pos) <- k in
	r_pos := !r_pos+1
      )
      ht
  in result
;;

let array_to_hashtbl ?(reduce=(fun x y -> x)) a =
  let ht = Hashtbl.create (Array.length a) in
  let () =
    Array.iter
      (fun (k,v) ->
	try
	  let old_entry = Hashtbl.find ht k in
	  Hashtbl.replace ht k (reduce v old_entry)
	with
	| Not_found -> Hashtbl.add ht k v)
      a in
  ht
;;

let components_of_connection fun_neighbours start_points =
  let ht_seen_all = Hashtbl.create 17 in
  let rec walk_cc ht_seen active_points next_generation =
    if Hashtbl.length active_points = 0 && Hashtbl.length next_generation = 0
    then ht_seen
    else
      let () = hashtbl_foreach_do active_points
	(fun pt _ ->
	   let () = Hashtbl.replace ht_seen_all pt true in
	   let () = Hashtbl.replace ht_seen pt true in
	   let neighbours = fun_neighbours pt in
	     Array.iter
	       (fun n -> Hashtbl.replace next_generation n true)
	       neighbours)
      in
      let () = Hashtbl.clear active_points in
      let ht_new_ng = active_points in
	walk_cc ht_seen next_generation ht_new_ng
  in
  let nr_start_points = Array.length start_points in
  let rec make_ccs have_ccs nr_pt =
    if nr_pt = nr_start_points
    then Array.of_list (List.rev have_ccs)
    else
      let have_seen_this =
	try
	  let _ = Hashtbl.find ht_seen_all start_points.(nr_pt) in true
	with | Not_found -> false
      in
	if have_seen_this
	then make_ccs have_ccs (1+nr_pt)
	else
	  let active_points =
	    let h = Hashtbl.create 3 in
	    let () = Hashtbl.add h start_points.(nr_pt) true in
	      h
	  in
	  let cc =
	    walk_cc (Hashtbl.create 17) active_points (Hashtbl.create 17)
	  in
	    make_ccs ((hashtbl_keys cc)::have_ccs) (1+nr_pt)
  in make_ccs [] 0
;;



let array_nums_with_property p nmax =
  let rec walk n so_far =
    if n=nmax then Array.of_list (List.rev so_far)
    else walk (1+n) (if p n then n::so_far else so_far)
  in walk 0 []
;;

let array_gcd_reduced arr =
  if Array.length arr < 2
  then arr
  else
    let gcd =
      Array.fold_left
	(fun so_far x -> if x =0 then so_far else int_gcd so_far x)
	arr.(0) arr
    in
    if gcd=0
    then arr
    else Array.map (fun n -> n / gcd) arr
;;

(* XXX actually, this is a design flaw: calling with an optarg will presumably
   require consing anyway, so we do not avoid consing here, as we tried to!
   XXX Change this, and adjust all the other code using it accordingly!
 *)
let float_arrays_avg ?storage arrs =
  let nr_arrs = Array.length arrs in
  let dim = Array.length arrs.(0) in
  let scale = 1.0/.(float_of_int nr_arrs) in
  let store_result =
    match storage with
      | None -> Array.make dim 0.0
      | Some x -> x
  in
    begin
      for i = 0 to dim-1 do
	store_result.(i) <- arrs.(0).(i);
	for a = 1 to nr_arrs-1 do
	  store_result.(i) <- store_result.(i)+.arrs.(a).(i);
	done;
	store_result.(i) <- store_result.(i)*.scale;
      done;
      store_result
    end
;;

let interpolate_points point_start point_end nr_steps =
  let step_factor = 1.0/.(float_of_int (nr_steps-1)) in
  let v_step = array_pointwise (fun x y -> step_factor*.(y-.x)) point_start point_end in
    Array.init nr_steps
      (fun n ->
	 array_pointwise (fun p0 dir -> p0+.(float_of_int n)*.step_factor*.dir) point_start v_step)
;;

let interpolate fun_add fun_scalar_mult (x1,v1) (x2,v2) param =
  let x = (param-.x1)/.(x2-.x1) in
  let xc = 1.0-.x in
  fun_add (fun_scalar_mult xc v1) (fun_scalar_mult x v2)
;;

let interpolate_piecewise fun_add fun_scalar_mult ranges x =
  let nr_ranges = Array.length ranges in
  let (x_start,val_start) = ranges.(0)
  and (x_end,val_end) = ranges.(nr_ranges-1) in
  if x <= x_start then val_start
  else if x >= x_end then val_end
  else
    let rec walk n =
      let (x_n,val_n) = ranges.(n) in
      if x <= x_n then
	interpolate fun_add fun_scalar_mult ranges.(n-1) ranges.(n) x
      else walk (1+n)
    in walk 1
;;

let color_interpolate_piecewise =
  interpolate_piecewise
    (array_pointwise (+.))
    (fun s -> (Array.map (fun x -> s*.x)))
;;

let a_array_to_string fun_stringify arr =
  let pieces= Array.map fun_stringify arr in
    Printf.sprintf "[|%s|]" (String.concat "; " (Array.to_list pieces))

let string_array_to_string = a_array_to_string (fun s -> (Printf.sprintf "\"%s\"" (String.escaped s)));;
let float_array_to_string = a_array_to_string string_of_float;;
let int_array_to_string = a_array_to_string string_of_int;;
let bool_array_to_string = a_array_to_string string_of_bool;;

let float_array2_to_string = a_array_to_string float_array_to_string;;
let int_array2_to_string = a_array_to_string int_array_to_string;;

let string_to_charcode_string s =
  int_array_to_string (Array.init (String.length s) (fun n -> Char.code s.[n]))
;;

let quote_string =
  (* XXX should be obsoleted! use String.escaped! *)
  let rx = Str.regexp "[\"\\]" in
    fun str ->
      let corpus =
	(* Str.global_replace rx "\\\0" str
	   Str.global_replace rx "\\0" str
	   ^ Note: these both do not seem to work (ocaml 3.09 bug?)
	*)
	Str.global_replace rx "\\\\\\0" str
      in
	Printf.sprintf "\"%s\"" corpus
;;

let string_compare_n n str1 str2 =
  let len1 = String.length str1
  and len2 = String.length str2
  in
  let rec walk pos =
    if pos = n then true
    else
      if pos = len1
      then (pos=len2)
      else
	if pos = len2
	then false
	else
	  if str1.[pos] = str2.[pos]
	  then walk (1+pos)
	  else false
  in walk 0
;;


let string_one_shorter s n =
  let nr_chars = String.length s in
  let result = String.create (nr_chars-1) in
    begin
      for i=0 to n-1 do
	result.[i] <- s.[i];
      done;
      for i=n+1 to nr_chars-1 do
	result.[i-1] <- s.[i];
      done;
      result
    end
;;

let string_append s1 s2 =
  let ns1 = String.length s1
  and ns2 = String.length s2
  in
  let s = String.create (ns1+ns2) in
    begin
    for i=0 to ns1-1 do s.[i] <- s1.[i] done;
    for i=0 to ns2-1 do s.[i+ns1] <- s2.[i] done;
    s
    end
;;

(* Given a string 's' and an array of possible prefixes 'start_strings',
 * this function returns 'Some index' if the string 's' starts with the string
 * 'start_strings.(index)' or 'None' if this is not the case for any
 * admissible value of 'index'.
 * EXAMPLE:
 *   The call: strings_start_with [|"one"; "two"|] "one cat"
 *   returns: Some 1
 *   The call: strings_start_with [|"one"; "two"|] "three cats"
 *   returns: None
 *)
let string_starts_with start_strings s =
  let max_idx = Array.length start_strings in
  let rec walk idx =
    if idx >= max_idx
    then None
    else
      let candidate = start_strings.(idx) in
        if (string_compare_n (String.length candidate) s candidate)
        then Some idx
        else walk (idx+1)
  in
    walk 0
;;

(* This function can be used to split the command line arguments passed in the
 * string array 'cmdline' into option groups. The function runs over the
 * elements of 'cmdline' and puts them into the given dictionary
 * 'args_by_group'. The way this happens is better understood with an example.
 * Let cmdline = [|"a0"; "a1"; "nice-opts"; "a3"; "a4"; "bad-opts"; "a5"|] and
 * let groups = [|"nice-opts"; "bad-opts"|]. If you call
 *
 *   let args_by_group = Hashtbl.create 10 in
 *     preparse_cmdline groups "bad-opts" args_by_group cmdline
 *
 * The args_by_group will contain two keys: "bad-opts" associated to the value
 * [|"a0"; "a1"; "a5"|] and the key "nice-opts" associated to the value
 * [|"a2"; "a3"|].
 * NOTE: the dictionary 'args_by_group' always has the key 'initial_group',
 *   even if 'cmdline' is an empty array. The other keys in 'groups' are
 *   inserted only if they got one or more strings from 'cmdline'.
 *)
let preparse_cmdline groups initial_group args_by_group cmdline =
  let add_opts group new_opts =
    let arr_old_opts = try Hashtbl.find args_by_group group with _ -> [||] in
    let arr_new_opts = Array.of_list (List.rev new_opts) in
      Hashtbl.replace args_by_group group
                      (Array.append arr_old_opts arr_new_opts)
  in
  let rec parse_next active_group accumulated_opts i =
    if i >= Array.length cmdline then
      add_opts active_group accumulated_opts
    else
      let opt = cmdline.(i) in
      let some_group_idx = string_starts_with groups opt
      in
        match some_group_idx with
          None ->                     (* Add the option to the active group *)
            parse_next active_group (opt::accumulated_opts) (i+1)
        | Some group_idx ->                      (* change the active group *)
          let next_active_group = groups.(group_idx) in
          let () = add_opts active_group accumulated_opts in
            parse_next next_active_group [] (i+1)
  in
    parse_next initial_group [] 0
;;

(* Quick hack - can be done in a better way! *)
let sparse_float_matrix_to_string ?(min_size=1.0e-6) mx =
  let pieces =
    Array.mapi
      (fun nr_row row ->
	(Array.mapi
	   (fun nr_col entry ->
	     if abs_float entry < min_size
	     then ""
	     else Printf.sprintf "%3d %3d: %f\n" nr_row nr_col entry)
	   row))
      mx
  in String.concat "" (Array.to_list (Array.concat (Array.to_list pieces)))
;;


let float_array_max arr =
  (* Note: does not work for length-0 arrays! *)
  let len = Array.length arr in
  let rec walk now pos =
    if pos=len then now
    else
      let here = arr.(pos) in
      walk (if here > now then here else now) (1+pos)
  in walk arr.(0) 1
;;

let rec array_position_if p arr start =
  if start = Array.length arr then (-1)
  else if p arr.(start) then start
  else array_position_if p arr (start+1)
;;

let rec array_position x arr start =
  if start = Array.length arr then (-1)
  else if x=arr.(start) then start
  else array_position x arr (start+1)
;;

let array_mapjoin mapper xss =
  let nr_arrays = Array.length xss in
  let len_result=Array.fold_left (fun sf x -> sf + Array.length x) 0 xss in
  let pos_first_nonempty_array =
    let rec trav j =
      if j = nr_arrays then (-1)
      else if Array.length xss.(j) > 0 then j
      else trav (1+ j) in
    trav 0 in
  if pos_first_nonempty_array = (-1)
  then [||]
  else let result=Array.make len_result (mapper xss.(pos_first_nonempty_array).(0)) in
  let rec trav pos_result pos_j pos_k =
    if pos_result = len_result
    then result
    else if pos_k = Array.length xss.(pos_j)
    then trav pos_result (1+ pos_j) 0
    else
      begin
	(result.(pos_result) <- mapper (xss.(pos_j).(pos_k));
	 trav (1+ pos_result) pos_j (1+ pos_k));
      end
  in trav 0 0 0;;

let array_join xss = array_mapjoin identity xss;;

let list_iteri f li =
  let rec walk n rest_li =
    match rest_li with
    | [] -> ()
    | (hd::tl) ->
	let () = f n hd in
	walk (1+n) tl
  in walk 0 li
;;

let list_intersection li_1 li_2 =
  let list_member x li =
    try let _ =
      List.find (fun y -> x=y) li
    in true
    with | Not_found -> false
  in
  let rec walk seen todo =
    match todo with
    | [] -> List.rev seen
    | (hd::tl) -> if list_member hd li_2 then walk (hd::seen) tl else walk seen tl
  in walk [] li_1
;;

let lists_intersection ?sorter lili =
  let rec walk lili =
    match lili with
    | [] -> []
    | [li] -> li
    | (head1::head2::rest) -> walk ((list_intersection head1 head2)::rest)
  in
  let result = walk lili in
  match sorter with
  | None -> result
  | Some f -> List.sort f result
;;

let list_mapfind f li =
  let rec walk rest =
    match rest with
    | (x::xs) ->
	(let here = f x in
	  match here with
	  | None -> walk xs
	  | Some _ -> here)
    | [] -> None
  in walk li
;;

let list_position_if p li =
  let rec walk n rest =
    match rest with
    | [] -> raise Not_found
    | (h::t) -> if p h then n else walk (1+n) t
  in walk 0 li
;;

let rec list_all_satisfy p li =
  match li with
    | [] -> true
    | (head::tail) -> (p head) && list_all_satisfy p tail
;;

let string_multifill_template_then_concat ?(separator="") template patterns replacements =
  let rx_str =
    (* Printf.sprintf "\\(%s\\)" *)
      (String.concat "\\|" (Array.to_list (Array.map Str.quote patterns)))
  in
  let rx = Str.regexp rx_str in
  let replaced_pieces =
    Array.map
      (fun this_instantiation ->
	 let replace_piece s =
	   let m = Str.matched_string s in
	   let pos = array_position_if (fun z -> z=m) patterns 0 in
	     if pos=(-1)
	     then failwith (Printf.sprintf "string_fill_template_then_concat: unknown key '%s'" m)
	     else this_instantiation.(pos)
	 in
	   Str.global_substitute rx replace_piece template
      ) replacements
  in
    String.concat separator (Array.to_list replaced_pieces)
;;

(*
string_multifill_template_then_concat
  ~separator:";"
  "M_%MAT% = (%ABS_M%)*<foo||bar_%MAT%>"
  [|"%MAT%";"%ABS_M%"|]
  [|[|"Py";"1.3"|];[|"Dy";"2.6"|]|];;

==>
 - : string = "M_Py = (1.3)*<foo||bar_Py>;M_Dy = (2.6)*<foo||bar_Dy>"
*)

let gauss_random ?(fun_rng  = fun x -> Random.float x) mean sigma =
  let u = fun_rng 1.0 in
  let v = fun_rng 1.0 in
  let rnd = mean +. sigma *. (cos(2.0*.pi*.u)*.sqrt(0.0-.2.0*.log(v))) in
  rnd
;;

let estimate_max_and_average
    ?(conservative_factor = 1.12)
    nr_probes f fun_random_point =
  let rec work n max_seen sum_now =
    if n = nr_probes
    then (max_seen *. conservative_factor, sum_now/. (float_of_int nr_probes))
    else
      let v = f (fun_random_point ())
      in work (1+n) (max v max_seen) (sum_now+.v)
  in work 1 (f (fun_random_point ())) 0.0
;;

(*
   Example:

   estimate_max_and_average
    ~conservative_factor:1.0
    100
    (fun x -> exp(0.0-.(x*.x)))
    (fun _ -> (Random.float 5.0) -. 2.5);;

   - : float * float = (0.999738789487001345, 0.337152520093218167)
*)

(* We have to be able to distribute points wrt some density
   function. We do not try to be overly clever here - as long as it works
   for simple applications, it's good enough for us.

   Note: presumably we may in the end want to move this and related
   functions to a package named "Bodies" or something similar.
*)

let distribute_points_randomly
    ?(fun_rng  = fun x -> Random.float x)
    ?max_density_and_average
    fun_random_point fun_density nr_points =
  let (max_density,density_average) =
    match max_density_and_average
    with
      | None -> estimate_max_and_average (nr_points*4) fun_density fun_random_point
      | Some x -> x
  in
  let result = Array.make nr_points (fun_random_point ()) in
  let rec work nr_point =
    if nr_point = nr_points
    then (result,density_average)
    else
      let p = fun_random_point () in
      let h_density = fun_density p in
      let rnd = fun_rng max_density in
	if rnd > h_density
	then (* reject the point *)
	  work nr_point
	else
	  begin
	    result.(nr_point) <- p;
	    work (1+nr_point)
	  end
  in work 0
;;

let gauss_random_n_dim
    ?(fun_rng  = fun x -> Random.float x)
    distribution_midpoint
    distribution_radius =
  Array.map
    (fun x -> gauss_random ~fun_rng:fun_rng x distribution_radius)
    distribution_midpoint
;;

(* Example: *)

(*
let ex_make_2d_gaussian_points n =
  distribute_points_randomly
    (fun _ -> [|(Random.float 6.0)-.3.0;(Random.float 6.0)-.3.0|])
    (fun [|x;y|] -> let d = x*.x+.y*.y in exp(0.0-.d))
    n
;;

  let random_pts = ex_make_2d_gaussian_points 10000;;

   for i = 0 to 10000-1 do let [|x;y|]=random_pts.(i) in Printf.printf "%3d %8.3f %8.3f\n" i x y done;;

   Indeed, gnuplot shows that this is a nice gaussian distribution.
 *)

(*
  let some_points = ex_make_2d_gaussian_points 200;;

  let some_points_nn = nearest_neighbours some_points;;
*)

let scalar_product v1 v2 =
  let len = Array.length v1 in
  if Array.length v2 <> len
  then failwith "Tried to calculate scalar product for vectors of different dimension"
  else
    let rec walk n so_far =
      if n = len
      then so_far
      else
	walk (n+1) (so_far+.v1.(n)*.v2.(n))
    in walk 0 0.0
;;

let cross_product_3d v1 v2 =
  [|v1.(1)*.v2.(2)-.v1.(2)*.v2.(1);
    v1.(2)*.v2.(0)-.v1.(0)*.v2.(2);
    v1.(0)*.v2.(1)-.v1.(1)*.v2.(0);
  |]
;;

let triangle_space_angle_3d =
  let r0 = Array.make 3 0.0
  and r1 = Array.make 3 0.0
  and r2 = Array.make 3 0.0
  and v_len x = sqrt(x.(0)*.x.(0)+.x.(1)*.x.(1)+.x.(2)*.x.(2))
  and sprod x y = x.(0)*.y.(0)+.x.(1)*.y.(1)+.x.(2)*.y.(2)
  in
    fun observer p0 p1 p2 ->
      begin
	for i=0 to 3-1 do
	  r0.(i) <- p0.(i) -. observer.(i);
	  r1.(i) <- p1.(i) -. observer.(i);
	  r2.(i) <- p2.(i) -. observer.(i);
	done;
	let r0l = v_len r0
	and r1l = v_len r1
	and r2l = v_len r2
	and sp01 = sprod r0 r1
	and sp12 = sprod r1 r2
	and sp20 = sprod r2 r0
	in
	let numerator = r0l*.r1l*.r2l+.r0l*.sp12+.r1l*.sp20+.r2l*.sp01
	and denominator =
	  sqrt(2.0*.(r1l*.r2l+.sp12)*.(r2l*.r0l+.sp20)*.(r0l*.r1l+.sp01))
	in
	  if denominator=0.0 then 0.0
	  else
	    let quot = numerator /. denominator in
	    let corrected_quot =
	      (if quot < -1.0 then -1.0 else if quot > 1.0 then 1.0 else quot)
	    in
	      (2.0*.(acos corrected_quot))
      end
;;

let euclidean_distance_sq v1 v2 =
  let len = Array.length v1 in
  if Array.length v2 <> len
  then failwith "Tried to calculate distance of points of different dimension"
  else
    let rec walk n so_far =
      if n = len
      then so_far
      else
	let dn = v1.(n)-.v2.(n) in
	walk (n+1) (so_far+.dn*.dn)
    in walk 0 0.0
;;

let euclidean_len_sq v =
  let len = Array.length v in
  let rec walk n so_far =
    if n = len
    then so_far
    else
      walk (n+1) (so_far+.v.(n)*.v.(n))
  in walk 0 0.0
;;

(* In three dimensions, the area of a triangle spawned by vectors v1, v2 is
   half the norm of the cross product. In higher dimensions, a
   "vector cross product" does not exist, but there is an appropriate
   generalization: half the Frobenius norm of the (antisymmetric) 2-vector
   made from v1,v2.
 *)

let triangle_area point1 point2 point3 =
  let v1 = array_pointwise (-.) point2 point1 in
  let v2 = array_pointwise (-.) point3 point1 in
  let dim = Array.length v1 in
  let rec walk1 pos1 sum =
    if pos1 = dim then sum
    else
      let rec walk2 pos2 sum =
	if pos2 = dim
	then walk1 (pos1+1) sum
	else
	  let contrib = v1.(pos1)*.v2.(pos2) -. v1.(pos2)*.v2.(pos1) in
	  walk2 (pos2+1) (sum+.contrib*.contrib)
      in
      walk2 (1+pos1) sum
  in
  let frobenius_norm_2vector = walk1 0 0.0 in
  0.5*.(sqrt frobenius_norm_2vector)
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

(* == Gradient == *)

(*
   Technical note:

   The result of computing a gradient at a given position is a vector.
   Normally, we would like this result vector to be dynamically
   allocated, but there are situations where this leads to unnecessary
   effort for dynamical memory management, especially if we compute a
   large number of gradients which are "consumed immediately".

   The alternative would be to provide a store-result-here output
   vector argument that is dynamically modified, so that memory
   management is up to the caller.

   The latter strategy is sometimes necessary.
   The former one, however, corresponds so much closer to the idea of
   "having a function that gives me a gradient" that we certainly should
   provide it. It can be used for good to simplify the structure of the code
   in non-timecritical situations.

   Our strategy is to implement the low-level variant, and build the
   convenient one on top of that.
 *)

(*
   symm_grad_storing_result : map a step size epsilon to a gradient function
   that stores its result in an output vector.

   Note about threads: this function is thread-safe, but the functions
   it generates are not (for performance reasons). As long as you use
   it to create for every thread in your program an own gradient
   function, you are fine. This is quite reasonable.
 *)

let symm_grad_storing_result ?(epsilon=1.0e-7) dim =
  let scratchpad_pos = Array.make dim 0.0 in
  let scale = 1.0/.(2.0*.epsilon) in
  let grad store_result_here f x0 =
    begin
      for i = 0 to dim-1 do
	scratchpad_pos.(i) <- x0.(i);
      done;
      for i = 0 to dim-1 do
	let () = scratchpad_pos.(i) <- x0.(i)+.epsilon in
	let val_plus = f scratchpad_pos in
	let () = scratchpad_pos.(i) <- x0.(i)-.epsilon in
	let val_minus = f scratchpad_pos in
	let () = scratchpad_pos.(i) <- x0.(i) in
	  store_result_here.(i) <- (val_plus -. val_minus)*.scale
      done;
    end
  in grad (* Return the gradient function *)
;;

let symm_div ?(epsilon=1.0e-7) dim =
  let pos = Array.make dim 0.0 in
  let div f x0 =
    let () =
      for i=0 to dim-1 do
	pos.(i) <- x0.(i);
      done
    in
    let rec walk nr_coord div_now =
      if nr_coord = dim then (div_now/.(2.0*.epsilon))
      else
	let p0 = pos.(nr_coord) in
	let () = pos.(nr_coord) <- p0 +. epsilon in
	let val_plus = (f pos).(nr_coord) in
	let () = pos.(nr_coord) <- p0 -. epsilon in
	let val_minus = (f pos).(nr_coord) in
	let () = pos.(nr_coord) <- p0 in
	walk (1+nr_coord) (div_now+. (val_plus-.val_minus))
    in walk 0 0.0
  in div
;;

(* We need a function that finds a minimum on a line. The method
   employed here is somewhat dumb and slow, but reliable... *)

let find_minimum_on_line ?(x_tolerance=1e-10) f startpoint endpoint =
  let rec increase_steplength_until_f_rises start_x end_x val_start val_end =
    let delta = end_x -. start_x in
    let overshooting_x = end_x +. delta in
    let val_overshooting = f overshooting_x in
      if val_overshooting < val_end
      then increase_steplength_until_f_rises start_x overshooting_x val_start val_overshooting
      else val_overshooting
  in
  let rec decrease_steplength start_x mid1_x mid2_x end_x val_mid1 val_mid2 =
    if abs_float(start_x-.end_x) < x_tolerance
    then mid1_x
    else if val_mid1 < val_mid2
    then
      let distance_3 = (mid2_x -. start_x) /. 3.0 in
      let new_mid1_x = start_x +.distance_3 in
      let new_mid2_x = new_mid1_x +.distance_3 in
      let val_new_mid1 = f new_mid1_x in
      let val_new_mid2 = f new_mid2_x in
	decrease_steplength start_x new_mid1_x new_mid2_x mid2_x val_new_mid1 val_new_mid2
    else
      let distance_3 = (end_x -. mid1_x) /. 3.0 in
      let new_mid1_x = mid1_x +.distance_3 in
      let new_mid2_x = new_mid1_x +.distance_3 in
      let val_new_mid1 = f new_mid1_x in
      let val_new_mid2 = f new_mid2_x in
      	decrease_steplength mid1_x new_mid1_x new_mid2_x end_x val_new_mid1 val_new_mid2
  in
  let seek_endpoint =
    increase_steplength_until_f_rises startpoint endpoint (f startpoint) (f endpoint)
  in
  let seek_distance_3 = (endpoint-.startpoint)/.3.0 in
  let midpoint1 = startpoint +. seek_distance_3 in
  let midpoint2 = midpoint1 +. seek_distance_3 in
    decrease_steplength
      startpoint midpoint1 midpoint2 seek_endpoint
      (f midpoint1) (f midpoint2)
;;

(*
Example:
# find_minimum_on_line (fun x -> (x-.5.2)**4.0) 1.0 2.0;;
- : float = 5.19999999996750617
*)

let walk_to_minimum ?x_tolerance ?(grad_step=1e-7) ?(stop_grad_len=1e-6) f startpos =
  let pos = Array.copy startpos in
  let grad_factor = 1.0/.(2.0*.grad_step) in
  let dim = Array.length pos in
  let v_grad = Array.make dim 0.0 in
  let internal_pos = Array.make dim 0.0 in
  let val_pm = Array.make 2 0.0 in
  let fun_on_grad_line x =
    begin
      for j=0 to dim-1 do
	internal_pos.(j) <- pos.(j) -. (v_grad.(j)*.x);
      done;
      f internal_pos
    end
  in
  let rec walk () =
    begin
      for j=0 to dim-1 do
	let pos_j = pos.(j) in
	  begin
	    pos.(j) <- pos_j +. grad_step;
	    val_pm.(0) <- f pos;
	    pos.(j) <- pos_j -. grad_step;
	    val_pm.(1) <- f pos;
	    pos.(j) <- pos_j;
	    v_grad.(j) <- (val_pm.(0) -. val_pm.(1))*.grad_factor;
	  end
      done;
      let abs_grad = sqrt(euclidean_len_sq v_grad) in
	if abs_grad <= stop_grad_len
	then pos
	else
	  let step = find_minimum_on_line ?x_tolerance fun_on_grad_line 0.0 0.01 in
	    begin
	      for i=0 to dim-1 do
		pos.(i) <- pos.(i)-.step*.v_grad.(i);
	      done;
	      walk()
	    end
    end
  in walk()
;;

(* A naive Gram-Schmidt Orthonormalizer
   Note that this was introduced quite late.
   If we had had this right from the beginning,
   some of the other snippets functions presumably
   would have been written in a different way.
*)

(* Note that the G.S. orthonormalizer behaves like an "object".
   XXX Needs documentation!
*)

let make_gram_schmidt_orthonormalizer dim =
  let buffer = Array.init dim (fun _ -> Array.make dim 0.0) in
  let nr_vectors_in_buffer = ref 0 in
  let fun_check_finished () =
    !nr_vectors_in_buffer = Array.length buffer
  and fun_clear () = nr_vectors_in_buffer:=0
  and fun_add_vector v =
    let nr_slot = !nr_vectors_in_buffer in
      begin
	for k=0 to dim-1 do buffer.(nr_slot).(k) <- v.(k) done;
	for i=0 to nr_slot-1 do
	  let sprod = scalar_product buffer.(i) buffer.(nr_slot) in
	    for k=0 to dim-1 do
	      buffer.(nr_slot).(k) <- buffer.(nr_slot).(k)-.sprod*.buffer.(i).(k)
	    done
	done;
	let len=sqrt(euclidean_len_sq buffer.(nr_slot)) in
	  if len = 0.0 then ()
	  else
	    let inv_len = 1.0/.len in
	    begin
	      for k=0 to dim-1 do
		buffer.(nr_slot).(k) <- buffer.(nr_slot).(k)*.inv_len
	      done;
	      nr_vectors_in_buffer := !nr_vectors_in_buffer+1
	    end
      end
  and fun_copy_vector n target =
    for k=0 to dim-1 do
      target.(k) <- buffer.(n).(k)
    done
  in
    (fun_clear,fun_check_finished,fun_add_vector,fun_copy_vector)
;;

(*
(* DDD *)
let (gs_clear,gs_check,gs_add,gs_copy)=make_gram_schmidt_orthonormalizer 3 in
let print_vec n = let a = Array.make 3 0.0 in let () = gs_copy n a in Printf.printf "%s\n" (float_array_to_string a)
in
  begin
    gs_clear();
    gs_add [|2.0;1.0;3.0|];
    gs_add [|1.0;-1.0;5.0|];
    gs_add [|1.0;0.0;0.0|];
    (if gs_check () then Printf.printf "OK 3!\n%!" else ());
    print_vec 0;
    print_vec 1;
    print_vec 2;
  end
;;
(* That's just right - we get an orthonormal frame, and the third vector
  is orthogonal to the first two initially given vectors! *)
*)


(*
  Determinants

  Similar to gradient, we use a function that creates a closure over a
  scratchpad where we store intermediate results. We use a simple 1d
  array to hold all entries, for faster reference.

  Note 1: This does not do any pivoting yet. Should be improved!

  Note 2: We need this in a two-stage form: when determining
  co-factors, we provide an own pre-initialized scratchpad, rather
  than doing an extra spurious copying step.

*)

let compute_det_on_scratchpad dim scratchpad =
  let size = dim*dim in
  let swap_rows row_ix_1 row_ix_2 =
    (* Note: row indices are 0;dim;2*dim;... *)
    for i = 0 to dim-1 do
      let x = scratchpad.(row_ix_1+i) in
	begin
	  scratchpad.(row_ix_1+i)<-scratchpad.(row_ix_2+i);
	  scratchpad.(row_ix_2+i)<-x;
	end
    done
  in
  let rec find_row_with_non0_entry col row_now =
    if row_now = size
    then (-1) (* Signal that there was none *)
    else
      if scratchpad.(row_now+col) <> 0.0
      then row_now
      else find_row_with_non0_entry col (row_now+dim)
  in
  let rec down_eliminate row_src_factor col_start row_src row_dst =
    (* row_src_factor is -1/M[row_src][col] *)
    if row_dst = size
    then ()
    else
      let factor = row_src_factor*.scratchpad.(row_dst+col_start) in
	begin
	  scratchpad.(row_dst+col_start)<-0.0;
	  for i = col_start+1 to dim-1 do
	    scratchpad.(row_dst+i)<-
	      scratchpad.(row_dst+i)+.factor*.scratchpad.(row_src+i)
	  done;
	  down_eliminate row_src_factor col_start row_src (row_dst+dim)
	end
  in
  let rec diagonal_product offset_now val_now =
    if offset_now >= size
    then val_now
    else
      diagonal_product
	(offset_now+dim+1)
	(val_now*.scratchpad.(offset_now))
  in
  let rec work sign col_ix row_ix =
    (* let () = Printf.printf "Work sign=%d col_ix=%d row_ix=%d\nMX=%s\n"
       sign row_ix col_ix (float_array_to_string scratchpad) in (* DDD *)
    *)
    if row_ix = size (* Finished *)
    then
      let dp=diagonal_product (dim+1) scratchpad.(0) in
	if sign = 1 then dp else 0.0-.dp
    else
      let row_non0 = find_row_with_non0_entry col_ix row_ix in
	if row_non0 = (-1)
	then 0.0 (* Determinant can only be zero *)
	else
	  begin
	    if row_non0 <> row_ix
	    then
	      (* We have to swap rows first! *)
	      begin
		swap_rows row_ix row_non0;
		down_eliminate
		  (-1.0/. scratchpad.(row_ix+col_ix))
		  col_ix row_ix (row_ix+dim);
		work (0-sign) (col_ix+1) (row_ix+dim);
	      end
	    else
	      begin
		down_eliminate
		  (-1.0/. scratchpad.(row_ix+col_ix))
		  col_ix row_ix (row_ix+dim);
		work sign (col_ix+1) (row_ix+dim);
	      end
	  end
  in
    work 1 0 0
;;


(* XXX NOTE: should move the pivoting into the determinant function as well!

   Note 1: this will return the determinant.

   Note 2: The code is far too long and ugly. I am not overly proud of it.
*)

let compute_inv_on_scratchpads dim scratchpad_src scratchpad_dst =
  let size = dim*dim in
  let swap_rows row_ix_1 row_ix_2 =
    (* Note: row indices are 0;dim;2*dim;... *)
    for i = 0 to dim-1 do
      let x1 = scratchpad_src.(row_ix_1+i)
      and x2 = scratchpad_dst.(row_ix_1+i) in
	begin
	  scratchpad_src.(row_ix_1+i)<-scratchpad_src.(row_ix_2+i);
	  scratchpad_src.(row_ix_2+i)<-x1;
	  scratchpad_dst.(row_ix_1+i)<-scratchpad_dst.(row_ix_2+i);
	  scratchpad_dst.(row_ix_2+i)<-x2;
	end
    done
  in
(*  let () = Printf.printf "snippets: swap_row\n%!" in
*)
 let normalize_row row_ix col_ix =
    let entry = scratchpad_src.(row_ix+col_ix) in
    let factor = (1.0)/.entry in
      begin
	scratchpad_src.(row_ix+col_ix) <- 1.0;
	for i=col_ix+1 to dim-1 do
	  scratchpad_src.(row_ix+i) <- scratchpad_src.(row_ix+i)*.factor;
	done;
	for i=0 to dim-1 do
	  scratchpad_dst.(row_ix+i) <- scratchpad_dst.(row_ix+i)*.factor;
	done;
      end
  in
(*  let () = Printf.printf "snippets: normalize_row\n%!" in
*)
  let rec find_row_with_best_entry_down best_now_val best_now_pos col row_now =
    if row_now = size
    then best_now_pos
    else
      let here = scratchpad_src.(row_now+col) in
      let abs_here = abs_float here in
	if abs_here > best_now_val
	then
	  find_row_with_best_entry_down abs_here row_now col (dim+ row_now)
	else
	  find_row_with_best_entry_down best_now_val best_now_pos col (dim+ row_now)
  in
(*  let () = Printf.printf "snippets: find_row_with_best_entry_down\n%!" in
*)
    let rec down_eliminate row_end col_start row_src row_dst =
    if row_dst = row_end
    then ()
    else
      let factor = 0.0-.scratchpad_src.(row_dst+col_start) in
	begin
	  scratchpad_src.(row_dst+col_start)<-0.0;
	  for i = col_start+1 to dim-1 do
	    scratchpad_src.(row_dst+i)<-
	      scratchpad_src.(row_dst+i)+.factor*.scratchpad_src.(row_src+i);
	  done;
	  for i=0 to dim-1 do
	    scratchpad_dst.(row_dst+i)<-
	      scratchpad_dst.(row_dst+i)+.factor*.scratchpad_dst.(row_src+i);
	  done;
	  down_eliminate row_end col_start row_src (row_dst+dim)
	end
  in
(*  let () = Printf.printf "snippets: down_eliminate\n%!" in
*)
  let rec work_down det col_ix row_ix =
    if row_ix = size
    then
      det
    else
      let row_pivot = find_row_with_best_entry_down (-1.0) (-1) col_ix row_ix in
	if row_pivot = (-1)
	then
	  raise Division_by_zero (* No Inverse! Det=0. *)
	else
	  let det =
	    (if row_pivot = row_ix
	     then det*.scratchpad_src.(row_ix+col_ix)
	     else let () = swap_rows row_ix row_pivot in
	       (0.0-.det*.scratchpad_src.(row_ix+col_ix)))
	  in
	    begin
	      normalize_row row_ix col_ix;
	      down_eliminate row_ix col_ix row_ix 0;
	      down_eliminate size col_ix row_ix (row_ix+dim);
	      work_down det (col_ix+1) (row_ix+dim)
	    end
  in
    work_down 1.0 0 0
;;


let hodge_dualize_to_1form dim =
(*
   Idea: fill scratchpad with vectors, add vectors
   1000, 0100, 0010, 0001 as last vector, and in every case
   compute the determinant. Yes, this is somewhat dumb
   and wasteful. But it should do the job...
*)
  let size = dim*dim in
  let lastrow = size-dim in
  let scratchpad = Array.make size 0.0 in
  fun vectors -> (* dim-1 vectors of length dim *)
    let result = Array.make dim 0.0 in
      begin
      for k=0 to dim-1 do
	begin
	    for nr_vec=0 to dim-1-1 do
	          for j=0 to dim-1 do
		          scratchpad.(nr_vec*dim+j) <- vectors.(nr_vec).(j)
		  done
		  done;
		      for j=0 to dim-1 do
			    scratchpad.(lastrow+j) <- 0.0
		      done;
		        scratchpad.(lastrow+k) <- 1.0;
			  result.(k) <- compute_det_on_scratchpad dim scratchpad
	end
      done;
      result
      end
;;

let _fill_from_mx dim scratchpad mx =
  let rec work nr_row row_offset =
    if nr_row=dim then ()
    else
      let the_row=mx.(nr_row) in
	begin
	  for i = 0 to dim-1 do
	    scratchpad.(row_offset+i) <- the_row.(i)
	  done;
	  work (nr_row+1) (row_offset+dim)
	end
  in work 0 0
;;


let _linear_array_to_mx dim arr =
  Array.init dim
    (fun nr_row -> Array.sub arr (dim*nr_row) dim)
;;

(* Note: docs must clearly state that this is intelligent enough to
   be more clever for multiple computations if the "dim" parameter is
   specified once, i.e. use

   let di3 = det_and_inv 3 in
   Array.map di3 arr

   rather than

   Array.map (det_and_inv 3) arr
 *)
let det_and_inv dim =
  let size = dim*dim in
  let scratchpad_src = Array.make size 0.0
  and scratchpad_dst = Array.make size 0.0
  in
  let inv mx =
    begin
      _fill_from_mx dim scratchpad_src mx;
      for i = 0 to size-1 do
	scratchpad_dst.(i) <- 0.0;
      done;
      let rec set_diag n =
	if n > size then ()
	else
	  let () = scratchpad_dst.(n) <- 1.0 in
	    set_diag (n+dim+1)
      in set_diag 0;
(*	let () = Printf.printf "snippets - det...%!" in
*)
	let det = compute_inv_on_scratchpads dim scratchpad_src scratchpad_dst in
(*	let () = Printf.printf "ok\n%!" in
*)
	  (det,Array.init dim (fun row -> Array.init dim (fun col -> scratchpad_dst.(row*dim+col))))
    end
  in
    inv
;;

(*
   let inv3 = det_and_inv 3;;
   let r = Array.init 3 (fun _ -> Array.init 3 (fun _ -> Random.float 4.0));;

   let (p,q)=inv3 r in (r,p,inv3 q);;
 *)

(* For testing matrix inverses, we need a simple matrix product... *)

let mx_mult m1 m2 =
  let dim = Array.length m2 in
  let rec rowcol_prod r c j now =
    if j=dim then now
    else rowcol_prod r c (j+1) (now+.m1.(r).(j)*.m2.(j).(c))
  in
    Array.init (Array.length m1)
      (fun r ->
	 Array.init (Array.length m2.(0))
	   (fun c ->
	      rowcol_prod r c 0 0.0))
;;

let mx_x_vec ?store_result mx v =
  let dim_le = Array.length mx in
  let dim_ri = Array.length v in
  let () = (if dim_ri <> Array.length mx.(0) then failwith "matrix/vector size mismatch!" else ()) in
  let result =
    match store_result with
    | None -> Array.make dim_le 0.0
    | Some x -> x
  in
  let rec sprod_row row pos so_far =
    if pos = dim_ri
    then so_far
    else sprod_row row (1+pos) (so_far+.v.(pos)*.row.(pos))
  in
  begin
    for i=0 to dim_le-1 do
      result.(i) <- sprod_row mx.(i) 0 0.0;
    done;
    result;
  end
;;

let mx_transpose mx =
  let nr_rows = Array.length mx in
    if nr_rows=0 then mx
    else
      let nr_cols = Array.length mx.(0) in
	Array.init nr_cols
	  (fun nr_col -> Array.init nr_rows (fun nr_row -> mx.(nr_row).(nr_col)))
;;

(* See comments concerning det_and_inv *)

let determinant dim =
  let size = dim*dim in
  let scratchpad = Array.make size 0.0 in
  let det mx =
    let () = _fill_from_mx dim scratchpad mx in
    compute_det_on_scratchpad dim scratchpad
  in det
;;

(* Example:
let det_3d = determinant 3;;

det_3d [|[| 0.0; 3.0; 0.0|];[|-2.0; 3.0; 5.0|];[| 4.0;-6.0;-6.0|]|];;
 *)


(* Combinatorics: distribute N stones to K buckets.

   Note: this implementation does make some use of continuation
   passing style. Note that:

   (1) There is no loop in it.
   (2) No function calls itself recursively.
   (3) We do not use any state or imperative features
       (= references, array modifications, etc.)
   (4) We never have to walk through lists to join them.

   Neat, isn't it?
*)

let all_distributions_to_buckets nr_buckets how_many_things =
  let add_stones distrib_now where how_many =
    let distrib_new = Array.copy distrib_now in
    let () = distrib_new.(where) <- how_many in
    distrib_new
  in
  let rec distrib
      distrib_now nr_still_to_distribute pos return =
    if nr_still_to_distribute = 0
    then return distrib_now
    else
      let rec all_choices_here n return =
	let distrib_new = add_stones distrib_now pos n in
	if n = nr_still_to_distribute
	then return distrib_new
	else
	  distrib distrib_new (nr_still_to_distribute-n) (pos+1)
	    (fun x -> (x::(all_choices_here (n+1) return)))
      in
      all_choices_here
	(if pos=nr_buckets-1 then nr_still_to_distribute else 0)
	return
  in
  Array.of_list
    (distrib (Array.make nr_buckets 0) how_many_things 0 (fun x -> [x]))
;;
(*
   Array.length (distributions_to_buckets 5 3);; (* -> 35 *)
*)

let partitioning_multinomial_weight partitioning =
  let nr_total = Array.fold_left (fun so_far x -> so_far+x) 0 partitioning in
  let numerator = float_factorial nr_total in
  Array.fold_left (fun sf x -> sf/.(float_factorial x)) numerator partitioning
;;


let hyperplane_eval eqn point =
  let dim = Array.length point in
  let rec walk j so_far =
    if j = dim then so_far
    else walk (1+ j) (so_far +. eqn.(j)*.point.(j)) in
    walk 0 eqn.(dim)
;;

(* Note: also, the det_and_inv comment applies here. *)

let hyperplane_eqn dim =
  let det_dim = determinant dim in
  let scratchpad = Array.init dim (fun _ -> Array.make dim 0.0) in
  let hyperplane_eqn points =
    if Array.length points <> dim
    then failwith (Printf.sprintf "Wrong number of points for hyperplane_eqn! Wanted: %d Got: %d\n" dim (Array.length points))
    else
      let p0 = points.(0) in
      let result = Array.make (1+dim) 0.0 in
      let s0 = scratchpad.(0) in
	begin
	  (* First, make p0 the coordinate origin: *)
	  for i = 0 to dim-1 do
	    for c = 0 to dim-1 do
	      scratchpad.(i).(c) <- points.(i).(c) -. p0.(c);
	    done;
	  done;
	  (* Now, let the 0'th scratchpad vector, which
	     initially is the null vector, loop over all
	     coordinate unit vectors and calculate volumes.

	     These volumes tell us how much the value of the
	     hyperplane eqn increases if we go one unit in the
	     corresponding direction. This is just the desired
	     coeffs of the hyperplane eqn.
	  *)
	  for i = 0 to dim-1 do
	    s0.(i) <- 1.0;
	    result.(i) <- det_dim scratchpad;
	    s0.(i) <- 0.0;
	  done;
	  (* We still need the inhomogeneous term. This is given
	     by the condition that inserting p0 into the hyperplane eqn
	     has to give zero. Note that result.(dim) is still =0.0 here.
	  *)
	  let v = hyperplane_eval result p0 in
	  let () = result.(dim) <- 0.0 -. v in
	    result
	end
  in
    hyperplane_eqn
;;

(* Functions whose name ends in _sr take an extra argument where to
   store their result. This is to limit consing.
*)

let points_mid_hyperplane_sr result p1 p2 =
  let dim = Array.length p1 in
  begin
    for i=0 to dim-1 do
      result.(i) <- p2.(i) -. p1.(i);
    done;
    result.(dim) <- 0.0;
    let val1 = hyperplane_eval result p1 in
    let val2 = hyperplane_eval result p2 in
    let () = result.(dim) <- 0.0-.(val1+.val2)*.0.5 in
    result
  end
;;

let hyperplane_do_normalize_return_norm hp =
  let dim = (Array.length hp)-1 in
  let rec compute_norm n so_far =
    if n = dim
    then sqrt(so_far)
    else compute_norm (n+1) (so_far+.hp.(n)*.hp.(n))
  in
  let norm = compute_norm 0 0.0 in
  if norm = 0.0
  then 0.0
  else
    let inv_norm = 1.0/.norm in
    begin
      for i=0 to dim+1-1 do
	hp.(i) <- hp.(i) *. inv_norm;
      done;
      norm
    end
;;

let hyperplanes_intersection_point dim =
  let scratchpad=Array.make (dim*dim) 0.0 in
  let scratchpad_result=Array.make (dim*dim) 0.0 in
  fun hyperplanes ->
    begin
      (* Fill the scratchpad with the coefficient matrix *)
      _fill_from_mx dim scratchpad hyperplanes;
      (* The destination has to start out as a unit matrix! *)
      for i=0 to dim-1 do
	for k=0 to dim-1 do
	  scratchpad_result.(i*dim+k) <- (if i=k then 1.0 else 0.0);
	done;
      done;
      ignore(compute_inv_on_scratchpads dim scratchpad scratchpad_result);
      let inv = _linear_array_to_mx dim scratchpad_result in
      let rhs = (Array.sub (Array.map (fun eqn -> 0.0-.eqn.(dim)) hyperplanes) 0 dim) in
      let result= mx_x_vec inv rhs in
      result
    end
;;


(* Note: here we assume that our simplex is of somewhat reasonable shape.
   If this assumption were not given, we would have to invest some more work
   and form a weighted average, with higher weights for the "better" values.

   Here, we just single out the first vertex and look for the intersection point
   of the mid-hyperplanes of all edges containing that vertex.

   As we cons the midpoint anyway, we do not bother consing the radius as well.

   Note: scratch_coords may be longer than needed (= an array of dim entries).
 *)

(* Note: also, the det_and_inv comment applies here. *)

(* Note: this is an extended version of the original function -
   The extension being that right now, we can also compute the midpoint
   of any p-dimensional simplex in q-dimensional space.
   (One use is to estimate the space angle ("Fraction of the sky") taken up
   by a D-1-dimensional simplex when observed from a given point.)

   Its sister function, simplex_incircle_midpoint_radius has not yet been
   generalized correspondingly. XXX This is work to do.
*)

let simplex_circumcircle_midpoint_radius ?(dim_simplex=(-1)) ?(smallest_allowed_distance=1e-6) dim_space =
  let dim_simplex=(if dim_simplex<0 then dim_space else dim_simplex) in
    (* ^ This is an evil hack to avoid consing if dim_simplex is not specified. *)
  let nr_vertices = dim_simplex + 1 in
  let intersect = hyperplanes_intersection_point dim_space in

    (* GB *)
  let hyperplane = hyperplane_eqn dim_space in
  let scratch_midpoint = Array.make dim_space 0.0 in
  let scratchpad_logs = Array.init nr_vertices (fun _ -> Array.copy scratch_midpoint) in
  let scratchpad_log_lens = Array.make nr_vertices 0.0 in

  let gram_schmidt = make_gram_schmidt_orthonormalizer dim_space in
  let (gs_clear, gs_check_finished, gs_add_vector, gs_copy_vector)=gram_schmidt in
  let scratch_coords = Array.init dim_space (fun _ -> Array.make (1+dim_space) 0.0) in
  let scratch_dir = Array.make dim_space 0.0 in
  let sprod_dim x y =
    let rec walk n sum =
      if n = dim_space then sum else walk (1+n) (sum+.x.(n)*.y.(n))
    in walk 0 0.0
  in
  (fun points_coords ->

    let midpoint = float_arrays_avg ~storage:scratch_midpoint points_coords in
    let () = for i=0 to nr_vertices-1 do
	for k=0 to dim_space-1 do
	  scratchpad_logs.(i).(k) <- points_coords.(i).(k) -. scratch_midpoint.(k);
	done;
	scratchpad_log_lens.(i) <- sqrt(euclidean_len_sq scratchpad_logs.(i));
	done
    in
    let max_log_len = float_array_max scratchpad_log_lens in
    let test_coplanar = hyperplane (array_one_shorter points_coords 0) in
    let normal = array_one_shorter test_coplanar dim_space in
    let point_out_of_plane = points_coords.(0) in
      (* distance point-plane *)
    let distance = abs_float (Array.fold_left ( +. )
				 test_coplanar.(dim_space)
				 (array_pointwise ( *. )
				     point_out_of_plane
				     normal) )
      /.
				 (sqrt (euclidean_len_sq normal))
    in


  if distance < smallest_allowed_distance *. max_log_len || is_obj_nan distance
      then (midpoint,1e10*.max_log_len)
      else
    begin
    for i=1 to nr_vertices-1 do
      ignore(points_mid_hyperplane_sr scratch_coords.(i-1)
	       points_coords.(0) points_coords.(i));
      (* hyperplane_do_normalize_return_norm scratch_coords.(i-1);
	 <- there is no need to normalize. *)
    done;
     (* Now that we have dim_simplex hyperplanes that intersect in a subspace
	meeting the simplex in just one point (the circumcircle midpoint),
	we still need extra hyperplanes that constrain us to the simplex.

	How to obtain these? Easy: we use Gram-Schmidt, starting from simplex
	directions, fill them up with simplex-normal directions, and construct
	hyperplanes.

	Note: perhaps we can do all this with far less computational effort!
     *)
     gs_clear();
     for i=1 to nr_vertices-1 do
       for k=0 to dim_space-1 do
	 scratch_dir.(k) <- points_coords.(i).(k) -. points_coords.(0).(k);
       done;
       gs_add_vector scratch_dir;
     done;
     for k=0 to dim_space-1 do
       scratch_dir.(k) <- 0.0;
     done;
     (* From now on, scratch_dir will be the null vector -
	up to temporary modifications. *)
     (let rec add_normal n =
	if gs_check_finished () then ()
	else if n=dim_space
	then failwith "circumcircle: something went badly wrong!"
	else
	  begin
	    scratch_dir.(n) <- 1.0;
	    gs_add_vector scratch_dir;
	    scratch_dir.(n) <- 0.0;
	    add_normal (1+n)
	  end
      in add_normal 0);
     (* Now that we extended the vertex directions with normals, translate back these normals
	to face equations
     *)
     for i=nr_vertices-1 to dim_space-1 do
       gs_copy_vector i scratch_coords.(i);
       (let offset = sprod_dim scratch_coords.(i) points_coords.(0) in
	  scratch_coords.(i).(dim_space) <- -.offset);
     done;
     let midpoint = intersect scratch_coords in
     let radius = sqrt (euclidean_distance_sq midpoint points_coords.(0)) in
       (midpoint,radius)
    end)
;;


let simplex_incircle_midpoint_radius dim ?(smallest_allowed_distance=1e-6) =
  let nr_vertices = dim+1 in
  let intersection = hyperplanes_intersection_point dim in
  let hyperplane = hyperplane_eqn dim in
  let scratch_midpoint = Array.make dim 0.0 in
  let scratchpad_logs = Array.init nr_vertices (fun _ -> Array.copy scratch_midpoint) in
  let scratchpad_log_lens = Array.make nr_vertices 0.0 in

  let scratch_hyperplanes = Array.init nr_vertices (fun _ -> Array.make nr_vertices 0.0) in
  (fun points_coords ->
    let midpoint = float_arrays_avg ~storage:scratch_midpoint points_coords in
    let () = for i=0 to nr_vertices-1 do
	for k=0 to dim-1 do
	  scratchpad_logs.(i).(k) <- points_coords.(i).(k) -. scratch_midpoint.(k);
	done;
	scratchpad_log_lens.(i) <- sqrt(euclidean_len_sq scratchpad_logs.(i));
	done
    in
    let max_log_len = float_array_max scratchpad_log_lens in
    let test_coplanar = hyperplane (array_one_shorter points_coords 0) in
    let normal = array_one_shorter test_coplanar dim in
    let point_out_of_plane = points_coords.(0) in
      (* distance point-plane *)
    let distance = abs_float (Array.fold_left ( +. )
				 test_coplanar.(dim)
				 (array_pointwise ( *. )
				     point_out_of_plane
				     normal) )
      /.
				 (sqrt (euclidean_len_sq normal))
    in

      if distance < smallest_allowed_distance *. max_log_len || is_obj_nan distance
      then (midpoint,0.0)
      else

    begin
      for i=0 to nr_vertices-1 do
	scratch_hyperplanes.(i) <- hyperplane (array_one_shorter points_coords i);
	ignore(hyperplane_do_normalize_return_norm scratch_hyperplanes.(i));
	( (* Have to ensure our hyperplanes are oriented in such a way that the midpoint
	     is on the same (here, negative) side - except for the last one,
	     where it has to be on the positive side. *)
	 if    (i < dim && hyperplane_eval scratch_hyperplanes.(i) midpoint <= 0.0)
	    || (i = dim && hyperplane_eval scratch_hyperplanes.(i) midpoint >= 0.0)
	 then ()
	 else
	   for k=0 to nr_vertices-1 do
	     scratch_hyperplanes.(i).(k) <- 0.0-.scratch_hyperplanes.(i).(k);
	   done;
	 );
      done;
      (* By now, we have all face hyperplanes.
	 Now, take one of them as special, and replace every other one by the average
	 of it with the special one. Of course, average is equivalent to sum here.
	 We are projective, after all.
	 Note: we take the last one as the special one, since we can then directly feed
	 our hyperplanes into the intersection function.
       *)
      for i=0 to nr_vertices-1-1 do
	for k=0 to nr_vertices-1 do
	  scratch_hyperplanes.(i).(k) <-
	    scratch_hyperplanes.(i).(k) +. scratch_hyperplanes.(dim).(k);
	done;
      done;
      let midpoint = intersection scratch_hyperplanes in
      let radius = hyperplane_eval scratch_hyperplanes.(dim) midpoint in
      (midpoint,radius)
    end
   )
;;


(* This function gives us upper and lower bounds
   for the spatial angle spawned by a set of vectors,
   as seen from the viewpoint of a given observer.

   This is used e.g. for adaptive subdivision schemes.
*)

let d_dimensional_space_angle_ub_lb dim =
  let buffer_inner = Array.init (dim+1) (fun n -> Array.make dim 0.0)
  and det = determinant dim
  and f_dim = float_of_int dim
  and circumcircle = simplex_circumcircle_midpoint_radius ~dim_simplex:(dim-1) dim
  in
    fun pos_observer vertices ->
      try
	begin
	  (* (1) Compute relative distance vectors *)
	  for i=0 to dim-1 do
	    for k=0 to dim-1 do
	      buffer_inner.(i).(k) <- vertices.(i).(k) -. pos_observer.(k)
	    done
	  done;
	  (* (2) Normalize these vectors to unit vectors *)
	  for i=0 to dim-1 do
	    let len =sqrt(euclidean_len_sq buffer_inner.(i)) in
	      if len=0.0
	      then raise Division_by_zero
	      else
		let inv_len = 1.0/.len in
		  for k=0 to dim-1 do
		    buffer_inner.(i).(k) <- inv_len*.buffer_inner.(i).(k)
		  done
	  done;
	  (* (3) We need both the volume of the simplex spawned by the
	     face's vectors and the origin, as well as the height
	     of that simplex. Getting the volume is easy -
	     it's just the determinant.

	     Concerning the height, the intersection point
	     of the face's plane with the tangent sphere must be the
	     circumcircle midpoint, since if we scale up the
	     tangent sphere to radius 1, it will intersect the face
	     in the circumcircle of the faces' vertices, and the
	     original touching point will just have moved outward
	     normal to the faces' hyperplane
	  *)
	  let (cc_mp,cc_r) = circumcircle buffer_inner
	  and volume = abs_float(det buffer_inner)
	  in
	  let height = sqrt(euclidean_len_sq cc_mp)
	  in
	  let volume_inner = volume /. (height *. f_dim) in
	  let volume_outer = volume_inner*.(height**(-.f_dim)) (* Note: may be "infinity" *)
	  in
	    (volume_outer,volume_inner)
	end
      with
	| Division_by_zero -> (0.0,0.0)
;;



let line_point_sr result line_point0 line_point1 param =
  let dim = Array.length line_point0
  and param0 = 1.0 -. param in
    begin
      for i = 0 to dim-1 do
	result.(i) <- line_point0.(i)*.param0+.line_point1.(i)*.param;
      done;
      result
    end
;;

let line_point line_point0 line_point1 param =
  line_point_sr
    (Array.copy line_point0)
    line_point0 line_point1 param
;;


let line_hyperplane_intersection_param line_p0 line_p1 eqn =
  let val0 = hyperplane_eval eqn line_p0
  and val1 = hyperplane_eval eqn line_p1
  in
    0.0 -. val0 /. (val1 -. val0)
;;

(* === Volume Functions === *)

(* Compute the volume of a regular simplex of dimension dim with edge length 1.
   Naive method.
   Hope that the formula is correct. Should be, though.
*)

let volume_regular_simplex dim =
  let dimf = float_of_int dim in
  let rec walk df vol_d height_d =
    if df = dimf (* floatingpoint is exact on ints *)
    then vol_d
    else
      let ddf = df+.1.0 in
      let height_dd = height_d*.sqrt(1.0-.1.0/.(ddf*.ddf)) in
      let vol_dd = (vol_d/.ddf)*.height_dd
      in walk ddf vol_dd height_dd
  in walk 1.0 1.0 1.0
;;

(* XXX Note: this does not do any kind of "pivoting". Maybe we should... *)
let volume_p_simplex_in_q_dimensions p dim_q =
  let buffer_vertices = Array.init (p-1) (fun _ -> Array.make dim_q 0.0) in
  let vol_factor=1.0/.(float_factorial (p-1)) in
    fun vertices ->
      try
	begin
	  for i=1 to p-1 do
	    for k=0 to dim_q-1 do
	      buffer_vertices.(i-1).(k) <- vertices.(i).(k) -. vertices.(0).(k)
	    done
	  done;
	  (* Now, the buffer contains all the edge vectors spawning the simplex.
	     Next, we use Gram-Schmidt.
	  *)
	  let rec walk n volume_now =
	    if n=p-1
	    then volume_now*.vol_factor
	    else
	      begin
		(* orthonormalize the n-th vector *)
		for i=0 to n-1 do
		  let sprod_n_i = scalar_product buffer_vertices.(n) buffer_vertices.(i) in
		    for k=0 to dim_q-1 do
		      buffer_vertices.(n).(k) <-
			buffer_vertices.(n).(k)-.sprod_n_i*.buffer_vertices.(i).(k)
		    done;
		done;
		let len_n = sqrt(euclidean_len_sq buffer_vertices.(n)) in
		let inv_len_n = if len_n=0.0 then raise Division_by_zero else 1.0/.len_n
		in
		  begin
		    for i=0 to dim_q-1 do
		      buffer_vertices.(n).(i) <- buffer_vertices.(n).(i)*.inv_len_n;
		    done;
		    walk (1+n) (volume_now*.len_n)
		  end
	      end
	  in walk 0 1.0
	end
      with | Division_by_zero -> 0.0
;;



(* It would be nice to have a function that computes the "volume"
   of a p-simplex embedded in q dimensional space...
 *)

let rec euler_gamma x =
  if x = 0.5	(* 0.5 is an exactly representable floatingpoint number. *)
  then sqrt(pi)
  else if x = 1.0
  then 1.0
  else
    let xx=x-.1.0 in
      xx*.euler_gamma(xx)
	(* No need to go tail recursive here. *)
;;

(* vol(d) = Pi^(d/2)/(Gamma(1+d/2)) *)

let volume_d_sphere d =
  let df = float_of_int d in
    (pi**(df*.0.5))/.(euler_gamma(1.0 +. df*.0.5))
;;

(* A(d) = 2*Pi^(d/2)/(Gamma(d/2)) *)

let surface_d_sphere d =
  let df = float_of_int d
  in 2.0*.(pi**(df*.0.5))/.(euler_gamma(df*.0.5))
;;


let points_on_3d_sphere subdivision_level =
  (* It is slightly subtle to see that we really
     may register float vectors with impunity here.
     We will not encounter spurious closeby points
     from roundoff errors...

     Note: for level N, we will obtain 2+4^(N+1) points.
   *)
  let ht_seen = Hashtbl.create 50 in
  let normalized v =
    let inv_len = 1.0/.(euclidean_len_sq v) in
    Array.map (fun x -> x*.inv_len) v
  in
  let midpoint p q =
    normalized (Array.init 3 (fun n -> 0.5*.(p.(n)+.q.(n))))
  in
  let registered_point x = let _ = Hashtbl.replace ht_seen x true in x in
  let rec walk level p0 p1 p2 =
    if level = subdivision_level then ()
    else
      let p01 = registered_point (midpoint p0 p1)
      and p02 = registered_point (midpoint p0 p2)
      and p12 = registered_point (midpoint p1 p2)
      in
      begin
	walk (1+level) p0 p01 p02;
	walk (1+level) p1 p01 p12;
	walk (1+level) p2 p02 p12;
	walk (1+level) p01 p02 p12;
      end
  in
  let p0 = registered_point [| 0.0; 0.0; 1.0|]
  and p1 = registered_point [| 0.0; 0.0;-1.0|]
  and p2 = registered_point [| 0.0; 1.0; 0.0|]
  and p3 = registered_point [| 0.0;-1.0; 0.0|]
  and p4 = registered_point [| 1.0; 0.0; 0.0|]
  and p5 = registered_point [|-1.0; 0.0; 0.0|]
  in
  begin
    walk 0 p0 p2 p4;
    walk 0 p1 p3 p5;
    walk 0 p0 p2 p5;
    walk 0 p1 p3 p4;
    walk 0 p0 p3 p4;
    walk 0 p1 p2 p5;
    walk 0 p0 p3 p5;
    walk 0 p1 p2 p4;
    hashtbl_keys ht_seen
  end
;;

(* Sphere packing ratios for the D_n lattices,
   which are densest lattice packings for dim>8 and dim<6.

   For dim=6,7,8, the E_dim lattices are the densest
   lattice packings. For high dim, densest packings
   seem to be nonlattice.
 *)

let sphere_packing_ratio_lattice_type_d dim =
  if dim=1 then 1.0
  else
    let lattice_vectors =
      Array.init dim
	(fun n ->
	  Array.init
	    dim
	    (fun j ->
	      if n=(dim-1) (* last one *)
	      then (if j >= dim-2 then 1.0 else 0.0)
	      else (if j = n then 1.0 else if j=(n+1) then (-1.0) else 0.0)))
    in
    let det_dim = determinant dim in
    let lattice_cell_volume = det_dim lattice_vectors in
    (* Well, actually, I do know what this is. This will always be 2. *)
    let sphere_radius=0.5*.sqrt(2.0) in
    let sphere_volume =
      sphere_radius**(float_of_int dim)*.(volume_d_sphere dim)
    in
    (* How many effective spheres are there per cell?
       Answer: just one, of course.
     *)
    sphere_volume/.lattice_cell_volume
;;


(* Note: we might also want to have a numerically_integrate_generic,
   which takes an adding and scaling function as args *)

let numerically_integrate_float_over_box
    ?(fun_rng = Random.float)
    ?(nr_points = 1000)
    (corner_nw,corner_se)
    f =
  let dim = Array.length corner_nw in
  let box_vol =
    let rec walk coord sub_vol =
      if coord=dim then sub_vol
      else walk (coord+1) (sub_vol*.(corner_se.(coord)-.corner_nw.(coord)))
    in walk 0 1.0
  in
  let scratch_pos = Array.make dim 0.0 in
  let set_random_pos () =
    for i=0 to dim-1 do
      scratch_pos.(i) <- corner_nw.(i) +. fun_rng(corner_se.(i)-.corner_nw.(i));
    done;
  in
  let rec collect n sum_now =
    if n = nr_points
    then box_vol*.sum_now/.(float_of_int nr_points)
    else
      let () = set_random_pos () in
      let contrib_here = f scratch_pos
      in collect (n+1) (sum_now+.contrib_here)
  in collect 0 0.0
;;

(* Test:

let raw_val =
  numerically_integrate_float_over_box ~nr_points:100000 ([|-1.0;-1.0|],[|1.0;1.0|])
    (fun [|x;y|] ->
      let xy = x*.x+.y*.y
      in if xy > 1.0 then 0.0 else sqrt(1.0-.xy))
in 2.0*.raw_val/.(4.0/.3.0);;

- : float = 3.15118814181817708

Approximately pi - good.

*)

(*

let line_hyperplane_intersection_sr result line_p0 line_p1 eqn =
  let val0 = hyperplane_eval eqn line_p0
  and val1 = hyperplane_eval eqn line_p1
  in
  let p = 0.0 -. val0 /. (val1 -. val0)
  in (* Note: if there is no intersection point, we get NaN. *)
    (p,line_point_sr result line_p0 line_p1 p)
;;

let line_hyperplane_intersection line_p0 line_p1 eqn =
  line_hyperplane_intersection_sr (Array.copy line_p0) line_p0 line_p1 eqn
;;
*)


let read_file_as_lines filename =
  let input_file = open_in filename in
  let bad_line = "bad" in
  let rec read_lines so_far =
    let next_line =
      try
        input_line input_file
      with
        End_of_file -> bad_line in
    if next_line == bad_line then
      let () = close_in input_file in
      List.rev so_far
    else
      read_lines (next_line::so_far)
  in
  let lines = read_lines []
  in
  lines
;;

let read_dir =
  let token = "" in
  fun dir_name ->
    let dir_h = Unix.opendir dir_name in
    (* Actually, we should do something unwind-protect-ish here... *)
    let rec work read =
      let next = try Unix.readdir dir_h with | End_of_file -> token
      in
      if next=token
      then List.sort compare read
      else if next = "." || next = ".."
      then work read
      else work (next::read)
    in
    let result = work [] in
    let () = Unix.closedir dir_h in
    result
;;

(* XXX Note that the following code at present is *not* valid OCaml
(it works, but uses hidden assumptions on iteration order which are
not guaranteed by the docs). I notified the OCaml developers and asked
them to correct this.
*)

let regexp_decompose nr_pieces rx_string =
  let rx = Str.regexp rx_string in
  fun str ->
    let rec walk so_far pos =
      let pos_found =
      try Str.search_forward rx str pos
      with
      | Not_found -> (-1)
            (* This is a hack to maintain the niceties of
               tail recursiveness (which exception handling
               would destroy) without consing an int option.
	     *)
      in
      if pos_found=(-1)
      then
	List.rev so_far
      else
	let pos_end = Str.match_end() in
	let pieces_here =
          Array.init nr_pieces
            (fun n ->
              try
		Some (Str.matched_group (n+1) str)
              with
            | Not_found -> None
            )
	in walk (pieces_here::so_far) pos_end
    in walk [] 0
;;

let chomp line =
  let len = String.length line in
  if len > 0 && String.get line (len-1) = '\n'
  then String.sub line 0 (len-1)
  else line
;;

let gensym =
  let ht_sym_count = Hashtbl.create 10 in
  fun name ->
    let n =
      try
	Hashtbl.find ht_sym_count name
      with
      | Not_found -> 0
    in
    let () = Hashtbl.replace ht_sym_count name (1+n) in
    let sym = Printf.sprintf "%s-%d" name (1+n)
    in sym
;;

let md5 str = Digest.to_hex (Digest.string str);;

let for_permutations m f =
  let choice = Array.make m 0 in
  let available_places = Array.make m 0 in
  let rec choose nr_j =
    if nr_j = m
    then f choice
    else
      for cj = 0 to m-1 do
	if available_places.(cj) = 0
	then
	  begin
	    available_places.(cj) <- 1;
	    choice.(cj) <- nr_j;
	    choose (1+nr_j);
	    available_places.(cj) <- 0;
	  end
	else ()
      done
  in choose 0
;;

(* Determine the sign of a permutation.
   While introductory courses often teach a O(N^2) method,
   this can be done in O(N), by implicitly using cycle notation.

   This is used for getting signs right when doing boundary element
   surface integrals.

   A nice didactic example for an "externally functional" function
   that internally uses side effects.
 *)

let permutation_sign perm =
  let len = Array.length perm in
  let was_processed = Array.make len false in (* Wasteful - a bit array would be nicer! *)
  let rec find_first_unprocessed_starting_at pos =
    if pos = len
    then (-1)
    else
      if was_processed.(pos)
      then find_first_unprocessed_starting_at (1+pos)
      else pos
  in
  let rec follow_cycle cycle_len pos =
    if was_processed.(pos)
    then cycle_len
    else
      begin
	was_processed.(pos) <- true;
	follow_cycle (1+cycle_len) perm.(pos)
      end
  in
  let rec walk sign_now pos =
    let continue_here = find_first_unprocessed_starting_at pos in
    if continue_here = (-1) then sign_now
    else
      let len_this_cycle = follow_cycle 0 continue_here in
      walk (if (len_this_cycle land 1)=0 then (0-sign_now) else sign_now) (1+continue_here)
  in
  walk 1 0
;;

let inverse_permutation perm =
  let len = Array.length perm in
  let result = Array.make len (-1) in
  let () = for i=0 to len-1 do
    result.(perm.(i)) <- i;
  done
  in
    result;;


(* Something quite weird: a simple convergence accelerator, along the lines of SICP *)

let guess_limit =
  (* Note: this function is non-reentrant, as we are
     internally re-using a once-allocated table *)
  let r_table = ref [|[||]|] in
  (*
  let print_table comment =
    let table = !r_table in
    let () = Printf.printf "=== TABLE %s ===\n" comment in
    for nr_row=0 to Array.length table-1 do
      let row=table.(nr_row) in
      begin
	Printf.printf "%3d:" nr_row;
	Array.iter (fun x -> Printf.printf " %6.4f" x) row;
	Printf.printf "\n";
      end
    done
  in
   *)
  let ensure_table_big_enough n =
    (* let () = Printf.printf "ensure_table_big_enough %d\n" n in *)
    let table = !r_table in
    let nr_rows =  Array.length table in
    let nr_rows_needed = (n+1)/2 in
    if nr_rows_needed <= nr_rows
    then ()
    else
      (* substitute the whole table by a bigger one - do not bother to
	 re-use previously allocated table rows. Let the GC take care of
	 that.
       *)
      let () =
	r_table:=
	  Array.init nr_rows_needed
	    (fun nr_row -> Array.make ((nr_rows_needed-nr_row)*2) 0.0)
      in
      (* let () = print_table "(after enlarging)" in *)
      ()
  in
  let rec propagate_row the_table nr_row nr_entries =
    (* let () = print_table (Printf.sprintf "(propagating row %d, %d entries)" nr_row nr_entries) in *)
    if nr_entries < 3
    then nr_row
    else
      let row = the_table.(nr_row) in
      let next_row = the_table.(1+nr_row) in
      begin
	for i=0 to nr_entries-2-1 do
	  let x0 = row.(i) in
	  let x1 = row.(i+1) in
	  let x2 = row.(i+2) in
	  let euler_denom = x0+.x2-.2.0*.x1 in
	  let euler_fraction =
	    if euler_denom=0.0
	    then 0.0
	    else
	      let z = x1-.x0 in z*.z/.euler_denom
	  in
	  next_row.(i) <- row.(i) -. euler_fraction
	      (* We set the i'th entry in next_row to the value
		 a geometric progression would converge to
		 which is given by the i'th, i+1'th, i+2'th entry
		 of the original row.
	       *)
	done;
	propagate_row the_table (1+nr_row) (nr_entries-2)
      end
  in
  fun ?(sum=false) num_sequence ->
    let len = Array.length num_sequence in
    let () = ensure_table_big_enough len in
    let table = !r_table in
    let nr_rows = Array.length table in
    let nr_start_row = nr_rows-(len+1)/2 in
    let start_row = table.(nr_start_row) in
    let () =
      for i=0 to len-1 do
	start_row.(i) <- num_sequence.(i)
      done
    in
    let () =
      (if sum
      then
	for i=1 to len-1 do
	  start_row.(i) <- start_row.(i)+.start_row.(i-1)
	done
      else ())
    in
    let rec process_table nr_start_row nr_entries =
      (* let () = Printf.printf "process_table nr_start_row=%d nr_entries=%d\n"
	 nr_start_row nr_entries in *)
      if nr_entries<3
      then
	table.(nr_start_row).(nr_entries-1) (* our best estimate *)
      else
	let nr_end_row = propagate_row table nr_start_row nr_entries in
	(* Now, the table.(nr_start_row+j).(0) entries
	   contain the convergence accelerated series.
	 *)
	(* let () = Printf.printf "DDD nr_end_row=%d nr_start_row=%d\n" nr_end_row nr_start_row in *)
	let len_subsequence = 1+nr_end_row-nr_start_row in
	let nr_new_start_row = nr_rows-(len_subsequence+2)/2 in
	let new_start_row=table.(nr_new_start_row) in
	let () =
	  for i=0 to len_subsequence-1 do
	    new_start_row.(len_subsequence-i-1) <- table.(nr_rows-i-1).(0)
	  done
	in
	process_table nr_new_start_row len_subsequence
    in
    process_table nr_start_row len
;;







type gcomp_tree = (* graph component tree *)
  | GCT_leaf of int
  | GCT_node of gcomp_tree * gcomp_tree;;

let rec gcomp_tree_iter f tree =
  match tree with
    | GCT_leaf n -> f n
    | GCT_node (le,ri) ->
	begin
	  gcomp_tree_iter f le;
	  gcomp_tree_iter f ri
	end;;


let rec gcomp_tree_number_leaves tree =
  match tree with
    | GCT_leaf _ -> 1
    | GCT_node (le,ri) ->
	(gcomp_tree_number_leaves le) + (gcomp_tree_number_leaves ri);;


let gcomp_tree_flatten tree =
  let size = gcomp_tree_number_leaves tree in
  let result = Array.make size 0 in
  let rec walk pos tree =
    match tree with
      | GCT_leaf n ->
	  begin
	    result.(pos) <- n;
	    pos + 1
	  end
      | GCT_node (le,ri) ->
	  let pos_right = walk pos le in
	  walk pos_right ri
  in
  let _ = walk 0 tree in result;;


let graph_components ?(fun_join_data=fun x y -> x) fun_body =
  let ht_link_leaders = Hashtbl.create 17 in
  let ht_group_and_data_by_leader = Hashtbl.create 17 in
  let produce link_le link_ri entry =
    let leader_le =
      hashtbl_find_or ~make_new_entry:true ht_link_leaders link_le (fun () -> link_le)
    in
    let leader_ri =
      hashtbl_find_or ~make_new_entry:true ht_link_leaders link_ri (fun () -> link_ri)
    in
      if leader_le = leader_ri then () (* ignore multi-edges *)
      else
	let (g_le,data_le) =
	  hashtbl_find_or ht_group_and_data_by_leader leader_le
	    (fun () -> (GCT_leaf link_le,entry))
	and (g_ri,data_ri) =
	  hashtbl_find_or ht_group_and_data_by_leader leader_ri
	    (fun () -> (GCT_leaf link_ri,entry))
	in
	let g_le_ri = GCT_node (g_le,g_ri) in
	let data_le_ri = fun_join_data data_le (fun_join_data data_ri entry) in
	let () = gcomp_tree_iter (fun link -> Hashtbl.replace ht_link_leaders link leader_le) g_ri in
	let () = Hashtbl.remove ht_group_and_data_by_leader leader_ri in
	let () = Hashtbl.replace ht_group_and_data_by_leader leader_le (g_le_ri,data_le_ri) in
	  ()
  in
  let () = fun_body ~produce in
    hashtbl_map_values (fun k (v,x) -> (gcomp_tree_flatten v,x)) ht_group_and_data_by_leader
;;



(* XXX

let graph_components ?(fun_join_data=fun x y -> x) fun_body =
  let add_to_array a x =
    if -1<>array_position x a 0 then a
    else Array.init (1+Array.length a) (fun n -> if n=0 then x else a.(n-1))
  in
  let ht_link_leaders = Hashtbl.create 17 in
  let ht_group_and_data_by_leader = Hashtbl.create 17 in
  let produce link_le link_ri entry =
    let leader_le =
      hashtbl_find_or ~make_new_entry:true ht_link_leaders link_le (fun () -> link_le)
    in
    let leader_ri =
      hashtbl_find_or ~make_new_entry:true ht_link_leaders link_ri (fun () -> link_ri)
    in
      if leader_le = leader_ri then () (* ignore multi-edges *)
      else
	let (g_le,data_le) =
	  hashtbl_find_or ht_group_and_data_by_leader leader_le
	    (fun () -> ([|link_le|],entry))
	and (g_ri,data_ri) =
	  hashtbl_find_or ht_group_and_data_by_leader leader_ri
	    (fun () -> ([|link_ri|],entry))
	in
	let g_le_ri = Array.append g_le g_ri in
	let data_le_ri = fun_join_data data_le (fun_join_data data_ri entry) in
	let () = Array.iter (fun link -> Hashtbl.replace ht_link_leaders link leader_le) g_ri in
	let () = Hashtbl.remove ht_group_and_data_by_leader leader_ri in
	let () = Hashtbl.replace ht_group_and_data_by_leader leader_le (g_le_ri,data_le_ri) in
	  ()
  in
  let () = fun_body ~produce in
    ht_group_and_data_by_leader
;;

*)



(*

Example:

(* Haus vom Nikolaus with attachments *)

let pieces = hashtbl_to_array
  (graph_components
     ~fun_join_data:(fun ((_,_,xv) as x) ((_,_,yv) as y) -> if xv > yv then x else y)
     (fun ~produce ->
	let xproduce x y v = produce x y (x,y,v) in
	begin
	  xproduce 1 4 2.0;
	  xproduce 2 3 5.0;
	  xproduce 12 11 3.0;
	  xproduce 4 5 2.2;
	  xproduce 1 2 8.0;
	  xproduce 3 4 4.0;
	  xproduce 2 4 9.0;
	  xproduce 10 11 7.0;
	  xproduce 10 12 1.0;
	  xproduce 17 17 1.0;
	  xproduce 3 5 1.2;
	  xproduce 20 21 1.0;
	  xproduce 1 3 1.8;
	  xproduce 23 22 1.0;
	end
     ))
;;

Returns:

val pieces : (int * (int array * (int * int * float))) array =
  [|(17, ([|17|], (17, 17, 1.))); (1, ([|5; 1; 4; 2; 3|], (2, 4, 9.)));
    (20, ([|20; 21|], (20, 21, 1.))); (23, ([|23; 22|], (23, 22, 1.)));
    (10, ([|10; 12; 11|], (10, 11, 7.)))|]

*)



let sleep_float n =
  let _ = Unix.select [] [] [] n in ()
;;

let expire_dir ?(silent=true) dir_name max_age_days =
  let () =
    try let _ = Unix.stat dir_name in ()
    with
    | Unix.Unix_error (Unix.ENOENT,_,_) ->
	Unix.mkdir dir_name 0o755
  in
  let dir_entries = read_dir dir_name in
  let now = Unix.gettimeofday () in
  List.iter
    (fun entry ->
      let full_name = Printf.sprintf "%s/%s" dir_name entry in
      let stat = Unix.stat full_name in
      if stat.Unix.st_kind = Unix.S_REG
          && (now-.(max stat.Unix.st_mtime stat.Unix.st_ctime) >
	      max_age_days*.86400.0)
      then
	let () = (if silent then () else Printf.fprintf stderr "Expiring: %s\n%!" full_name)
	in
	Unix.unlink full_name
      else ())
    dir_entries
;;


(* Debugging helpers *)

let timing ?(tagfun= fun _ -> "Time passed: ") ?(channel=stdout) f x =
  let tag = tagfun x in
  let t0 = Unix.gettimeofday () in
  let result = f x in
  let t1 = Unix.gettimeofday () in
  let () = Printf.fprintf channel "%s%f sec\n%!" tag (t1-.t0) in
  result
;;

(* Imperative / Flow control related definitions *)

(* Note: for these functions, it is highly useful to pass both the
   linear index and the multi-index to the loop-body function.

   Furthermore, we re-use the multi-index array - so the loop-body
   function may have to take a copy!
 *)

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

let array_multiinit v_max_indices f =
  (* Here, we once more encounter nasty
     type system problems that force us
     to write bad code! *)
  let nr_entries = Array.fold_left ( * ) 1 v_max_indices in
  if nr_entries=0
  then [||]
  else
    let a = Array.make nr_entries None in
    let () =
      multifor v_max_indices
	(fun n v_indices ->
	  begin
	    a.(n) <- Some (f n v_indices);
	  end)
    in
    Array.map
      (fun x ->
	match x with
	| None -> impossible()
	| Some z -> z)
      a
;;


(* This is intended for multiprocessor parallelization of an easily
   subdividable workload (such as rendering an image)
 *)

let split_range nr_pieces range_start range_end =
  let range_length = range_end-range_start in
  let chunk_length =
    (float_of_int range_length) /. (float_of_int nr_pieces)
  in
    Array.init nr_pieces
      (fun n ->
	 let start_n = if n=0 then range_start
	   else
	     range_start+(int_of_float (chunk_length*.(float_of_int n)))
	 and end_n = if n+1=nr_pieces then range_end else
	     range_start+(int_of_float (chunk_length*.(float_of_int (n+1))))
	 in
	   (start_n,end_n))
;;

let compute_uniform_workload_forked
    ?(bailout=
	(fun str ->
	   let () = Printf.fprintf stderr "AIEE! %s\n%!" str in
	     exit 1))
    ~fun_combine
    v_work =
  let bailout s dummy = let _ = bailout s in dummy in
    (* Note that we use the "bailout" function in two different places where it expects
       different return types. Hence, we have to bend over backwards to get the type
       system to accept what we actually want to do...
    *)
  let nr_processes = Array.length v_work in
  let rec setup_childs nr_process child_info =
    if nr_process = nr_processes
    then List.rev child_info (* This ensures we get the data in proper order. *)
    else
      let (fd_read,fd_write) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let pid = Unix.fork () in
	if pid == (-1) (* fork failure *)
	then
	  bailout "fork() failure!" child_info
	else
	  if pid == 0 (* We are the child - compute our share and exit *)
	then
	  let () = Unix.close fd_read in
	  let s_write = Unix.out_channel_of_descr fd_write in
	  let result = v_work.(nr_process) () in
	  let () = Marshal.to_channel s_write result [] in
	    exit 0
	else
	  (* We are the parent *)
	  let () = Unix.close fd_write in
	  let s_read = Unix.in_channel_of_descr fd_read in
	    setup_childs (1+nr_process) ((s_read,pid)::child_info)
  in
  let all_childs_info = setup_childs 1 [] in
    (* Note that it is important that we start counting at 1 here, as the parent will do
       chunk #0!
    *)
  let result_chunk0 = v_work.(0) () in
    (* Note that we just do assume that all pieces of the computation take the same time.
       We are not trying to be overly sophisticated, fetching data from the fastest
       child first. Also, if we wanted a more powerful tool to compute with forked processes,
       we might want to divide the big task in a more fine-grained way and hand out sub-tasks
       to processes through a scheduler that takes care of when which process finishes
       which sub-task. For now, this is overkill.
    *)
  let rec collect_child_results have child_info_todo =
    match child_info_todo with
      | [] -> have
      | ((s_read,pid)::child_info_todo_next) ->
	  let contrib = Marshal.from_channel s_read in
	  let (returned_pid,status) = Unix.waitpid [] pid in
	    if status <> Unix.WEXITED 0
	    then
	      bailout "Child failure!\n%!" have
	    else
	      collect_child_results
		(fun_combine contrib have)
		child_info_todo_next
  in collect_child_results result_chunk0 all_childs_info
;;


(* ---
(* === Example === *)
let sum_of_inverse_squares =
  compute_uniform_workload_forked
    ~fun_combine:(fun a b -> a+.b)
    (let nr_processes=4 in
     let ranges=split_range nr_processes 1 100000 in
     let work subrange_start subrange_end =
       let () = Printf.printf "PID: %d SUB-RANGE %d - %d\n%!"
	 (Unix.getpid()) subrange_start subrange_end
       in
       let rec walk n sum =
	 if n = subrange_end then sum
	 else walk (1+n) (let fn = float_of_int n in sum +. 1.0/.(fn*.fn))
       in walk subrange_start 0.0
     in
       (Array.init nr_processes
	  (fun n ->
	     let (r_s,r_e) = ranges.(n) in
	       fun () -> work r_s r_e)))
;;

(* This gives: 1.64492406679822967
   The full sum would be pi^2/6 = 1.64493406684822641
*)
--- *)

let parse_or_error ?(fun_fail=failwith) fun_lex fun_parse str =
  let lexbuf = Lexing.from_string str in
  let locate char_pos =
    let rec walk p nr_lf pos_after_last_lf =
      if p=String.length str || p=char_pos
      then ((1+nr_lf),pos_after_last_lf)
      else
	if str.[p]='\n'
	then walk (1+p) (1+nr_lf) (1+p)
	else walk (1+p) nr_lf pos_after_last_lf
    in walk 0 0 0
  in
    try
      let parsed = fun_parse fun_lex lexbuf in
	parsed
    with
      | _ ->
	  let err_start = Lexing.lexeme_start lexbuf in
	  let err_end = Lexing.lexeme_end lexbuf in
	  let (line_nr,line_start) = locate err_start in
	  let line_intro = String.sub str line_start (err_start-line_start) in
	  let err = String.sub str err_start (err_end-err_start) in
	  let msg = Printf.sprintf "Parse error in line %d, char %d-%d: >>%s<< >>%s<<\n%s"
	    line_nr err_start err_end line_intro err
	    (if String.length str < 2000
	     then Printf.sprintf "=== CODE ===\n%s\n=== END CODE ===\n" str
	     else "")
	  in fun_fail msg
;;

let token_stream lexer eof_token str =
  let lexbuf = Lexing.from_string str in
  let rec walk have =
    let next = lexer lexbuf in
      if next = eof_token then List.rev have
      else walk (next::have)
  in walk []
;;


external marshal_to_bigarray :
  'a -> Marshal.extern_flags list ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  = "caml_marshal_to_bigarray"

external demarshal_from_bigarray :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a
  = "caml_demarshal_from_bigarray"

let gc_info verbose =
  let s = if verbose then Gc.stat() else Gc.quick_stat() in
    Printf.sprintf
"=== GC Stats (%s) ===
minor_words:       %f
promoted_words:    %f
major_words:       %f
minor_collections: %d
major_collections: %d
heap_words:        %d
heap_chunks:       %d
live_words:        %d
live_blocks:       %d
free_words:        %d
free_blocks:       %d
largest_free:      %d
fragments:         %d
compactions:       %d
top_heap_words:    %d
"
      (if verbose then "verbose" else "quick")
      s.Gc.minor_words
      s.Gc.promoted_words
      s.Gc.major_words
      s.Gc.minor_collections
      s.Gc.major_collections
      s.Gc.heap_words
      s.Gc.heap_chunks
      s.Gc.live_words
      s.Gc.live_blocks
      s.Gc.free_words
      s.Gc.free_blocks
      s.Gc.largest_free
      s.Gc.fragments
      s.Gc.compactions
      s.Gc.top_heap_words
;;

(* Low-level extensions *)

external encode_double_into_7bit_string: string -> float -> unit = "caml_7bit_encode_double_into_string";;

external decode_7bit_string_to_double: string -> float = "caml_7bit_decode_string_to_double";;

external debugprint_entity: 'a -> unit = "caml_debugprint_entity";;

external raw_execution_count: unit -> float = "caml_execution_count";;

external memory_footprint: 'a -> (float * float * float) = "caml_memory_footprint";;

(* We use this e.g. in our Petsc interface: *)
external put_double_into_string: string -> float -> bool -> unit = "caml_put_double_into_string";;

external read_double_from_string: string -> bool -> float = "caml_read_double_from_string";;

(* Note: I do not yet know what the best interface is... playing around with various options... *)

let with_cpu_instructions_counted f x cont =
  let c0 = raw_execution_count () in
  let v = f x in
  let c1 = raw_execution_count () in
    cont (c1-.c0) v
;;

let funcall_printing_cpu_instructions ?(channel=stderr) message f x =
  let c0 = raw_execution_count () in
  let v = f x in
  let c1 = raw_execution_count () in
  let () = Printf.printf "%s: %f CPU cycles\n%!" message (c1-.c0) in
    v
;;


let funcall_logging_cpu_instructions fun_log f x =
  let c0 = raw_execution_count () in
  let v = f x in
  let c1 = raw_execution_count () in
  let () = fun_log (c1-.c0) in
    v
;;

(* Something rather weird: we sometimes get into the situation where
   we have to provide information from a <something> -> unit function
   as a return value. This wrapper-generator allows us to do that
   systematically:
*)

let snapping_return_value f =
  let result = ref None in
  let produce x = result := Some x in
  let () = f produce in
    match !result with
      | None -> failwith "snapping_return_value: no value was produced!"
      | Some x -> x
;;

(* Startup-related *)

let register_version piece_strings =
  let s = md5 (String.concat ":" piece_strings) in
    register_feature "version" s
;;

(* This function returns an array containing two floats [|vmsize; vmrss|]
   which are obtained by reading the corresponding entries from
   the file /proc/self/status, usually present only on linux systems.
   If for any reasons the operation fails the array [|0.0; 0.0|]
   is returned instead. This function is intended only for debugging purposes.
   There may be a much better way to obtain the same data, we don't care
   for now. Notice that usually the numbers appears in the format "1234 kB".
   this function deals only with that suffix (kB), assuming that this never
   changes (we assume the kernel reports the numbers always expressed in KB).
 *)
let memstats ?(self_status_file="/proc/self/status") () =
  let vmsize_vmrss = [|0.0;  0.0|] in
  let re = Str.regexp "\\(VmSize\\|VmRSS\\):[ \t]+\\([0-9]+\\)[ \t]+[kK][bB]" in
  let rec next_line fd num_val_read =
    let l = input_line fd in
    if Str.string_match re l 0 then
      let key = Str.matched_group 1 l in
      let value = Str.matched_group 2 l in
      let key_index = (if key = "VmSize" then 0 else 1) in
      let () = vmsize_vmrss.(key_index) <- float_of_string value in
      if num_val_read > 1 then
        next_line fd (num_val_read-1)
      else
        0
    else
      next_line fd num_val_read
  in
    try
      let fd = open_in self_status_file in
      let _ = next_line fd 2 in
      let () = close_in fd
      in
        vmsize_vmrss
    with
      _ ->  vmsize_vmrss
;;

type mem_report_handler = float * float * float * string

let mem_report_begin id_string =
  let ms = memstats () in
    (ms.(0), ms.(1), Unix.gettimeofday (), id_string)

let mem_report_end (vmsize0, vmrss0, t0, id_string) =
  let ms = memstats () in
  let delta_vmsize = ms.(0) -. vmsize0 in
  let delta_vmrss = ms.(1) -. vmrss0 in
  let delta_t0 = Unix.gettimeofday () -. t0 in
    Printf.sprintf
      "Computation of \"%s\" required: dt=%f dvmsize=%f dvmrss=%f\n"
      id_string delta_t0 delta_vmsize delta_vmrss
;;

(* Functions to pack and unpack two integer numbers (int) inside one int32
   number. a and b must be positive numbers less than 0x10000 (65536).
   This can be useful to reduce memory consumption.
   * for 32 bit machines two int occupy 8 bytes,
     while an int32 occupy 4 bytes --> reduction to 50 % in mem. req.
   * for 64 bit machines two int occupy 16 bytes,
     while an int32 occupy 4 bytes --> reduction to 25 %
 *)
let pack_int_couple a b =
  if a < 0 or a > 0x7fff or b < 0 or b > 0xffff then
    failwith "pack_int_couple cannot pack integers: args out of bounds!"
  else
    (a lsl 16) lor b
;;

let unpack_int_couple ab = (ab lsr 16, ab land 0xffff);;

let time_passed =
  let time_zero = ref None
  in
    (fun () ->
       match !time_zero with
       | Some t0 -> Unix.gettimeofday() -. t0
       | None -> let () = time_zero := Some (Unix.gettimeofday()) in 0.0)
;;

let time_vmem_rss () =
  let mem = memstats () in ((time_passed()), mem.(0), mem.(1))
;;

(*
let pack_int_couple a b =
  if a < 0 or a > 0xffff or b < 0 or b > 0xffff then
    failwith "pack_int_couple cannot pack integers: args out of bounds!"
  else
    lor (lsl a 16) b
;;

let unpack_int_couple ab =
  let b32 = Int32.logand ab 0xffffl in
  let a32 = Int32.shift_right_logical ab 16
  in
    (Int32.to_int a32, Int32.to_int b32)
;;
*)

external reset_signal_handler: int -> unit = "caml_reset_signal_handler";;

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
  if pq_is_empty pq
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
  if pq_is_empty pq
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

(* --- 2d gaussian integration --- *)

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

let integrate2d =
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
  in
    fun ?(max_evaluations=(-1)) ?(tolerance=1e-8) ~points ~simplices f ->
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

(* Here are a few support functions that chiefly deal with importing OOMMF data files.
   This is rather specialized and most likely will not remain in this module for long.

   === BEGIN OOMMF-RELATED ===
*)


let interpolate_on_grid corner step_sizes ranges fun_access =
  let f_range_max = Array.map (fun n -> (float_of_int (n-1))) ranges in
  let dim = Array.length ranges in
  let buf_rel = Array.make dim 0.0 in
  let buf_dists = Array.make dim 0.0 in
  let buf_indices_lo = Array.make dim 0 in
  let buf_indices_hi = Array.make dim 0 in
  let buf_indices = Array.make dim 0 in
  let inv_step_sizes = Array.map (fun x -> 1.0/.x) step_sizes in
  let bit_range = 1 lsl dim in
  let fun_interpolate coords =
    let () =
      (* Set relative coordinates with clamping to within the interpolation region *)
      for i=0 to dim-1 do
	let x = (coords.(i)-.corner.(i))*.inv_step_sizes.(i) in
	let x_clamped = if x < 0.0 then 0.0 else if x > f_range_max.(i) then f_range_max.(i) else x in
	let lo = floor x_clamped in
	let hi = ceil x_clamped in
	let dist = x_clamped-.lo in
	  begin
	    buf_rel.(i) <- x_clamped;
	    buf_indices_lo.(i) <- int_of_float lo;
	    buf_indices_hi.(i) <- int_of_float hi;
	    buf_dists.(i) <- dist;
	  end
      done;
    in
    let rec walk n weighted_sum =
      if n = bit_range then weighted_sum
      else
	let rec contrib ix partial_weight =
	  if ix = dim
	  then
	    (fun_access buf_indices)*.partial_weight
	  else
	      if n land (1 lsl ix) <> 0
	      then
		let () = buf_indices.(ix) <- buf_indices_hi.(ix) in
		  contrib (1+ix) (partial_weight*.buf_dists.(ix))
	      else
		let () = buf_indices.(ix) <- buf_indices_lo.(ix) in
		  contrib (1+ix) (partial_weight*.(1.0-.buf_dists.(ix)))
	in
	  walk (1+n) (weighted_sum +. (contrib 0 1.0))
    in
    let result = walk 0 0.0 in
      result
  in
    fun_interpolate
;;


(* Example...

let the_grid =
   [|[|0.0;1.0;2.0;|];
     [|1.0;2.0;3.0;|];
     [|3.0;4.0;5.0;|];|]
;;

let f_grid =
   interpolate_on_grid
   [|100.0;100.0;100.0|]
   [|20.0;20.0;20.0|]
   [|3;3|]
   (fun xy -> the_grid.(xy.(1)).(xy.(0)));;

*)

(* === Reading OOMMF rectangular grid files for sampling... === *)

let oommf_data oommf_filename =
  let expected_line1 = "# OOMMF: rectangular mesh v1.0" in
  let rx_header = Str.regexp "^# \\([a-z]+\\):[ \\t]*\\([^ \\t]*\\)" in
  let rx_data = Str.regexp "^# Begin: Data \\(.*\\)" in
  let fd = open_in oommf_filename in
  let cleanup () =
    close_in fd
  in
    try
      let line1 = input_line fd in
      let () = (if line1 <> expected_line1
		then failwith
		  (Printf.sprintf "Cannot read file '%s'. Expected first line: %s Got: %s"
		     oommf_filename expected_line1 line1)
		else ())
      in
	(* We have to extract header fields for:
	   meshunit, valueunit,
	   valuemultiplier,
	   xmin,ymin,zmin,xmax,ymax,zmax
	   meshtype (must be: rectangular)
	   xbase,ybase,zbase,
	   xstepsize,ystepsize,zstepsize
	   xnodes,ynodes,znodes
	   # Begin: data <text | binary 4 | binary 8>
	*)
      let ht_header = Hashtbl.create 100 in
      let rec walk () =
	let line = input_line fd in
	  if Str.string_match rx_data line 0
	  then
	    (* have complete header *)
	    let dtype = Str.matched_group 1 line in
	    let ddd = Printf.printf "DDD dtype: '%s'\n%!" dtype in (* DDD *)
	    let () =
	      (if dtype = "Text" then ()
	       else failwith "FIXME: reading OOMMF binary data files is not supported yet. For now, you have to convert to text data files.")
	    in
	      dtype
	  else
	    let () =
	      if Str.string_match rx_header line 0
	      then
		let key = Str.matched_group 1 line in
		let datum = Str.matched_group 2 line in
		  Hashtbl.replace ht_header key datum
	      else ()
	    in walk ()
      in
      let typeof_data = walk () in
	(* We now have a fully populated header entries array. *)
      let () = (if Hashtbl.find ht_header "meshtype" <> "rectangular"
		then failwith "Problem: can only read rectangular-layout OOMMF data."
		else ())
      in
      let () = (if Hashtbl.find ht_header "meshunit" <> "nm"
		then failwith "Problem: assumption failed: OOMMF mesh unit is not 'nm'."
		else ())
      in
      let () = (if Hashtbl.find ht_header "valueunit" <> "kA/m"
		then failwith "Problem: assumption failed: OOMMF data unit is not 'kA/m'."
		else ())
      in
      let valuemultiplier = float_of_string (Hashtbl.find ht_header "valuemultiplier") in
	(*
	  let xmin = float_of_string (Hashtbl.find ht_header "xmin") in
	  let xmax = float_of_string (Hashtbl.find ht_header "xmax") in
	  let ymin = float_of_string (Hashtbl.find ht_header "ymin") in
	  let ymax = float_of_string (Hashtbl.find ht_header "ymax") in
	  let zmin = float_of_string (Hashtbl.find ht_header "zmin") in
	  let zmax = float_of_string (Hashtbl.find ht_header "zmax") in
	*)
      let xbase = float_of_string (Hashtbl.find ht_header "xbase") in
      let ybase = float_of_string (Hashtbl.find ht_header "ybase") in
      let zbase = float_of_string (Hashtbl.find ht_header "zbase") in
      let xstepsize = float_of_string (Hashtbl.find ht_header "xstepsize") in
      let ystepsize = float_of_string (Hashtbl.find ht_header "ystepsize") in
      let zstepsize = float_of_string (Hashtbl.find ht_header "zstepsize") in
      let xnodes = int_of_string (Hashtbl.find ht_header "xnodes") in
      let ynodes = int_of_string (Hashtbl.find ht_header "ynodes") in
      let znodes = int_of_string (Hashtbl.find ht_header "znodes") in
      let xyz_data =
	Array.init xnodes
	  (fun nz -> Array.init ynodes
	     (fun ny -> Array.init znodes
		(fun nx -> Array.make 3 0.0)))
      in
      let xyz = [|0;0;0|] in
      let incf_coords () =
	let xc = xyz.(0) in
	  if xc < xnodes-1 then
	    let () = xyz.(0) <- 1+xc in true
	  else
	    let () = xyz.(0) <- 0 in
	    let yc = xyz.(1) in
	      if yc < ynodes-1 then
		let () = xyz.(1) <- 1+yc in true
	      else
		let () = xyz.(1) <- 0 in
		let zc = xyz.(2) in
		  if zc < znodes-1 then
		    let () = xyz.(2) <- 1+zc in true
		  else false
      in
      let must_change_byteorder =
	int_of_float(read_double_from_string "\x49\x96\xb4\x38" true) <> 1234567
      in
      let readfloat_byte n =
	let buf = String.make n ' ' in
	  fun () ->
	    let () =
	      for i=0 to n-1 do
		buf.[i] <- input_char fd
	      done
	    in
	      (read_double_from_string buf must_change_byteorder)*.valuemultiplier
      in
      let xscale x = x*.valuemultiplier in
      let readfloat_text () =
	Scanf.fscanf fd "%f" xscale
      in
      let readfloat =
	match typeof_data with
	  | "Text" -> readfloat_text
	  | "Byte 8" -> readfloat_byte 8
	  | "Byte 4" -> readfloat_byte 4
	  | _ -> failwith (Printf.sprintf "Unknown OOMMF data format: '%s'" typeof_data)
      in
      let readvec () =
	let val_x = readfloat () in
	let val_y = readfloat () in
	let val_z = readfloat () in
	  begin
	    xyz_data.(xyz.(2)).(xyz.(1)).(xyz.(0)).(0) <- val_x;
	    xyz_data.(xyz.(2)).(xyz.(1)).(xyz.(0)).(0) <- val_y;
	    xyz_data.(xyz.(2)).(xyz.(1)).(xyz.(0)).(0) <- val_z;
	    incf_coords()
	  end
      in
      let () =
	(if typeof_data = "Text" then ()
	 else
	   let n1 = readfloat () in
	     (if n1 <> 1234567.0 && n1 <> 123456789012345.0
	      then failwith "Mangled OOMMF file: Byte order seems broken!"
	      else ()))
      in
      let rec populate () =
	let more = readvec () in
	  if more then populate ()
	  else ()
      in
      let () = populate () in
      let result =
	(* Now we have everything. Let us return the interesting data... *)
	([|xbase;ybase;zbase;|],
	 [|xstepsize;ystepsize;zstepsize;|],
	 [|xnodes;ynodes;znodes|],
	 xyz_data)
      in
      let () = cleanup () in
	result
    with | x ->
      begin
	cleanup();
	raise x
      end
;;

let interpolated_oommf_data filename =
  let (base,step_sizes,ranges,data) = oommf_data filename in
  let fx = interpolate_on_grid base step_sizes ranges
    (fun xyz -> data.(xyz.(0)).(xyz.(1)).(xyz.(2)).(0))
  and fy = interpolate_on_grid base step_sizes ranges
    (fun xyz -> data.(xyz.(0)).(xyz.(1)).(xyz.(2)).(1))
  and fz = interpolate_on_grid base step_sizes ranges
    (fun xyz -> data.(xyz.(0)).(xyz.(1)).(xyz.(2)).(2))
  in
    fun coords -> [|fx coords;fy coords;fz coords|]
;;

(*
   Example:
   let f_oommf = interpolated_oommf_data "/tmp/oommf.omf";;
*)

(* === END OOMMF-RELATED === *)

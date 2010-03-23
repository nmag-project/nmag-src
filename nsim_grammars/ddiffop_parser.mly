%{

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

let hashtbl_arbitrary_element ht =
  let have_it = ref None in
  let () =
    try
      Hashtbl.iter
	(fun k v ->
	   begin
	     have_it := Some (k,v);
	     raise Not_found; (* abusing this exception here... *)
	   end)
	ht
    with
      | Not_found -> ()
  in !have_it;;


let hashtbl_keys ?sorter ht =
  let nr_entries = Hashtbl.length ht in
    if nr_entries = 0 
    then [||]
    else
      let opt_kv = hashtbl_arbitrary_element ht in
      let result =
	match opt_kv with
	  | None -> failwith "Impossible!"
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

let map_hashtbl_to_array ?sorter mapper ht =
  let nr_entries = Hashtbl.length ht in
    if nr_entries = 0 
    then [||]
    else
      let opt_kv = hashtbl_arbitrary_element ht in
      let result =
	match opt_kv with
	  | None -> failwith "Impossible!"
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

let optionally f opt_x =
  match opt_x with
    | None -> None
    | Some x -> Some (f x)
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
	     | IX_name name ->
		 let rec walk n =
		   if v_all_index_vars.(n) = name then v_index_set.(n)
		   else walk (1+n)
		 in walk 0
	 in
	   f ~ix_value)
;;

let instantiate_field_indices ~ix_value nsi_field =
  {f_name=nsi_field.fnsi_name;
   f_indices=Array.map ix_value nsi_field.fnsi_indices;
   f_bspec=nsi_field.fnsi_bspec
  }
;;


  (* Note: there would be some opportunity for higher-order abstraction in the functions below,
     but this would be too convoluted to be worth the effort...
  *)
  
  let all_symbolic_indices nsi_fields =
    let ht = Hashtbl.create 1 in
    let () = Array.iter
      (fun nsi_field ->
	 Array.iter (fun ix ->
		       match ix with
			 | IX_name s -> Hashtbl.replace ht s true
			 | _ -> ())
	   nsi_field.fnsi_indices)
      nsi_fields
    in
    let nr_vars = Hashtbl.length ht in
    let result = Array.make nr_vars "" in
    let r_n = ref 0 in
    let () = Hashtbl.iter (fun k _ -> let () = result.(!r_n) <- k in r_n:= 1* !r_n) ht in
      result
  ;;

  (* Note: this function is quite a recent introduction; perhaps,
     the amdmt_get_* functions should use that as well...
   *)
  let expand_fields sum_specs nsi_fields =
    let ht = Hashtbl.create 1 in
    let () =
      List.iter
	(fun nsi_field ->
	  forall_index_instantiations (all_symbolic_indices [|nsi_field|]) sum_specs
	    (fun ~ix_value ->
	      let field = instantiate_field_indices ~ix_value nsi_field in
	      Hashtbl.replace ht field true))
	nsi_fields
    in hashtbl_keys ht
;;


  let amdmt_get_diag_ones sum_specs amendments =
    let ht = Hashtbl.create 1 in
    let () =
      List.iter
	(fun amd ->
	   match amd with 
	     | AMDMT_DIAGONAL_ONES (nsi_field_le,nsi_field_ri) ->
		 forall_index_instantiations (all_symbolic_indices [|nsi_field_le;nsi_field_ri|]) sum_specs
		   (fun ~ix_value ->
		      let field_le_ri = (instantiate_field_indices ~ix_value nsi_field_le,
					 instantiate_field_indices ~ix_value nsi_field_ri)
		      in
			Hashtbl.replace ht field_le_ri true)
	     | _ -> ())
	amendments
    in hashtbl_keys ht
;;
  
  let amdmt_get_gauge_fixed sum_specs amendments =
    let ht = Hashtbl.create 1 in
    let () =
      List.iter
	(fun amd ->
	   match amd with 
	     | AMDMT_GAUGE_FIX nsi_field ->
		 forall_index_instantiations (all_symbolic_indices [|nsi_field|]) sum_specs
		   (fun ~ix_value ->
		      let field = instantiate_field_indices ~ix_value nsi_field in
			Hashtbl.replace ht field true)
	     | _ -> ())
	amendments
    in hashtbl_keys ht
;;
		 
  let amdmt_get_periodic sum_specs amendments =
    let ht = Hashtbl.create 1 in
    let () =
      List.iter
	(fun amd ->
	   match amd with 
	     | AMDMT_PERIODIC nsi_field ->
		 forall_index_instantiations (all_symbolic_indices [|nsi_field|]) sum_specs
		   (fun ~ix_value ->
		      let field = instantiate_field_indices ~ix_value nsi_field in
			Hashtbl.replace ht field true)
	     | _ -> ())
	amendments
    in hashtbl_keys ht
;;

  let amdmt_get_mxdim sum_specs amendments =
    let ht_le = Hashtbl.create 1 in
    let ht_ri = Hashtbl.create 1 in
    let () =
      List.iter
	(fun amd ->
	   match amd with 
	     | AMDMT_MXDIM (opt_le,opt_ri) ->
		 let process_fields ht li_fields =
		   List.iter
		     (fun nsi_field ->
		       forall_index_instantiations
			 (all_symbolic_indices [|nsi_field|]) sum_specs
			 (fun ~ix_value ->
			   let field = instantiate_field_indices ~ix_value
			       nsi_field
			   in
			   Hashtbl.replace ht field true))
		     li_fields in
		 let _ = optionally (process_fields ht_le) opt_le in
		 let _ = optionally (process_fields ht_ri) opt_ri in
		 ()
	     | _ -> ()
	)
	amendments
    in (hashtbl_keys ht_le,
	hashtbl_keys ht_ri)
;;


  let build_ddiffop contribs sum_specs amendments =
    let ht_collect = Hashtbl.create 17 in
    let dolist li f = List.iter f li in
    let pushed_new li x =
      if List.mem x li then li else x::li
    in
    let extended_index_vars in_this_field plus_this_difftype seen =
      let nr_indices = Array.length in_this_field.fnsi_indices in
      let rec walk nr_ix have =
	if nr_ix = nr_indices then have
	else 
	  match in_this_field.fnsi_indices.(nr_ix) with
	    | IX_int _ -> walk (1+nr_ix) have
	    | IX_name n -> walk (1+nr_ix) (pushed_new have n)
      in walk 0
	   (match plus_this_difftype with
	      | PDIFF_vol (IX_name n) -> (pushed_new seen n)
	      | PDIFF_boundary (IX_name n) -> (pushed_new seen n)
	      | _ -> seen)
    in
    let () = dolist contribs
      (fun (coeff,((field_le,pdifftype_le),(field_ri,pdifftype_ri),opt_field_mid)) ->
	 let v_all_index_vars =
	   Array.of_list 
	     (extended_index_vars field_le pdifftype_le
		(extended_index_vars field_ri pdifftype_ri
		   (match opt_field_mid with
		      | None -> []
		      | Some m -> extended_index_vars m PDIFF_none [])))
	 in
	   forall_index_instantiations v_all_index_vars sum_specs
	     (fun ~ix_value ->
		let difftype_of pdt =
		  match pdt with
		    | PDIFF_none -> DIFF_none
		    | PDIFF_vol ix -> DIFF_vol (ix_value ix)
		    | PDIFF_boundary ix -> DIFF_boundary (ix_value ix)
		in
		let contrib_tag = 
		  match opt_field_mid with
		    | None ->
			[|instantiate_field_indices ~ix_value field_le;
			  instantiate_field_indices ~ix_value field_ri;
			|]
		    | Some field_mid ->
			[|instantiate_field_indices ~ix_value field_le;
			  instantiate_field_indices ~ix_value field_ri;
			  instantiate_field_indices ~ix_value field_mid;
			|]
		in
		let ht_this_tag =
		  try Hashtbl.find ht_collect contrib_tag
		  with
		    | Not_found -> 
			let h = Hashtbl.create 1 in
			let () = Hashtbl.add ht_collect contrib_tag h in
			  h
		in
		let contrib_difftype =
		  (difftype_of pdifftype_le, difftype_of pdifftype_ri)
		in
		let old_coeff =
		  try Hashtbl.find ht_this_tag contrib_difftype with
		    | Not_found -> 0.0
		in
		let new_coeff = coeff +. old_coeff in
		  if new_coeff = 0.0
		  then Hashtbl.remove ht_this_tag contrib_difftype
		  else Hashtbl.replace ht_this_tag contrib_difftype new_coeff))
    in
      (* Note: the structure of the code above may seem a bit strange - we first
	 put all our data in a hash table, and later on, we map this back to an array.
	 
	 The reason is that during the course of development, it became clear that
	 we need "pattern flexibility" in the contrib_tags, so we cannot use this
	 as a hash key.
      *)
      {diff_contribs=
	  map_hashtbl_to_array ~sorter:compare
	    (fun tag entries -> (tag,entries)) ht_collect;
       diff_mxdim=amdmt_get_mxdim sum_specs amendments;
       diff_diag_ones=amdmt_get_diag_ones sum_specs amendments;
       diff_gauge_fix=amdmt_get_gauge_fixed sum_specs amendments;
       diff_periodic=amdmt_get_periodic sum_specs amendments;
      }

%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <float> SIGN
%token DDX_VOL DDX_BOUNDARY
%token DOF_REGION_AND
%token DOF_REGION_OR
%token DOF_REGION_NOT
%token LPAREN RPAREN
%token STAR SLASH
%token LANGLE RANGLE
%token GE LE
%token VBAR COMMA COLON SEMICOLON MXDIM EQUALS LBRACKET RBRACKET HASH
%token DOF_REGION_SOME DOF_REGION_ALL
%token GAUGEFIX PERIODIC
%token EOF

/* Note: an ocamlyacc grammar may have multiple entry points,
   but we have to define %start and %type for every single one of them.
*/

%start parse_ddiffop  
%type <ddiffop> parse_ddiffop

/* Another entry point... */
%start short_vector_restriction
%type <(string * dof_logic) array option> short_vector_restriction

/* Another entry point: Region logic */
%start region_logic
%type <dof_logic> region_logic

%%

parse_ddiffop:
  | contribs opt_amendment_specs opt_sum_specs { build_ddiffop $1 $3 $2 }
;

contribs:
  | contrib                     { [$1] }
  | contribs SIGN contrib       { let (c1, t2) = $3 in ($2*.c1, t2)::$1 }
;

contrib:
   unsigned_contrib             { $1 }
 | SIGN contrib                 { let (c, t) = $2 in ($1*.c, t) }
;

unsigned_contrib:
   bracket                      { (1.0,$1) }
 | expr bracket                 { ($1,$2) }
 | expr STAR bracket           { ($1,$3) }
;

expr:
   unsigned_scalar              { $1 }
 | LPAREN signed_scalar RPAREN  { $2 }
 | LPAREN expr RPAREN           { $2 }
;

signed_scalar:
   SIGN unsigned_scalar         { $1*.$2 }
 | SIGN signed_scalar           { $1*.$2 }
;

unsigned_scalar:
   INT                          { float_of_int $1 }
 | FLOAT                        { $1 }
;

bracket:
   LANGLE opt_diff_field VBAR VBAR opt_diff_field RANGLE { ($2,$5,None) }
 | LANGLE opt_diff_field VBAR field VBAR opt_diff_field RANGLE { ($2,$6,Some $4) }
;
     

opt_diff_field:
    field         { ($1,PDIFF_none) }
  | diff field    { ($2,$1) }
;

diff:
    DDX_VOL     diff_index     { PDIFF_vol $2 }
  | DDX_BOUNDARY diff_index     { PDIFF_boundary $2 }
;

diff_index:
    INT                    { IX_int $1 }
  | STRING                 { IX_name $1 }

field:
    field_name opt_bspec     { {fnsi_name=$1;fnsi_indices=[||];fnsi_bspec=$2;} }
  | field_name opt_bspec LPAREN field_indices RPAREN { {fnsi_name=$1;fnsi_indices=Array.of_list $4;fnsi_bspec=$2;} }
;

opt_bspec:
  LBRACKET region_logic RBRACKET                   {$2}
  |                                                { DLOG_true }

region_logic_atomic:
  | LPAREN region_logic RPAREN {$2}
  | INT                    {DLOG_some (string_of_int $1)}
  | STRING                 {DLOG_some $1}
  | DOF_REGION_ALL EQUALS INT   {DLOG_all (string_of_int $3)}
  | DOF_REGION_SOME EQUALS INT  {DLOG_some (string_of_int $3)}
  | DOF_REGION_ALL EQUALS STRING   {DLOG_all $3}
  | DOF_REGION_SOME EQUALS STRING  {DLOG_some $3}
  | HASH EQUALS INT    {DLOG_nregions $3}

region_logic_opt_not:
  | DOF_REGION_NOT region_logic_atomic {DLOG_not $2}
  | region_logic_atomic                {$1}

region_logic_and:
  | region_logic_opt_not DOF_REGION_AND region_logic_and {DLOG_and [$1;$3]}
  | region_logic_opt_not {$1}

region_logic_or:
  | region_logic_and DOF_REGION_OR region_logic_or {DLOG_or [$1;$3]}
  | region_logic_and {$1}


region_logic:
  | region_logic_or     {$1}


field_name:
  STRING            { $1 }
;

field_indices:
    field_index     { [$1] }
  | field_index COMMA field_indices { $1::$3 }
;

field_index:
  | INT             { IX_int $1 }
  | STRING          { IX_name $1 }
;

opt_sum_specs:
  |                            { [] }
  | COMMA sum_specs            { $2 }

sum_specs:
  | sum_spec                   { [$1] }
  | sum_specs COMMA sum_spec   { $3::$1 }

sum_spec:
  STRING COLON INT             { ($1,$3) }

opt_amendment_specs:
  |                            { [] }
  | SEMICOLON amendment_spec opt_amendment_specs     { $2::$3 }

amendment_spec:
  | amdmt_mxdim             { $1 }
  | amdmt_diagonal_ones     { $1 }
  | amdmt_gauge_fix         { $1 }
  | amdmt_periodic          { $1 }

amdmt_mxdim:
  | MXDIM EQUALS LPAREN opt_fields VBAR VBAR opt_fields RPAREN { AMDMT_MXDIM ($4,$7) }

amdmt_diagonal_ones:
  | field EQUALS field  { AMDMT_DIAGONAL_ONES ($1,$3) }

amdmt_gauge_fix:
  | GAUGEFIX field { AMDMT_GAUGE_FIX $2 }

amdmt_periodic:
  | PERIODIC field { AMDMT_PERIODIC $2 }


opt_fields:
  | STAR                       { None }
  | fields                     { Some $1 }

fields:
  | field                        { [$1] }
  | field COMMA fields           { $1::$3 }

short_vector_restriction:
  |  opt_fields opt_sum_specs    { match $1 with
				     | None -> None
				     | Some fields ->
					 let v_fields = expand_fields $2 fields in
					   (* XXX NOTE: this actually is somewhat broken: expanding the
					      field indices wrt sum specs will not give us what we expected
					      in the next step, when we again throw away all indices:
					   *)
					   Some (Array.map (fun field -> (field.f_name, field.f_bspec)) v_fields)
				 }

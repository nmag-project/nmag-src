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
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <float> SIGN
%token LPAREN RPAREN LBRACKET RBRACKET
%token STAR SLASH
%token COMMA COLON SEMICOLON
%token LOCAL RANGE ASSIGN
%token EOF

/* Note: an ocamlyacc grammar may have multiple entry points,
   but we have to define %start and %type for every single one of them.
*/

%start parse_localeqn
%type <local_eqn> parse_localeqn

%%

parse_localeqn:
  | local_and_range_definitions assignments {let (ld,rd) = $1 in (Array.of_list ld,Array.of_list rd,Array.of_list $2)}

local_and_range_definitions:
  |                                         {([],[])}
  | LOCAL num_tensors SEMICOLON local_and_range_definitions {let (ld,rd)=$4 in (List.append $2 ld,rd)}
  | RANGE ix_ranges SEMICOLON local_and_range_definitions {let (ld,rd)=$4 in (ld,List.append $2 rd)}

/* Tensors with number-only indices */

num_tensors:
  |             {[]}
  | num_tensor  {[$1]}
  | num_tensor COMMA num_tensors {$1::$3}

num_tensor:
  | STRING { ($1,[||]) }
  | STRING LPAREN ints RPAREN {($1,Array.of_list $3)}

ints:
  | {[]}
  | INT {[$1]}
  | INT COMMA ints {$1::$3}

ix_ranges:
  |            {[]}
  | STRING COLON INT {[($1,$3)]}
  | STRING COLON INT COMMA ix_ranges {($1,$3)::$5}

assignments:
  | {[]}
  | lvalue ASSIGN tensor_term SEMICOLON assignments {($1,$3)::$5}

lvalue:
  | var_tensor  {$1}
  | STRING      {($1,[||])}

var_tensor:
  | STRING {($1,[||])}
  | STRING LPAREN indices_or_vars RPAREN {($1,Array.of_list $3)}

indices_or_vars:
  | {[]}
  | index_or_var {[$1]}
  | index_or_var COMMA indices_or_vars {$1::$3}

index_or_var:
  | INT  {IX_int $1}
  | STRING  {IX_var $1}

tensor_term:
  | LPAREN tensor_term RPAREN {$2}
  | tensor_sum {$1}

tensor_sum:
  | tensor_product {Tensor_sum [$1]}
  | tensor_product sign_tensor_sum {let Tensor_sum x = $2 in Tensor_sum ($1::x)}
  | sign_tensor_sum {$1}

sign_tensor_sum:
  | {Tensor_sum []}
  | SIGN tensor_product sign_tensor_sum {if $1 = 1.0
					 then
					   let Tensor_sum s = $3 in Tensor_sum ($2::s)
					 else
					   Tensor_product [(Tensor_float (-1.0));
							   let Tensor_sum s = $3 in Tensor_sum ($2::s)]
					}

tensor_product:
  | tensor_factor  {Tensor_product [$1]}
  | tensor_factor STAR tensor_product {let Tensor_product x = $3 in Tensor_product ($1::x)}

/* XXX TODO: change function and tensor argument parentheses: tensors use [] indexing, functions use ()! */
tensor_factor:
  | FLOAT          {Tensor_float $1}
  | var_tensor     {Tensor_varindexed $1}
  | STRING LBRACKET tensor_term RBRACKET {Tensor_func ($1,$3)} 
  | LPAREN tensor_term RPAREN {$2}

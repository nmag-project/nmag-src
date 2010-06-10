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

let new_tensor_product ?(extra_sign=1.0) sign_list_pair =
  let s1, fl = sign_list_pair in
  let s = s1*.extra_sign in
    Tensor_product
      (if s == 1.0
       then fl
       else (Tensor_float s)::fl)
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
    local_and_range_definitions assignments {let (ld,rd) = $1 in (Array.of_list ld,Array.of_list rd,Array.of_list $2)}

local_and_range_definitions:
                                         {([],[])}
  | LOCAL num_tensors SEMICOLON local_and_range_definitions
                                         {let (ld, rd) = $4 in
                                            (List.append $2 ld, rd)}
  | RANGE ix_ranges SEMICOLON local_and_range_definitions
                                         {let (ld, rd) = $4 in
                                            (ld,List.append $2 rd)}

/* Tensors with number-only indices */

num_tensors:
                                         {[]}
  | num_tensor                           {[$1]}
  | num_tensor COMMA num_tensors         {$1::$3}

num_tensor:
    STRING                               { ($1,[||]) }
  | STRING LPAREN ints RPAREN            {($1,Array.of_list $3)}

ints:
                                         {[]}
  | INT                                  {[$1]}
  | INT COMMA ints                       {$1::$3}

ix_ranges:
  |                                      {[]}
  | STRING COLON INT                     {[($1,$3)]}
  | STRING COLON INT COMMA ix_ranges     {($1,$3)::$5}

assignments:
  |                                      {[]}
  | lvalue ASSIGN tensor_term SEMICOLON assignments
                                         {($1,$3)::$5}

lvalue:
    var_tensor                           {$1}

var_tensor:
    STRING opt_indices_or_vars           {($1, $2)}

opt_indices_or_vars:
                                         {[||]}
  | LPAREN indices_or_vars RPAREN        {Array.of_list $2}

indices_or_vars:
                                         {[]}
  | index_or_var                         {[$1]}
  | index_or_var COMMA indices_or_vars   {$1::$3}

index_or_var:
    INT                                  {IX_int $1}
  | STRING                               {IX_var $1}

tensor_term:
    tensor_sum                           {let Tensor_sum pl = $1
                                          in Tensor_sum (List.rev pl)}
tensor_sum:
    tensor_product                       {Tensor_sum [new_tensor_product $1]}
  | tensor_sum SIGN tensor_product       {let p = new_tensor_product
                                                    ~extra_sign:$2 $3 in
                                          let Tensor_sum pl = $1 in
                                            Tensor_sum (p::pl)}

/* signed_tensor_product --> (sign; [list of factors]) */
tensor_product:
    signed_tensor_factor                     {let s, f = $1 in (s, [f])}
  | signed_tensor_factor STAR tensor_product {let s1, f = $1 in
                                              let s2, fl = $3 in
                                                (s1*.s2, f::fl)}

/* The previous grammar was heavily broken. Here we fix it by multiplying
   all the signs together and returning a tuple (sign, factor).
   mf, 10 Jun 2010.
 */
signed_tensor_factor:
    tensor_factor                            {(1.0, $1)}
  | SIGN signed_tensor_factor                {let sign, tens_fact = $2
                                              in (sign*.$1, tens_fact)}

/* XXX TODO: change function and tensor argument parentheses: tensors use []
   indexing, functions use ()!
 */
tensor_factor:
    FLOAT                                    {Tensor_float $1}
  | var_tensor                               {Tensor_varindexed $1}
  | STRING LBRACKET tensor_term RBRACKET     {Tensor_func ($1, $3)}
  | LPAREN tensor_term RPAREN                {$2}

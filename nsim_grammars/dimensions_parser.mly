%{
%}

%token <int> INT
%token <string> STRING
%token LPAREN RPAREN 
%token SLASH HAT
%token EOF

/* Note: an ocamlyacc grammar may have multiple entry points,
   but we have to define %start and %type for every single one of them.
*/

%start parse_dimension
%type <physical_dimension> parse_dimension

%%

parse_dimension:
  | dimensions {Array.of_list $1}
  | dimensions SLASH dimensions  {Array.of_list (List.append $1 (List.map (fun (n,num,den) -> (n,-num,den)) $3))}

dimensions:
  | {[]}
  | INT dimensions {if $1 <> 1 then failwith "Bad integer in dimension specification (should be 1)" else $2}
  | dimension dimensions {$1::$2}

dimension:
  | STRING {($1,1,1)}
  | STRING HAT power {let (n,d)=$3 in ($1,n,d)}

power:
  | LPAREN power RPAREN {$2}
  | INT  {($1,1)}
  | INT SLASH INT {($1,$3)}

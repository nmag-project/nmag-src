%{

type parser_index = PIX_int of int | PIX_name of string 

type parser_field_name = string * parser_index array

type parser_diff_field_spec = parser_field_name * parser_index option

type parser_diffop =
    ((float * (parser_diff_field_spec*parser_diff_field_spec*parser_field_name option)) list) * (string * int) list

%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <float> SIGN
%token DDX
%token LPAREN RPAREN
%token TIMES
%token LANGLE RANGLE
%token VBAR COMMA COLON
%token EOF
%start main             /* the entry point */
%type <parser_diffop> main

%%

main:
  | contribs                    { ($1,[]) }
  | contribs COMMA sum_specs    { ($1,$3) }
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
 | expr TIMES bracket           { ($1,$3) }
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
    field         { ($1,None) }
  | diff field    { ($2, Some $1) }
;

diff:
    DDX INT         { PIX_int $2 }
  | DDX STRING      { PIX_name $2 }
;

field:
    field_name      { ($1,[||]) }
  | field_name LPAREN field_indices RPAREN { ($1,Array.of_list $3) }
;

field_name:
  STRING            { $1 }
;

field_indices:
    field_index     { [$1] }
  | field_index COMMA field_indices { $1::$3 }
;

field_index:
  | INT             { PIX_int $1 }
  | STRING          { PIX_name $1 }
;

sum_specs:
  | sum_spec                   { [$1] }
  | sum_specs COMMA sum_spec   { $3::$1 }

sum_spec:
  STRING COLON INT             { ($1,$3) }

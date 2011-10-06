%{
  open Ddiffop;;
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
%type <Ddiffop.ddiffop> parse_ddiffop

/* Another entry point... */
%start short_vector_restriction
%type <(string * Ddiffop.dof_logic) array option> short_vector_restriction

/* Another entry point: Region logic */
%start region_logic
%type <Ddiffop.dof_logic> region_logic

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

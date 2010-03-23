{
open Diffop_parser   (* The type token is defined in parser.mli *)
}

  rule token = parse
  [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['0'-'9']* '.' ['0'-'9']+ (['e' 'E'] ['0'-'9']+)? as lxm { FLOAT(float_of_string lxm) }
| ['0'-'9']+ ['e' 'E'] ['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {STRING(lxm)} (* Names MAY start with underscores! *)
| "d/dx"         { DDX }

| '('            { LPAREN }
| ')'            { RPAREN }

| '+'            { SIGN(1.0) }
| '-'            { SIGN(-.1.0) }
| '*'            { TIMES }

| '<'            { LANGLE }
| '>'            { RANGLE }
| '|'            { VBAR }
| ','            { COMMA }
| ':'            { COLON }

| eof            { EOF }

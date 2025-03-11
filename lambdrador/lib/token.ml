type token_type =
  | LD
  | TAG
  | WHEN
  | MATCH
  | WITH
  | CASE
  | CONS
  | BOOL
  | LESS
  | GREATER
  | LESSEQ
  | GREATEREQ
  | NEQ
  | IF
  | THEN
  | ELSE
  | OPAR
  | CPAR
  | ASS
  | ADD
  | MULT
  | SUB
  | EQ
  | DELIM
  | EOF
  | PRINT
  | PRINTLN
  | NAMESPACE
  | STRING
  | INTEGER
  | FLOAT
  | NUMBER
  | OSQP
  | CSQP
[@@deriving show]

type token = {tliteral : string; ttype : token_type}

let print_token t : unit = 
  let open Format in
  print_string (t.tliteral ^ "\n");
  print_endline (show_token_type t.ttype);

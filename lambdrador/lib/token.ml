type token_type =
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
  | NAMESPACE
  | STRING
  | INTEGER
  | FLOAT
  | NUMBER
[@@deriving show]

type token = {tliteral : string; ttype : token_type}

let print_token t : unit = 
  let open Format in
  print_string (t.tliteral ^ "\n");
  print_endline (show_token_type t.ttype);

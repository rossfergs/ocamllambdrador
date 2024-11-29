type token_type =
    OPAR
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
type token = { tliteral : string; ttype : token_type; }
val print_token : token -> unit

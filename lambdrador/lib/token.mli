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
type token = { tliteral : string; ttype : token_type; }
val print_token : token -> unit

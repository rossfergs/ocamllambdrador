type token_type =
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
type token = { tliteral : string; ttype : token_type; }
val print_token : token -> unit

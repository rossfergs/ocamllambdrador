type token_type =
    OPAR
  | CPAR
  | ASS
  | FOR
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

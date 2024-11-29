val skip_whitespace : string -> int -> int
val collect :
  string ->
  int ->
  Token.token_type ->
  (char -> bool) -> (char -> bool) -> literal:string -> Token.token * int
val collect_number : string -> int -> Token.token * int
val collect_string : string -> int -> char -> Token.token * int
val lex : string -> int -> Token.token * int

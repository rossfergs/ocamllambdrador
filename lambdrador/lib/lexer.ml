
(* 
  finds the index of the next non whitespace character or stops at EOF 
*)
let rec skip_whitespace input_string idx: int = 
  if idx < String.length input_string then
    match String.get input_string idx with
      | ' ' | '\n' -> skip_whitespace input_string (idx+1)
      | _ -> idx
  else idx

(* 
  collect - collects a multicharacter token through tail recursion stuff
*)
let rec collect input_string idx token_type char_check end_char ~literal:literal 
    : (Token.token * int) =
  if idx >= String.length input_string || end_char (String.get input_string idx)
    then (Token.{tliteral = literal; ttype = token_type}, idx)
  else
    let ch = String.get input_string idx in
    if char_check ch
      then 
        collect 
          input_string 
          (idx+1) 
          token_type 
          char_check 
          end_char 
          ~literal:(literal ^ (String.make 1 ch))
    else
      let open Error in
      let error_msg = match token_type with
        | Token.NUMBER -> "non-numeric character in number literal '" ^ (String.make 1 ch) ^ "'"
        | Token.STRING -> "forbidden character in string '" ^ (String.make 1 ch) ^ "'"
        | Token.NAMESPACE -> "forbidden character in namespace '" ^ (String.make 1 ch) ^ "'"
        | _ -> "forbidden character in multi-character string '" ^ (String.make 1 ch) ^ "'"
      in
        raise (Lexer_Error error_msg)


let collect_operator input_string idx : (Token.token * int) =
  let open Token in
  let check_char = function
    | '+' | '-' | '=' | '<' | '>' | '*' | '/' | ':' | '!' -> true
    | _ -> false
  in
  let check_end ch = not (check_char ch)
  in
  let tok, next_idx = collect
    input_string
    idx
    EQ
    check_char
    check_end
    ~literal: ""
  in
  let token_class = match tok.tliteral with
  | "/" -> DIV
  | "=" -> EQ
  | "*" -> MULT
  | "-" -> SUB
  | "+" -> ADD
  | "<" -> LESS
  | "<=" -> LESSEQ
  | ">" -> GREATER
  | ">=" -> GREATEREQ
  | "!=" -> NEQ
  | "::" -> CONS
  | _ -> raise (Error.Lexer_Error ("Unrecognised operator '" ^ tok.tliteral ^ "'"))
  in
  {tliteral = tok.tliteral; ttype = token_class}, next_idx


let collect_number input_string idx : (Token.token * int) =
  let open Token in
  let check_end = function
    | ' ' | ']' | ')' | ';' | '/' | '+' | '=' | '*' | '-' | ',' | '\n' -> true
    | _ -> false in
  let t, i = collect 
    input_string
    idx
    NUMBER
    (fun c -> ('0' <= c && c <= '9') || c == '.')
    check_end
    ~literal:""
  in
  if String.contains t.tliteral '.' then
    {tliteral = t.tliteral; ttype = FLOAT}, i
  else
    {tliteral = t.tliteral; ttype = INTEGER}, i


let collect_string input_string idx qm : (Token.token * int) =
  collect
    input_string
    (idx+1)
    Token.STRING
    (fun _ -> true)
    (fun c -> c == qm)
    ~literal:""

(* 
  TODO: update this to the current python implementation,
  currently only returns namespace tokens
*)
let collect_keyword_or_namespace input_string idx : (Token.token * int) =
  let valid_character = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '`' -> true
    | _ -> false 
  in
  let check_end = function
    | ' ' | '\n' | ')' | ';' | '+' | '=' | '*' | '-' | '[' | ']' -> true
    | _ -> false 
  in
  let result, next_idx = collect
    input_string
    idx
    Token.NAMESPACE
    valid_character
    check_end
    ~literal:"" in
  match result.tliteral with
  | "let" -> {tliteral = result.tliteral; ttype = Token.ASS}, next_idx
  | "print" -> {tliteral = result.tliteral; ttype = Token.PRINT}, next_idx
  | "println" -> {tliteral = result.tliteral; ttype = Token.PRINTLN}, next_idx
  | "ld" -> {tliteral = result.tliteral; ttype = Token.LD}, next_idx
  | "if" -> {tliteral = result.tliteral; ttype = Token.IF}, next_idx
  | "then" -> {tliteral = result.tliteral; ttype = Token.THEN}, next_idx
  | "else" -> {tliteral = result.tliteral; ttype = Token.ELSE}, next_idx
  | "true" -> {tliteral = result.tliteral; ttype = Token.BOOL}, next_idx
  | "false" -> {tliteral = result.tliteral; ttype = Token.BOOL}, next_idx
  | "match" -> {tliteral = result.tliteral; ttype = Token.MATCH}, next_idx
  | "case" -> {tliteral = result.tliteral; ttype = Token.CASE}, next_idx
  | "when" -> {tliteral = result.tliteral; ttype = Token.WHEN}, next_idx
  | "rec" -> {tliteral = result.tliteral; ttype = Token.REC}, next_idx
  | "import" -> {tliteral = result.tliteral; ttype = Token.IMPORT}, next_idx
  | "with" -> {tliteral = result.tliteral; ttype = Token.WITH}, next_idx
  | literal when String.get literal 0 = '`' -> {tliteral = literal; ttype = TAG}, next_idx
  | _ -> result, next_idx

(* 
  lexer function, takes in the input code as a string and an integer to
  get the token from, outputs the token and the next index to search from
*)
let rec lex input_string idx: (Token.token * int) =
  let open Token in
  let next_idx = skip_whitespace input_string idx in
  if next_idx >= String.length input_string 
    then {tliteral = "EOF"; ttype = EOF},(String.length input_string)-1 
  else
  let ch = String.get input_string next_idx in
  let str_ch = String.make 1 ch in
  match ch with
  | '@' -> skip_comment input_string (next_idx+1)
  | '0'..'9' -> collect_number input_string next_idx
  | '\'' | '"' -> collect_string input_string next_idx ch
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '`' -> collect_keyword_or_namespace input_string next_idx
  | '(' -> ({tliteral = str_ch; ttype = OPAR}, next_idx + 1)
  | ')' -> ({tliteral = str_ch; ttype = CPAR}, next_idx + 1)
  | '/' | '*' | '+' | '-' | '=' | '<' | '!' | '>' | ':' -> collect_operator input_string next_idx
  | ';' -> ({tliteral = str_ch; ttype = DELIM}, next_idx + 1)
  | '[' -> ({tliteral = str_ch; ttype = OSQP}, next_idx + 1)
  | ']' -> ({tliteral = str_ch; ttype = CSQP}, next_idx + 1)
  | _ -> raise (Error.Lexer_Error (String.cat "unrecognised character " str_ch))
  
and skip_comment input_string idx =
  let open Token in
  if idx >= String.length input_string then {tliteral = "EOF"; ttype = EOF}, idx
  else 
    match input_string.[idx] with
    | '@' -> lex input_string (idx+1)
    | _ -> skip_comment input_string (idx+1)

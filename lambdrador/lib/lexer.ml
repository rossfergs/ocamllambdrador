
(* 
  finds the index of the next non whitespace character or stops at EOF 
*)
let rec skip_whitespace input_string idx: int = 
  if idx < String.length input_string then
  match String.get input_string idx with
  | ' ' -> skip_whitespace input_string (idx+1)
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
      match token_type with
      | Token.NUMBER -> raise (LexerError "non-numeric character in number literal")
      | Token.STRING -> raise (LexerError "forbidden character in string")
      | Token.NAMESPACE -> raise (LexerError "forbidden character in namespace")
      | _ -> 
        (* TODO: THIS NEEDS CHANGED TO WORK FOR ALL ERROR MESSAGES *)
        let msg = 
          "forbidden character in multi-character string" 
          ^ (String.make 1 input_string.[idx]) in
        raise (LexerError msg)

 
let collect_number input_string idx : (Token.token * int) =
  let open Token in
  let check_end c = match c with
  | ' ' | ')' | ';' | '+' | '=' | '*' | '-' -> true
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
  let valid_character c = match c with
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false in
  let check_end c = match c with
  | ' ' | ')' | ';' | '+' | '=' | '*' | '-' -> true
  | _ -> false in
  let result, next_idx = collect
    input_string
    idx
    Token.NAMESPACE
    valid_character
    check_end
    ~literal:"" in
  match result.tliteral with
  | "let" -> {tliteral = result.tliteral; ttype = Token.ASS}, next_idx
  | "print" -> {tliteral = result.tliteral; ttype = Token.ASS}, next_idx
  | _ -> result, next_idx

(* 
  lexer function, takes in the input code as a string and an integer to
  get the token from, outputs the token and the next index to search from
*)
let lex input_string idx: (Token.token * int) =
  let open Token in
  let next_idx = skip_whitespace input_string idx in
  if next_idx >= String.length input_string 
    then {tliteral = "EOF"; ttype = EOF},(String.length input_string)-1 
  else
  let ch = String.get input_string next_idx in
  let str_ch = String.make 1 ch in
  match ch with
  | '0'..'9' -> collect_number input_string next_idx
  | '\'' | '"' -> collect_string input_string next_idx ch
  | 'a' .. 'z' | 'A' .. 'Z' -> collect_keyword_or_namespace input_string next_idx
  | '(' -> ({tliteral = str_ch; ttype = OPAR}, next_idx + 1)
  | ')' -> ({tliteral = str_ch; ttype = CPAR}, next_idx + 1)
  | '*' -> ({tliteral = str_ch; ttype = MULT}, next_idx + 1)
  | '+' -> ({tliteral = str_ch; ttype = ADD}, next_idx + 1)
  | '-' -> ({tliteral = str_ch; ttype = SUB}, next_idx + 1)
  | ';' -> ({tliteral = str_ch; ttype = DELIM}, next_idx + 1)
  | '\n' -> ({tliteral = str_ch; ttype = EOF}, next_idx + 1)
  | '=' -> ({tliteral = str_ch; ttype = EQ}, next_idx + 1)
  | _ -> raise (Error.LexerError (String.cat "unrecognised character " str_ch))
  
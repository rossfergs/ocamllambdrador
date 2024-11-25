
(* 
  finds the index of the next non whitespace character or stops at EOF 
*)
let rec skip_whitespace input_string idx: int = 
  match (String.get input_string idx, idx >= (String.length input_string)) with
  | ' ', false -> skip_whitespace input_string (idx+1)
  | _, _ -> idx

(* 
  collect - collects a multicharacter token through tail recursion stuff
*)
let rec collect input_string idx token_type char_check end_char ~literal:literal : (Token.token * int) =
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
      | _ -> raise (LexerError "forbidden character in multi-character string")

 
let collect_number input_string idx : (Token.token * int) =
  let check_end c = match c with
  | ' ' | ')' | ';' | '+' | '=' | '*' | '-' -> true
  | _ -> false in
  collect 
    input_string
    idx
    Token.NUMBER
    (fun c -> ('0' <= c && c <= '9') || c == '.')
    check_end
    ~literal:""


let collect_string input_string idx qm : (Token.token * int) =
  collect
    input_string
    idx
    Token.STRING
    (fun _ -> true)
    (fun c -> c == qm)
    ~literal:""

(* 
  TODO: update this to the current python implementation,
  currently only returns namespace tokens
*)
let collect_keyword input_string idx : (Token.token * int) =
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
  result, next_idx

(* 
  lexer function, takes in the input code as a string and an integer to
  get the token from, outputs the token and the next index to search from
*)
let lex input_string idx: (Token.token * int) =
  let next_idx = skip_whitespace input_string idx in
  match (idx >= String.length input_string, String.get input_string idx) with
  | true, _ -> {Token.tliteral = "EOF"; Token.ttype = Token.EOF}, idx
  | false, ch -> 
    let str_ch = String.make 1 ch in
    match ch with
    | '0'..'9' -> collect_number input_string next_idx
    | '\'' | '"' -> collect_string input_string next_idx ch
    | 'a' .. 'z' | 'A' .. 'Z' -> collect_keyword input_string idx
    | '(' -> (Token.{tliteral = str_ch; ttype = Token.OPAR}, next_idx + 1)
    | ')' -> (Token.{tliteral = str_ch; ttype = Token.CPAR}, next_idx + 1)
    | '*' -> (Token.{tliteral = str_ch; ttype = Token.MULT}, next_idx + 1)
    | '+' -> (Token.{tliteral = str_ch; ttype = Token.ADD}, next_idx + 1)
    | '-' -> (Token.{tliteral = str_ch; ttype = Token.SUB}, next_idx + 1)
    | ';' -> (Token.{tliteral = str_ch; ttype = Token.DELIM}, next_idx + 1)
    | '\n' -> (Token.{tliteral = str_ch; ttype = Token.EOF}, next_idx + 1)
    | '=' -> (Token.{tliteral = str_ch; ttype = Token.EQ}, next_idx + 1)
    | _ -> raise (Error.LexerError (String.cat "unrecognised character " str_ch))
  
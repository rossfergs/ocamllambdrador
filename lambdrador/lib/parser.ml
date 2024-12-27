

let check_delim input_string idx = 
  let next_token, next_idx = Lexer.lex input_string idx in
  match next_token.ttype with
    | Token.DELIM -> true, next_idx | _ -> false, idx 


let rec parse input_string : Parse_node.statement_node list =
  parse_program input_string 0 ~statement_list:[]


and parse_program input_string idx ~statement_list:statement_list : Parse_node.statement_node list = 
  let current_token, _ = Lexer.lex input_string idx in
  match current_token.ttype with 
    | Token.EOF -> List.rev statement_list
    | _ ->
      let statement, next_idx = parse_statement input_string idx in
      parse_program input_string next_idx ~statement_list:(statement :: statement_list)


and parse_parameters input_string idx ~parameter_list:parameter_list : string list * int =
  let current_token, current_idx = Lexer.lex input_string idx in
  match current_token.ttype with
    | Token.NAMESPACE ->
      parse_parameters input_string current_idx ~parameter_list:(current_token.tliteral :: parameter_list)
    | Token.EQ ->
      List.rev parameter_list, current_idx
    | _ ->
      let msg = "invalid function parameter" ^ current_token.tliteral in
      raise (Error.Parser_Error msg)


and parse_if input_string idx = 
  let open Parse_node in
  let expr, expr_idx = Pratt_parser.parse_expression input_string idx in
  let then_tok, then_tok_idx = Lexer.lex input_string expr_idx in
  if then_tok.ttype != THEN then raise (Error.Parser_Error "'then' required in if statement")
  else

  let then_block, then_idx = parse_block input_string then_tok_idx ~input_params:[] ~statement_list:[] in
  let else_tok, else_tok_idx = Lexer.lex input_string then_idx in
  if else_tok.ttype != ELSE then raise (Error.Parser_Error "'else' required in if statement")
  else

  let else_block, else_idx = parse_block input_string else_tok_idx ~input_params:[] ~statement_list:[] in
  If_node {condition = expr; then_block = then_block; else_block = else_block}, else_idx


(*
  helper function for parsing expression to deal with 'if' expressions
*)
and parse_expression input_string idx = 
  let tok, tok_idx = Lexer.lex input_string idx in

  if tok.ttype != IF then Pratt_parser.parse_expression input_string idx 
  else parse_if input_string tok_idx


and parse_block input_string idx ~input_params:input_params ~statement_list:statement_list 
  : Parse_node.block_node * int = 
let next_statement, next_idx = parse_statement input_string idx in
match next_statement with
  | Expr e -> 
    {parameters = input_params; statements = List.rev statement_list; expression = e}, next_idx
  | _ -> 
    parse_block input_string 
      next_idx 
      ~input_params:input_params 
      ~statement_list:(next_statement :: statement_list)


and parse_print input_string idx : Parse_node.statement_node * int = 
let expr, current_idx = Pratt_parser.parse_expression input_string idx in
let delim_present, end_idx = check_delim input_string current_idx in
if not delim_present then
  raise (Error.Parser_Error "Delim required after print")
else
  Print_Node expr, end_idx


and parse_assignment input_string idx : Parse_node.statement_node * int = 
let current_token, current_idx = Lexer.lex input_string idx in
match current_token.ttype with
  | Token.NAMESPACE ->
    let params, params_idx = parse_parameters input_string current_idx ~parameter_list:[] in
    let new_block, block_idx =
      parse_block input_string params_idx ~input_params:params ~statement_list:[] in
    let delim_present, end_idx = check_delim input_string block_idx in
    if delim_present then
      Let_Node {namespace = current_token.tliteral; block = new_block}, end_idx
    else
      raise (Error.Parser_Error "delimiter needed after statement")
  | _ -> raise (Error.Parser_Error ("invalid namespace after let: " ^ current_token.tliteral))


and parse_statement input_string idx : Parse_node.statement_node * int =
  let open Token in
  let current_token, current_idx = Lexer.lex input_string idx in
  match current_token.ttype with
    | ASS -> parse_assignment input_string current_idx
    | PRINT -> parse_print input_string current_idx
    | _ -> let e, expr_idx = parse_expression input_string idx in
      Expr e, expr_idx


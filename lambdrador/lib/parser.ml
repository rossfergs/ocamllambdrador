let parse input_string : Parse_node.statement_node list =

  let open Parse_node in

  let check_delim idx = 
    let next_token, next_idx = Lexer.lex input_string idx in
    match next_token.ttype with
    | Token.DELIM -> true, next_idx
    | _ -> false, idx 
  in

  let rec parse_program idx ~statement_list:statement_list : statement_node list = 
    let () = print_string "start" in
    let current_token, _ = Lexer.lex input_string idx in
    match current_token.ttype with 
    | Token.EOF -> statement_list
    | _ -> 
      let statement, next_idx = (parse_statement idx) in
      parse_program next_idx ~statement_list:(statement :: statement_list)

  and parse_parameters idx ~parameter_list:parameter_list : string list * int =
    let current_token, current_idx = Lexer.lex input_string idx in
    match current_token.ttype with
    | Token.NAMESPACE ->
      parse_parameters current_idx ~parameter_list:(current_token.tliteral :: parameter_list)
    | Token.EQ ->
      List.rev parameter_list, current_idx
    | _ ->
      let msg = "invalid function parameter" ^ current_token.tliteral in
      raise (Error.ParserError msg)

  and parse_block idx ~input_params:input_params ~statement_list:statement_list 
      : block_node * int = 
    let next_statement, next_idx = parse_statement idx in
    match next_statement with
    | Expr e -> 
      {parameters = input_params; statements = List.rev statement_list; expression = e}, next_idx
    | _ -> 
      parse_block 
        next_idx 
        ~input_params:input_params 
        ~statement_list:(next_statement :: statement_list)

  and parse_print idx : statement_node * int = 
    let expr, current_idx = Pratt_parser.parse_expression input_string idx in
    let delim_present, end_idx = check_delim current_idx in
    if not delim_present then
      raise (Error.ParserError "Delim required after print")
    else
      Print_Node expr, end_idx

  and parse_assignment idx : statement_node * int = 
    let current_token, current_idx = Lexer.lex input_string idx in
    match current_token.ttype with
    | Token.NAMESPACE -> 
      let params, params_idx = parse_parameters current_idx ~parameter_list:[] in
      let new_block, block_idx = 
        parse_block params_idx ~input_params:params ~statement_list:[] in
      let delim_present, end_idx = check_delim block_idx in
      if delim_present then
        Let_Node {namespace = current_token.tliteral; block = new_block}, end_idx
      else
        raise (Error.ParserError "delimiter needed after statement")
    | _ -> raise (Error.ParserError ("invalid namespace after let: " ^ current_token.tliteral))

  and parse_statement idx : statement_node * int = 
    let open Token in
    let current_token, current_idx = Lexer.lex input_string idx in
    match current_token.ttype with
    | ASS -> parse_assignment current_idx
    | PRINT -> parse_print current_idx
    | _ -> let e, expr_idx = Pratt_parser.parse_expression input_string idx in
      Expr e, expr_idx
  in

  parse_program 0 ~statement_list:[]
let _check_delim input_string idx = 
  let next_token, next_idx = Lexer.lex input_string idx in
  match next_token.ttype with
  | Token.DELIM -> true, next_idx | _ -> false, idx 


let get_lbp input_token =
  let open Token in
  match input_token.ttype with
  | EQ | NEQ  | LESS | LESSEQ | GREATER | GREATEREQ  -> 5
  | ADD | SUB | CONS -> 2
  | MULT | DIV -> 3
  | EOF | DELIM -> -1
  | _ -> 0


let rec parse input_string : Parse_node.statement_node list =
  parse_program input_string 0 ~statement_list:[]


and parse_program input_string idx ~statement_list:statement_list : Parse_node.statement_node list = 
  let current_token, _ = Lexer.lex input_string idx in
  match current_token.ttype with 
  | Token.EOF -> List.rev statement_list
  | _ ->
    let statement, next_idx = parse_statement input_string idx in
    parse_program input_string next_idx ~statement_list:(statement :: statement_list)


and parse_list input_string idx : Parse_node.expression_node * int =
  let open Parse_node in
  let open Token in
  let rec construct_list current_idx = 
    let tok, tok_idx = Lexer.lex input_string current_idx in
    if tok.ttype = CSQP then (Nil, tok_idx) else
      let expr, expr_idx = parse_expression input_string current_idx in
      let delim_tok, delim_idx = Lexer.lex input_string expr_idx in
      match delim_tok.ttype with
      | CSQP -> Node (expr, Nil), delim_idx
      | DELIM ->
          let remainder, end_idx = construct_list delim_idx in
          Node (expr, remainder), end_idx
      | _ -> raise (Error.Parser_Error "list expressions must be seperated by ; or ended by ]")
  in
  let l, end_idx = construct_list idx in
  List_Node l, end_idx


and parse_parameters input_string idx ~parameter_list:parameter_list : string list * int =
  let current_token, current_idx = Lexer.lex input_string idx in
  match current_token.ttype with
  | Token.NAMESPACE ->
    parse_parameters input_string current_idx ~parameter_list:(current_token.tliteral :: parameter_list)
  | Token.EQ ->
    List.rev parameter_list, current_idx
  | _ ->
    let msg = "invalid function parameter " ^ current_token.tliteral in
    raise (Error.Parser_Error msg)


and parse_match input_string idx : Parse_node.expression_node * int =
  let open Parse_node in
  let open Token in
  let rec parse_cases aux_idx case_list : expression_node list * int = 
    let tok, tok_idx = Lexer.lex input_string aux_idx in
    match tok.ttype with
    | CASE ->
      let pattern, pattern_idx = pratt_parse input_string 0 tok_idx in
      let guard_tok, guard_tok_idx = Lexer.lex input_string pattern_idx in
      (
        match guard_tok.ttype with
        | WHEN ->
          let guard, guard_idx = pratt_parse input_string 0 guard_tok_idx in
          let then_tok, then_idx = Lexer.lex input_string guard_idx in 
          if then_tok.ttype != THEN then raise (Error.Parser_Error "then rthenuired before block in match case")
          else
          let block, block_idx = 
            parse_block input_string then_idx ~input_params:[] ~statement_list:[] 
          in
          let new_case = Case_Node {pattern = pattern; guard = Some guard; block = block} in
          parse_cases block_idx (new_case :: case_list)
        | _ ->
          let then_tok, then_idx = Lexer.lex input_string pattern_idx in 
          if then_tok.ttype != THEN then raise (Error.Parser_Error "then rthenuired before block in match case")
          else
          let block, block_idx = 
            parse_block input_string then_idx ~input_params:[] ~statement_list:[] 
          in
          let new_case = Case_Node {pattern = pattern; guard = None; block = block} in
          parse_cases block_idx (new_case :: case_list)
      )
    | _ -> List.rev case_list, aux_idx
  in
  let expr, expr_idx = pratt_parse input_string 0 idx in
  let with_tok, with_idx = Lexer.lex input_string expr_idx in
  if with_tok.ttype != WITH then raise (Error.Parser_Error "with required after expression in match expression")
  else
    let cases, end_idx = parse_cases with_idx [] in
    Match_Node {expr = expr; cases = cases}, end_idx


and parse_anonymous_func input_string idx = 
  let params, params_idx = parse_parameters input_string idx ~parameter_list:[] in
  parse_block input_string params_idx ~input_params:params ~statement_list:[]


and parse_if input_string idx = 
  let open Parse_node in
  let expr, expr_idx = pratt_parse input_string 0 idx in
  let then_tok, then_tok_idx = Lexer.lex input_string expr_idx in
  if then_tok.ttype != THEN then raise (Error.Parser_Error "'then' required in if statement")
  else

  let then_block, then_idx = parse_block input_string then_tok_idx ~input_params:[] ~statement_list:[] in
  let else_tok, else_tok_idx = Lexer.lex input_string then_idx in
  if else_tok.ttype != ELSE then raise (Error.Parser_Error "'else' required in if statement")
  else

  let else_block, else_idx = parse_block input_string else_tok_idx ~input_params:[] ~statement_list:[] in
  If_Node {condition = expr; then_block = then_block; else_block = else_block}, else_idx


(*
  helper function for parsing expression to deal with 'if' expressions
*)
and parse_expression input_string idx = 
    pratt_parse input_string 0 idx


and parse_block input_string idx ~input_params:input_params ~statement_list:statement_list 
  : Parse_node.expression_node * int = 
  let next_statement, next_idx = parse_statement input_string idx in
  match next_statement with
  | Expr e -> 
      Block_Node {parameters = input_params; statements = List.rev statement_list; expression = e; closure_scope = None}, next_idx
  | _ -> 
    let delim_tok, delim_idx = Lexer.lex input_string next_idx in
    if delim_tok.ttype != DELIM 
    then raise (Error.Interpreter_Error "delim required after statement in a block")
    else parse_block input_string 
      delim_idx 
      ~input_params:input_params 
      ~statement_list:(next_statement :: statement_list)


and parse_print input_string idx : Parse_node.statement_node * int = 
  let expr, current_idx = pratt_parse input_string 0 idx in
  Print_Node expr, current_idx

and parse_println input_string idx : Parse_node.statement_node * int = 
  let expr, current_idx = pratt_parse input_string 0 idx in
  Println_Node expr, current_idx

and parse_assignment input_string idx : Parse_node.statement_node * int = 
  let current_token, current_idx = Lexer.lex input_string idx in
  match current_token.ttype with
  | Token.NAMESPACE ->
    let params, params_idx = parse_parameters input_string current_idx ~parameter_list:[] in
    let new_block, block_idx =
      parse_block input_string params_idx ~input_params:params ~statement_list:[] in
      Let_Node {namespace = current_token.tliteral; block = new_block}, block_idx
  | _ -> raise (Error.Parser_Error ("invalid namespace after let: " ^ current_token.tliteral))


and parse_statement input_string idx : Parse_node.statement_node * int =
  let open Token in
  let current_token, current_idx = Lexer.lex input_string idx in
  match current_token.ttype with
  | IMPORT ->
      let tok, tok_idx = Lexer.lex input_string current_idx in
      (match tok.ttype with
      | STRING -> Parse_node.Import_Node (tok.tliteral), tok_idx+1
      | _ -> raise (Error.Parser_Error "filename must be given as a string in import statement")
      )
  | ASS -> parse_assignment input_string current_idx
  | PRINT -> parse_print input_string current_idx
  | PRINTLN -> parse_println input_string current_idx
  | _ -> let e, expr_idx = parse_expression input_string idx in
      Expr e, expr_idx

(* 
  BELOW IS A PRATT PARSER, IDK HOW TO PUT THIS INTO ITS OWN MODULE
*)
and pratt_parse input_string lbp idx =
  let current_token, current_idx = Lexer.lex input_string idx in
  let first_node, next_idx = nud input_string current_token current_idx in
  collect_expression input_string first_node lbp next_idx


and collect_expression input_string left_node lbp start_idx : (Parse_node.expression_node * int) =
  let current_token, current_idx = Lexer.lex input_string start_idx in
  if get_lbp current_token <= lbp
    then left_node, start_idx
  else
    let expr, next_idx = led input_string left_node current_token current_idx in
    collect_expression input_string expr lbp next_idx


and nud input_string input_token start_idx : (Parse_node.expression_node * int) =
  let open Token in
  let open Parse_node in
  match input_token.ttype with
  | SUB ->
      let next_tok, next_idx = Lexer.lex input_string start_idx in
      let node, end_idx = nud input_string next_tok next_idx in
      Negative_Node node, end_idx
  | LD -> 
      parse_anonymous_func input_string start_idx
  | BOOL ->
      let node = match input_token.tliteral with
      | "true" -> Bool_Node true
      | "false" -> Bool_Node false
      | _ -> 
        raise (Error.Parser_Error ("invalid string " ^ input_token.tliteral ^ " for bool"))
      in
      node, start_idx
  | NAMESPACE ->
      if input_token.tliteral = "_" then Variable_Node {namespace = "_"; parameters = []}, start_idx else
      let collected_params, next_idx = collect_parameters input_string start_idx ~parameter_list:[] in
      Variable_Node {namespace = input_token.tliteral; parameters = collected_params}, next_idx
  | INTEGER ->
      Integer_Node (int_of_string input_token.tliteral), start_idx
  | FLOAT ->
      Float_Node (float_of_string input_token.tliteral), start_idx
  | STRING ->
      (* Add one to the returned index to account for the end quote *)
      String_Node input_token.tliteral, start_idx+1
  | IF ->
      parse_if input_string start_idx
  | MATCH ->
      parse_match input_string start_idx
  | OSQP ->
      parse_list input_string start_idx
  | OPAR ->
      (* get the expression within the parenthesis *)
      let paren_result, paren_idx = pratt_parse input_string 0 start_idx in
      let next_token, next_idx = Lexer.lex input_string paren_idx in
      (* if the next token isnt a closing parenthesis, ")",  throw an error*)
      if next_token.ttype != Token.CPAR then
        raise (Error.Parser_Error "Unclosed parenthesis")
      else
        paren_result, next_idx
  | TAG ->
      let next_token, _ = Lexer.lex input_string start_idx in
      (match next_token.ttype with
      | OPAR | OSQP | STRING | FLOAT | INTEGER | IF | MATCH | NAMESPACE | BOOL ->
          let tagged_data, tag_idx = pratt_parse input_string 0 start_idx in
          Tagged_Node {tag = input_token.tliteral; data = Some tagged_data}, tag_idx
      | _ -> Tagged_Node {tag = input_token.tliteral; data = None}, start_idx
      )
  | _ -> raise (Error.Parser_Error ("unknown token in expression '" ^ input_token.tliteral ^ "'"))


and led input_string left_node operator start_idx : (Parse_node.expression_node * int) =
  let open Token in
  let open Parse_node in
  let binding_power = match operator.ttype with
  | CONS -> (get_lbp operator) - 1
  | _ -> get_lbp operator
  in
  let right_node, current_idx = pratt_parse input_string binding_power start_idx in
  match operator.ttype with
  | CONS ->
      Cons_Node (left_node, right_node), current_idx
  | ADD ->
      Add_Node (left_node, right_node), current_idx
  | MULT ->
      Mult_Node (left_node, right_node), current_idx
  | SUB ->
      Sub_Node (left_node, right_node), current_idx
  | DIV ->
      Div_Node (left_node, right_node), current_idx
  | EQ ->
      Eq_Node (left_node, right_node), current_idx
  | NEQ ->
      Not_Eq_Node (left_node, right_node), current_idx
  | GREATER -> 
      Greater_Node (left_node, right_node), current_idx
  | GREATEREQ ->
      Greq_Node (left_node, right_node), current_idx
  | LESS ->
      Less_Node (left_node, right_node), current_idx
  | LESSEQ ->
      Leq_Node (left_node, right_node), current_idx
  | _ -> let msg = "Unrecognised operator in expression" ^ operator.tliteral in 
      raise (Error.Parser_Error msg)


and collect_parameters input_string idx ~parameter_list:parameter_list : Parse_node.expression_node list * int =
  let current_token, current_idx = Lexer.lex input_string idx in
  let open Token in
  let open Parse_node in
  match current_token.ttype with
  | OPAR | STRING | INTEGER | FLOAT | NAMESPACE | BOOL | OSQP | TAG ->
      let p = Variable_Node{namespace = current_token.tliteral; parameters = []} in
      if current_token.ttype == NAMESPACE then
        collect_parameters 
          input_string
          current_idx
          ~parameter_list:(p :: parameter_list)
      else
        let next_param, next_idx = nud input_string current_token current_idx in
        collect_parameters
          input_string
          next_idx
          ~parameter_list:(next_param :: parameter_list)
  | _ -> List.rev parameter_list, idx



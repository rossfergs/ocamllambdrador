let parse_expression input_string start_idx = 

  let get_lbp input_token =
    let open Token in
    match input_token.ttype with
      | EQ | NEQ  | LESS | LESSEQ | GREATER | GREATEREQ  -> 5
      | ADD | SUB | CONS -> 2
      | MULT -> 3
      | EOF | DELIM -> -1
      | _ -> 0
  in

  let rec parse lbp idx =
    let current_token, current_idx = Lexer.lex input_string idx in
    let first_node, next_idx = nud current_token current_idx in
    collect_expression first_node lbp next_idx

  and collect_expression left_node lbp start_idx : (Parse_node.expression_node * int) =
    let current_token, current_idx = Lexer.lex input_string start_idx in
    if get_lbp current_token <= lbp
      then left_node, start_idx
    else
      let expr, next_idx = led left_node current_token current_idx in
      collect_expression expr lbp next_idx

  and nud input_token start_idx : (Parse_node.expression_node * int) =
    let open Token in
    let open Parse_node in
    match input_token.ttype with
      | BOOL ->
          let node = match input_token.tliteral with
          | "true" -> Bool_Node true
          | "false" -> Bool_Node false
          | _ -> 
            raise (Error.Parser_Error ("invalid string " ^ input_token.tliteral ^ " for bool"))
          in
          node, start_idx
      | NAMESPACE ->
        let collected_params, next_idx = collect_parameters start_idx ~parameter_list:[] in
        Variable_Node {namespace = input_token.tliteral; parameters = collected_params}, next_idx
      | INTEGER ->
        Integer_Node (int_of_string input_token.tliteral), start_idx
      | FLOAT ->
        Float_Node (float_of_string input_token.tliteral), start_idx
      | STRING ->
        (* Add one to string to account for the end quote *)
        String_Node input_token.tliteral, start_idx+1
      | OPAR ->
        (* get the expression within the parenthesis *)
        let paren_result, paren_idx = parse 0 start_idx in
        let next_token, next_idx = Lexer.lex input_string paren_idx in
        (* if the next token isnt a closing parenthesis, ")",  throw an error*)
        if next_token.ttype != Token.CPAR then
          raise (Error.Parser_Error "Unclosed parenthesis")
        else
          paren_result, next_idx
      | _ -> raise (Error.Parser_Error ("unknown token in expression '" ^ input_token.tliteral ^ "'"))

  and led left_node operator start_idx : (Parse_node.expression_node * int) =
    let open Token in
    let open Parse_node in
    let right_node, current_idx = parse (get_lbp operator) start_idx in
    match operator.ttype with
      | CONS ->
        Cons_Node (left_node, right_node), current_idx
      | ADD ->
        Add_Node (left_node, right_node), current_idx
      | MULT ->
        Mult_Node (left_node, right_node), current_idx
      | SUB ->
        Sub_Node (left_node, right_node), current_idx
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

  and collect_parameters idx ~parameter_list:parameter_list : Parse_node.expression_node list * int =
    let current_token, current_idx = Lexer.lex input_string idx in
    let open Token in
    let open Parse_node in
    match current_token.ttype with
      | OPAR | STRING | INTEGER | FLOAT | NAMESPACE | BOOL ->
        let p = Variable_Node{namespace = current_token.tliteral; parameters = []} in
        if current_token.ttype == NAMESPACE then
          collect_parameters 
            current_idx
            ~parameter_list:(p :: parameter_list)
        else
          let next_param, next_idx = nud current_token current_idx in
          collect_parameters
            next_idx
            ~parameter_list:(next_param :: parameter_list)
      |_ -> List.rev parameter_list, idx
  in
  parse 0 start_idx

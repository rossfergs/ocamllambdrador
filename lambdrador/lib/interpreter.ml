let rec interpret_program input_string : unit =
  let parse_tree = Parser.parse input_string in
  let rec intprt nodes scope idx =
    if idx >= List.length nodes then
      ()
    else
      let new_scope = interpret_statement (List.nth nodes idx) scope in
    intprt nodes new_scope (idx+1)
    in
  let empty_scope = {Scope.inner_scope = Hashtbl.create 255; outer_scope = None} in
  intprt parse_tree empty_scope 0

and interpret_statement node scope : Scope.scope =
  let open Parse_node in
  match node with
  | Print_Node e -> interpret_print e
  | Let_Node b -> scope.inner_scope


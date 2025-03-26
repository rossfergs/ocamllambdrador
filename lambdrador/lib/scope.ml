
module String_map = Map.Make(String)

type scope = 
  {inner_scope: Parse_node.expression_node String_map.t; outer_scope: scope option}

let to_list_tuple s = 
  let sl = ref [] in
  let el = ref [] in
  String_map.iter (fun s e -> sl := s :: !sl; el := e :: !el; ()) s;
  (List.rev !sl,List.rev !el)


let rec print_scope scope = 
  String_map.iter (fun k _ -> print_string k; print_endline "->"; print_endline ("EXPR")) scope.inner_scope;
  print_newline ();
  match scope.outer_scope with
    | None -> ()
    | Some s -> print_scope s

let bind nis cs = 
  { inner_scope = nis; outer_scope = cs.outer_scope }

let empty = { inner_scope = String_map.empty; outer_scope = None }

let rec get name scope = 
  try
    String_map.find name scope.inner_scope
  with Not_found ->
    match scope.outer_scope with
      | None -> raise (Error.Interpreter_Error ("Variable " ^ name ^ " not found in scope"))
      | Some s -> get name s

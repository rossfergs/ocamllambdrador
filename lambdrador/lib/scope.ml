
module String_map = Map.Make(String)

type scope = 
  {inner_scope: Parse_node.expression_node String_map.t; outer_scope: scope option}

let to_list_tuple s = 
  let sl = ref [] in
  let el = ref [] in
  String_map.iter (fun s e -> sl := s :: !sl; el := e :: !el; ()) s;
  (!sl,!el)


let rec print_scope scope = 
  let open Parse_node in
  let rec print_block = function
    | Block_Node {statements = s :: ss; expression; parameters; _} -> 
        (match s with
        | Let_Node _ -> print_endline "let"
        | Print_Node _ | Println_Node _ -> print_endline "print"
        | Expr _ -> print_endline "end expression");
        print_block (Block_Node {statements = ss; expression; parameters; closure_scope = None})
    | Block_Node {statements = []; parameters; _} ->
        print_endline "expression";
        List.iter (fun n -> print_string n; print_string " ") parameters
    | _ -> print_endline "not a block"
  in
  String_map.iter (fun k v -> print_string k; print_endline "->"; print_block v) scope.inner_scope;
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

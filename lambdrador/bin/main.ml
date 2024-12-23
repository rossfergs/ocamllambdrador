(*
  imperative file reading solution, excluding new_lines
  found from stack overflow by user gascha in this answer:
  https://stackoverflow.com/a/15848796
*)
let read_file file =
  let ic = open_in file in
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done; assert false
  with End_of_file ->
  String.concat " " !lines

(* 
  defining a helperfunction to catch any error during the interpreting proccess and pring the error in a prittier way 
*)
let interpret' user_input scope =
  try
    Lambdrador.Interpreter.interpret user_input scope
  with ex ->
    Lambdrador.Error.print_error ex;
    scope

(* a function to loop continuously, even when an error is occured *)
let repl_loop : unit =
  let repl_scope = 
    ref (Some Lambdrador.Scope.{inner_scope = String_map.empty; outer_scope = None}) in
  while true do
    print_string ">>> ";
    let user_input = read_line () in
    print_string "  > ";
    repl_scope := interpret' user_input !repl_scope;
  done

let () = 
  if Array.length Sys.argv < 1 then repl_loop else

  let argi = ref 0 in
  let argc = Array.length Sys.argv in
  while !argi <= argc do
    match Sys.argv.(!argi) with 
    | "-v" | "--version" ->
      print_endline "LAMBDRADOR VERSION 0.0";
      argi := argc
    | flag when String.starts_with ~prefix:"-" flag ->
      print_endline ("Unrecognised command: " ^ flag);
      argi := argc
    | file_path -> 
      let file = read_file file_path in
      print_endline file;
      let _ = interpret' file None in
      argi := argc
  done

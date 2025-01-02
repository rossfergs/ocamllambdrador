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
  String.concat "" (List.rev !lines)

(* 
  defining a helperfunction to catch any error during the interpreting proccess and pring the error in a prittier way 
*)
let interpret' user_input scope =
  try
    Lambdrador.Interpreter.interpret user_input scope
  with ex ->
    Lambdrador.Error.print_error ex;
    scope

(*
  a function to loop continuously, even when an error is occured
*)
let repl_loop () : unit =
  let repl_scope = ref (Some Lambdrador.Scope.empty) in
  let repl_looping = ref true in
  while !repl_looping do
    print_string ">>> ";
    match read_line () with
      | "EXIT" -> 
        repl_looping := false;
      | user_input ->
        print_string "  > ";
        repl_scope := interpret' user_input !repl_scope;
    print_newline ();
  done


(*
  main process
  if no arguements are given the REPL is then activated
  otherwise we iterate through the arguments and read a 
  given file if found
*)
let () =
  if Array.length Sys.argv <= 1 then repl_loop () else

  let argi = ref 1 in
  let argc = Array.length Sys.argv in
  while !argi < argc do
    match Sys.argv.(!argi) with
    | "-v" | "--version" ->
      print_endline "\\ _ /_";
      print_endline "/\\-/\\";
      print_endline " 0.0";
      argi := argc + 1
    | flag when String.starts_with ~prefix:"-" flag ->
      print_endline ("Unrecognised command: " ^ flag);
      argi := argc + 1
    | file_path ->
      let file = read_file file_path in
      let _ = interpret' file (Some (Lambdrador.Scope.empty)) in
      argi := argc + 1
  done

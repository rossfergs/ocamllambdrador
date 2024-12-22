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

let repl_loop : unit = 
  while true do
    print_string ">>> ";
    let user_input = read_line () in
    print_string "  > ";
    try
	Lambdrador.Interpreter.interpret user_input
    with ex ->
	Lambdrador.Error.print_error ex;

  done

let () = 
  print_string ("BLAH!" ^ Sys.argv.(0));
  if Array.length Sys.argv <= 1 then repl_loop else

  let argi = ref 1 in
  let argc = Array.length Sys.argv in
  while !argi < argc do
    match Sys.argv.(!argi) with 
    | "-v" | "--version" ->
      print_endline "LAMBDRADOR VERSION 0.0";
      argi := argc
    | flag when String.starts_with ~prefix:"-" flag ->
      print_endline ("Unrecognised command: " ^ flag);
      argi := argc
    | file_path -> 
      let file = read_file file_path in
      Lambdrador.Interpreter.interpret file;
      argi := argc
  done

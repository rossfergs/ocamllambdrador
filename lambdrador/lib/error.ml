exception Lexer_Error of string

exception Parser_Error of string

exception Interpreter_Error of string

let print_error = function
| Lexer_Error msg -> 
  print_endline ("Lexer Error: " ^ msg)
| Parser_Error msg ->
  print_endline ("Parser Error: " ^ msg)
| Interpreter_Error msg ->
  print_endline ("Interpreter Error: " ^ msg)
| exn -> 
  print_endline ("Exception: " ^ Printexc.to_string exn);
  Printexc.print_backtrace stdout

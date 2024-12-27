exception Lexer_Error of string

exception Parser_Error of string

exception Interpreter_Error of string

let print_error = function
| Lexer_Error msg -> 
  print_endline ("Lexer_ Error: " ^ msg)
| Parser_Error msg ->
  print_endline ("Parser Error: " ^ msg)
| Interpreter_Error msg ->
  print_endline ("Interpreter Error: " ^ msg)
| _ -> 
  print_endline "Unknown Error Type??"

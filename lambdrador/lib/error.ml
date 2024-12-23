exception LexerError of string

exception ParserError of string

exception InterpreterError of string

let print_error = function
| LexerError msg -> 
  print_endline ("Lexer Error: " ^ msg)
| ParserError msg ->
  print_endline ("Parser Error: " ^ msg)
| InterpreterError msg ->
  print_endline ("Interpreter Error: " ^ msg)
| _ -> 
  print_endline ("Unknown Error Type??")
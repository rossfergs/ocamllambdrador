exception Lexer_Error of string
exception Parser_Error of string
exception Interpreter_Error of string
val print_error: exn -> unit

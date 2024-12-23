exception LexerError of string
exception ParserError of string
exception InterpreterError of string
val print_error: exn -> unit
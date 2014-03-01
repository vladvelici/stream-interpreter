open InterpreterObjects 
open Printer

let _ = 
    try (
        let lexbuf = Lexing.from_channel stdin 
        in while true do
            try
                let result = Parser.main Lexer.token lexbuf 
                in prettyPrint result ; print_newline() ; flush stdout 
           with Parsing.Parse_error -> print_string "Parsing error. Sorry. \n"; flush stdout
        done
    ) with Lexer.Eof -> print_string "Bye :-h\n" ; flush stdout ; exit 0;

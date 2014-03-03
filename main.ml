open InterpreterObjects 
open Printer
open Environment
open Eval2

(* Root Environment *)

let roottbl:((string, variable) Hashtbl.t)  = Hashtbl.create 10;;

Hashtbl.replace roottbl "input_length" (Function (Int, []), (ValFunction (NativeFunc ("input_length", []))));;
Hashtbl.replace roottbl "get_input" (Function (Int ,[]), (ValFunction (NativeFunc ("get_input", [Int]))));;

let root = RootEnv roottbl;;

let _ = 
    try (
        let lexbuf = Lexing.from_channel stdin 
        in while true do
            try
                let result = Parser.main Lexer.token lexbuf 
                in
                prettyPrint result;
                print_newline();
                print_string "Eval result:\n";
                prettyPrint (Primitive (eval result root));
                print_string "\n";
                flush stdout; 
           with Parsing.Parse_error -> print_string "Parsing error. Sorry. \n"; flush stdout
        done
    ) with Lexer.Eof -> print_string "Bye :-h\n" ; flush stdout ; exit 0;

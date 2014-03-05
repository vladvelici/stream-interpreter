open InterpreterObjects 
open Printer
open Environment
open Eval2
open Exceptions
(* Root Environment *)

let roottbl:((string, variable) Hashtbl.t)  = Hashtbl.create 10;;

Hashtbl.replace roottbl "input_length" (Function (Int, []), (ValFunction (NativeFunc (Int, "input_length", []))));;
Hashtbl.replace roottbl "input" (Function (Stream Int, [Int]), (ValFunction (NativeFunc (Stream Int, "input", [Int]))));;
Hashtbl.replace roottbl "output" (Function (Unit, [Stream Int]), (ValFunction (NativeFunc (Unit, "output", [Stream Int]))));;

let in_file = open_in Sys.argv.(1)

let root = RootEnv roottbl;;
    let _ = 
        try (
            let lexbuf = Lexing.from_channel in_file
            in while true do
                try
                    let result = Parser.main Lexer.token lexbuf 
                    in match result with
                    | Expression e ->
                        eval e root;
                        flush stdout;
                    | Empty -> ()
               with Parsing.Parse_error -> print_string "Parsing error. Sorry. \n"; flush stdout
            done
        ) with
        | Lexer.Eof ->  flush stdout ; exit 0;



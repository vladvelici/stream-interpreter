open Args
open InterpreterObjects 
open Printer
open Environment
open Eval2
open Exceptions
(* Root Environment *)

let roottbl:((string, variable) Hashtbl.t)  = Hashtbl.create 10;;

Hashtbl.replace roottbl "input_length" (Function (Int, []), (ValFunction (NativeFunc (Int, "input_length", []))));;
Hashtbl.replace roottbl "no_of_inputs" (Function (Int, []), (ValFunction (NativeFunc (Int, "no_of_inputs", []))));;

Hashtbl.replace roottbl "input" (Function (Stream Int, [Int]), (ValFunction (NativeFunc (Stream Int, "input", [Int]))));;
Hashtbl.replace roottbl "output" (Function (Unit, [Stream Int]), (ValFunction (NativeFunc (Unit, "output", [Stream Int]))));;
Hashtbl.replace roottbl "reverse" (Function (Stream Int, [Stream Int]), (ValFunction (NativeFunc (Stream Int, "reverse", [Stream Int]))));;

Hashtbl.replace roottbl "input_float" (Function (Stream Float, [Int]), (ValFunction (NativeFunc (Stream Float, "input", [Int]))));;
Hashtbl.replace roottbl "output_float" (Function (Unit, [Stream Float]), (ValFunction (NativeFunc (Unit, "output", [Stream Float]))));;
Hashtbl.replace roottbl "reverse_float" (Function (Stream Float, [Stream Float]), (ValFunction (NativeFunc (Stream Float, "reverse", [Stream Float]))));;

let _ = read_arguments 0;;

let root = RootEnv roottbl;;

let _ = 
    try (
        let lexbuf = Lexing.from_channel !program_file
        in while true do
            try
                let result = Parser.main Lexer.token lexbuf 
                in match result with
                | Expression e -> if !interactive then (
                    prettyPrint (Primitive (eval e root));
                    print_string "\n";
                    flush stdout;
                ) else ((eval e root); ())
                | Empty -> ()
           with Parsing.Parse_error -> print_string "Parsing error. Sorry. \n"; flush stdout
        done
    ) with
    | Lexer.Eof ->  flush stdout ; exit 0;



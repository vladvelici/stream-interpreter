open Args
open InterpreterObjects 
open Printer
open Environment
open Eval
open Exceptions
(* Root Environment *)

let roottbl:((string, variable) Hashtbl.t)  = Hashtbl.create 10;;
let _ = read_arguments 0;;

let root = RootEnv roottbl;;
Native.init root;;

let _ = 
  try (
    let lexbuf = Lexing.from_channel !program_file
    in while true do
      try
        let result = Parser.main Lexer.token lexbuf 
        in match result with
        | Expression e ->
          let evaluated = (eval e root) in
          if !interactive then (
            prettyPrint (Primitive evaluated);
            print_string "\n";
            flush stdout;
          ) 
        | Empty -> ()
      with 
      | Parsing.Parse_error -> print_string "Parsing error. Sorry. \n"; flush stdout
      | IncompatibleTypes (t1, t2) -> print_string ("Incompatible types. Expected " ^ (string_of_type t1) ^ " but found " ^ (string_of_type t2) ^ "\n"); flush stdout

    done
  ) with
  | Lexer.Eof ->  flush stdout ; exit 0;



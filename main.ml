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
      | Parsing.Parse_error -> Printf.eprintf "Parsing error. Sorry. \n"; flush stderr
      | IncompatibleTypes (t1, t2) -> Printf.eprintf "Incompatible types. Expected %s but found %s.\n" (string_of_type t1) (string_of_type t2); flush stderr
      | IncompatibleTypesList (t1, t2) -> Printf.eprintf "Incompatible types. Expected one of %s but found %s.\n"; flush stderr
      | CannotRedefineVariable name -> Printf.eprintf "Cannot redefine variable %s.\n" name; flush stderr
      | UndefinedVariable name -> Printf.eprintf "Variable %s is undefined.\n" name; flush stderr
      | NotAFunction name -> Printf.eprintf "%s is not a function.\n" name; flush stderr
      | DivisionByZero -> Printf.eprintf "Division by zero.\n"; flush stderr
      | NotAStream -> Printf.eprintf "Not a stream.\n"; flush stderr
      | NotANumber -> Printf.eprintf "Not a number.\n"; flush stderr

    done
  ) with
  | Lexer.Eof ->  flush stdout ; exit 0;



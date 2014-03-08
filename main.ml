open Args
open InterpreterObjects 
open Printer
open Environment
open Eval
open Exceptions
open Native
(* Root Environment *)

let roottbl:((string, variable) Hashtbl.t)  = Hashtbl.create 10;;

let _ = read_arguments 0;;

let root = RootEnv roottbl;;

register_native_function root "input_length"    Int             [];;
register_native_function root "no_of_inputs"    Int             [];;
register_native_function root "input"           (Stream Int)    [Int];;
register_native_function root "input_float"     (Stream Float)  [Int];;
register_native_function root "output"          Unit            [Stream Int];;
register_native_function root "output_float"    Unit            [Stream Float];;
register_native_function root "reverse"         (Stream Int)    [Stream Int];;
register_native_function root "reverse_float"   (Stream Float)  [Stream Float];;
register_native_function root "int_of_float"    Int             [Float];;
register_native_function root "float_of_int"    Float           [Int];;

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



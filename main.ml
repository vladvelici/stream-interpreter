open InterpreterObjects 

let rec string_of_expression = function
    | DeclAssign (name, ty, exp) -> (string_of_type ty) ^ " " ^ name ^ " = [" ^ (string_of_expression exp) ^ "]"
    | CtxDeclaration (name, exp) -> name ^ ":=" ^ "["^(string_of_expression exp)^"]"
    | Assignment (name, exp) -> name ^ " = ["^(string_of_expression exp)^"]"
    | VarName name -> name
    | _ -> "not yet implemented"


and string_of_type = function
    | Int -> "int"
    | Float -> "float"
    | Boolean -> "bool"
    | String -> "string"
    | Function (a, b) -> "func(" ^ (string_of_ArgList b) ^ ")" ^ (string_of_type a)

and string_of_ArgList = function
    | [] -> ""
    | a :: b -> (string_of_type a) ^ "," ^ (string_of_ArgList b)

and prettyPrint exp = print_string (" ** " ^ (string_of_expression exp));;


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

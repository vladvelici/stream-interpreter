open InterpreterObjects 

let string_of_expression = function
    | DeclAssign (name, ty, exp) -> (string_of_type ty) ^ " " ^ name ^ " = [" ^ (prettyPrint exp) ^ "]"
    | CtxDeclaration (name, exp) -> name ^ ":=" ^ "["^(prettyPrint exp)^"]"
    | Assignment (name, exp) -> name ^ " = ["^(prettyPrint exp)^"]"
    | VarName name -> name
    | _ -> "not yet implemented";;
;;

let string_of_type = function
    | Int -> "int"
    | Float -> "float"
    | Boolean -> "bool"
    | String -> "string"
    | Func (a, b) -> "func(" ^ (string_of_ArgList b) ^ ")" ^ (string_of_type a);;

let string_of_ArgList = function
    | None -> ""
    | Args (a, b) -> (string_of_type a) ^ "," ^ (string_of_ArgList b);;

let _ = 
  try
    let lexbuf = Lexing.from_channel stdin 
    in  
       let result = Parser.main Lexer.token lexbuf 
       in
         prettyPrint result ; print_newline() ; flush stdout 
  with Parsing.Parse_error -> print_string "There was a problem parsing the SDL program. Please check your syntax. \n" ; flush stdout

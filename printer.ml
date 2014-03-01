open InterpreterObjects

let rec string_of_expression = function
    | DeclAssign (name, ty, exp) -> (string_of_type ty) ^ " " ^ name ^ " = [" ^ (string_of_expression exp) ^ "]"
    | CtxDeclaration (name, exp) -> name ^ ":=" ^ "["^(string_of_expression exp)^"]"
    | Assignment (name, exp) -> name ^ " = "^(string_of_expression exp)^""
    | VarName name -> name
    | Primitive p -> (
        match p with
            | ValInt v -> string_of_int v
            | ValString v -> v
            | ValFloat v -> string_of_float v
            | ValBoolean v -> string_of_bool v
            | ValFunction v -> string_of_func v
            | Null -> "Null"
            | Undefined -> "Undefined"
    )
    | _ -> "_not implemented_"

and string_of_func = function
    | Func (tipe, arg, exprList) -> "func (" ^ (string_of_ArgList arg) ^ ") " ^ (string_of_type tipe) ^
        " { " ^ (string_of_exprList exprList) ^ "}"

and string_of_type = function
    | Int -> "<int>"
    | Float -> "<float>"
    | Boolean -> "<bool>"
    | String -> "<string>"
    | Function (a, b) -> "<func (" ^ (string_of_TypeList b) ^ ") " ^ (string_of_type a) ^ ">"

and string_of_TypeList = function
    | [] -> ""
    | a :: [] -> (string_of_type a)
    | a :: b -> (string_of_type a) ^ "," ^ (string_of_TypeList b)

and string_of_ArgList = function
    | [] -> ""
    | Argument (name, tipe) :: [] -> (string_of_type tipe) ^ " " ^ name
    | Argument (name, tipe) :: b -> (string_of_type tipe) ^ " " ^ name ^ ", " ^ (string_of_ArgList b)

and string_of_exprList = function
    | [] -> ""
    | expr :: [] -> "\n" ^ (string_of_expression expr) ^ "\n"
    | expr :: b -> "\n" ^ (string_of_expression expr) ^ (string_of_exprList b)

and prettyPrint exp = print_string (" => " ^ (string_of_expression exp));;



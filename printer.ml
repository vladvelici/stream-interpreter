open InterpreterObjects

let rec string_of_expression = function
  | DeclAssign (name, ty, exp) -> (string_of_type ty) ^ " " ^ name ^ " = [" ^ (string_of_expression exp) ^ "]"
  | CtxDeclaration (name, exp) -> name ^ ":=" ^ "["^(string_of_expression exp)^"]"
  | Assignment (name, exp) -> name ^ " = "^(string_of_expression exp)^""
  | VarName name -> name
  | Primitive p -> string_of_primitive p

  | ApplyFunction (name, exec) -> name ^ "(" ^ (string_of_exprList ", " exec) ^ ")"
  (* (func...)(arg) *)
  | ApplyLambda (func, exec) -> "(" ^  (string_of_primitive (ValFunction func)) ^ ") (" ^ (string_of_exprList ", " exec) ^ ")"
  (* Arithmetics operators *)
  | PlusOperator (e1, e2) -> (string_of_expression e1) ^ "+" ^ (string_of_expression e2)
  | MinusOperator (e1, e2) -> (string_of_expression e1) ^ "-" ^ (string_of_expression e2)
  | MultiplyOperator (e1, e2) -> (string_of_expression e1) ^ "*" ^ (string_of_expression e2)
  | DivOperator (e1, e2) -> (string_of_expression e1) ^ "/" ^ (string_of_expression e2)
  | ExponentOperator (e1, e2) -> (string_of_expression e1) ^ "^" ^ (string_of_expression e2)
  | ModOperator (e1, e2) -> (string_of_expression e1) ^ "%" ^ (string_of_expression e2)
  | NegationOperator e -> "-" ^ (string_of_expression e)
  
  | NewStream e -> "~" ^ (string_of_expression e)
  | ReadStream e -> "<<" ^ (string_of_expression e)

  (* Equality testing operators *)
  | Equal (e1, e2) -> (string_of_expression e1) ^ "==" ^ (string_of_expression e2)
  | NonEqual (e1, e2) -> (string_of_expression e1) ^ "!=" ^ (string_of_expression e2)

  (* Order relation operators *)
  | Less(e1, e2) -> (string_of_expression e1) ^ "<" ^ (string_of_expression e2) 
  | Greater(e1, e2) -> (string_of_expression e1) ^ ">" ^ (string_of_expression e2) 
  | LessEqual(e1, e2) -> (string_of_expression e1) ^ "<=" ^ (string_of_expression e2) 
  | GreaterEqual(e1, e2) -> (string_of_expression e1) ^ ">=" ^ (string_of_expression e2) 

  (* Boolean logic operators *)
  | Not e -> "!" ^ (string_of_expression e)
  | Or(e1, e2) -> (string_of_expression e1) ^ "||" ^ (string_of_expression e2) 
  | And(e1, e2) -> (string_of_expression e1) ^ "&&" ^ (string_of_expression e2) 

  (* If statement operators *)
  | If (cond, exec) -> "if (" ^ (string_of_expression cond) ^ ") {\n" ^ (string_of_exprList "\n" exec) ^ "\n}"
  | IfElse (cond, th, el) -> "if (" ^ (string_of_expression cond) ^ ") {\n" ^
    (string_of_exprList "\n" th) ^ "\n} else {\n" ^
    (string_of_exprList "\n" el) ^ "\n}"
 
  (* For Loop operator *)
  | ForLoop (init, cond, afterthought, exec) -> "for (" ^ (string_of_expression init) ^ ", " ^ (string_of_expression cond) ^ ", " ^ (string_of_expression afterthought) ^ ") {\n" ^
        string_of_exprList "\n" exec ^ "\n}" 

  (* While Loop operator *)
  | WhileLoop (cond, exec) -> "while (" ^ (string_of_expression cond) ^ ") {\n" ^ (string_of_exprList "\n" exec) ^ "\n}"

  (* Do-While Loop operator *)
  | DoWhileLoop (cond, exec) -> "do {\n" ^ (string_of_exprList "\n" exec) ^ "\n} while (" ^ (string_of_expression cond) ^ ")"

(* ------------CONVERSIONS------------ *)
and string_of_primitive p =  match p with
      | ValInt v -> string_of_int v
      | ValFloat v -> string_of_float v
      | ValBoolean v -> string_of_bool v
      | ValFunction v -> string_of_func v
      | ValStream (t, _) -> "stream:" ^ (string_of_type t)
      | Null -> "Null"
      | Undefined -> "Undefined"

and string_of_func = function
  | Func (tipe, arg, exprList) -> "func (" ^ (string_of_ArgList arg) ^ ") " ^ (string_of_type tipe) ^
                                  " { \n" ^ (string_of_exprList "\n" exprList) ^ "\n}"
  | NativeFunc (tipe, _, typeList) -> "native func (" ^ (string_of_TypeList typeList) ^ ") " ^ (string_of_type tipe) 

and string_of_type = function
  | Int -> "<int>"
  | Float -> "<float>"
  | Boolean -> "<bool>"
  | Unit -> "<Unit>"
  | Stream t -> "<Stream:" ^ string_of_type t ^ ">"
  | Function (a, b) -> "<func (" ^ (string_of_TypeList b) ^ ") " ^ (string_of_type a) ^ ">"

and string_of_TypeList = function
  | [] -> ""
  | a :: [] -> (string_of_type a)
  | a :: b -> (string_of_type a) ^ "," ^ (string_of_TypeList b)

and string_of_ArgList = function
  | [] -> ""
  | Argument (name, tipe) :: [] -> (string_of_type tipe) ^ " " ^ name
  | Argument (name, tipe) :: b -> (string_of_type tipe) ^ " " ^ name ^ ", " ^ (string_of_ArgList b)

and string_of_exprList sep = function
  | [] -> ""
  | expr :: [] -> (string_of_expression expr)
  | expr :: b -> (string_of_expression expr) ^ sep ^ (string_of_exprList sep b)

and prettyPrint exp = print_string (" => " ^ (string_of_expression exp))


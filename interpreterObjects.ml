type tipe = Int | String | Float | Boolean | Func of tipe * (tipe list);;

let type_of_string = function
  | "bool" -> Boolean
  | "int" -> Int
  | "string" -> String
  | "float" -> Float 
  | _ -> failwith "Invalid argument for type_of_string.";;

type expression =
  | DeclAssign of string * tipe * expression (* int a = 3  *)
  | CtxDeclaration of string * expression (* a:=3 *)
  | Assignment of string * expression (* a = 4 *)
  | VarName of string (* a // this needs to return the value of a *)
  | Applyfunction of string * (expression list)        (* f(a, 3, b) *)

  (* Operators *)
  | PlusOperator of expression * expression       (* a+3 or a+b *)
  | MinusOperator of expression * expression      (* a-b  *)
  | MultiplyOperator of expression * expression   (* a*b *)
  | DivOperator of expression * expression        (* a/b *)
  | ExponentOperator of expression * expression   (* a^b *)
  | ModOperator of expression * expression;;      (* a%b *)

type argument = string * tipe;;

type funct = tipe * (argument list) * (expression list);;

type varValue =
  | ValInt of int
  | ValString of string
  | ValFloat of float
  | ValBoolean of bool
  | ValFunction of funct
  | Null
  | Undefined;;

type variable = tipe * varValue;;

type iexpr =
  | Expression of expression

  (* Any value typed directly into the interpreter, including functions *)
  | Primitive of varValue;;
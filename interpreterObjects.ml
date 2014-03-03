type expression =
  | DeclAssign of string * tipe * expression        (* int a = 3  *)
  | CtxDeclaration of string * expression           (* a:=3 *)
  | Assignment of string * expression               (* a = 4 *)
  | VarName of string                               (* a *)
  | ApplyFunction of string * (expression list)     (* f(a, 3, b) *)
  | ApplyLambda of func * (expression list)         (* (func...)(arg) *)

  (* Operators *)
  | PlusOperator of expression * expression         (* a+3 or a+b *)
  | MinusOperator of expression * expression        (* a-b  *)
  | MultiplyOperator of expression * expression     (* a*b *)
  | DivOperator of expression * expression          (* a/b *)
  | ExponentOperator of expression * expression     (* a^b *)
  | ModOperator of expression * expression          (* a%b *)

  | Primitive of varValue

and tipe = Int | String | Float | Boolean | Function of tipe * (tipe list) | Unit
and argument = Argument of string * tipe

and func = Func of tipe * (argument list) * (expression list)

and varValue =
  | ValInt of int
  | ValString of string
  | ValFloat of float
  | ValBoolean of bool
  | ValFunction of func
  | Null
  | Undefined

and variable = tipe * varValue;;

let type_of_string = function
  | "bool" -> Boolean
  | "int" -> Int
  | "string" -> String
  | "float" -> Float 
  | _ -> failwith "Invalid argument for type_of_string.";;



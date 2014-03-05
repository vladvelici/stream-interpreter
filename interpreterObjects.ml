type expression =
  | DeclAssign of string * tipe * expression        (* int a = 3  *)
  | CtxDeclaration of string * expression           (* a := 3 *)
  | Assignment of string * expression               (* a = 4 *)
  | VarName of string                               (* a *)
  | ApplyFunction of string * (expression list)     (* f(a, 3, b) *)
  | ApplyLambda of func * (expression list)         (* (func...)(arg) *)
  | Primitive of varValue

  (* Arithmetics operators *)
  | PlusOperator of expression * expression         (* a + 3 or a + b *)
  | MinusOperator of expression * expression        (* a - b  *)
  | MultiplyOperator of expression * expression     (* a * b or b * a*)
  | DivOperator of expression * expression          (* a / b *)
  | ExponentOperator of expression * expression     (* a ^ b *)
  | ModOperator of expression * expression          (* a % b *)
  | NegationOperator of expression		    (* - a *)

  (* Equality testing operators *)
  | Equal of expression * expression		    (* a == b *)
  | NonEqual of expression * expression		    (* a != b *)

  (* Order relation operators *)
  | Less of expression * expression		    (* a < b *)
  | Greater of expression * expression		    (* a > b *)
  | LessEqual of expression * expression	    (* a <= b *)
  | GreaterEqual of expression * expression	    (* a >= b *)

  (* Boolean logic operators *)
  | Not of expression				    (* !a *)
  | Or of expression * expression		    (* a || b *)
  | And of expression * expression		    (* a && b *)

  (* If statement operators *)
  | If of expression * (expression list)	    			(* if ( a > b ) { a = b... } *) 
  | IfElse of expression * (expression list) * (expression list)	(* if ( a > b ) { a = b... } Else {...} *)

  (* For Loop operator: for ( int i:=0, i < 5, i+1 ) { a = a + 2 } *)
  | ForLoop of expression * expression * expression * (expression list)

  (* While Loop operator: while ( i < 5 ) { a = a + 2... } *)
  | WhileLoop of expression * (expression list)

  (* Do-While Loop operator: do { a = a + 2... } while ( i < 5 ) *)
  | DoWhileLoop of expression * (expression list)

and tipe = Int | String | Float | Boolean | Function of tipe * (tipe list) | Unit
and argument = Argument of string * tipe

and func = Func of tipe * (argument list) * (expression list)

and varValue =
  | ValInt of int
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



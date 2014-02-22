type Expression =
        | Declaration of string * Type (* int a *)
        | DeclAssign of string * Type * Expression (* int a = 3  *)
        | CtxDeclaration of string * Expression (* a:=3 *)
        | Assignment of string * Expression (* a = 4 *)
        | VarName of string (* a // this needs to return the value of a *)
        | ApplyFunction of string * ArgumentList        (* f(a, 3, b) *)

        (* Operators *)
        | PlusOperator of Expression * Expression       (* a+3 or a+b *)
        | MinusOperator of Expression * Expression      (* a-b  *)
        | MultiplyOperator of Expression * Expression   (* a*b *)
        | DivOperator of Expression * Expression        (* a/b *)
        | ExponentOperator of Expression * Expression   (* a^b *)
        | ModOperator of Expression * Expression        (* a%b *)

        (* Any value typed directly into the interpreter, including functions *)
        | Primitive of VarValue

        (* more expressions, useful for function bodies *)
        | Sequence of Expression * Expression;;
 
type Type = Int | String | Float | Boolean | Func of Type * TypeArgList;;
type TypeArgList = None | ArgTypes of Type * TypeArgList;;

let type_of_string = function
        | "bool" -> Boolean
        | "int" -> Int
        | "string" -> String
        | "float" -> Float;;

type VarValue =
        | ValInt of int
        | ValString of string
        | ValFloat of float
        | ValBoolean of bool
        | ValFunction of Function
        | Null
        | Undefined;;

type Variable = string * Type * VarValue;;

type Function = Type * VariableList * Expression;;
type VariableList = None | Vars of Variable * VariableList;; 

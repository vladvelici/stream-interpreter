open InterpreterObjects
open Environment
open Exceptions

(* typeOf expression in some environment *)
let rec typeOf exp env = match exp with 
    | DeclAssign _ -> Unit 
    | CtxDeclaration _ -> Unit 
    | Assignment _ -> Unit
    | VarName a -> lookup_variable_type env a
    | ApplyFunction (name, _) -> return_type (lookup_variable_type env name) name
    | ApplyLambda (fnc, _) -> return_type (typeOf (Primitive (ValFunction fnc)) env) "-lambda-"
    | NewStream expr -> Stream (return_type (typeOf expr env) "-newstream-")
    | ReadStream expr -> return_type_stream expr env
    | Primitive (ValInt _) -> Int
    | Primitive (ValFloat _) -> Float
    | Primitive (ValBoolean _) -> Boolean
    | Primitive (Null) -> Unit
    | Primitive (Undefined) -> Unit
    | Primitive (ValStream (t, _)) -> Stream t
    | Primitive (ValFunction (Func (t, arglist, _))) -> Function (t, (typelist_of_arglist arglist))
    | _ -> raise (NotYetImplemented exp)

(* given an arugmnet list, return a list of their types only *)
and typelist_of_arglist = function
    | Argument (_, t) :: l -> t :: typelist_of_arglist l
    | [] -> []

and type_is_stream expr env = match expr with
    | NewStream _ -> true
    | VarName name -> (match (lookup_variable_type env name) with Stream _ -> true | _ -> false)
    | Primitive (ValStream _) -> true
    | _ -> false

(* return type of a given function *)
and return_type fnc name = match fnc with
    | Function (t, _) -> t
    | _ -> raise (NotAFunction name)

and return_type_stream expr env = 
    let stream = typeOf expr env in (match stream with Stream t -> t | _ -> raise NotAStream)

(* whether the type of Primitive is a function or not *)
and type_is_function = function
    | Function _ -> true
    | _ -> false

(* make sure arguments types match with function arguments *)
let rec typecheck_parameters args params env = match args, params with
    | [], [] -> true
    | Argument(_, t_arg) :: arglist, expr :: paramlist -> (types_compatible t_arg (typeOf expr env)) && (typecheck_parameters arglist paramlist env)
    | _, _ -> false

(* check if t1 is compatible with t2 (a variable of t1 can accept a t2). Undefined and Null have type Unit which is accepted by any type *)
and types_compatible t1 t2 = t1 = t2 || t2 = Unit

(* check if t1 is the same as t2. Undefined and Null (type Unit) are not accepted as valid types for t2 *)
and types_identical t1 t2 = t1 = t2


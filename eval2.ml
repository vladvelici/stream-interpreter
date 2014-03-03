open InterpreterObjects
open Environment

exception CannotRedefineVariable of string;;
exception UndefinedVariable of string;;
exception IncompatibleTypes of tipe * tipe;; (* Expected tipe 1, found tipe 2 *)
exception NotAFunction of string;;

let rec typelist_of_arglist = function
    | Argument (_, t) :: l -> t :: typelist_of_arglist l
    | [] -> [];;

let rec validate_arglist arglist typelist = match arglist, typelist with
    | [], [] -> true
    | Argument(_, t_arg) :: r_arglist, t :: r_typelist -> (t_arg = t) && (validate_arglist r_arglist r_typelist)
    | _, _ -> false;;

let rec typeOf exp env = match exp with 
    | DeclAssign _ -> Unit 
    | CtxDeclaration _ -> Unit 
    | Assignment _ -> Unit
    | VarName a -> lookup_variable_type env a
    | ApplyFunction (name, _) -> returnType (lookup_variable_type env name) name
    | ApplyLambda (fnc, _) -> returnType fnc "-lambda-"
    | Primitive (ValInt _) -> Int
    | Primitive (ValFloat _) -> Float
    | Primitive (ValBoolean _) -> Boolean
    | Primitive (Null) -> Unit
    | Primitive (Undefined) -> Unit
    | Primitive (ValFunction (Func (t, arglist, _))) -> Function (t, (typelist_of_arglist arglist))

and returnType fnc name = match fnc with
    | Function (t, _) -> t
    | _ -> raise (NotAFunction name);;

let value_of_var = function (_, v) -> v;;
let type_of_var = function (t, _) -> t;;

let is_function = function
    | Function _ -> true
    | _ -> false;;

let rec eval exp env = match exp with
    | DeclAssign (name, t, expr) -> do_declare_assig env name ((t, value):variable)
    | CtxDeclaration (name, expr) -> do_declare_assig env name (((typeOf value), value):variable)
    | Assignment (name, expr) -> do_assign env name value
    | VarName name -> lookup_variable env name
 (*    | ApplyFunction (name, arguments) -> let varDetails = (lookup_variable env name) in match varDetails with (ftype, f, fenv) -> *)


and do_declare_assig env name (var:variable) =
    if is_variable_local env name then 
        raise (CannotRedefineVariable name)
    else
        put_variable env name var

and do_assign env name value =
    try (
        let tvar = lookup_variable_type name
        and tval = typeOf value
        in if tvar == tval then put_variable env name (tvar, value) else (raise (IncompatibleTypes tvar tval))
    ) with Not_found -> (raise (UndefinedVariable name))



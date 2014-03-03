open InterpreterObjects
open Environment
open Native

exception CannotRedefineVariable of string;;
exception UndefinedVariable of string;;
exception UninitializedVariable of string;;
exception IncompatibleTypes of tipe * tipe;; (* Expected tipe 1, found tipe 2 *)
exception NotAFunction of string;;
exception NotYetImplemented of expression;;

let rec typeOf exp env = match exp with 
    | DeclAssign _ -> Unit 
    | CtxDeclaration _ -> Unit 
    | Assignment _ -> Unit
    | VarName a -> lookup_variable_type env a
    | ApplyFunction (name, _) -> returnType (lookup_variable_type env name) name
    | ApplyLambda (fnc, _) -> returnType (typeOf (Primitive (ValFunction fnc)) env) "-lambda-"
    | Primitive (ValInt _) -> Int
    | Primitive (ValFloat _) -> Float
    | Primitive (ValBoolean _) -> Boolean
    | Primitive (Null) -> Unit
    | Primitive (Undefined) -> Unit
    | Primitive (ValFunction (Func (t, arglist, _))) -> Function (t, (typelist_of_arglist arglist))
    | _ -> raise (NotYetImplemented exp)

and returnType fnc name = match fnc with
    | Function (t, _) -> t
    | _ -> raise (NotAFunction name)

and value_of_var = function (_, v) -> v
and type_of_var = function (t, _) -> t

and is_function = function
    | Function _ -> true
    | _ -> false

and typelist_of_arglist = function
    | Argument (_, t) :: l -> t :: typelist_of_arglist l
    | [] -> [];;

let rec validate_parameters args params env = match args, params with
    | [], [] -> true
    | Argument(_, t_arg) :: arglist, expr :: paramlist -> (t_arg = (typeOf expr env)) && (validate_parameters arglist paramlist env)
    | _, _ -> false;;


(* EVALUATION *)
let rec eval exp env = match exp with
    | DeclAssign (name, t, expr) -> let expr_type = typeOf expr env in 
        if expr_type = t || expr = Primitive Undefined then
            do_declare_assig env name t expr
        else 
            raise (IncompatibleTypes (t, expr_type))
    | CtxDeclaration (name, expr) -> do_declare_assig env name (typeOf expr env) expr
    | Assignment (name, expr) -> do_assign env name expr
    | VarName name -> fetch_variable env name
    | ApplyLambda (f, params) -> apply_function env env f params
    | ApplyFunction (name, params) -> let (decl_scope, (varType, f)) = lookup_variable_all env name
        in if (is_function varType) then apply_function env decl_scope (func_of_varval f) params else raise (NotAFunction name) 
    | Primitive p -> p
    | tmp -> raise (NotYetImplemented tmp)

and func_of_varval = function
    | ValFunction f -> f
    | _ -> raise (NotAFunction "...")

and do_declare_assig env name var_type expr =
    if is_variable_local env name then 
        raise (CannotRedefineVariable name)
    else
        put_variable env name ((var_type, (eval expr env)):variable); Undefined

and do_assign env name expr =
    try (
        let (varEnv, (tvar, _)) = lookup_variable_all env name
        and tval = typeOf expr env
        in if tvar == tval then put_variable varEnv name (tvar, (eval expr env)) else (raise (IncompatibleTypes (tvar, tval))); Undefined
    ) with Not_found -> (raise (UndefinedVariable name))

and fetch_variable env name =
    try (
        let var = lookup_variable env name
        in match var with
            | (_, Undefined) -> raise (UninitializedVariable name)
            | (_, v) -> v
    ) with Not_found -> raise (UndefinedVariable name)

and apply_param_bindings func_scope call_scope params arguments = match params, arguments with
    | expr :: param_list, Argument (name, arg_type) :: arg_list -> put_variable func_scope name ((arg_type, (eval expr call_scope)):variable)
    | [], [] -> ()
    | _, _ -> raise (IncompatibleTypes (Unit, Unit)) 

and eval_func_body scope body : varValue= match body with
    | [] -> Undefined
    | expr :: [] -> eval expr scope
    | expr :: exprList -> eval expr scope; eval_func_body scope exprList

and apply_function call_scope decl_scope f params = match f with
    | NativeFunc (name, arguments) -> run_native_code name params 
    | Func (retrunType, arguments, body) ->
        if (validate_parameters arguments params call_scope) then
            let func_scope = new_environment decl_scope
            in apply_param_bindings func_scope call_scope params arguments; eval_func_body func_scope body
        else raise (IncompatibleTypes (Unit, Unit))
;;



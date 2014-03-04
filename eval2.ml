open InterpreterObjects
open Environment
open Native

(* Common exceptions *)
exception CannotRedefineVariable of string;;
exception UndefinedVariable of string;;
exception UninitializedVariable of string;;
exception IncompatibleTypes of tipe * tipe;; (* Expected tipe 1, found tipe 2 *)
exception NotAFunction of string;;
exception NotYetImplemented of expression;;

(* typeOf expression in some environment *)
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

(* return type of a given function *)
and returnType fnc name = match fnc with
    | Function (t, _) -> t
    | _ -> raise (NotAFunction name)

(* helpers for working with functions *)
and value_of_var = function (_, v) -> v
and type_of_var = function (t, _) -> t

(* whether the Primitive is a function or not *)
and is_function = function
    | Function _ -> true
    | _ -> false

(* given an arugmnet list, return a list of their types only *)
and typelist_of_arglist = function
    | Argument (_, t) :: l -> t :: typelist_of_arglist l
    | [] -> [];;

(* make sure arguments types match with function arguments *)
let rec validate_parameters args params env = match args, params with
    | [], [] -> true
    | Argument(_, t_arg) :: arglist, expr :: paramlist -> (types_compatible t_arg (typeOf expr env)) && (validate_parameters arglist paramlist env)
    | _, _ -> false;;

(* check if t1 is compatible with t2 (a variable of t1 can accept a t2). Undefined and Null have type Unit which is accepted by any type *)
let types_compatible t1 t2 = t1 = t2 || t2 = Unit;;

(* check if t1 is the same as t2. Undefined and Null (type Unit) are not accepted as valid types for t2 *)
let types_identical t1 t2 = t1 = t2;;

(* EVALUATION *)
let rec eval exp env = match exp with
    | DeclAssign (name, t, expr) -> let expr_type = typeOf expr env in 
        if types_compatible t expr_type then
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

(* helper function to return the function from a varValue *)
and func_of_varval = function
    | ValFunction f -> f
    | _ -> raise (NotAFunction "...")

(* declare a variable in given environment. if already declare raise CannotRedefineVariable *)
and do_declare_assig env name var_type expr =
    if is_variable_local env name then 
        raise (CannotRedefineVariable name)
    else
        put_variable env name ((var_type, (eval expr env)):variable); Undefined

(* assignment. raises UndefinedVariable if variable is not defined or IncompatibleTypes if the types don't match. *)
and do_assign env name expr =
    try (
        let (varEnv, (tvar, _)) = lookup_variable_all env name
        and tval = typeOf expr env
        in if types_compatible tval tvar then put_variable varEnv name (tvar, (eval expr env)) else (raise (IncompatibleTypes (tvar, tval))); Undefined
    ) with Not_found -> (raise (UndefinedVariable name))

(* fetch a variable by name from the given environment (or any parent environments)
 * raises UninitializedVariable if the value is Undefined
 * raise UndefinedVariable if the variable is not found in any of the environments *)
and fetch_variable env name =
    try (
        let var = lookup_variable env name
        in match var with
            | (_, Undefined) -> raise (UninitializedVariable name)
            | (_, v) -> v
    ) with Not_found -> raise (UndefinedVariable name)

(* binds the variables from the parameters into arguments in the func_scope environment.
 * Evaluates the values of parameters in the call_scope. *)
and apply_param_bindings func_scope call_scope params arguments = match params, arguments with
    | expr :: param_list, Argument (name, arg_type) :: arg_list -> put_variable func_scope name ((arg_type, (eval expr call_scope)):variable)
    | [], [] -> ()
    | _, _ -> raise (IncompatibleTypes (Unit, Unit)) 

(* Evalues a list of expressions and returns the value of the last one. Undefined if empty *)
and eval_func_body scope body : varValue= match body with
    | [] -> Undefined
    | expr :: [] -> eval expr scope
    | expr :: exprList -> eval expr scope; eval_func_body scope exprList

(* calls the given function in the given call_scope. Note that the function scope inherits the declaration scope, not the call scope. *)
and apply_function call_scope decl_scope f params = match f with
    | NativeFunc (name, arguments) -> run_native_code name params 
    | Func (retrunType, arguments, body) ->
        if (validate_parameters arguments params call_scope) then
            let func_scope = new_environment decl_scope
            in apply_param_bindings func_scope call_scope params arguments; eval_func_body func_scope body
        else raise (IncompatibleTypes (Unit, Unit))
;;



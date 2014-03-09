open InterpreterObjects
open Environment
open Exceptions
open Typechecker
open EvalHelperFunctions

(* EVALUATION *)
let rec eval exp env = match exp with
  | DeclAssign (name, t, expr) -> let expr_type = typeOf expr env in 
    types_compatible t expr_type;
    do_declare_assig env name t expr
  | CtxDeclaration (name, expr) -> do_declare_assig env name (typeOf expr env) expr
  | Assignment (name, expr) -> do_assign env name expr
  | VarName name -> fetch_variable env name
  | ApplyLambda (f, params) -> apply_function env env f params
  | ApplyFunction (name, params) -> let (decl_scope, (varType, f)) = lookup_variable_all env name
    in if (type_is_function varType) then apply_function env decl_scope (func_of_varval f) params else raise (NotAFunction name) 
  | NewStream expr -> if type_is_function (typeOf expr env) then let f = func_of_varval (eval expr env) in create_new_stream f env else raise (NotAFunction "-new stream-")
  | ReadStream expr -> if type_is_stream expr env then (let stream = eval expr env in read_stream stream) else raise (NotAStream)
  | Primitive p -> p

  (* Arithmetics *)
  | PlusOperator (e1, e2) ->
    types_compatible_nrop (typeOf e1 env);
    types_compatible_nrop (typeOf e2 env);
    plus (eval e1 env) (eval e2 env)

  | MinusOperator (e1, e2) -> 
    types_compatible_nrop (typeOf e1 env);
    types_compatible_nrop (typeOf e2 env);
    minus (eval e1 env) (eval e2 env)

  | MultiplyOperator (e1, e2) -> 
    types_compatible_nrop (typeOf e1 env);
    types_compatible_nrop (typeOf e2 env);
    multiply (eval e1 env) (eval e2 env)

  | DivOperator (e1, e2) -> 
    types_compatible_nrop (typeOf e1 env);
    types_compatible_nrop (typeOf e2 env);
    div (eval e1 env) (eval e2 env)

  | ExponentOperator (e1, e2) -> 
    types_compatible_nrop (typeOf e1 env);
    types_compatible_nrop (typeOf e2 env);
    exponent (eval e1 env) (eval e2 env)

  | ModOperator (e1, e2) -> 
    let accepted = [Int; Stream Int; Unit] in 
    (type_is_one_of accepted accepted (typeOf e1 env);
     type_is_one_of accepted accepted (typeOf e2 env));
    modulo (eval e1 env) (eval e2 env)

  | NegationOperator e -> 
    types_compatible_nrop (typeOf e env);
    negation (eval e env)

  (* Equality testing *)
  | Equal (e1, e2) -> equal (eval e1 env) (eval e2 env)
  | NonEqual (e1, e2) -> nonEqual (eval e1 env) (eval e2 env)

  (* Order relation operators *)
  | Less (e1, e2) -> less (eval e1 env) (eval e2 env)
  | Greater (e1, e2) -> greater (eval e1 env) (eval e2 env)
  | LessEqual (e1, e2) -> lessEqual (eval e1 env) (eval e2 env)
  | GreaterEqual (e1, e2) -> greaterEqual (eval e1 env) (eval e2 env)

  (* Boolean logic operators *)
  | Not e -> types_identical Boolean (typeOf e env); let valb = (eval e env) in 
    (match valb with
     | ValBoolean b -> ValBoolean (not b) 
     | _ -> raise (IncompatibleTypes (Boolean, (typeOf (Primitive valb) env))))

  | Or (e1, e2) -> types_identical Boolean (typeOf e1 env); types_identical Boolean (typeOf e2 env);       
    let val1 = (eval e1 env) and val2 = (eval e2 env) in 
    (match val1, val2 with
     | ValBoolean b1, ValBoolean b2 -> ValBoolean (b1 || b2)
     | _, _ -> raise (IncompatibleTypes (Unit, Unit)))

  | And (e1, e2) -> types_identical Boolean (typeOf e1 env); types_identical Boolean (typeOf e2 env);       
    let val1 = (eval e1 env) and val2 = (eval e2 env) in 
    (match val1, val2 with 
     | ValBoolean b1, ValBoolean b2 -> ValBoolean (b1 && b2)
     | _, _ -> raise (IncompatibleTypes (Unit, Unit)))

  (* If statement operators *)
  | If (e1, e2) -> if bool_check (eval e1 env) then (eval_func_body e2 env)  else Undefined
  | IfElse (e1, e2, e3) -> if bool_check (eval e1 env) then (eval_func_body e2 env) else (eval_func_body e3 env)

  (* Loops *)
  | ForLoop (init, cond, afterthought, body) -> begin 
      let env2 = new_environment env in
      types_identical Boolean (typeOf cond env2);
      eval_and_unit init env2;
      while (bool_check (eval cond env2)) do
        begin
          eval_func_body_and_unit body env2;
          eval_and_unit afterthought env2
        end
      done
    end; Undefined

  | WhileLoop (condition, seq) ->
    let env2 = new_environment env in
    types_identical Boolean (typeOf condition env2);
    begin  
      while (bool_check (eval condition env2)) do
        eval_func_body_and_unit seq env2
      done;
      Undefined
    end

  | DoWhileLoop (condition, seq) -> let env2 = new_environment env in
    types_identical Boolean (typeOf condition env2);  
    eval_func_body_and_unit seq env2;
    while (bool_check (eval condition env2)) do
      eval_func_body_and_unit seq env2;
    done;
    Undefined

and eval_and_unit expr env = let _ = eval expr env in ()
and eval_func_body_and_unit exprList env = let _ = eval_func_body exprList env in ()

(* Reads the next element from a stream *)
and read_stream stream = match stream with
  | ValStream (t, s) -> (try (Stream.next s) with Stream.Failure -> Null)
  | _ -> raise NotAStream

(* create new stream (returns ValStream) *)
and create_new_stream f env = let stream_type = match f with Func (t, _, _) -> t | NativeFunc (t, _, _) -> t
  in ValStream (stream_type, Stream.from (internal_new_stream env f))

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
    in types_compatible tvar tval; put_variable varEnv name (tvar, (eval expr env)); Undefined
  ) with Not_found -> (raise (UndefinedVariable name))

(* fetch a variable by name from the given environment (or any parent environments)
 * raise UndefinedVariable if the variable is not found in any of the environments *)
and fetch_variable env name =
  try (
    let var = lookup_variable env name
    in match var with
    | (_, v) -> v
  ) with Not_found -> raise (UndefinedVariable name)

(* binds the variables from the parameters into arguments in the func_scope environment.
 * Evaluates the values of parameters in the call_scope. *)
and apply_param_bindings func_scope call_scope params arguments = match params, arguments with
  | expr :: param_list, Argument (name, arg_type) :: arg_list -> put_variable func_scope name ((arg_type, (eval expr call_scope)):variable)
  | [], [] -> ()
  | _, _ -> raise (IncompatibleTypes (Unit, Unit)) 

(* Evalues a list of expressions and returns the value of the last one. Undefined if empty *)
and eval_func_body body scope : varValue= match body with
  | [] -> Undefined
  | expr :: [] -> eval expr scope
  | expr :: exprList -> eval_and_unit expr scope; eval_func_body exprList scope

and params_to_values params env = match params with
  | p :: l -> (eval p env) :: params_to_values l env
  | [] -> []

(* calls the given function in the given call_scope. Note that the function scope inherits the declaration scope, not the call scope. *)
and apply_function call_scope decl_scope f params = match f with
  | NativeFunc (_, name, arguments) ->
    check_parameter_types params arguments call_scope;
    Native.run name (params_to_values params call_scope)

  | Func (rType, arguments, body) ->
    check_parameter_types params (typelist_of_arglist arguments) call_scope;
    let func_scope = new_environment decl_scope
    in apply_param_bindings func_scope call_scope params arguments;
    let res = eval_func_body body func_scope
    in let resType = typeOf (Primitive res) func_scope
    in types_compatible rType resType; res

and internal_new_stream decl_scope f count =
  let return = eval (ApplyLambda (f, [Primitive (ValInt count)])) decl_scope  
  in if return = Undefined || return = Null then None else Some return
;;



open InterpreterObjects
open Environment
open Native
open Exceptions
open Typechecker
open EvalHelperFunctions

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
    in if (type_is_function varType) then apply_function env decl_scope (func_of_varval f) params else raise (NotAFunction name) 
  | NewStream expr -> if type_is_function (typeOf expr env) then let f = func_of_varval (eval expr env) in create_new_stream f env else raise (NotAFunction "--")
  | ReadStream expr -> if type_is_stream expr env then (let stream = eval expr env in read_stream stream) else raise (NotAStream)
  | Primitive p -> p

  (* Arithmetics *)
  | PlusOperator (e1, e2) -> plus (eval e1 env) (eval e2 env)
  | MinusOperator (e1, e2) -> minus (eval e1 env) (eval e2 env)
  | MultiplyOperator (e1, e2) -> multiply (eval e1 env) (eval e2 env)
  | DivOperator (e1, e2) -> let n = (eval e2 env) in
    if (is_zero n) = false then (div (eval e1 env) n) else raise DivisionByZero
  | ExponentOperator (e1, e2) -> exponent (eval e1 env) (eval e2 env)
  | ModOperator (e1, e2) -> let ValInt v1 = (eval e1 env) and ValInt v2 = (eval e2 env) in ValInt (v1 mod v2)
  | NegationOperator e -> (match (eval e env) with ValFloat v -> ValFloat (-.v) | ValInt v -> ValInt (-v) | _ -> raise NotANumber)

  (* Equality testing *)
  | Equal (e1, e2) -> equal (eval e1 env) (eval e2 env)
  | NonEqual (e1, e2) -> nonEqual (eval e1 env) (eval e2 env)

  (* Order relation operators *)
  | Less (e1, e2) -> less (eval e1 env) (eval e2 env)
  | Greater (e1, e2) -> greater (eval e1 env) (eval e2 env)
  | LessEqual (e1, e2) -> lessEqual (eval e1 env) (eval e2 env)
  | GreaterEqual (e1, e2) -> greaterEqual (eval e1 env) (eval e2 env)

  (* Boolean logic operators *)
  | Not e -> let valb = (eval e env) in (match valb with ValBoolean b -> ValBoolean (not b) | _ -> raise (IncompatibleTypes (Boolean, (typeOf (Primitive valb) env))))
  | Or (e1, e2) -> if (typeOf e1 env) = Boolean && (typeOf e2 env) = Boolean then
      let (ValBoolean val1) = (eval e1 env) and (ValBoolean val2) = (eval e2 env) in (ValBoolean (val1 || val2))
  else
      raise (IncompatibleTypes (Boolean, Unit))
    | And (e1, e2) -> if (typeOf e1 env) = Boolean && (typeOf e2 env) = Boolean then
        let (ValBoolean val1) = (eval e1 env) and (ValBoolean val2) = (eval e2 env) in (ValBoolean (val1 && val2))
    else
        raise (IncompatibleTypes (Boolean, Unit))

  (* If statement operators *)
  | If (e1, e2) -> if bool_check (eval e1 env) then (eval_func_body env e2)  else Undefined
  | IfElse (e1, e2, e3) -> if bool_check (eval e1 env) then (eval_func_body env e2) else (eval_func_body env e3)

  (* Loops *)
  | ForLoop (e1, e2, e3, e4) -> begin 
      let env2 = new_environment env in
      eval e1 env2;
      while (bool_check (eval e2 env2)) do
        begin
            eval_func_body env2 e4;
            eval e3 env2
        end
      done;
      Undefined
    end

  | WhileLoop (condition, seq) -> let env2 = new_environment env in 
    while (bool_check (eval condition env2)) do
      eval_func_body env2 seq
    done; Undefined

  | DoWhileLoop (condition, seq) -> let env2 = new_environment env in
    begin
      eval_func_body env2 seq;
      while (bool_check (eval condition env2)) do
        eval_func_body env2 seq
      done;
      Undefined
    end


  | tmp -> raise (NotYetImplemented tmp)

and is_zero = function
  | ValInt n -> n = 0
  | ValFloat n -> n = 0.0
  | _ -> raise (NotANumber)

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
  | NativeFunc (_, name, arguments) -> run_native_code name params 
  | Func (rType, arguments, body) ->
    if (typecheck_parameters arguments params call_scope) then
      let func_scope = new_environment decl_scope
      in apply_param_bindings func_scope call_scope params arguments;
      let res = eval_func_body func_scope body
      in let resType = typeOf (Primitive res) func_scope
      in if (types_compatible rType resType) then res else raise (IncompatibleTypes (rType, resType))
    else raise (IncompatibleTypes (Unit, Unit))

and internal_new_stream decl_scope f count =
  let return = eval (ApplyLambda (f, [Primitive (ValInt count)])) decl_scope  
  in if return = Undefined || return = Null then None else Some return
;;



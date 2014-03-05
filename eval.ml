(** Evaluation of expressions, given as big step semantics. *) 

open Syntax
open EvalHelperFunctions

(** EVALUATION **)
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
    | NewStream expr -> create_new_stream expr env
    | ReadStream expr -> if is_stream expr env then (let stream = eval expr env in read_stream stream) else raise (NotAStream)
    | Primitive p -> p
    | tmp -> raise (NotYetImplemented tmp)

    and read_stream stream = match stream with
    | ValStream s -> (try (Stream.next s) with Stream.Failure -> Null)

    (* Create new stream *)
    and create_new_stream expr env =
      let stream_const = (
        match expr with
        | Primitive (ValFunction f) -> internal_new_stream env f
        | VarName name -> let (scope, (_, (ValFunction f))) = lookup_variable_all env name in internal_new_stream scope f
      ) in ValStream (Stream.from stream_const) 

    (* Helper function to return the function from a varValue *)
    and func_of_varval = function
    | ValFunction f -> f
    | _ -> raise (NotAFunction "...")

    (* Declare a variable in given environment. if already declare raise CannotRedefineVariable *)
    and do_declare_assig env name var_type expr =
      if is_variable_local env name then 
        raise (CannotRedefineVariable name)
      else
        put_variable env name ((var_type, (eval expr env)):variable); Undefined

    (* Assignment. raises UndefinedVariable if variable is not defined or IncompatibleTypes if the types don't match. *)
    and do_assign env name expr =
      try (
        let (varEnv, (tvar, _)) = lookup_variable_all env name
        and tval = typeOf expr env
        in if types_compatible tval tvar then put_variable varEnv name (tvar, (eval expr env)) else (raise (IncompatibleTypes (tvar, tval))); Undefined
      ) with Not_found -> (raise (UndefinedVariable name))

    (* Fetch a variable by name from the given environment (or any parent environments)
     * raises UninitializedVariable if the value is Undefined
     * raise UndefinedVariable if the variable is not found in any of the environments *)
    and fetch_variable env name =
      try (
        let var = lookup_variable env name
        in match var with
            | (_, Undefined) -> raise (UninitializedVariable name)
            | (_, v) -> v
      ) with Not_found -> raise (UndefinedVariable name)

    (* Binds the variables from the parameters into arguments in the func_scope environment.
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

  (* Calls the given function in the given call_scope. Note that the function scope inherits the declaration scope, not the call scope. *)
  and apply_function call_scope decl_scope f params = match f with
  | NativeFunc (_, name, arguments) -> run_native_code name params 
  | Func (retrunType, arguments, body) ->
        if (validate_parameters arguments params call_scope) then
            let func_scope = new_environment decl_scope
            in apply_param_bindings func_scope call_scope params arguments; eval_func_body func_scope body
        else raise (IncompatibleTypes (Unit, Unit))

    and internal_new_stream decl_scope f count =
       let return = eval (ApplyLambda (f, [Primitive (ValInt count)])) decl_scope in
         if return = Undefined then None else Some return

  (* Arithmetics *)
  | PlusOperator (e1, e2) -> plus (eval e1 env) (eval e2 env)
  | MinusOperator (e1, e2) -> minus (eval e1 env) (eval e2 env)
  | MultiplyOperator (e1, e2) -> multiply (eval e1 env) (eval e2 env)
  | DivOperator (e1, e2) -> let n = (eval e2 env) in
         			if n <> 0 then (div (eval e1 env) n) else raise DivisionByZero
  | ExponentOperator (e1, e2) -> exponent (eval e1 env) (eval e2 env)
  | ModOperator (e1, e2) -> (eval e1 env) mod (eval e2 env)
  | NegationOperator e -> - (eval e env)

  (* Equality testing *)
  | Equal (e1, e2) -> ValBoolean (equal (eval e1 env) (eval e2 env))  (* We may or may not need this ValBoolean there *)
  | NonEqual (e1, e2) -> ValBoolean (nonEqual (eval e1 env) (eval e2 env)) (* We may or may not need this ValBoolean there *)

  (* Order relation operators *)
  | Less (e1, e2) -> ValBoolean (less (eval e1 env) (eval e2 env))
  | Greater (e1, e2) -> ValBoolean (greater (eval e1 env) (eval e2 env))
  | LessEqual (e1, e2) -> ValBoolean (lessEqual (eval e1 env) (eval e2 env))
  | GreaterEqual (e1, e2) -> ValBoolean (greaterEqual (eval e1 env) (eval e2 env))

  (* Boolean logic operators *)
  | Not e -> not (eval e env)
  | Or (e1, e2) -> ValBoolean (eval e1 env) || (eval e2 env) (* We may or may not need this ValBoolean there *)
  | And (e1, e2) -> ValBoolean (eval e1 env) && (eval e2 env) (* We may or may not need this ValBoolean there *)

  (* If statement operators *)
  | If (e1, e2) -> if (eval e1 env) then (eval e2 env)
  | IfElse (e1, e2, e3) -> if (eval e1 env) then (eval e2 env) else (eval e3 env)

  (* Loops *)
  | ForLoop (e1, e2, e3, e4) -> begin 
				   let (env2 = new_environmet env) in
					eval e1 env2
				   while (check_bool (eval e2 env2)) do
					begin
					   eval_func_body env2 e4
					   eval e3 env2
					end
				   done
				end

  | WhileLoop (e1, e2) -> let (env2 = new_environment env) in 
			     while (check_bool (eval e1 env2)) do
				eval_func_body env2 e2
			     done
 
  | DoWhileLoop (e1, e2) -> let (env2 = new_environmnet env) in
			    	begin
				   eval_func_body env2 e1
				   while (check_bool (eval e2 env2)) do
					eval_func_body env2 e1
				   done
				end
;; 

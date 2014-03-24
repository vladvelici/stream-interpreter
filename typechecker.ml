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

  | PlusOperator (e1, e2) -> let t1 = typeOf e1 env and t2 = typeOf e2 env in type_of_numeric_op t1 t2
  | MinusOperator (e1, e2) -> let t1 = typeOf e1 env and t2 = typeOf e2 env in type_of_numeric_op t1 t2
  | MultiplyOperator (e1, e2) -> let t1 = typeOf e1 env and t2 = typeOf e2 env in type_of_numeric_op t1 t2
  | DivOperator (e1, e2) -> let t1 = typeOf e1 env and t2 = typeOf e2 env in type_of_numeric_op t1 t2

  | ExponentOperator (e1, e2) -> let t1 = typeOf e1 env and t2 = typeOf e2 env in type_of_numeric_op t1 t2
  | ModOperator (e1, e2) -> let t1=typeOf e1 env and t2=typeOf e2 env in 
    (match t1, t2 with
     | Int, Int -> Int
     | Stream Int, Stream Int -> Stream Int
     | Stream Int, Int -> Stream Int
     | Int, Stream Int -> Stream Int
     | _, _ -> Unit  
    )

  | Equal (_, _) -> Boolean
  | NonEqual (_, _) -> Boolean
  | Or (_, _) -> Boolean
  | And (_, _) -> Boolean
  | Less (_, _) -> Boolean
  | Greater (_, _) -> Boolean
  | LessEqual (_, _) -> Boolean
  | GreaterEqual (_, _) -> Boolean
  | Not _ -> Boolean

  | NegationOperator e -> let t = typeOf e env in type_of_numeric_op t t 

  | If (_, _) -> Unit
  | IfElse (_, _, _) -> Unit
  | ForLoop (_, _, _, _) -> Unit
  | WhileLoop (_, _) -> Unit
  | DoWhileLoop (_, _) -> Unit

  | Primitive (ValInt _) -> Int
  | Primitive (ValFloat _) -> Float
  | Primitive (ValBoolean _) -> Boolean
  | Primitive (Null) -> Unit
  | Primitive (Undefined) -> Unit
  | Primitive (ValStream (t, _)) -> Stream t
  | Primitive (ValFunction (Func (_, t, arglist, _))) -> Function (t, (typelist_of_arglist arglist))
  | Primitive (ValFunction (NativeFunc (t, _, typelist))) -> Function (t, (typelist))

and type_of_numeric_op t1 t2 = match t1, t2 with
  | Int, Int -> Int
  | Float, Int -> Float
  | Int, Float -> Float
  | Float, Float -> Float

  | Stream Int, Int -> Stream Int
  | Int, Stream Int -> Stream Int

  | Stream Int, Float -> Stream Float
  | Float, Stream Int -> Stream Float

  | Stream Float, Int -> Stream Float
  | Int, Stream Float -> Stream Float

  | Stream Float, Float -> Stream Float
  | Float, Stream Float -> Stream Float

  | Stream Int, Stream Int -> Stream Int
  | Stream Int, Stream Float -> Stream Float
  | Stream Float, Stream Int -> Stream Float
  | Stream Float, Stream Float -> Stream Float

  | Unit, _ -> Unit
  | _, Unit -> Unit

  | a, b -> let accepted = [Int; Float; Stream Int; Stream Float; Unit] in (type_is_one_of accepted accepted a; type_is_one_of accepted accepted b; Unit) 

and types_compatible_nrop t =
  let accepted = [Int; Float; Stream Int; Stream Float; Unit] in
  type_is_one_of accepted accepted t

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

and check_parameter_types params typelist env = match params, typelist with
  | p :: p_list, t :: t_list -> types_compatible t (typeOf p env); check_parameter_types p_list t_list env
  | [], [] -> ()
  | [], t::_ -> raise (IncompatibleTypes (t, Unit))
  | p::_, [] -> raise (IncompatibleTypes (Unit, (typeOf p env)))

and type_is_one_of tlist tlistexp t2 = match tlist with
  | t :: tl -> if not (t = t2) then type_is_one_of tl tlistexp t2 
  | [] -> raise (IncompatibleTypesList (tlistexp, t2))

(* check if t1 is compatible with t2 (a variable of t1 can accept a t2). Undefined and Null have type Unit which is accepted by any type 
 * If the types are not compatible it raises an exception *)
and types_compatible t1 t2 = if not (t1 = t2 || t2 = Unit) then raise (IncompatibleTypes (t1, t2))

(* check if t1 is the same as t2. Undefined and Null (type Unit) are not accepted as valid types for t2
 * If the types are not identical it raises an exception *)
and types_identical t1 t2 = if not (t1 = t2) then raise (IncompatibleTypes (t1, t2))


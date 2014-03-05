(** Evaluation of expressions, given as big step semantics. *) 

open Syntax

(** [eval e] evaluates the expression [e] to an integer. It raises an
	expressions if division by zero occurs. *)
let rec eval exp env = match exp with
  | Primitive e -> e

  (* Arithmetics *)
  | PlusOperator (e1, e2) -> (eval e1 env) + (eval e2 env)
  | MinusOperator (e1, e2) -> (eval e1 env) - (eval e2 env)
  | MultiplyOperator (e1, e2) -> (eval e1 env) * (eval e2 env)
  | DivOperator (e1, e2) -> let n = (eval e2 env) in
         			if n <> 0 then (eval e1 env) / n else failwith "Division by zero"
  | ExponentOperator (e1, e2) -> (eval e1 env) ** (eval e2 env)
  | ModOperator (e1, e2) -> (eval e1 env) mod (eval e2 env)
  | NegationOperator e -> - (eval e env)

  (* Equality testing *)
  | Equal (e1, e2) -> ValBoolean (eval e1 env) == (eval e2 env)  (* We may or may not need this ValBoolean there *)
  | NonEqual (e1, e2) -> ValBoolean (eval e1 env) != (eval e2 env) (* We may or may not need this ValBoolean there *)

  (* Order relation operators *)
  | Less (e1, e2) -> less (eval e1 env) (eval e2 env)
  | Greater (e1, e2) -> greater (eval e1 env) (eval e2 env)
  | LessEqual (e1, e2) -> lessEqual (eval e1 env) (eval e2 env)
  | GreaterEqual (e1, e2) -> greaterEqual (eval e1 env) (eval e2 env)

  (* Boolean logic operators *)
  | Not e -> not (eval e env)
  | Or (e1, e2) -> ValBoolean (eval e1 env) || (eval e2 env) (* We may or may not need this ValBoolean there *)
  | And (e1, e2) -> ValBoolean (eval e1 env) && (eval e2 env) (* We may or may not need this ValBoolean there *)

  (* If statement operators *)
  | If (e1, e2) -> if (eval e1 env) then (eval e2 env)
  | IfElse (e1, e2, e3) -> if (eval e1 env) then (eval e2 env) else (eval e3 env)

  (* Loops *)
 (* | ForLoop (e1, e2, e3, e4) -> new_env env 
  | WhileLoop (e1, e2) -> if (eval e1 env) then 
  | DoWhileLoop (e1, e2) -> *)

(** Comparison functions *)
let less n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 < n2
  | ValInt n1, ValFloat n2 -> n1 < n2
  | ValFloat n1, ValInt n2 -> n1 < n2
  | ValFloat n1, ValFloat n2 -> n1 < n2

let greater n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 > n2
  | ValInt n1, ValFloat n2 -> n1 > n2
  | ValFloat n1, ValInt n2 -> n1 > n2
  | ValFloat n1, ValFloat n2 -> n1 > n2

let lessEqual n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 <= n2
  | ValInt n1, ValFloat n2 -> n1 <= n2
  | ValFloat n1, ValInt n2 -> n1 <= n2
  | ValFloat n1, ValFloat n2 -> n1 <= n2

let greaterEqual n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 >= n2
  | ValInt n1, ValFloat n2 -> n1 >= n2
  | ValFloat n1, ValInt n2 -> n1 >= n2
  | ValFloat n1, ValFloat n2 -> n1 >= n2
  

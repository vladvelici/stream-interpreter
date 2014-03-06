(** Helper functions for evaluation **)
open InterpreterObjects
open Exceptions

(* Arithmetics functions *)
let plus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 + n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) +. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 +. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat(n1 +. n2)
  | _, _ -> Undefined;;

let minus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 - n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) -. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 -. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 -. n2)
  | _ , _ -> Undefined;;

let multiply n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 * n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) *. n2)
  | ValFloat n1, ValInt n2 -> ValFloat (n1 *. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 *. n2)
  | _ , _ -> Undefined;;

let div n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 / n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) /. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 /. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 /. n2)
  | _ , _ -> Undefined;;

let exponent n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (int_of_float ((float_of_int n1) ** (float_of_int n2)))
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) ** n2)
  | ValFloat n1, ValInt n2 -> ValFloat (n1 ** (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 ** n2)
  | _ , _ -> Undefined;;

let modulo n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 mod n2)
  | _, _ -> Undefined;;

(* Equality testing functions *)
let equal n1 n2 = ValBoolean (n1 = n2);;

let nonEqual n1 n2 = ValBoolean (not (n1 = n2));;

(* Comparison functions *)
let less n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValBoolean (n1 < n2)
  | ValInt n1, ValFloat n2 -> ValBoolean ((float_of_int n1) < n2)
  | ValFloat n1, ValInt n2 -> ValBoolean (n1 < (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValBoolean (n1 < n2)
  | _ , _ -> Undefined;;

let greater n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValBoolean (n1 > n2)
  | ValInt n1, ValFloat n2 -> ValBoolean ((float_of_int n1) > n2)
  | ValFloat n1, ValInt n2 -> ValBoolean (n1 > (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValBoolean (n1 > n2)
  | _ , _ -> Undefined;;

let lessEqual n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValBoolean (n1 <= n2)
  | ValInt n1, ValFloat n2 -> ValBoolean ((float_of_int n1) <= n2)
  | ValFloat n1, ValInt n2 -> ValBoolean (n1 <= (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValBoolean (n1 <= n2)
  | _ , _ -> Undefined;;

let greaterEqual n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValBoolean (n1 >= n2)
  | ValInt n1, ValFloat n2 -> ValBoolean ((float_of_int n1) >= n2)
  | ValFloat n1, ValInt n2 -> ValBoolean (n1 >= (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValBoolean (n1 >= n2)
  | _ , _ -> Undefined;;

(* Check if the expression is of type boolean and return it if it is *)
let bool_check t = match t with
  | ValBoolean b -> b
  | _ -> raise (IncompatibleTypes (Boolean, Unit));;


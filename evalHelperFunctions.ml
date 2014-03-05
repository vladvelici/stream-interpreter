(** Helper functions for evaluation **)
open InterpreterObjects
open Exceptions
(* Arithmetics functions *)
let plus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 + n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) +. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 +. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat(n1 +. n2)
  | _, _ -> raise (IncompatibleTypes (Unit, Unit));;

let minus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 - n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) -. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 -. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 -. n2)
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit));;

let multiply n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 * n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) *. n2)
  | ValFloat n1, ValInt n2 -> ValFloat (n1 *. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 *. n2)
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit));;

let div n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 / n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) /. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 /. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 /. n2)
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit));;

let exponent n1 n2 = ValFloat (match n1, n2 with
  | ValInt n1, ValInt n2 -> (float_of_int n1) ** (float_of_int n2)
  | ValInt n1, ValFloat n2 -> (float_of_int n1) ** n2
  | ValFloat n1, ValInt n2 -> n1 ** (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 ** n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;

(* Equality testing functions *)
let equal n1 n2 = ValBoolean (match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 == n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) == n2
  | ValFloat n1, ValInt n2 -> n1 == (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 == n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;

let nonEqual n1 n2 = ValBoolean (match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 != n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) != n2
  | ValFloat n1, ValInt n2 -> n1 != (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 != n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;

(* Comparison functions *)
let less n1 n2 = ValBoolean (match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 < n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) < n2	
  | ValFloat n1, ValInt n2 -> n1 < (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 < n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;

let greater n1 n2 = ValBoolean (match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 > n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) > n2
  | ValFloat n1, ValInt n2 -> n1 > (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 > n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;

let lessEqual n1 n2 = ValBoolean (match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 <= n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) <= n2
  | ValFloat n1, ValInt n2 -> n1 <= (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 <= n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;

let greaterEqual n1 n2 = ValBoolean (match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 >= n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) >= n2
  | ValFloat n1, ValInt n2 -> n1 >= (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 >= n2
  | _ , _ -> raise (IncompatibleTypes (Unit, Unit)));;
(* Check if the expression is of type boolean and return it if it is *)
let bool_check t = match t with
  | Primitive (ValBoolean b) -> b
  | _ -> raise (IncompatibleTypes (Boolean, Unit));;


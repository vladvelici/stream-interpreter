(** Helper functions for evaluation **)

(* Arithmetics functions *)
let plus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 + n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) +. n2	
  | ValFloat n1, ValInt n2 -> n1 +. (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 +. n2

let minus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 - n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) -. n2	
  | ValFloat n1, ValInt n2 -> n1 -. (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 -. n2

let multiply n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 * n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) *. n2	
  | ValFloat n1, ValInt n2 -> n1 *. (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 *. n2

let div n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 / n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) /. n2	
  | ValFloat n1, ValInt n2 -> n1 /. (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 /. n2

let exponent n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> (float_of_int n1) ** (float_of_int n2)
  | ValInt n1, ValFloat -> (float_of_int n1) ** n2
  | ValFloat n1, ValInt n2 -> n1 ** (float_of_int n2)
  | ValFloat n1. ValFloat n2 -> n1 ** n2

(* Equality testing functions *)
let equal n1, n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 == n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) == n2
  | ValFloat n1, ValInt n2 -> n1 == (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 == n2

let nonEqual n1, n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 != n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) != n2
  | ValFloat n1, ValInt n2 -> n1 != (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 != n2

(* Comparison functions *)
let less n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 < n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) < n2	
  | ValFloat n1, ValInt n2 -> n1 < (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 < n2

let greater n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 > n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) > n2
  | ValFloat n1, ValInt n2 -> n1 > (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 > n2

let lessEqual n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 <= n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) <= n2
  | ValFloat n1, ValInt n2 -> n1 <= (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 <= n2

let greaterEqual n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> n1 >= n2
  | ValInt n1, ValFloat n2 -> (float_of_int n1) >= n2
  | ValFloat n1, ValInt n2 -> n1 >= (float_of_int n2)
  | ValFloat n1, ValFloat n2 -> n1 >= n2
 
(* Check if the expression is of type boolean and return it if it is *)
let bool_check t = match t with
  | Primitive (ValBoolean t) -> t
  | _ -> raise (IncompatibleTypes Boolean (typeOf t))

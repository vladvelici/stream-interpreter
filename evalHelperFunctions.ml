(** Helper functions for evaluation **)
open InterpreterObjects
open Exceptions

let not_zero = function
    | ValInt n -> if n=0 then raise DivisionByZero
    | ValFloat n -> if n=0.0 then raise DivisionByZero
    | _ -> ();;

let withNextElement stream fnc count = 
  try (
    let nxt = Stream.next stream in Some (fnc nxt)
  ) with Stream.Failure -> None

let withNextElements stream1 stream2 fnc count =
  try (
    let nxt1 = Stream.next stream1
    and nxt2 = Stream.next stream2
    in Some (fnc nxt1 nxt2)
  ) with Stream.Failure -> None

(* Arithmetics functions *)
let rec plus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 + n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) +. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 +. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat(n1 +. n2)

  | ValStream (t1, s1), ValStream (t2, s2) -> ValStream (Typechecker.type_of_numeric_op t1 t2, Stream.from (withNextElements s1 s2 plus))
  | ValStream (t, s), n -> ValStream (t, Stream.from (withNextElement s (fun el -> plus el n)))
  | n, ValStream (t, s) -> ValStream (t, Stream.from (withNextElement s (fun el -> plus n el)))

  | _, _ -> Undefined

and minus n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 - n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) -. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 -. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 -. n2)

  | ValStream (t1, s1), ValStream (t2, s2) -> ValStream (Typechecker.type_of_numeric_op t1 t2, Stream.from (withNextElements s1 s2 minus))
  | ValStream (t, s), n -> ValStream (t, Stream.from (withNextElement s (fun el -> minus el n)))
  | n, ValStream (t, s) -> ValStream (t, Stream.from (withNextElement s (fun el -> minus n el)))

  | _ , _ -> Undefined

and negation = function
    | ValInt n -> ValInt (-n)
    | ValFloat n -> ValFloat (-.n)

    | ValStream (t, s) -> ValStream (t, Stream.from (withNextElement s (fun el -> negation el)))

    | _ -> Undefined

and multiply n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 * n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) *. n2)
  | ValFloat n1, ValInt n2 -> ValFloat (n1 *. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 *. n2)

  | ValStream (t1, s1), ValStream (t2, s2) -> ValStream (Typechecker.type_of_numeric_op t1 t2, Stream.from (withNextElements s1 s2 multiply))
  | ValStream (t, s), n -> ValStream (t, Stream.from (withNextElement s (fun el -> multiply el n)))
  | n, ValStream (t, s) -> ValStream (t, Stream.from (withNextElement s (fun el -> multiply n el)))

  | _ , _ -> Undefined

and div n1 n2 = not_zero n2; match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (n1 / n2)
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) /. n2)	
  | ValFloat n1, ValInt n2 -> ValFloat (n1 /. (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 /. n2)

  | ValStream (t1, s1), ValStream (t2, s2) -> ValStream (Typechecker.type_of_numeric_op t1 t2, Stream.from (withNextElements s1 s2 div))
  | ValStream (t, s), n -> ValStream (t, Stream.from (withNextElement s (fun el -> div el n)))
  | n, ValStream (t, s) -> ValStream (t, Stream.from (withNextElement s (fun el -> div n el)))

  | _ , _ -> Undefined

and exponent n1 n2 = match n1, n2 with
  | ValInt n1, ValInt n2 -> ValInt (int_of_float ((float_of_int n1) ** (float_of_int n2)))
  | ValInt n1, ValFloat n2 -> ValFloat ((float_of_int n1) ** n2)
  | ValFloat n1, ValInt n2 -> ValFloat (n1 ** (float_of_int n2))
  | ValFloat n1, ValFloat n2 -> ValFloat (n1 ** n2)

  | ValStream (t1, s1), ValStream (t2, s2) -> ValStream (Typechecker.type_of_numeric_op t1 t2, Stream.from (withNextElements s1 s2 exponent))
  | ValStream (t, s), n -> ValStream (t, Stream.from (withNextElement s (fun el -> exponent el n)))
  | n, ValStream (t, s) -> ValStream (t, Stream.from (withNextElement s (fun el -> exponent n el)))

  | _ , _ -> Undefined

and modulo n1 n2 = match n1, n2 with
  | ValInt v1, ValInt v2 -> ValInt (v1 mod v2)
  | ValInt v1, ValStream (Int, s) -> ValStream (Int, Stream.from (withNextElement s (fun el -> modulo n1 el)))
  | ValStream (Int, s), ValInt v2 -> ValStream (Int, Stream.from (withNextElement s (fun el -> modulo el n2)))
  | ValStream (Int, s1), ValStream (Int, s2) -> ValStream (Int, Stream.from (withNextElements s1 s2 modulo))
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
  | Undefined -> false
  | Null -> false
  | _ -> raise (IncompatibleTypes (Boolean, Unit));;


open Hashtbl
open InterpreterObjects

type environment = 
    | RootEnv of (string, variable) Hashtbl.t
    | Environment of environment * (string, variable) Hashtbl.t
;;

let new_environment parent = Environment (parent, (Hashtbl.create 10));;

(* lookup_variable_all reutrns the variable /tipe * varVal/ and the environment where it's defined *)
let rec lookup_variable_all env varname = match env with
    | RootEnv ht -> (env, Hashtbl.find ht varname)
    | Environment (p, ht) -> try ( (env, Hashtbl.find ht varname) ) with Not_found -> lookup_variable p varname;;

let lookup_variable env varname = let l = lookup_variable_all env varname in match l with (_, var) -> var;;
let lookup_variable_type env varname = let l = lookup_variable_all env varname in match l with (_, (t, _)) -> t;;
let lookup_variable_value env varname = let l = lookup_variable_all env varname in match l with (_, (_, v)) -> v;;
let lookup_variable_environment env varname = let l = lookup_variable_all env varname in match l with (e, _) -> e;;

let rec is_variable_defined env varname = match env with
    | RootEnv ht -> Hashtbl.mem ht varname
    | Environment (p, ht) -> if (Hashtbl.mem ht varname) then true else is_variable_defined p varname;;

let is_variable_local env varname = match env with
    | RootEnv ht -> Hashtbl.mem ht varname
    | Environment (_, ht) -> Hashtbl.mem ht varname;;

let put_variable env name var = match env with
    | RootEnv ht -> Hashtbl.replace ht name var
    | Environment (_, ht) -> Hashtbl.replace ht name var;;

(*
let root = RootEnv (Hashtbl.create 10);;
let env1 = new_environment root;; 

put_variable root "a" 43;;
put_variable root "b" 2;;
put_variable env1 "c" 55;;
put_variable env1 "d" 66;;

let va = lookup_variable env1 "a";;

match va with (_, v) -> print_int v;;

print_string "\n";;
*)

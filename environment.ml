(** **)

open InterpreterObjects
open Exceptions

let new_environment parent = Environment (parent, (Hashtbl.create 10));;

(* lookup_variable_all reutrns the variable /tipe * varVal/ and the environment where it's defined *)
let rec lookup_variable_all env varname = 
  match env with
  | RootEnv ht -> (try (env, Hashtbl.find ht varname) with Not_found -> raise (UndefinedVariable varname))
  | Environment (p, ht) -> (try ( (env, Hashtbl.find ht varname) ) with Not_found -> lookup_variable_all p varname)
  | NullEnvironment -> raise NullEnvironmentFound;;

let lookup_variable env varname = let l = lookup_variable_all env varname in match l with (_, var) -> var;;
let lookup_variable_type env varname = let l = lookup_variable_all env varname in match l with (_, (t, _)) -> t;;
let lookup_variable_value env varname = let l = lookup_variable_all env varname in match l with (_, (_, v)) -> v;;
let lookup_variable_environment env varname = let l = lookup_variable_all env varname in match l with (e, _) -> e;;

let rec is_variable_defined env varname = match env with
  | RootEnv ht -> Hashtbl.mem ht varname
  | Environment (p, ht) -> if (Hashtbl.mem ht varname) then true else is_variable_defined p varname
  | NullEnvironment -> raise NullEnvironmentFound;;

let is_variable_local env varname = match env with
  | RootEnv ht -> Hashtbl.mem ht varname
  | Environment (_, ht) -> Hashtbl.mem ht varname
  | NullEnvironment -> raise NullEnvironmentFound;;

let put_variable env name var = match env with
  | RootEnv ht -> Hashtbl.replace ht name var
  | Environment (_, ht) -> Hashtbl.replace ht name var
  | NullEnvironment -> raise NullEnvironmentFound;;

let update_variable env varname var =
  let varEnv = lookup_variable_environment env varname
  in put_variable varEnv varname var;;


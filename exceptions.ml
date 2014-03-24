(** Common exceptions **)
open InterpreterObjects

exception CannotRedefineVariable of string;;
exception UndefinedVariable of string;;
exception IncompatibleTypes of tipe * tipe;; (* Expected tipe 1, found tipe 2 *)
exception IncompatibleTypesList of (tipe list) * tipe;;
exception NotAFunction of string;;
exception DivisionByZero;;
exception NotAStream;;
exception NotANumber;;
exception NullEnvironmentFound;;


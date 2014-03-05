(** Common exceptions **)

exception CannotRedefineVariable of string;;
exception UndefinedVariable of string;;
exception UninitializedVariable of string;;
exception IncompatibleTypes of tipe * tipe;; (* Expected tipe 1, found tipe 2 *)
exception NotAFunction of string;;
exception DivisionByZero;;
exception NotYetImplemented of expression;;
exception NotAStream;;

open InterpreterObjects
open Scanf
open Exceptions
open Printer
open Args
open Environment

let no_of_inputs = ref 0
and input_length = ref 0;;

let inputs_read = ref false;;
let inputs_header_read = ref false;;

let inputs = ref (Array.make_matrix 0 0 (ValInt 0));;

let read_headers _ =
  if not !inputs_header_read then
    let f n m = no_of_inputs := n; input_length := m in bscanf !input_channel "%d %d " f;
    inputs := Array.make_matrix !no_of_inputs !input_length (ValInt 0);
    inputs_header_read := true;;

let read t =
  if not !inputs_read then
    for i=0 to !no_of_inputs-1 do
      for j=0 to !input_length-1 do
        match t with
        | Int -> let f element = !inputs.(i).(j) <- (ValInt element) in
          bscanf !input_channel " %d" f
        | Float -> let f element = !inputs.(i).(j) <- (ValFloat element) in
          bscanf !input_channel " %f" f
        | _ -> raise NotANumber
      done
    done;
  inputs_read := true;;

let stream_from_array arr t count = read t; if count < Array.length arr then Some (arr.(count)) else None;;

let output_format_stream s = let count = ref 0 and l = ref [] in (
    let f e = count:=!count+1; l:=e :: !l in Stream.iter f s;
    (!count, List.rev !l)
  );;

let rec print_list = function 
    [] -> ()
  | e::l -> print_string (string_of_primitive e) ; print_string " " ; print_list l;;

let print_stream (count, lst) = print_int count; print_string "\n"; print_list lst; print_string "\n";;  

let reverse_stream stream = let process_stream s = (let l = ref [] in ( let f e = l:=e :: !l in Stream.iter f s; !l ))
  in Stream.of_list (process_stream stream);;

let functions_tbl = Hashtbl.create 15;;

let reg name retType args fnc = Hashtbl.replace functions_tbl name (retType, args, fnc);;

(* Defining all the native functions  *)

reg "no_of_inputs" Int [] (fun _ -> read_headers 0; ValInt !no_of_inputs);;
reg "input_length" Int [] (fun _ -> read_headers 0; ValInt !input_length);; 

reg "input" (Stream Int) [Int] (function
    | [ValInt nr] -> read_headers 0; ValStream (Int, Stream.from (stream_from_array !inputs.(nr) Int))
    | _ -> raise (IncompatibleTypes (Int, Unit))
  );;

reg  "output" Unit [Stream Int] (function
    | [ValStream (t, s)] -> read_headers 0; print_stream (output_format_stream s); Undefined
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

reg "reverse" (Stream Int) [Stream Int] (function
    | [ValStream (t, s)] -> read_headers 0; ValStream (t, (reverse_stream s))
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

(* float native functions *)
reg "input_float" (Stream Float) [Int] (function 
    | [ValInt nr] -> read_headers 0; ValStream (Float, Stream.from (stream_from_array !inputs.(nr) Float))
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

reg "output_float" Unit [Stream Float] (function 
    | [ValStream (t, s)] -> read_headers 0; print_stream (output_format_stream s); Undefined
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

reg "reverse_float" (Stream Float) [Stream Float] (function
    | [ValStream (t, s)] -> read_headers 0; ValStream (t, (reverse_stream s))
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

(* type conversions *)
reg "int_of_float" Int [Float] (function
    | [ValFloat v] -> ValInt (int_of_float v)
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

reg "float_of_int" Float [Int] (function
    | [ValInt v] -> ValFloat (float_of_int v)
    | _ -> raise (IncompatibleTypes (Unit, Unit))
  );;

(* Iterates through the hashtable that holds all the native functions and binds them into the given (expecting root) environment *)
let rec init env =
  (let f name (ret, arg, fnc) = bind env name ret arg in
   Hashtbl.iter f functions_tbl)

and run name params =
  (let (_, _, fnc) = Hashtbl.find functions_tbl name
   in fnc params)

and bind env name returnType argumentsType  =
  put_variable env name ((Function (returnType, argumentsType)), (ValFunction (NativeFunc (returnType, name, argumentsType))));;


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

let run_native_code name params = match name with 
    | "no_of_inputs" -> read_headers 0; ValInt !no_of_inputs
    | "input_length" -> read_headers 0; ValInt !input_length

    (* int native functions *)
    | "input" -> (match params with [ValInt nr] -> read_headers 0; ValStream (Int, Stream.from (stream_from_array !inputs.(nr) Int)))
    | "output" -> (match params with [ValStream (t, s)] -> read_headers 0; print_stream (output_format_stream s); Undefined)
    | "reverse" -> (match params with [ValStream (t, s)] -> read_headers 0; ValStream (t, (reverse_stream s)))

    (* float native functions *)
    | "input_float" -> (match params with [ValInt nr] -> read_headers 0; ValStream (Float, Stream.from (stream_from_array !inputs.(nr) Float)))
    | "output_float" -> (match params with [ValStream (t, s)] -> read_headers 0; print_stream (output_format_stream s); Undefined)
    | "reverse_float" -> (match params with [ValStream (t, s)] -> read_headers 0; ValStream (t, (reverse_stream s)))

    | _ -> Null;;

let register_native_function env name returnType argumentsType  =
    put_variable env name ((Function (returnType, argumentsType)), (ValFunction (NativeFunc (returnType, name, argumentsType))));;

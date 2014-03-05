open InterpreterObjects
open Scanf

let file = "p2.txt";; 

let no_of_inputs = ref 0
and input_length = ref 0;;

let input_channel = Scanning.open_in file;;
let f n m = no_of_inputs := n; input_length := m in bscanf input_channel "%d %d " f;;

let inputs = Array.make_matrix !no_of_inputs !input_length 0;;

for i=0 to !no_of_inputs-1 do
    for j=0 to !input_length-1 do
         let f element = inputs.(i).(j) <- element in
        bscanf input_channel "%d " f
    done
done;;

let stream_from_array arr count = if count < Array.length arr then Some (ValInt arr.(count)) else None;;

let run_native_code name params = match name with 
    | "no_of_inputs" -> ValInt !no_of_inputs
    | "input_length" -> ValInt !input_length
    | "get_input" -> match params with [ValInt nr] -> ValStream (Int, Stream.from (stream_from_array inputs.(nr)))
    | _ -> Null;;

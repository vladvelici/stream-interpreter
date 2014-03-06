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

let output_format_stream s = let count = ref 0 and l = ref [] in (
    let f (ValInt e) = count:=!count+1; l:=e :: !l in Stream.iter f s;
    (!count, List.rev !l)
);;

let rec print_list = function 
    [] -> ()
    | e::l -> print_int e ; print_string " " ; print_list l;;

let print_stream (count, lst) = print_int count; print_string "\n"; print_list lst; print_string "\n";;  

let run_native_code name params = match name with 
    | "no_of_inputs" -> ValInt !no_of_inputs
    | "input_length" -> ValInt !input_length
    | "input" -> (match params with [ValInt nr] -> ValStream (Int, Stream.from (stream_from_array inputs.(nr))))
    | "output" -> (match params with [ValStream (t, s)] -> print_stream (output_format_stream s); Undefined)
    | _ -> Null;;



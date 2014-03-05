open InterpreterObjects
open Scanf

let file = "p1.txt";; 

let no_of_inputs = ref 0
and input_length = ref 0;;

let input_channel = Scanning.open_in file;;
let f n m = no_of_inputs := n; input_length := m in bscanf input_channel "%d %d " f;;

let inputs = Array.make !no_of_inputs (Array.make !input_length 0);;

for i=0 to !no_of_inputs-1 do
    let current_input = inputs.(i) in
    for j=0 to !input_length-1 do
        let f element = current_input.(j) <- element in
        bscanf input_channel "%d " f
    done
done

let run_native_code name params = match name with 
    | "no_of_inputs" -> ValInt !no_of_inputs
    | "input_length" -> ValInt !input_length
    | "get_input" -> Undefined
    | _ -> Null;;

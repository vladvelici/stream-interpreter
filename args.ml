open Scanf

let input_channel = ref Scanning.stdin;;
let interactive = ref false;;
let program_file = ref stdin;;

let read_arguments _ = 
  if Array.length Sys.argv == 2 then
    program_file := open_in Sys.argv.(1)
  else if Array.length Sys.argv == 3 then
    begin
      program_file := stdin;
      interactive := true;
      input_channel := Scanning.open_in Sys.argv.(2)
    end
  else
    invalid_arg "Invalid arguments";;


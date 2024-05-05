open Core

let read_lines () = In_channel.(input_lines stdin)
let ( >> ) f g x = g (f x)
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false

let () =
  read_lines ()
  |> List.map ~f:(String.split ~on:' ' >> List.sum (module Int) ~f:int_of_string)
  |> list_to_tuple2
  |> fun (a, b) -> printf "%d\n" (a - b + 1)

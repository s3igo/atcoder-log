open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false

let () =
  let _ = read_line () in
  let ab = read_lines () |> List.map ~f:(string_to_int_list >> list_to_tuple2) in
  let sum_a = List.sum (module Int) ~f:fst ab in
  List.(map ab ~f:(fun (a, b) -> sum_a - a + b) |> max_elt ~compare:Int.compare)
  |> Option.iter ~f:(printf "%d\n")

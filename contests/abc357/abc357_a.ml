open Core

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false
let zipi l = l |> List.(zip_exn (init (length l) ~f:Fn.id))

let () =
  let _, m = read_line () |> string_to_int_list |> list_to_tuple2 in
  let h = read_line () |> string_to_int_list in
  h |> zipi
  |> List.fold_until ~init:m
       ~f:(fun acc (i, x) -> if acc - x >= 0 then Continue (acc - x) else Stop i)
       ~finish:(fun _ -> List.length h)
  |> printf "%d\n"

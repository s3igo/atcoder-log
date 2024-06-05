open Core
open Scanf

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string

let () =
  let _, k = sscanf (read_line ()) "%d %d" Tuple2.create in
  read_line () |> string_to_int_list
  |> List.fold ~init:(1, k) ~f:(fun (cnt, rem) x ->
         if x <= rem then (cnt, rem - x) else (cnt + 1, k - x))
  |> fst |> printf "%d\n"

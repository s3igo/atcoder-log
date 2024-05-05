open Core
open Scanf

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string

let () =
  let n, _ = sscanf (read_line ()) "%d %d" Tuple2.create in
  let t = read_line () |> string_to_int_list in
  List.(
    sort t ~compare:Int.compare |> group ~break:Int.( <> ) >>| length
    |> count ~f:(fun x -> x % 2 = 1))
  |> fun x -> n - x |> printf "%d\n"

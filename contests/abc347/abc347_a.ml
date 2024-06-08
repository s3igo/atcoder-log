open Core
open Scanf

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let int_list_to_string = List.map ~f:string_of_int >> String.concat ~sep:" "

let () =
  let _, k = sscanf (read_line ()) "%d %d" Tuple2.create in
  List.(read_line () |> string_to_int_list |> filter ~f:(fun x -> x % k = 0) >>| (Fn.flip ( / )) k)
  |> int_list_to_string |> print_endline

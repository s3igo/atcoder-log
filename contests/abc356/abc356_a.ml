open Core
open Scanf

let ( >> ) f g x = g (f x)
let int_list_to_string = List.map ~f:string_of_int >> String.concat ~sep:" "

let split2 ~l ~r lst =
  let prefix, suffix = List.split_n lst l in
  let body, suffix = List.split_n suffix (r - l) in
  (prefix, body, suffix)

let () =
  scanf "%d %d %d" @@ fun n l r ->
  let prefix, body, suffix = n |> List.init ~f:(fun i -> i + 1) |> split2 ~l:(l - 1) ~r in
  prefix @ List.rev body @ suffix |> int_list_to_string |> print_endline

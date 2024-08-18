open Core
open Poly

let read_line () = In_channel.(input_line_exn stdin)
let list_to_tuple3 = function [ a; b; c ] -> (a, b, c) | _ -> assert false
let cond b = if b then "Yes" else "No"

let () =
  read_line () |> String.to_list |> list_to_tuple3 |> fun (a, b, c) ->
  (a = 'R' || (b = 'R' && c = 'M')) |> cond |> print_endline

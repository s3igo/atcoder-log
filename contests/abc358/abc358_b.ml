open Core
open Poly

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false

let peek_int x =
  printf "%d\n" x;
  x

let () =
  let _, a = read_line () |> string_to_int_list |> list_to_tuple2 in
  let prev = ref 0 in
  read_line () |> string_to_int_list
  |> List.iter ~f:(fun x -> prev := Int.max !prev x + a |> peek_int)

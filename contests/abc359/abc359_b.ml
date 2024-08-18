open Core
open Poly

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string

let windows3 l =
  let rec aux acc = function
    | a :: (b :: c :: _ as t) -> aux ((a, b, c) :: acc) t
    | _ -> List.rev acc
  in
  aux [] l

let () =
  let _ = read_line () in
  read_line () |> string_to_int_list |> windows3
  |> List.count ~f:(fun (a, b, c) -> a = c)
  |> printf "%d\n"

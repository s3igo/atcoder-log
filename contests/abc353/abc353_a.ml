open Core

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let split_first = function [] -> None | h :: t -> Some (h, t)

let () =
  let _ = read_line () in
  read_line () |> string_to_int_list |> split_first |> Option.value_exn |> fun (h, t) ->
  t
  |> List.findi ~f:(fun i x -> x > h)
  |> Option.map ~f:(fun (i, _) -> i + 2)
  |> Option.value ~default:(-1) |> printf "%d\n"

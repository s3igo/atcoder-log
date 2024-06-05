open Core
open Poly
open Scanf

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let zipi l = l |> List.(zip_exn (init (length l) ~f:Fn.id))

let () =
  let n = read_line () |> int_of_string in
  let sc = List.(read_lines () >>| fun s -> sscanf s "%s %d" Tuple2.create) in
  let s = sc |> List.sum (module Int) ~f:snd in
  List.(sc >>| fst |> sort ~compare:ascending |> zipi |> (Fn.flip nth_exn) (s % n))
  |> snd |> print_endline

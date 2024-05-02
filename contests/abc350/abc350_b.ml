open Core
open Scanf

let read_line () = In_channel.(input_line_exn stdin)
let string_to_int_list s = String.split ~on:' ' s |> List.map ~f:int_of_string

let () =
  let n, _ = sscanf (read_line ()) "%d %d" Tuple2.create in
  let t = read_line () |> string_to_int_list in
  List.fold t ~init:(Array.create ~len:n true) ~f:(fun acc x ->
      acc.(x - 1) <- not acc.(x - 1);
      acc)
  |> Array.count ~f:Fn.id |> printf "%d\n"

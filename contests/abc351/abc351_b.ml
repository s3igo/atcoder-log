open Core
open Scanf

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)

module List = struct
  include List

  let position2 ~f a b =
    List.findi (List.zip_exn a b) ~f:(fun _ (x, y) -> f x y)
    |> Option.map ~f:fst
end

let () =
  let n = read_line () |> Int.of_string in
  let a, b = read_lines () |> (Fn.flip List.split_n) n in
  List.position2 ~f:Char.( <> )
    (String.concat a |> String.to_list)
    (String.concat b |> String.to_list)
  |> Option.map ~f:(fun i -> ((i / n) + 1, (i % n) + 1))
  |> Option.value_exn
  |> fun (i, j) -> printf "%d %d\n" i j

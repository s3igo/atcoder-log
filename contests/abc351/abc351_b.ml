open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)

let position2 ~f a b =
  List.(zip_exn a b |> findi ~f:(fun _ (x, y) -> f x y)) |> Option.map ~f:fst

let () =
  let n = read_line () |> Int.of_string in
  let a, b = read_lines () |> (Fn.flip List.split_n) n in
  position2 ~f:Char.( <> )
    String.(concat a |> to_list)
    String.(concat b |> to_list)
  |> Option.map ~f:(fun i -> ((i / n) + 1, (i % n) + 1))
  |> Option.iter ~f:(fun (i, j) -> printf "%d %d\n" i j)

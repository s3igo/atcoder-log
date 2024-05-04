open Core
open Scanf

let () =
  scanf "%d" @@ fun n ->
  List.init ~f:(fun x -> if (x + 1) % 3 = 0 then 'x' else 'o') n
  |> String.of_char_list |> print_endline

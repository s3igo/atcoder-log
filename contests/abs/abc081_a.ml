open Core
open Scanf

let () =
  scanf "%s" @@ fun ss ->
  ss |> String.to_list |> List.count ~f:(Char.equal '1') |> printf "%d\n"

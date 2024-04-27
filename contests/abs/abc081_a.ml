open Core
open Scanf

let () =
  scanf "%s" @@ String.to_list
  |> List.count ~f:(Char.equal '1')
  |> printf "%d\n"

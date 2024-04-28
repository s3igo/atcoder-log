open Core
open Scanf

let contains l r x =
  let chars = string_of_int x |> String.to_list in
  List.(chars >>| String.make 1 |> sum (module Int) ~f:int_of_string)
  |> fun sum -> l <= sum && sum <= r

let () =
  scanf "%d %d %d" @@ fun n a b ->
  Iter.(1 -- n |> filter (contains a b) |> sum) |> printf "%d\n"

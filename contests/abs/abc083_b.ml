open Core
open Scanf

let contains ~lower ~upper x = lower <= x && x <= upper

let satisfies l r x =
  let chars = string_of_int x |> String.to_list in
  List.(chars >>| String.make 1 |> sum (module Int) ~f:int_of_string) |> contains ~lower:l ~upper:r

let () =
  scanf "%d %d %d" @@ fun n a b -> Iter.(1 -- n |> filter (satisfies a b) |> sum) |> printf "%d\n"

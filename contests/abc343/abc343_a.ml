open Core
open Scanf

let () =
  scanf "%d %d" @@ fun a b ->
  Iter.(0 -- 9 |> find (fun x -> if x <> a + b then Some x else None))
  |> Option.iter ~f:(printf "%d\n")

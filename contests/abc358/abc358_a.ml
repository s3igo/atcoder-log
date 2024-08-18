open Core
open Poly
open Scanf

let cond b = if b then "Yes" else "No"
let () = scanf "%s %s" @@ fun s t -> (s = "AtCoder" && t = "Land") |> cond |> print_endline

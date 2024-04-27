open Core
open Scanf

let () = scanf "%d %d %d %s" @@ fun a b c s -> printf "%d %s\n" (a + b + c) s

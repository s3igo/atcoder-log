open Core
open Scanf

let ceil_div x y =
  if x + y - 1 < 0 && (x + y - 1) % y <> 0 then ((x + y - 1) / y) - 1 else (x + y - 1) / y

let () = scanf "%d" @@ (Fn.flip ceil_div) 10 |> printf "%d\n"

open Core
open Scanf

let flat_matrix m =
  let rows, cols, sub_rows, sub_cols =
    Array.(length m, length m.(0), length m.(0).(0), length m.(0).(0).(0))
  in
  Iter.(
    0 -- (rows - 1) >>= fun i ->
    0 -- (sub_rows - 1) >>= fun j ->
    0 -- (cols - 1) >>= fun k -> return m.(i).(k).(j))
  |> Iter.to_list |> Array.concat |> Array.to_list
  |> List.chunks_of ~length:(sub_cols * cols)
  |> List.map ~f:List.to_array |> List.to_array

let rec carpet n =
  if n = 0 then [| [| '#' |] |]
  else
    let subcarpet = carpet (n - 1) in
    let full = Array.make_matrix ~dimx:3 ~dimy:3 subcarpet in
    full.(1).(1) <- Array.make_matrix ~dimx:Int.(3 ** (n - 1)) ~dimy:Int.(3 ** (n - 1)) '.';
    full |> flat_matrix

let () = scanf "%d" @@ carpet |> Array.map ~f:String.of_array |> Array.iter ~f:print_endline

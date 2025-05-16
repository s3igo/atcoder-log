let folder (t0, x0, y0) (t, x, y) =
    let dt = t - t0
    let dx = x - x0 |> abs
    let dy = y - y0 |> abs
    let d = dx + dy

    if d <= dt && dt - d |> System.Int64.IsEvenInteger then
        Some(t, x, y)
    else
        None

let n = stdin.ReadLine() |> int

fun _ -> stdin.ReadLine().Split() |> Array.map int64 |> FSharpx.Collections.Array.toTuple
|> Array.init n
|> Array.toList
|> FSharpPlus.Data.List.foldM folder (0L, 0L, 0L)
|> Option.isSome
|> fun b -> if b then "Yes" else "No"
|> printfn "%s"

stdin.ReadLine() |> ignore

stdin.ReadLine().Split()
|> Array.map int64
|> Array.sortDescending
|> Array.fold (fun (l, r) x -> x + r, l) (0L, 0L)
||> (-)
|> abs
|> printfn "%d"

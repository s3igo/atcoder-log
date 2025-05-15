let both f (a, b) = f a, f b

stdin.ReadLine() |> ignore

stdin.ReadLine().Split()
|> Array.map uint64
|> Array.sortDescending
|> Array.toList
|> fun xs -> List.foldBack (fun x (evens, odds) -> x :: odds, evens) xs ([], [])
|> both List.sum
||> (-)
|> printfn "%d"

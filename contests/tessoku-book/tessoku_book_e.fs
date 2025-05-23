open FSharpPlus.Operators

let inRange (l, r) n = l <= n && n <= r

let [| n; k |] = stdin.ReadLine().Split() |> Array.map int64

(+) <!> [| 1L .. n |] <*> [| 1L .. n |]
|> Array.map ((-) k)
|> Array.filter (inRange (1L, n))
|> Array.length
|> printfn "%d"

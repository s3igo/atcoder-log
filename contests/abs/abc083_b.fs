let rec digits n =
    let struct (d, r) = System.Math.DivRem(n, 10UL)
    if n = 0UL then [] else r :: digits d

let inRange (l, r) n = l <= n && n <= r

let [| n; a; b |] = stdin.ReadLine().Split() |> Array.map uint64

[| 1UL .. n |]
|> Array.filter (digits >> List.sum >> inRange (a, b))
|> Array.sum
|> printfn "%d"

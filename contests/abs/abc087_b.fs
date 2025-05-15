let [| a; b; c; x |] = stdin.ReadToEnd().Split()[0..3] |> Array.map uint64

let sums =
    [| for i in 0UL .. a do
           for j in 0UL .. b do
               for k in 0UL .. c -> 500UL * i + 100UL * j + 50UL * k |]

sums |> Array.filter ((=) x) |> Array.length |> printfn "%d"

let n = stdin.ReadLine() |> int

stdin.ReadToEnd().Split()[0 .. n - 1]
|> Array.distinct
|> Array.length
|> printfn "%d"

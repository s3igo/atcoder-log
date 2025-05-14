stdin.ReadLine().Split()
|> Array.map uint64
|> FSharpx.Collections.Array.toTuple
||> (*)
|> System.UInt64.IsOddInteger
|> fun b -> if b then "Odd" else "Even"
|> printfn "%s"

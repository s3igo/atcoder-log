let bool f t cond = if cond then t else f

stdin.ReadLine().Split()
|> Array.map uint64
|> FSharpx.Collections.Array.toTuple
||> (*)
|> System.UInt64.IsOddInteger
|> bool "Even" "Odd"
|> printfn "%s"

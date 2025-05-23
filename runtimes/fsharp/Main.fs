open FSharpPlus.Operators
let arrayToTuple = FSharpx.Collections.Array.toTuple
let bool f t cond = if cond then t else f
let inRange (l, r) n = l <= n && n <= r

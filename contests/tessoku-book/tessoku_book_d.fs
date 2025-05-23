stdin.ReadLine()
|> int
|> fun x -> System.Convert.ToString(x, 2).PadLeft(10, '0')
|> printfn "%s"

main :: IO ()
main = readLn @Int >>= \n -> putStrLn . take n $ map (\i -> if i `mod` 3 == 0 then 'x' else 'o') ([1 ..] :: [Int])

divCeil :: Int -> Int -> Int
divCeil a b = (a + b - 1) `div` b

main :: IO ()
main = print . flip divCeil 10 =<< readLn

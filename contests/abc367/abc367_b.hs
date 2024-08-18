main :: IO ()
main = getLine >>= putStrLn . reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

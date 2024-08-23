import Data.List.Split

main :: IO ()
main = putStrLn . last . splitOn "." =<< getLine

main :: IO ()
main = do
  s <- getLine
  putStrLn $ init s ++ "4"

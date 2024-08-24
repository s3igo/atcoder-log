main :: IO ()
main = do
  n <- readLn @Int
  putStrLn $ "L" ++ replicate n 'o' ++ "ng"

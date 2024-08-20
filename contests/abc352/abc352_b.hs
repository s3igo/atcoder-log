import Data.List

main :: IO ()
main = do
  s <- getLine
  putStrLn . unwords . map (show . succ) . findIndices (`elem` s) =<< getLine

-- TODO: WA

import Data.List

main :: IO ()
main = do
  [(n, _)] <- reads <$> getLine
  print . (n -) . length . filter odd . map length . group . sort . map (read @Int) . words =<< getLine

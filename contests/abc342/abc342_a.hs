import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  let only = head . head . filter ((== 1) . length) . group $ sort s
  print . succ . fromJust $ elemIndex only s

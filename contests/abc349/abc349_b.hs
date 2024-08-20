import Data.Bool (bool)
import Data.List

main :: IO ()
main = putStrLn . bool "No" "Yes" . all ((== 2) . length) . group . sort . map length . group . sort =<< getLine

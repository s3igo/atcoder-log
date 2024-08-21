import Data.List
import Data.List.Extra (nubOrd)

substrings :: [a] -> [[a]]
substrings xs = [ys | xs' <- inits xs, ys <- tails xs', not $ null ys]

main :: IO ()
main = print . length . nubOrd . substrings =<< getLine

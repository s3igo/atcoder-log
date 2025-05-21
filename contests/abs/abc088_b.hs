import Data.List qualified as L
import Flow

main :: IO ()
main = getLine >> getLine >>= words .> map (read @Int) .> L.sortBy (flip compare) .> L.foldl' (\(l, r) x -> (x + r, l)) (0, 0) .> uncurry (-) .> abs .> print

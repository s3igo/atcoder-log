import Data.List.Extra (nubOrd)
import Flow

main :: IO ()
main = getLine >> getContents >>= lines .> map (read @Int) .> nubOrd .> length .> print

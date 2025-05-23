import Control.Monad (join)
import Flow

main :: IO ()
main = readLn @Int >>= join (*) .> print

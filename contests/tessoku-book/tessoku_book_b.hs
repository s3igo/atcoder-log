import Data.Bool (bool)
import Data.Functor ((<&>))
import Flow

main :: IO ()
main = do
  [_, x] <- getLine <&> words
  getLine >>= words .> elem x .> bool "No" "Yes" .> putStrLn

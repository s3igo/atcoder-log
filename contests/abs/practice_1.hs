import Data.Functor ((<&>))
import Flow

main :: IO ()
main = do
  a <- readLn @Int
  [b, c] <- getLine <&> words .> map (read @Int)
  s <- getLine
  [a + b + c |> show, s] |> unwords |> putStrLn

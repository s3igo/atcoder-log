import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Flow

main :: IO ()
main = do
  [[_, k], ps, qs] <- getLine <&> words .> map (read @Int) |> replicateM 3
  (+) <$> ps <*> qs |> elem k |> bool "No" "Yes" |> putStrLn

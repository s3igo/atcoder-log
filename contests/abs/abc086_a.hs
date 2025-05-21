import Data.Bool (bool)
import Flow

main :: IO ()
main = getLine >>= words .> map (read @Int) .> product .> odd .> bool "Even" "Odd" .> putStrLn

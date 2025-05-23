import Text.Printf (printf)

main :: IO ()
main = readLn @Int >>= printf "%010b\n"

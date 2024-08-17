import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS

addAsterisks :: Int -> [BS.ByteString] -> [BS.ByteString]
addAsterisks n = map $ \s -> s `BS.append` BS.replicate (n - BS.length s) '*'

main :: IO ()
main = do
  n <- readLn @Int
  ss <- replicateM n BS.getLine
  let maxLen = maximum $ map BS.length ss
  mapM_ (BS.putStrLn . BS.reverse . BS.dropWhile ('*' ==)) . BS.transpose $ addAsterisks maxLen ss

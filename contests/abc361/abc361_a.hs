import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Foldable qualified as S
import Data.List
import Data.Sequence qualified as S

main :: IO ()
main = do
  [_, k, x] <- unfoldr trimInt <$> BS.getLine
  putStrLn . unwords . map show . S.toList . S.insertAt k x . S.unfoldr trimInt =<< BS.getLine
  where
    trimInt = BS.readInt . BS.dropWhile isSpace

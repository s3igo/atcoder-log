import Control.Monad (foldM_)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.Map qualified as M

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

type HashBag a = M.Map a Int

emptyBag :: HashBag a
emptyBag = M.empty

insert :: (Ord a) => a -> HashBag a -> HashBag a
insert x = M.insertWith (+) x 1

-- occur :: (Ord a) => a -> HashBag a -> Int
-- occur = M.findWithDefault 0

delete :: (Ord a) => a -> HashBag a -> HashBag a
delete = M.update (\n -> if n > 1 then Just (n - 1) else Nothing)

main :: IO ()
main = do
  q <- readLn @Int
  let aux bag _ = do
        query <- readInts
        case query of
          [1, x] -> do
            return $ Main.insert x bag
          [2, x] -> do
            return $ Main.delete x bag
          [3] -> do
            print $ M.size bag
            return bag
          _ -> undefined
  foldM_ aux emptyBag [1 .. q]

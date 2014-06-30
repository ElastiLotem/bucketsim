import Control.Monad
import System.Random
import qualified Data.Vector.Mutable as V

bucketCount = 1024

unsafeModify v ix f = do
  x <- V.unsafeRead v ix
  V.unsafeWrite v ix (f x)
  return x

simulate = do
  gen <- newStdGen
  buckets <- V.new bucketCount
  mapM_ (flip (V.unsafeWrite buckets) 0) [0..bucketCount-1]
  let go iter 0 _ = return iter
      go iter emptyCount (r:rs) = do
        oldCount <- unsafeModify buckets (r `mod` bucketCount) (+1)
        let emptyCount' | oldCount == 0 = emptyCount - 1
                        | otherwise     = emptyCount
        go (iter+1) emptyCount' rs
  go 0 bucketCount (randoms gen)

fi = fromIntegral

main = do
  xs <- replicateM 100 simulate
  mapM_ print xs
  putStrLn $ "Average is: " ++ show (sum xs / fi (length xs))
  putStrLn $ "Predicted average is: " ++ show (log (fi bucketCount) * fi bucketCount)

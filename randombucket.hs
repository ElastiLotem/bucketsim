{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
-- import Data.List (foldl1')
import System.Random
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

unsafeModify :: VM.IOVector a -> Int -> (a -> a) -> IO a
unsafeModify v ix f = do
  x <- VM.unsafeRead v ix
  VM.unsafeWrite v ix (f x)
  return x

simulate :: Int -> IO (Integer, V.Vector Integer)
simulate bucketCount = do
  gen <- newStdGen
  buckets <- VM.new bucketCount
  mapM_ (flip (VM.unsafeWrite buckets) 0) [0..bucketCount-1]
  let go iter 0 _ = do
        snapshot <- V.freeze buckets
        return (iter, snapshot)
      go iter emptyCount (r:rs) = do
        oldCount <- unsafeModify buckets (r `mod` bucketCount) (+1)
        let emptyCount' | oldCount == 0 = emptyCount - 1
                        | otherwise     = emptyCount
        go (iter+1) emptyCount' rs
      go _ _ [] = error "randoms list ended?"
  go 0 bucketCount (randoms gen)

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

avg :: Fractional a => [a] -> a
avg xs = sum xs / fi (length xs)

-- vectorAdd :: Num c => V.Vector c -> V.Vector c -> V.Vector c
-- vectorAdd = V.zipWith (+)

main :: IO ()
main = forM_ [512, 1024, 2048] $ \bucketCount -> do
  putStrLn $ "-------------------------------------"
  putStrLn $ "Bucket count = " ++ show bucketCount
  putStrLn $ "-------------------------------------"
  let simulationCount = 1000
  xs <- replicateM simulationCount $ simulate bucketCount

  let (iters, snapshots) = unzip xs
  let ratio snapshot = fi (V.maximum snapshot) / fi (V.minimum snapshot)
  let ratios = map ratio snapshots

  let avgIter :: Double
      avgIter = avg (map fi iters)
  putStrLn $ "    Average iter count is: " ++ show avgIter
  let predicted :: Double
      predicted = log (fi bucketCount) * fi bucketCount
  putStrLn $ "Predicted iter average is: " ++ show predicted
  putStrLn $ "      Average / Predicted: " ++ show (avgIter / predicted)

  putStrLn $ "Average ratio is: " ++ show (avg ratios :: Double)
  -- let totalSum = foldl1' vectorAdd snapshots
  -- putStrLn $ "After " ++ show simulationCount ++ " additions ratio is: " ++ show (ratio totalSum)

{-#LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns#-}
-- {-#OPTIONS -fllvm -optlo-mem2reg -optlc-O3 -fexcess-precision -msse2 -funbox-strict-fields#-}
module Main where
import Numeric.Optimization.Algorithms.DifferentialEvolution 
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector as V
import System.Random.MWC
import System.Environment

import Debug.Trace

f4 :: Int -> V.Vector Vector -> V.Vector (Double, Vector)
f4 dim x = 
  let fit = VUB.sum . VUB.map (\x -> x*x-10*cos(2*pi*x)+10)
      res = V.map (\ y -> (fit y, y)) x
  in trace (show res) res

f4B :: Int -> (VUB.Vector Double, VUB.Vector Double)
f4B dim = (VUB.replicate dim (-5), VUB.replicate dim 5)

cases dim = [("f4",(f4B dim,f4 dim))]

createSeed fls = initialize (VUB.fromList fls) >>= save

-- parallelExperiment :: [[Word32]] -> (CaseSpec) -> IO ()
-- parallelExperiment :: [Word32] -> CaseSpec -> IO ()
parallelExperiment seeds casespec = do
  seed <- createSeed seeds
  let dim = 1 -- VUB.length $ fst bounds
      params = defaultParams (return . f4 dim) (f4B dim)
  x <- de params{spop = 50} seed
  print x
  return ()

main :: IO ()
main = do
  dim:testsToRun <- getArgs 
  let seeds = [0..256] -- take 1 $ crunch [1..256] 
  parallelExperiment seeds (head $ cases (read dim))

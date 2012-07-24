{-#LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns#-}
-- {-#OPTIONS -fllvm -optlo-mem2reg -optlc-O3 -fexcess-precision -msse2 -funbox-strict-fields#-}
module Main where
import Numeric.Optimization.Algorithms.DifferentialEvolution 
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!))
import Data.List
import System.Random.MWC
import Control.Monad.ST
import Control.Monad.Reader
import System.Environment
import System.Directory
import Data.Label

f4 :: Int -> Vector -> Double
f4 dim = VUB.sum . VUB.map (\x -> x*x-10*cos(2*pi*x)+10)

f4B :: Int -> (VUB.Vector Double, VUB.Vector Double)
f4B dim = (VUB.replicate dim (-5), VUB.replicate dim 5)

cases dim = [("f4",(f4B dim,f4 dim))]

createSeed fls = initialize (VUB.fromList fls) >>= save

-- parallelExperiment :: [[Word32]] -> (CaseSpec) -> IO ()
-- parallelExperiment :: [Word32] -> CaseSpec -> IO ()
parallelExperiment seeds casespec = do
  seed <- createSeed seeds
  let params = defaultParams (return . f4 3) (f4B 3)
  x <- de params{spop = 10} seed -- undefined -- (f4B dim) undefined)) seed
  print x
  return ()
  -- print ts
{-
          mapM_ (\s -> runDE (run casespec s)) seeds
    where
        run (tfname,(bounds::(VUB.Vector Double,VUB.Vector Double),tf)) s = do
           seed <- createSeed s
           let dim = VUB.length $ fst bounds
           de $ DEArgs (Rand1Exp 0.6 0.70) tf (Just bounds) dim 60 (5000*dim) seed
           ts <- get trace    
           return ()
           -- return $ (reverse ts, $(mkRI) (show s) "DE" tfname (show $ dim))
-}

-- maul n = (map concat . transpose . map permutations . splitToLength n)
-- crunch = concatMap (maul 4) . maul 8

main = do
  dim:testsToRun <- getArgs 
  let seeds = [0..] -- take 1 $ crunch [1..256] 
  parallelExperiment seeds (head $ cases (read dim))
  -- stopGlobalPool
    

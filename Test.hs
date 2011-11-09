{-#LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns#-}
{-#OPTIONS -fllvm -optlo-mem2reg -optlc-O3 -fexcess-precision -msse2 -funbox-strict-fields#-}
module Main where
import Numeric.Optimization.Algorithms.DifferentialEvolution 
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!))
import Data.List
import Utils.List
import System.Random.MWC
-- import RunDB
-- import Control.Concurrent.ParallelIO.Global
import System.Environment
import System.Directory
import Utils.File
import Data.Label

f4 dim = VUB.sum . VUB.map (\x -> x*x-10*cos(2*pi*x)+10)


f4B dim = (VUB.replicate dim (-5), VUB.replicate dim 5)

cases dim = [("f4",(f4B dim,f4 dim))]

createSeed fls = initialize (VUB.fromList fls) >>= save

-- parallelExperiment :: [[Word32]] -> (CaseSpec) -> IO ()
parallelExperiment seeds casespec = do
       inDirectory "results/" $ do
          let ops = map (\s -> runDE (run casespec s)) seeds
          parallel_ $ map persist ops
    where
        run (tfname,(bounds::(VUB.Vector Double,VUB.Vector Double),tf)) s = do
           seed <- liftST $ createSeed s
           let dim = VUB.length $ fst bounds
           de $ DEArgs (Rand1Exp 0.6 0.70) tf (Just bounds) dim 60 (5000*dim) seed
           ts <- getM trace    
           return $ (reverse ts, $(mkRI) (show s) "DE" tfname (show $ dim))

maul n = (map concat . transpose . map permutations . splitToLength n)
crunch = concatMap (maul 4) . maul 8

main = do
      dim:testsToRun <- getArgs 
      let seeds = take 1 $ crunch [1..256] 
      parallelExperiment seeds (head $ cases (read dim))
      stopGlobalPool
    

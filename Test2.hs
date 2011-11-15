{-#LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns, TupleSections#-}
{-#OPTIONS -fllvm -optlo-mem2reg -optlc-O3 -fexcess-precision -msse2 -funbox-strict-fields#-}
module Main where
import Numeric.Optimization.Algorithms.DifferentialEvolution 
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!))
import Data.List
import Utils.List
import Control.Arrow
import System.Random.MWC
import System.Environment
import System.Directory
import Utils.File
import Data.Label
import Data.Monoid
import Graphics.Gnuplot.Simple
import qualified Data.DList as DL

f4 dim = VUB.sum . VUB.map (\x -> x*x-10*cos(2*pi*x)+10)
f4b dim = (VUB.replicate dim (-6)
          ,VUB.replicate dim (6)) 

fsphere :: Int -> VUB.Vector Double -> Double
fsphere dim = VUB.sum . VUB.map (**2)

createSeed fls = initialize (VUB.fromList fls) >>= save

main = do
    seed <- createSeed [11..256]
    let params0 = (defaultParams (f4 10) (f4b 10) (DL.singleton . currentBest))
    let params1 = (defaultParams (f4 10) (f4b 10) (DL.singleton . currentBest))
                 {destrategy = Prox1Bin 0.3 0.7}
    w  <- de params0 seed
    w2 <- de params1 seed
    plotLists [Custom "logscale" ["y"]] $ [map fst . DL.toList $ w, map fst . DL.toList $ w2]
    let ds = transpose . map VUB.toList . map snd $ DL.toList w
    plotListsStyle [Custom "logscale" ["y"]] (map (defaultStyle{plotType = Points},) ds) --[map abs . decreasing . DL.toList $ f, DL.toList s]
    

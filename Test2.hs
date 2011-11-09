{-#LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns#-}
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
    w <- de (defaultParams (f4 10) (f4b 10) 
              (DL.singleton . currentBest)) seed
    plotList [Custom "logscale" ["y"]] $ map fst . DL.toList $  w
    let ds = transpose . map VUB.toList . map snd $ DL.toList w
    plotLists [Custom "logscale" ["y"]] ds --[map abs . decreasing . DL.toList $ f, DL.toList s]
    

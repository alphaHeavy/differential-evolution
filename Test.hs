module Main where
import Numeric.Optimization.Algorithms.DifferentialEvolution 
import qualified Data.Vector.Unboxed as VUB
import System.Random.MWC

f4 dim = VUB.sum . VUB.map (\x -> x*x-10*cos(2*pi*x)+10)
f4B dim = VUB.replicate dim (-5,5)

main = do
      x <- withSystemRandom $ \x -> (save x:: IO Seed)
      print $ runDE $ de $ DEArgs (Rand1Exp 0.1 0.98) (f4 100) (Nothing) 100 60 500000 x
    

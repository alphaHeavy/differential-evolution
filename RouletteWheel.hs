{-#LANGUAGE ViewPatterns, ScopedTypeVariables#-}
module RouletteWheel where

import Control.Arrow
import Control.Monad
import Data.List
import Debug.Trace
import System.Random.MWC as MWC
import Test.QuickCheck as QC 
import Test.QuickCheck.Modifiers
import Control.Monad.Primitive

import Debug.Trace

{-#INLINE rouletteSelection#-}
rouletteSelection
  :: (PrimMonad f, Functor f) =>
     Wheel b -> MWC.Gen (PrimState f) -> f b
rouletteSelection wheel gen = rouletteSelection' wheel `fmap` uniform gen

{-#INLINE rouletteSelection'#-}
rouletteSelection' :: Wheel a -> Double -> a
rouletteSelection' (Wheel wheel) ix = sel wheel
   where
      sel ((f1,x1):xs) 
         | ix < f1             = x1
         | ix >= f1            = sel xs
      sel []                   = error "Sel" 

newtype Wheel a = Wheel [(Double,a)] deriving Show

wheel :: [(Double,a)] -> Wheel a
wheel pop = Wheel . (flip zip) (elems)  . tail . scanl (+) 0 . map (/mass) $ fits
   where 
    mass  = sum fits
    (fits,elems) = unzip pop

-- * Testing

genDist :: QC.Gen [Int]
genDist = do
     let n = 1000
         es :: [(Double,Int)]
         es = [(0.1,i) | i <- [1..10]]
     d <- vectorOf n (choose (0,1::Double))
     return  $ map snd . distrib . map (rouletteSelection' (wheel es)) $ d

unPos (Positive a) = a

distrib :: (Ord a) => [a] -> [(a,Int)]
distrib = map (head &&& length) . group . sort

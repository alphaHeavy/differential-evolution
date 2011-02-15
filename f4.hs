{-#LANGUAGE ScopedTypeVariables, BangPatterns#-}
import Criterion.Main
import qualified Data.Vector.Unboxed as VUB
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as MUB
import Control.DeepSeq
import Control.Monad

f4' dim = VUB.sum . VUB.map (\x -> x*x-10*cos(2*pi*x)+10)

f4  dim x = work 0 (dim-1)
    where
        work !n 0 = n
        work !n !i = let xi = x ! i in n + xi*xi-10*cos(2*pi*xi)+10 


main = defaultMain [
                     bench "replicate" $ nf (\x -> (VUB.sum $ VUB.replicate x (1::Double))) 100
                    ,bench "f4-1-100" $ nf (\x -> f4 x  (VUB.generate x (\i-> fromIntegral $ i*2::Double))) 100
                    ,bench "f4-2-100" $ nf (\x -> f4' x (VUB.generate x (\i-> fromIntegral $ i*2::Double))) 100
                   ]


{-#LANGUAGE ScopedTypeVariables#-}
import Criterion.Main
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector.Unboxed.Mutable as MUB
import Control.DeepSeq
import Control.Monad

instance (NFData a) => NFData (VUB.Vector a) where
    rnf v = v `seq` ()

expCrossover :: Int -> Int -> Int -> VUB.Vector Double -> VUB.Vector Double -> VUB.Vector Double
expCrossover l n' index a b = 
        let n = min n' (l-1)
            m = index + n - l
            nmax = min n (l-index)
        in  VUB.generate l (\i -> if i>=index && i < (index+n) || i < m
                                       then a VUB.! i else b VUB.! i )

expCrossover2 :: Int -> Int -> Int -> VUB.Vector Double -> VUB.Vector Double -> VUB.Vector Double
expCrossover2 l n' index a b = 
        let n = min n' (l-1)
            m = index + n - l
            nmax = min n (l-index)
        in  VUB.map (\(e1,e2,i) -> if (i>=index && i<(index+n)) || i<m then e2 else e1) 
                $ VUB.zip3 a b (VUB.enumFromN 0 l)

expCrossover3 :: Int -> Int -> Int -> VUB.Vector Double -> VUB.Vector Double -> VUB.Vector Double
expCrossover3 l n' index a b = 
        let n = min n' (l-1)
            m = index + n - l
            nmax = min n (l-index)
        in VUB.modify (\v -> do
                                forM_ [index..n] $ \i -> (MUB.write v i (b VUB.! i))
                                forM_ [0..(index+n-l)]    $ \i -> (MUB.write v i (b VUB.! i))     ) 
                        a

main = defaultMain [
                     bench "replicate" $ nf (\x -> ((VUB.sum $ VUB.replicate x (0::Double)),(VUB.sum $ VUB.replicate x (1::Double)))) 100

                    ,bench "exp-1-100" $ nf (\x -> (VUB.sum $ expCrossover x 25 90 (VUB.replicate x 0) (VUB.replicate x 1))) 100
                    ,bench "exp-2-100" $ nf (\x -> (VUB.sum $ expCrossover2 x 25 90 (VUB.replicate x 0) (VUB.replicate x 1))) 100
                    ,bench "exp-3-100" $ nf (\x -> (VUB.sum $ expCrossover3 x 25 90 (VUB.replicate x 0) (VUB.replicate x 1))) 100
                   ]

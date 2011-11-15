{-#LANGUAGE ViewPatterns#-}
module Distance where
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V
import Data.Maybe

type DistMap = HashMap Int (HashMap Int Double)

distances :: (a->a->Double) -> [a] -> DistMap
distances dist xs =
   HM.fromList $ [(j,HM.fromList [(i,dist a b)| (a,i) <- zip xs [0..]])
				 | (b,j) <- zip xs [0..]]


rowSum :: Int -> DistMap -> Double
rowSum i (HM.lookup i -> Just hm) = HM.foldr (+) 0 $ hm
rowSum _ _ = error "No such row"

toProbabilities :: DistMap -> DistMap
toProbabilities = HM.map normRow
   where
    normRow hm = let s = HM.foldr (+) 0 hm
	         in HM.map (\x -> 1-x/s) hm



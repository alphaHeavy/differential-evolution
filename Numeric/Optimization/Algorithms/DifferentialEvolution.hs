{-# LANGUAGE ScopedTypeVariables, ViewPatterns, BangPatterns, DeriveDataTypeable, RecordWildCards, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, TypeFamilies, UndecidableInstances, TemplateHaskell #-}
module Numeric.Optimization.Algorithms.DifferentialEvolution where

import qualified Control.Parallel.Strategies as CPS
import Control.DeepSeq

import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as VUB

import Data.Function
import Data.Record.Label
import Control.Monad
import Control.Arrow ((&&&))
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Primitive
import System.Random.MWC
import Data.Word

type Vector  = VUB.Vector Double
type Bounds  = VUB.Vector (Double,Double)
type Fitness = Vector -> Double
type Budget = Int

data DEParams s = DEParams {_gen :: GenST s
                           ,_ec  :: Int
                           ,_pop :: V.Vector (Double,Vector)
                           ,_trace :: [(Int,Double,String)] }

$(mkLabels [''DEParams])

newtype DeMonad s a = DE (StateT (DEParams s) (ST s) a) deriving (Monad)

instance MonadState (DEParams s) (DeMonad s) where
    get   = DE $ get
    put a = DE $ put a


instance HasPRNG (DeMonad s) where
    type S (DeMonad s) = s
    withGen op = get >>= \x -> op (_gen x)

liftST :: ST s a -> DeMonad s a
liftST op = DE $ lift op

runDE :: (forall s. DeMonad s a) -> a
runDE de = runST (let (DE a ) = de in evalStateT a 
                                       $ DEParams (error "Generator uninitialized") 
                                                  0
                                                  (error "Population unitialized") 
                                                  [])
logPoint :: String -> DeMonad s ()
logPoint tx = do
    (cb,_) <- getM pop >>= return . V.minimumBy (compare`on`fst)
    e <- getM ec
    modM trace ((e,cb,tx):)


-- HasPRNG related
class HasPRNG m where
    type S m :: *
    withGen :: (Gen (S m) -> m b) -> m b

selectRandom :: (PrimMonad m) => Gen (PrimState m) -> Int -> V.Vector a -> m [a]
selectRandom gen n vec = do
    idx <- replicateM n (randomIndex (V.length vec) gen)
    return $ map (vec !) idx

{-#INLINE randomIndex#-}
randomIndex ub gen = uni >>= return . floor . (fromIntegral ub*)
 where 
  uni = do x <- (uniform gen)
           return (x::Float)  

expVariate lambda gen = work 0
    where
     work !n = do
               x :: Float <- uniform gen
               if x<lambda then work (n+1)
                           else return (n::Int)
-- -- --
-- These should also have their own Vector - module
(<+>),(<->) :: Vector -> Vector -> Vector
a <+> b = VUB.zipWith (+) a b
a <-> b = VUB.zipWith (-) a b

(*|)  :: Double -> Vector -> Vector
a *|  b = VUB.map (a*) b
-- -- --


-- These should go to their own module
data Strategy  = Rand1Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Rand2Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Rand1Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double} 
               | Rand2Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double} 
               deriving (Show)

type DEStrategy s = GenST s -> Int -> Vector -> V.Vector (Double, Vector) -> DeMonad s Vector
strategy :: Strategy -> DEStrategy s
strategy (Rand1Bin{..}) = strat' f cr rand1 binCrossover
strategy (Rand2Bin{..}) = strat' f cr rand2 binCrossover
strategy (Rand1Exp{..}) = strat' f cr rand1 expCrossover
strategy (Rand2Exp{..}) = strat' f cr rand2 expCrossover 
strat' f cr m co =  \gen l parent pop -> liftST (m gen f pop >>= \x -> co l cr parent x gen)
{-# INLINE strategy #-}


rand1 :: GenST s -> Double -> V.Vector (Double,Vector) -> ST s Vector
rand1 gen f pop = do
            [x1,x2,x3] <- selectRandom gen 3 $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3))

rand2 :: GenST s -> Double -> V.Vector (a, Vector) -> ST s Vector
rand2 gen f pop = do
            [x1,x2,x3,x4,x5] <- selectRandom gen 5 $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3)) <+> (f *| (x4 <-> x5))

expCrossover
  :: (PrimMonad m, VUB.Unbox t) =>
     Int -> Float -> VUB.Vector t -> VUB.Vector t -> Gen (PrimState m) -> m (VUB.Vector t) 
expCrossover l cr a b gen = do
        n :: Int <- expVariate cr gen
        index <- randomIndex l gen
        let m = index + n - l
        return $ VUB.map (\(e1,e2,i) -> if (i>=index && i<(index+n)) || i<m then e2 else e1) 
                $ VUB.zip3 a b (VUB.enumFromN 0 l)

binCrossover
  :: (PrimMonad m, VUB.Unbox t) =>
     Int -> Float -> VUB.Vector t -> VUB.Vector t -> Gen (PrimState m) -> m (VUB.Vector t)

binCrossover l cr a b gen = do
        randoms :: VUB.Vector Float <- VUB.replicateM l (uniform gen)
        index   :: Int <- randomIndex l gen
        return $ VUB.map (\(x,e1,e2,i) -> if x<cr || i == index then e2 else e1) 
                $ VUB.zip4 randoms a b (VUB.enumFromN 0 l)

data DEArgs = DEArgs {
                      destrategy :: Strategy
                     ,fitness     :: Fitness
                     ,bounds      :: Maybe Bounds
                     ,dim         :: Int
                     ,spop        :: Int
                     ,budget      :: Budget
                     ,seed        :: Seed}

saturateVector bounds  x = VUB.zipWith saturate bounds x
saturate (lb,ub) x = min (max x lb) ub


de :: DEArgs -> DeMonad s (Double,Vector)
de DEArgs{..} = do
    liftST (restore seed) >>= setM gen
    init <- withGen $ \g -> liftST (V.replicateM spop $ uniformVector g dim) 
    pop =: V.map (fitness &&& id) init
    work
    where
     strat = strategy destrategy
     work = do logPoint ""
               e <- getM ec 
               if e > budget 
                then getM pop >>= return . V.minimumBy (compare`on`fst)
                else getM pop >>= deStep strat bounds fitness >>= setM pop >> work

deStep :: DEStrategy s -> Maybe Bounds -> Fitness 
          -> V.Vector (Double,Vector)
          -> DeMonad s (V.Vector (Double,Vector))  
deStep strat bounds fitness pop = do 
        modM ec (+V.length pop)
        withGen $ \g -> (V.mapM (candidate g) pop) 
   where 
    l = VUB.length . snd . V.head $ pop
    candidate gen orig@(ft,a) = do
        w <- strat gen l a pop
        return (select orig (postProcess w))
    select (fa,a) b@(fitness -> fb) = if fa < fb then (fa,a) else (fb,b) 
    postProcess x | Just bds <-  bounds = saturateVector bds x
                  | otherwise           = x


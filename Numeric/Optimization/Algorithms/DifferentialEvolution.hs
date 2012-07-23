{-# LANGUAGE ScopedTypeVariables, ViewPatterns, BangPatterns, DeriveDataTypeable, RecordWildCards, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, TypeFamilies, UndecidableInstances, TemplateHaskell,TypeOperators, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
-- |Module    : Numeric.Optimization.Algorithms.DifferentialEvolution
-- Copyright   : (c) 2011 Ville Tirronen
-- License     : MIT
--
-- Maintainer  : ville.tirronen@jyu.fi
--
-- This module implements basic version of Differential Evolution algorithm
-- for finding minimum of possibly multimodal and non-differentiable real valued
-- functions. 
-- 
module Numeric.Optimization.Algorithms.DifferentialEvolution(
        -- * Basic Types
        Vector, Bounds, Fitness, Budget, DeMonad, DEParams(..),
        -- * Control Parameters
        DEArgs(..), Strategy(..), strategy, defaultParams,
        -- * Accessing internal state of the algorithm
        evaluationCount, population, currentBest,
        -- * Executing the algorithm
        runDE, de, de', deStep) where

import qualified Control.Parallel.Strategies as CPS
import Control.DeepSeq

import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector.Unboxed.Mutable as MUB

import Control.Applicative
import Control.Arrow ((&&&),first,second)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State hiding (get,gets,modify)
import Control.Monad.Writer
import Data.Function
import Data.Label as L
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Word
import System.Random.MWC
import qualified Control.Monad.State as ST (get,modify)
import qualified Data.HashMap.Strict as HM

import RouletteWheel
import Distance

import qualified Debug.Trace as DBG

-- Essential Types

-- |Vector type for storing trial points
type Vector  = VUB.Vector Double
-- |Type for storing function domain orthope. (Structure of arrays seems 
--   more efficient than array of structures)
type Bounds  = (VUB.Vector Double, VUB.Vector Double)
-- |Fitness function type
type Fitness m = Vector -> m Double
-- |Termination condition type. (Currently just a hard limit on evaluation count)
type Budget = Int

-- * Optimizer State

data DEParams = DEParams {_ec  :: Int
                         ,_pop :: V.Vector (Double,Vector)}

$(mkLabels [''DEParams])

-- |The current number of fitness evaluations
evaluationCount :: DEParams :-> Int
evaluationCount = ec

-- |The current set of active trial points
population :: DEParams :-> V.Vector (Double,Vector)
population = pop

-- | Get the current best indevidual and Fitness
currentBest :: DEParams -> (Double,Vector)
currentBest = V.minimumBy (comparing fst) . get pop

-- | Population Size
sPop = V.length . get pop

-- | Problem dimension
dimension = VUB.length . snd . V.head . get pop 

-- * Context monad

-- |Monad for storing optimization trace and random number generator

type DeMonad m gen w  = ReaderT gen (WriterT w m) 
runDE :: (PrimMonad m) => DeMonad m (Generator m) w a -> (Generator m) -> m (a,w)
runDE m g = runWriterT (runReaderT m g)

type Generator m = Gen (PrimState m)

selectRandom :: (PrimMonad m) => Gen (PrimState m) -> Int -> V.Vector a -> m [a]
selectRandom !gen !n vec = do
    idx <- randomIndexes (V.length vec) n gen
    return $ map (V.unsafeIndex vec) idx

{-#INLINE randomIndex#-}
randomIndex !ub gen = uni >>= return . floor . (fromIntegral ub*)
 where 
  uni = do x <- (uniform gen)
           return (x::Float)  

{-#INLINE randomIndexes#-}
randomIndexes :: (PrimMonad m) => Int -> Int -> Generator m -> m [Int]
randomIndexes !ub n gen = sel n []
 where 
    sel 0 ss = return ss
    sel n ss = do
      x <- randomIndex ub gen
      if x `elem` ss 
          then (sel n ss)
          else sel (n-1) (x:ss)

randomP
  :: (PrimMonad m, Functor m, Eq a) =>
     Wheel a -> Int -> Generator m -> m [a]

randomP probs n gen = sel n []
 where 
    sel 0 ss = return ss
    sel n ss = do
      x <- rouletteSelection probs gen
      if x `elem` ss  
          then (sel n ss)
          else sel (n-1) (x:ss)
    

expVariate !lambda gen = do
    u :: Float <- uniform gen
    return . round $ (- log (u))/(1-lambda)

-- --
-- These should also have their own Vector - module
(<+>),(<->) :: Vector -> Vector -> Vector
a <+> b = VUB.zipWith (+) a b
a <-> b = VUB.zipWith (-) a b

(*|)  :: Double -> Vector -> Vector
a *|  b = VUB.map (a*) b

(|**)  :: Vector -> Double -> Vector
a |**  b = VUB.map (**b) a
-- -- --


-- |Different strategies for optimization
data Strategy  = Rand1Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Prox1Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Rand2Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Rand1Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double} 
               | Rand2Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double} 
               deriving (Show)

type DEStrategy m w = Vector -> Generator m -> DeMonad m (Generator m) w Vector

-- |Convert a Showable strategy into executable one
strategy :: (PrimMonad m, Monoid w) => Int -> V.Vector (Double,Vector) -> Strategy -> DEStrategy m w
strategy l pop (Rand1Bin{..}) = strat' l pop (rand1 f) (binCrossover cr)
strategy l pop (Rand2Bin{..}) = strat' l pop (rand2 f) (binCrossover cr)
strategy l pop (Rand1Exp{..}) = strat' l pop (rand1 f) (expCrossover cr)
strategy l pop (Rand2Exp{..}) = strat' l pop (rand2 f) (expCrossover cr)

strategy l pop (Prox1Bin{..}) = \parent gen -> (lift . lift) $ do
                        c <- rand2 f pop gen
                        expCrossover cr l parent c gen
    where
    norm a b = VUB.sum . VUB.map sqrt $ (a<->b) |** 2
    getWheel pt ps = let dists = V.map (norm pt &&& id) ps
                         s = V.sum . V.map fst $ dists  
                         probs = V.map (\(d,x) -> (1-d/s,x)) dists 
                     in wheel . V.toList $ probs
    

{-# INLINE strategy #-}

strat' l pop m co =  \parent gen -> (lift . lift) (m pop gen >>= \x -> co l parent x gen)

rand1Weighted :: (PrimMonad m, Functor m) => Wheel Vector -> Generator m -> Double -> m Vector

rand1Weighted weights gen f = do
            [x1,x2,x3] <- randomP weights 3 gen -- V.toList $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3))

rand1 :: PrimMonad m => Double -> V.Vector (Double, Vector) -> Generator m -> m Vector
rand1 f pop gen = do
            [x1,x2,x3] <- selectRandom gen 3 $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3))

rand2 :: PrimMonad m => Double -> V.Vector (Double, Vector) -> Generator m -> m Vector
rand2 f pop gen = do
            [x1,x2,x3,x4,x5] <- selectRandom gen 5 $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3)) <+> (f *| (x4 <-> x5))

expCrossover
  :: (PrimMonad m, VUB.Unbox t) =>
      Float ->Int -> VUB.Vector t -> VUB.Vector t -> Generator m -> m (VUB.Vector t) 
expCrossover cr l a b gen = do
        n' :: Int <- expVariate cr gen
        index <- randomIndex l gen
        let n = min n' (l-1)
            m = index + n - l
            nmax = min n (l-index)
            overflow = index+n-l
            end = min l (index+n)
        return (moduloReplacement1 index n l a b)

moduloReplacement1 start length dim a b 
    = VUB.modify (\v -> do 
                        MUB.write v start (b VUB.! start)
                        forM_ ([0..overflow-1]++[start..end-1]) $ \i -> (MUB.write v i (b VUB.! i)))
                        a
    where
     overflow = start+length-dim
     end      = min dim $ start+length


binCrossover
  :: (PrimMonad m, VUB.Unbox t) =>
     Float -> Int -> VUB.Vector t -> VUB.Vector t -> Gen (PrimState m) -> m (VUB.Vector t)

binCrossover cr l a b gen = do
        randoms :: VUB.Vector Float <- VUB.replicateM l (uniform gen)
        index   :: Int <- randomIndex l gen
        return $ VUB.map (\(x,e1,e2,i) -> if x<cr || i == index then e2 else e1) 
               $ VUB.zip4 randoms a b (VUB.enumFromN 0 l)

-- |Parameters for algorithm execution
data DEArgs m w = DEArgs {
  -- |Mutation strategy
   destrategy :: Strategy
  -- |N-dimensional function to be minimized
  ,fitness     :: Fitness m
  -- |N-orthope describing the domain of the fitness function
  ,bounds      :: Bounds
  -- |N, this should work well with dimension from 2-100
  ,dim         :: Int
  -- |Number of individuals to use in optimization (60 is good)
  ,spop        :: Int
  -- |Number of fitness function evaluations until termination
  ,budget      :: Budget
  -- |Seed value for random number generation. (You often wish 
  --  to replicate results without storing)
  ,seed        :: Seed
  -- |Tracing function. You can use this to track parameters of the evolution.
  --  (This is how you access the final fitness, result vector and fitness 
  --  trace).
  ,trace       :: (Monoid w) => DEParams -> w
  }

-- |Generate a parameter setting for DE. 
defaultParams
  :: Fitness m
  -> (VUB.Vector Double, VUB.Vector Double)
  -> (Monoid w => DEParams -> w)
  -> DEArgs m w
defaultParams fitness bounds = DEArgs (Rand1Exp 0.9 0.70) 
                                         fitness bounds dimension 
                                         60 (5000*dimension) seed
                        where seed = runST (create >>=save)
                              dimension = VUB.length . fst $ bounds  

saturateVector :: Bounds -> VUB.Vector Double -> VUB.Vector Double
saturateVector (mn,mx) x = VUB.generate d (\i -> min (mx!i) (max (mn!i) (x!i)))
    where
     (!) = (VUB.!)
     d = VUB.length x

-- | Run DE algorithm over a 'PrimMonad' such as 'IO' or 'ST'. Returns the fitness trace 
--   specified by 'DEArgs.trace'
de :: (Functor m, Monoid w, PrimMonad m) => DEArgs m w -> Seed -> m w 
de args seed = restore seed >>= runDE (de' args) >>= return . snd

-- | Create a Differential Evolution process inside DeMonad
de' :: (Functor m, Monoid w, PrimMonad m) => DEArgs m w -> DeMonad m (Generator m) w ()
de' DEArgs{..} = do
    gen  <- lift . lift $ restore seed
    init <- lift . lift $ V.replicateM spop (scale <$> uniformVector gen dim)
    population <- V.forM init $ \ x -> (,x) <$> (lift . lift $ fitness x)
    work gen $ DEParams (V.length population) population
    where
     (lb,ub) = (fst bounds, snd bounds)
     scale x = VUB.zipWith3 (\l u x -> l+x*(u-l)) lb ub x
     work gen state = do 
               lift . tell $ trace state
               if get ec state > budget 
                then return () 
                else deStep destrategy bounds fitness gen state >>= work gen

-- | Single iteration of Differential Evolution. Could be an useful building block
--   for other algorithms as well.
deStep :: (Functor m, Monoid w, PrimMonad m) =>
          Strategy -> Bounds -> Fitness m
          -> Generator m
          -> DEParams 
          -> DeMonad m (Generator m) w DEParams 
deStep strate bounds fitness gen params = do 
    newPop <- V.mapM (update gen) . get pop $ params
    return . modify ec (+ sPop params) . set pop newPop $ params 
   where 
    l = dimension params
    strat = strategy l (get pop params) strate
    update gen orig@(ft,a) = do
      val <- saturateVector bounds <$> strat a gen
      fitnessVal <- lift . lift $ fitness val
      return $! minBy fst orig (fitnessVal, val)

minBy :: Ord x => (a -> x) -> a -> a -> a
minBy f a b | f a <= f b  = a 
            | otherwise  = b

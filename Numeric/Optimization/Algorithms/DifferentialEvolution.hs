{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

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
module Numeric.Optimization.Algorithms.DifferentialEvolution
  ( -- * Basic Types
    Vector
  , Bounds
  , Fitness
  , Budget
  , DeMonad
  , DEParams(..)
  , -- * Control Parameters
    DEArgs(..)
  , Strategy(..)
  , strategy
  , defaultParams
  , -- * Accessing internal state of the algorithm
    evaluationCount
  , population
  , currentBest
  , -- * Executing the algorithm
    runDE
  , de
  , de'
  , deStep
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector.Unboxed.Mutable as MUB

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Data.Label as L
import Data.Ord
import System.Random.MWC

-- Essential Types

-- |
-- Vector type for storing trial points
type Vector  = VUB.Vector Double

-- |
-- Type for storing function domain orthope. (Structure of arrays seems
-- more efficient than array of structures)
type Bounds  = (VUB.Vector Double, VUB.Vector Double)

-- |
-- Fitness function type
type Fitness i m o = Vector -> m o

-- |
-- Termination condition type. (Currently just a hard limit on evaluation count)
type Budget = Int

-- * Optimizer State

data DEParams o = DEParams
  { _ec  :: {-# UNPACK #-} !Int
  , _pop :: V.Vector (o, Vector)
  }

$(mkLabels [''DEParams])

-- |
-- The current number of fitness evaluations
evaluationCount
  :: DEParams o :-> Int
evaluationCount = ec

-- |
-- The current set of active trial points
population
  :: DEParams o :-> V.Vector (o, Vector)
population = pop

-- |
-- Get the current best individual and Fitness
currentBest
  :: Ord o
  => DEParams o
  -> (o, Vector)
currentBest = V.minimumBy (comparing fst) . get pop

-- |
-- Population Size
sPop
  :: DEParams o
  -> Int
sPop = V.length . get pop

-- |
-- Problem dimension
dimension
  :: DEParams o
  -> Int
dimension = VUB.length . snd . V.head . get pop

-- * Context monad

-- |
-- Monad for storing optimization trace and random number generator
type DeMonad gen m  = ReaderT gen m

runDE
  :: PrimMonad m
  => DeMonad (Generator m) m a
  -> Generator m
  -> m a
runDE m g = runReaderT m g

type Generator m = Gen (PrimState m)

selectRandom
  :: PrimMonad m
  => Gen (PrimState m)
  -> Int
  -> V.Vector a
  -> m [a]
selectRandom gen n vec = do
  idx <- randomIndexes (V.length vec) n gen
  return $! map (V.unsafeIndex vec) idx

randomIndex
  :: forall a b m . (Integral a, Integral b, PrimMonad m)
  => a
  -> Gen (PrimState m)
  -> m b
{-# INLINE randomIndex #-}
randomIndex ub gen = do
  uni <- uniform gen
  return $! floor (fromIntegral ub * (uni :: Float))

randomIndexes
  :: PrimMonad m
  => Int
  -> Int
  -> Generator m
  -> m [Int]
{-# INLINE randomIndexes #-}
randomIndexes ub n gen =
  let sel 0 ss = return ss
      sel n ss = do
        x <- randomIndex ub gen
        if x `elem` ss
          then sel n ss
          else sel (n-1) (x:ss)
  in sel n []

expVariate
  :: (Integral b, PrimMonad m)
  => Float
  -> Gen (PrimState m)
  -> m b
expVariate lambda gen = do
  uni <- uniform gen
  return $! round $ (- log uni)/(1-lambda)

-- --
-- These should also have their own Vector - module
(<+>),(<->) :: Vector -> Vector -> Vector
a <+> b = VUB.zipWith (+) a b
a <-> b = VUB.zipWith (-) a b

(*|) :: Double -> Vector -> Vector
a *| b = VUB.map (a*) b

-- |
-- Different strategies for optimization
data Strategy
  = Rand1Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
  | Prox1Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
  | Rand2Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
  | Rand1Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
  | Rand2Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
    deriving (Show)

-- |
-- Convert a Showable strategy into executable one
strategy
  :: PrimMonad m
  => Int
  -> V.Vector (o, Vector)
  -> Strategy
  -> VUB.Vector Double
  -> Generator m
  -> m (VUB.Vector Double)
{-# INLINE strategy #-}
strategy l pop Rand1Bin{..} = strat' l pop (rand1 f) (binCrossover cr)
strategy l pop Rand2Bin{..} = strat' l pop (rand2 f) (binCrossover cr)
strategy l pop Rand1Exp{..} = strat' l pop (rand1 f) (expCrossover cr)
strategy l pop Rand2Exp{..} = strat' l pop (rand2 f) (expCrossover cr)
strategy l pop Prox1Bin{..} = \parent gen -> do
  c <- rand2 f pop gen
  expCrossover cr l parent c gen

strat'
  :: Monad m
  => Int 
  -> V.Vector (o, Vector)
  -> (V.Vector (o, Vector) -> Generator m -> m Vector)
  -> (Int -> Vector -> Vector -> Generator m -> m Vector)
  -> Vector
  -> Generator m 
  -> m Vector
strat' l pop m co parent gen = m pop gen >>= \x -> co l parent x gen

rand1
  :: PrimMonad m
  => Double
  -> V.Vector (o, Vector)
  -> Generator m
  -> m Vector
rand1 f pop gen = do
  [x1,x2,x3] <- selectRandom gen 3 $ V.map snd pop
  return $! x1 <+> (f *| (x2 <-> x3))

rand2
  :: PrimMonad m
  => Double
  -> V.Vector (o, Vector)
  -> Generator m
  -> m Vector
rand2 f pop gen = do
  [x1,x2,x3,x4,x5] <- selectRandom gen 5 $ V.map snd pop
  return $! x1 <+> (f *| (x2 <-> x3)) <+> (f *| (x4 <-> x5))

expCrossover
  :: (PrimMonad m, VUB.Unbox a)
  => Float
  -> Int
  -> VUB.Vector a
  -> VUB.Vector a
  -> Generator m
  -> m (VUB.Vector a)
expCrossover cr l a b gen = do
  n' :: Int <- expVariate cr gen
  index <- randomIndex l gen
  let n = min n' (l-1)
  return $! moduloReplacement1 index n l a b

moduloReplacement1
  :: forall a . MUB.Unbox a
  => Int
  -> Int
  -> Int
  -> VUB.Vector a
  -> VUB.Vector a
  -> VUB.Vector a
moduloReplacement1 start length dim a b = VUB.modify step a where
  overflow = start+length-dim
  end      = min dim $ start+length
  step :: MUB.MVector s a -> ST s ()
  step v = do
    MUB.write v start (b VUB.! start)
    forM_ ([0..overflow-1]++[start..end-1]) $ \i -> MUB.write v i (b VUB.! i)

binCrossover
  :: (PrimMonad m, VUB.Unbox a)
  => Float
  -> Int
  -> VUB.Vector a
  -> VUB.Vector a
  -> Gen (PrimState m)
  -> m (VUB.Vector a)
binCrossover cr l a b gen = do
  randoms <- VUB.replicateM l (uniform gen)
  index :: Int <- randomIndex l gen
  return $! VUB.map (\(x,e1,e2,i) -> if x<cr || i == index then e2 else e1)
          $ VUB.zip4 randoms a b (VUB.enumFromN 0 l)

-- |
-- Parameters for algorithm execution
data DEArgs i m o = DEArgs {
  -- | Mutation strategy
    destrategy :: Strategy
  -- | N-dimensional function to be minimized
  , fitness    :: Fitness i m o
  -- | N-orthope describing the domain of the fitness function
  , bounds     :: Bounds
  -- | N, this should work well with dimension from 2-100
  , dim        :: Int
  -- | Number of individuals to use in optimization (60 is good)
  , spop       :: Int
  -- | Number of fitness function evaluations until termination
  , budget     :: Budget
  -- | Seed value for random number generation. (You often wish
  --  to replicate results without storing)
  , seed       :: Seed
  }

-- |
-- Generate a parameter setting for DE.
defaultParams
  :: Fitness i m o
  -> (VUB.Vector Double, VUB.Vector Double)
  -> DEArgs i m o
defaultParams fitness bounds =
  let dimension = VUB.length . fst $ bounds
  in DEArgs
       { destrategy = Rand1Exp 0.9 0.70
       , fitness    = fitness
       , bounds     = bounds
       , dim        = dimension
       , spop       = 60
       , budget     = 5000 * dimension
       , seed       = runST (create >>=save)
       }

saturateVector :: Bounds -> VUB.Vector Double -> VUB.Vector Double
saturateVector (mn,mx) x =
  let (!) = (VUB.!)
      d = VUB.length x
  in VUB.generate d $ \ i -> min (mx!i) (max (mn!i) (x!i))

-- |
-- Run DE algorithm over a 'PrimMonad' such as 'IO' or 'ST'.
de
  :: (Functor m, Ord o, PrimMonad m)
  => DEArgs i m o
  -> Seed
  -> m [(Vector, o)]
de args seed = restore seed >>= runDE (de' args)

swap
  :: (a, b)
  -> (b, a)
swap (a, b) = (b, a)

-- |
-- Create a Differential Evolution process inside DeMonad
de'
  :: forall i m o . (Functor m, Ord o, PrimMonad m)
  => DEArgs i m o
  -> DeMonad (Generator m) m [(Vector, o)]
de' DEArgs{..} = do
  let (lb, ub) = bounds
      scale x  = VUB.zipWith3 (\l u x -> l+x*(u-l)) lb ub x
      work :: Generator m -> DEParams o -> DeMonad (Generator m) m [(Vector, o)]
      work gen state
        | get ec state > budget = return $! fmap swap . V.toList $ get pop state
        | otherwise = deStep destrategy bounds fitness gen state >>= work gen

  gen  <- lift $ restore seed
  init <- lift $ V.replicateM spop (scale <$> uniformVector gen dim)
  population <- V.forM init $ \ x -> (,x) <$> (lift $ fitness x)
  work gen $ DEParams (V.length population) population

-- |
-- Single iteration of Differential Evolution. Could be an useful building block
-- for other algorithms as well.
deStep
  :: (Functor m, Ord o, PrimMonad m)
  => Strategy
  -> Bounds
  -> Fitness i m o
  -> Generator m
  -> DEParams o
  -> DeMonad (Generator m) m (DEParams o)
deStep strate bounds fitness gen params = do
  let l = dimension params
      strat = strategy l (get pop params) strate
      update gen orig@(_ft,a) = lift $ do
        val <- saturateVector bounds <$> strat a gen
        fitnessVal <- fitness val
        return $! minBy fst orig (fitnessVal, val)

  newPop <- V.mapM (update gen) . get pop $ params
  return $! modify ec (+ sPop params) . set pop newPop $ params

minBy
  :: Ord x
  => (a -> x)
  -> a
  -> a
  -> a
minBy f a b
  | f a <= f b = a
  | otherwise  = b

{-# LANGUAGE ScopedTypeVariables, ViewPatterns, BangPatterns, DeriveDataTypeable, RecordWildCards, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, TypeFamilies, UndecidableInstances, TemplateHaskell,TypeOperators, FlexibleContexts #-}
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
import Data.Function
import Data.Label as L
import Data.Monoid
import Data.Ord
import Control.Monad
import Control.Monad.ST
import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State hiding (get,gets,modify)
import qualified Control.Monad.State as ST (get,modify)
import Control.Monad.Primitive
import System.Random.MWC
import Data.Word

-- Essential Types

-- |Vector type for storing trial points
type Vector  = VUB.Vector Double
-- |Type for storing function domain orthope. (Structure of arrays seems 
--   more efficient than array of structures)
type Bounds  = (VUB.Vector Double, VUB.Vector Double)
-- |Fitness function type
type Fitness = Vector -> Double
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
    --let idx = replicate n 0
    idx <- replicateM n (randomIndex (V.length vec) gen)
    return $ map (V.unsafeIndex vec) idx

{-#INLINE randomIndex#-}
randomIndex !ub gen = uni >>= return . floor . (fromIntegral ub*)
 where 
  uni = do x <- (uniform gen)
           return (x::Float)  

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
-- -- --


-- |Different strategies for optimization
data Strategy  = Rand1Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Rand2Bin {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double}
               | Rand1Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double} 
               | Rand2Exp {cr ::{-# UNPACK #-} !Float, f ::{-# UNPACK #-} !Double} 
               deriving (Show)

type DEStrategy m w = Generator m -> Int -> Vector -> V.Vector (Double, Vector) 
                        -> DeMonad m (Generator m) w Vector

-- |Convert a Showable strategy into executable one
strategy :: (PrimMonad m, Monoid w) => Strategy -> DEStrategy m w
strategy (Rand1Bin{..}) = strat' f cr rand1 binCrossover
strategy (Rand2Bin{..}) = strat' f cr rand2 binCrossover
strategy (Rand1Exp{..}) = strat' f cr rand1 expCrossover
strategy (Rand2Exp{..}) = strat' f cr rand2 expCrossover 
{-# INLINE strategy #-}

strat' f cr m co =  \gen l parent pop -> (lift . lift) (m gen f pop >>= \x -> co l cr parent x gen)


rand1 :: (PrimMonad m) => Generator m -> Double -> V.Vector (Double,Vector) -> m Vector
rand1 gen f pop = do
            [x1,x2,x3] <- selectRandom gen 3 $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3))

rand2 :: (PrimMonad m) => Generator m -> Double -> V.Vector (a, Vector) -> m Vector
rand2 gen f pop = do
            [x1,x2,x3,x4,x5] <- selectRandom gen 5 $ V.map snd pop
            return $ x1 <+> (f *| (x2 <-> x3)) <+> (f *| (x4 <-> x5))

expCrossover
  :: (PrimMonad m, VUB.Unbox t) =>
     Int -> Float -> VUB.Vector t -> VUB.Vector t -> Generator m -> m (VUB.Vector t) 
expCrossover l cr a b gen = do
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
     Int -> Float -> VUB.Vector t -> VUB.Vector t -> Gen (PrimState m) -> m (VUB.Vector t)

binCrossover l cr a b gen = do
        randoms :: VUB.Vector Float <- VUB.replicateM l (uniform gen)
        index   :: Int <- randomIndex l gen
        return $ VUB.map (\(x,e1,e2,i) -> if x<cr || i == index then e2 else e1) 
               $ VUB.zip4 randoms a b (VUB.enumFromN 0 l)

-- |Parameters for algorithm execution
data DEArgs w = DEArgs {
                      -- |Mutation strategy
                      destrategy :: Strategy
                      -- |N-dimensional function to be minimized
                     ,fitness     :: Fitness
                      -- |N-orthope describing the domain of the fitness function
                     ,bounds      :: Bounds
                      -- |N, this should work well with dimension from 2-100
                     ,dim         :: Int
                      -- |Number of indeviduals to use in optimization (60 is good)
                     ,spop        :: Int
                      -- |Number of fitness function evaluations until termination
                     ,budget      :: Budget
                      -- |Seed value for random number generation. (You often wish 
                      --  to replicate results without storing)
                     ,seed        :: Seed
                      -- |Tracing function. You can use this to track parameters of the evolution.
                      --  (Ie. This is how you access the final fitness, result vector and fitness 
                      --  trace)
                     ,trace       :: (Monoid w) => DEParams -> w
                     }

-- |Generate a parameter setting for DE. 
defaultParams fitness bounds = DEArgs (Rand1Exp 0.9 0.70) 
                                         fitness bounds dimension 
                                         60 (5000*dimension) seed
                        where seed = runST (create >>=save)
                              dimension = VUB.length . fst $ bounds  
saturateVector :: Bounds -> VUB.Vector Double -> VUB.Vector Double
saturateVector (mn,mx) x = VUB.modify (\m -> go m (MUB.length m-1)) x
    where
     go :: MUB.MVector s Double -> Int -> ST s ()
     go x 0 = return ()
     go !x !i = do
              xi <- MUB.read x i
              when (xi < (mn VUB.! i)) $  MUB.write x i (mn VUB.! i)
              when (xi > (mn VUB.! i)) $  MUB.write x i (mx VUB.! i)

-- | Run DE algorithm over a 'PrimMonad' such as 'IO' or 'ST'. Returns the fitness trace 
--   specified by 'DEArgs.trace'
de :: (Functor m, Monoid w, PrimMonad m) => DEArgs w -> Seed -> m w 
de args seed = restore seed >>= runDE (de' args) >>= return . snd

-- | Create a Differential Evolution process inside DeMonad
de' :: (Functor m, Monoid w, PrimMonad m) => DEArgs w -> DeMonad m (Generator m) w ()
de' DEArgs{..} = do
    gen  <- lift . lift $ restore seed
    init <- lift . lift $ V.replicateM spop (uniformVector gen dim >>= return.scale) 
    let population =  V.map (fitness &&& id) init
        state      = DEParams (V.length population) population
    work gen state
    where
     (lb,ub) = (fst bounds, snd bounds)
     scale x = VUB.zipWith3 (\l u x -> l+x*(u-l)) lb ub x
     strat = strategy destrategy
     work gen state = do 
               lift . tell $ trace state
               if get ec state > budget 
                then return () 
                else deStep strat bounds fitness gen state >>= work gen

-- | Single iteration of Differential Evolution. Could be an useful building block
--   for other algorithms as well.
deStep :: (Functor m, Monoid w, PrimMonad m) =>
          DEStrategy m w -> Bounds -> Fitness 
          -> Generator m
          -> DEParams 
          -> DeMonad m (Generator m) w DEParams 
deStep strat bounds fitness gen params = do 
    newPop <- V.mapM (update gen) . get pop $ params
    return $ modify ec (+ sPop params) . set pop newPop $ params 
   where 
    l = dimension params
    update gen orig@(ft,a) = minBy fst orig 
                             . (fitness &&& id)
                             . saturateVector bounds 
                             <$> strat gen l a (get pop params)

minBy :: Ord x => (a -> x) -> a -> a -> a
minBy f a b | f a <= f b  = a 
            | otherwise  = b


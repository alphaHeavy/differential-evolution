{-# LANGUAGE ScopedTypeVariables, ViewPatterns, BangPatterns, DeriveDataTypeable, RecordWildCards, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ImpredicativeTypes, TypeFamilies, UndecidableInstances, TemplateHaskell,TypeOperators #-}
-- | Module    : Numeric.Optimization.Algorithms.DifferentialEvolution
-- Copyright   : (c) 2011 Ville Tirronen
-- License     : MIT
--
-- Maintainer  : ville.tirronen@jyu.fi
-- Stability   : experimental
-- Portability : portable
--
-- This module implements basic version of Differential Evolution algorithm
-- for finding minimum of possibly multimodal and non-differentiable real valued
-- functions. 
-- 

module Numeric.Optimization.Algorithms.DifferentialEvolution(
        -- * Basic Types
        Vector, Bounds, Fitness, Budget, DeMonad,
        -- * Control Parameters
        DEArgs(..), Strategy(..), strategy, defaultParams,
        -- * Accessing internal state of the algorithm
        evaluationCount, population, optimizationTrace,
        -- * Executing the algorithm
        runDE, de, deStep) where

import qualified Control.Parallel.Strategies as CPS
import Control.DeepSeq

import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector.Unboxed.Mutable as MUB

import Data.Function
import Data.Record.Label
import Data.Monoid
import Data.Ord
import Control.Monad
import Control.Arrow ((&&&))
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Primitive
import System.Random.MWC
import Data.Word

-- Essential Types

type Vector  = VUB.Vector Double
type Bounds  = (VUB.Vector Double, VUB.Vector Double)
type Fitness = Vector -> Double
type Budget = Int

-- * Optimizer State

data DEParams s w = DEParams {_gen :: GenST s
                             ,_ec  :: Int
                             ,_pop :: V.Vector (Double,Vector)
                           ,_trace :: w}

$(mkLabels [''DEParams])

-- |The current number of fitness evaluations
evaluationCount :: forall w. forall s. DEParams s w :-> Int
evaluationCount = ec

-- |The current set of active trial points
population :: forall w. forall s. DEParams s w :-> V.Vector (Double,Vector)
population = pop
{-
-- | Get the current best indevidual and Fitness
currentBest :: forall w. forall s. DEParams s w :-> (Double,Vector)
currentBest = V.minimumBy (comparing fst) . get pop
-}
-- |The execution trace of current run
optimizationTrace :: (Monoid w) => forall s. DEParams s w :-> w
optimizationTrace = trace

-- * Context monad

-- |Monad for storing optimization trace and random number generator
newtype DeMonad s w a = DE (StateT (DEParams s w) (ST s) a) deriving (Monad)

type Generator m = Gen (PrimState m)

instance MonadState (DEParams s w) (DeMonad s w) where
    get   = DE $ get
    put a = DE $ put a

instance HasPRNG (DeMonad s w) where
    type S (DeMonad s w) = s
    withGen op = get >>= \x -> op (_gen x)

liftST :: ST s a -> DeMonad s w a
liftST op = DE $ lift op

-- |Extract values from the DeMonad.
runDE :: Monoid w => (forall s. DeMonad s w a) -> a
runDE de = runST (let (DE a ) = de in evalStateT a 
                                       $ DEParams (error "Generator uninitialized") 
                                                  0
                                                  (error "Population unitialized") 
                                                  mempty)

logPoint :: String -> DeMonad s w ()
logPoint tx = do
    (cb,_) <- getM pop >>= return . V.minimumBy (compare`on`fst)
    e <- getM ec
    return ()
--    modM trace ((e,cb,tx):)


-- HasPRNG related
class HasPRNG m where
    type S m :: *
    withGen :: (Gen (S m) -> m b) -> m b

selectRandom :: (PrimMonad m) => Gen (PrimState m) -> Int -> V.Vector a -> m [a]
selectRandom !gen !n vec = do
    let idx = replicate n 0
    --idx <- replicateM n (randomIndex (V.length vec) gen)
    return $ map (V.unsafeIndex vec) idx

{-#INLINE randomIndex#-}
randomIndex !ub gen = uni >>= return . floor . (fromIntegral ub*)
 where 
  uni = do x <- (uniform gen)
           return (x::Float)  

expVariate !lambda gen = do
    u :: Float <- uniform gen
    return . round $ (- log (u))/(1-lambda)

expVariate' !lambda gen = work 0
    where
     work !n = do
               x :: Float <- uniform gen
               if x<lambda then work (n+1)
                           else return (n::Int)
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

type DEStrategy s w = GenST s -> Int -> Vector -> V.Vector (Double, Vector) -> DeMonad s w Vector

-- |Convert a Showable strategy into executable one
strategy :: Strategy -> DEStrategy s w
strategy (Rand1Bin{..}) = strat' f cr rand1 binCrossover
strategy (Rand2Bin{..}) = strat' f cr rand2 binCrossover
strategy (Rand1Exp{..}) = strat' f cr rand1 expCrossover
strategy (Rand2Exp{..}) = strat' f cr rand2 expCrossover 
{-# INLINE strategy #-}

strat' f cr m co =  \gen l parent pop -> liftST (m gen f pop >>= \x -> co l cr parent x gen)


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

-- |Parameters for algorothm execution
data DEArgs = DEArgs {
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
                     ,seed        :: Seed}

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
   
-- | Create a Differential Evolution process
de :: (Monoid w) => DEArgs -> DeMonad s w (Double,Vector)
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

-- | Single iteration of Differential Evolution. Could be an useful building block
--   for other algorithms as well.
deStep :: (Monoid w) =>
          DEStrategy s w -> Bounds -> Fitness 
          -> V.Vector (Double,Vector)
          -> DeMonad s w (V.Vector (Double,Vector))  
deStep strat bounds fitness pop = do 
        modM ec (+V.length pop)
        withGen $ \g -> (V.mapM (candidate g) pop) 
   where 
    l = VUB.length . snd . V.head $ pop
    candidate gen orig@(ft,a) = do
        w <- strat gen l a pop
        return (select orig (postProcess w))
    select (fa,a) b@(fitness -> fb) = if fa < fb then (fa,a) else (fb,b) 
    postProcess x = saturateVector bounds x


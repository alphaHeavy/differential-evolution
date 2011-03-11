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
-- Example
-- >>>import Data.Vector.Unboxed as VUB
--
-- >>>import Numeric.Optimization.Algorithms.DifferentialEvolution
-- 
-- >>>let fitness = VUB.sum . VUB.map (*2)
--  
-- >>>de (defaultParams fitness ((VUB.replicate 60 0), (VUB.replicate 60 0)))
-- (0.12486060253695,fromList [2.481036288296201e-3, ... ]
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
import Control.Monad
import Control.Arrow ((&&&))
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Primitive
import System.Random.MWC
import Data.Word

-- import Test.QuickCheck hiding (Gen)


type Vector  = VUB.Vector Double
type Bounds  = (VUB.Vector Double, VUB.Vector Double)
type Fitness = Vector -> Double
type Budget = Int

data DEParams s = DEParams {_gen :: GenST s
                           ,_ec  :: Int
                           ,_pop :: V.Vector (Double,Vector)
                           ,_trace :: [(Int,Double,String)] }

$(mkLabels [''DEParams])

-- |The current number of fitness evaluations
evaluationCount :: forall s. DEParams s :-> Int
evaluationCount = ec

-- |The current set of active trial points
population :: forall s. DEParams s :-> V.Vector (Double,Vector)
population      = pop

-- |The execution trace of current run
optimizationTrace :: forall s. DEParams s :-> [(Int,Double,String)]
optimizationTrace = trace

-- |Monad for storing optimization trace and random number generator
newtype DeMonad s a = DE (StateT (DEParams s) (ST s) a) deriving (Monad)

instance MonadState (DEParams s) (DeMonad s) where
    get   = DE $ get
    put a = DE $ put a

instance HasPRNG (DeMonad s) where
    type S (DeMonad s) = s
    withGen op = get >>= \x -> op (_gen x)

liftST :: ST s a -> DeMonad s a
liftST op = DE $ lift op

-- |Extract values from the DeMonad.
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

type DEStrategy s = GenST s -> Int -> Vector -> V.Vector (Double, Vector) -> DeMonad s Vector
-- |Convert a Showable strategy into executable one
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
        n' :: Int <- expVariate cr gen
        index <- randomIndex l gen
        let n = min n' (l-1)
            m = index + n - l
            nmax = min n (l-index)
            overflow = index+n-l
            end = min l (index+n)
        return (moduloReplacement1 index n l a b)
       -- return $ VUB.modify (\v -> do
       --                        -- MUB.write v index (b VUB.! index)
       --                         forM_ ([0..overflow-1]++[index..end-1]) $ \i -> (MUB.write v i (b VUB.! i)))
       --                 a

{-
prop_r1_len s' l' = VUB.length (moduloReplacement1 s l dim a b) == dim
    where
     s = abs s' `mod` dim
     l = abs l' `mod` dim
     dim = 100
     a   = VUB.replicate dim (0::Int)
     b   = VUB.replicate dim (1::Int)

prop_r1_cs s' l' = (VUB.length $ VUB.filter (==1) (moduloReplacement1 s l tdim tva tvb)) == max 1 l
    where
     s = abs s' `mod` tdim
     l = abs l' `mod` tdim

prop_r1_eq2 s' l' = (moduloReplacement1 s l tdim tva tvb) == 
                    (moduloReplacement2 s l tdim tva tvb)
    where
     s = abs s' `mod` tdim
     l = abs l' `mod` tdim

prop_r1_eq3 s' l' = (moduloReplacement1 s l tdim tva tvb) == 
                    (moduloReplacement3 s l tdim tva tvb)
    where
     s = abs s' `mod` tdim
     l = abs l' `mod` tdim
tdim = 100
tva   = VUB.replicate tdim (0::Int)
tvb   = VUB.replicate tdim (1::Int)
-}

moduloReplacement1 start length dim a b 
    = VUB.modify (\v -> do 
                        MUB.write v start (b VUB.! start)
                        forM_ ([0..overflow-1]++[start..end-1]) $ \i -> (MUB.write v i (b VUB.! i)))
                        a
    where
     overflow = start+length-dim
     end      = min dim $ start+length

{-#INLINE moduloReplacement2 #-}
moduloReplacement2 start length dim a b 
       = VUB.generate dim (\i -> if (i>=start && i < end) || i < overflow || i==start
                                  then b VUB.! i else a VUB.! i )
    where
     overflow = start+length-dim
     end      = min dim $ start+length

{-#INLINE moduloReplacement3 #-}
moduloReplacement3 start length dim a b 
       = VUB.map (\(e1,e2,i) -> if (i>=start && i<end) || i < overflow || i==start then e2 else e1) 
                 $ VUB.zip3 a b (VUB.enumFromN 0 dim)
    where
     overflow = start+length-dim
     end      = min dim $ start+length


        --return $ VUB.take m a +++ VUB.slice m index b +++ VUB.dr
       -- return $ {-#SCC "Generate"#-} VUB.generate l (\i -> if i>=index && i < (index+n) || i < m
       --                                then a VUB.! i else b VUB.! i )
       -- return $ VUB.map (\(e1,e2,i) -> if (i>=index && i<(index+n)) || i<m then e2 else e1) 
       --         $ VUB.zip3 a b (VUB.enumFromN 0 l)


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
   
--saturateVector (mn,mx)  x = VUB.zipWith max mn $ VUB.zipWith min mx x
--saturate (lb,ub) x = min (max x lb) ub


-- | Create a Differential Evolution process
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

-- | Single iteration of Differential Evolution. Could be an useful building block
--   for other algorithms as well.
deStep :: DEStrategy s -> Bounds -> Fitness 
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
    postProcess x = saturateVector bounds x

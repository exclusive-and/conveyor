
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Core
-- Description  : Lazy Producers and General Structure of Conveyors
--
module Conveyor.Core
    ( -- * Conveyor Structure
      ConveyorBody (..)
      -- $conveyorExamples
      
      -- * Conveyor Composition
    , (|>)
    , (<|)
    , bindConveyors
      -- * Lazy Producer-Only Conveyors
    , Conveyor
    , Signal
    , mapC
    , mapMC
    , takeC
    ) where

import              Control.Monad (ap, liftM)
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Functor.Identity
import              Data.Void (Void)
import qualified    Data.Void as Void


---------------------------------------------------------------------
-- Conveyor Structure

-- |
-- Conveyors store sequences of values, one value after the next.
-- Each value is stored on the conveyor in a cons-cell along with the
-- continuation of the sequence.
-- 
-- For generality, this type accepts a /sequencing functor/ @f@, which
-- controls the relationship between the value and continuation of
-- each cons-cell. This way, the operational behaviour of a conveyor
-- is determined by the choice of functor, enabling more flexible and
-- configurable designs.
--
data ConveyorBody f m r
    -- |
    -- Cons-cell containing one thing on the conveyor.
    -- 
    = OneThing  (f (ConveyorBody f m r))

    -- |
    -- Record an effect to be performed when running the conveyor.
    --
    | Effect    (m (ConveyorBody f m r))

    -- |
    -- The last thing on a conveyor. Always whatever value we want
    -- the conveyor to return when it's finished working.
    --
    | Finished  r


instance (Functor f, Monad m) => Functor (ConveyorBody f m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance (Functor f, Monad m) => Applicative (ConveyorBody f m) where
    pure  = Finished
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance (Functor f, Monad m) => Monad (ConveyorBody f m) where
    return = pure
    {-# INLINE return #-}

    (>>=) = bindConveyors
    {-# INLINE (>>=) #-}


---------------------------------------------------------------------
-- Monad Transformer Instances

instance Functor f => MonadTrans (ConveyorBody f) where
    lift = Effect . fmap Finished

instance (Functor f, MonadIO m) => MonadIO (ConveyorBody f m) where
    liftIO = lift . liftIO


-- $conveyorExamples
-- 
-- = Some Example Conveyors
-- 
-- Some examples of conveyors using different sequencing functors are:
--
--  - Producer streams. These are the most basic conveyors: they
--    just store data as-is so that it can be moved. See 'Conveyor'
--    and "Conveyor.Strict".
--
--  - Consumer streams. Data that's been put into a consumer cannot
--    be accessed from outside the conveyor, but it can influence the
--    continuation. The only way to return data from a pure consumer
--    is by finishing. See 'Consumer'.
--
--  - Hybrid streams. Hybrids combine the capabilities of producers
--    and consumers. Consumer-like steps input into the continuation
--    from upstream, and producer-like steps output data that can be
--    accessed downstream. See "Conveyor.Hybrid".


---------------------------------------------------------------------
-- Conveyor Composition

infixl 0 |>
infixr 0 <|

-- |
-- Apply the second argument to the first. Equivalent to @flip ($)@.
--
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- |
-- Apply the first argument ot the second. Equivalent to @($)@.
--
(<|) :: (a -> b) -> a -> b
f <| x = f x

-- |
-- Connect two conveyors end-to-end.
--
-- Runs one conveyor until it produces a 'Finished' result, and then
-- passes that as an input to the next.
--
bindConveyors
    :: (Functor f, Monad m)
    => ConveyorBody f m r0
    -> (r0 -> ConveyorBody f m r1)
    -> ConveyorBody f m r1

bindConveyors conveyor f = go conveyor where
    go = \case
        OneThing s -> go <$> s |> OneThing
        Effect   m -> go <$> m |> Effect
        Finished r -> f r


---------------------------------------------------------------------
-- Lazy Producer-Only Conveyors

-- |
-- The simplest kind of conveyor: a lazy producer.
--
-- This conveyor implements sequential data-moving for a type @a@.
-- Each cons-cell is a pair @(x, following)@, where @x :: a@ is the
-- datum at the head, and @following@ is the continuation.
--
-- Ignoring effects, this is a free list: @(x0, (x1, (x2, ...)))@.
--
type Conveyor a = ConveyorBody ((,) a)

-- |
-- 'Signal' is our version of Clash's @Signal dom@ functor, extended
-- with monadic effects.
--
-- NOTE: To run a Signal, you /must/ sample a finite number of
--       values from it.
--
type Signal m a = Conveyor a m Void

-- |
-- Map a function over a conveyor.
--
mapC :: Monad m => (a -> b) -> Conveyor a m r -> Conveyor b m r
mapC f = loop where
    loop = \case
        OneThing s -> loop <$> s |> go
        Effect   m -> loop <$> m |> Effect
        Finished r -> Finished r

    go (x, xs) = OneThing (f x, xs)

-- |
-- Map a monadic action over a conveyor.
--
mapMC :: Monad m => (a -> m b) -> Conveyor a m r -> Conveyor b m r
mapMC f = loop where
    loop = \case
        OneThing s -> loop <$> s |> go
        Effect   m -> loop <$> m |> Effect
        Finished r -> Finished r

    go (x, xs) = Effect $ OneThing . (, xs) <$> f x

-- |
-- Take an integer number of values from a conveyor, and discard
-- the rest.
--
takeC :: Monad m => Int -> Conveyor a m r -> Conveyor a m ()
takeC = loop where
    loop n _ | n <= 0 = pure ()

    loop n s = case s of
        OneThing (x, xs) -> OneThing (x, loop (n - 1) xs)
        Effect   m       -> Effect $ loop n <$> m
        Finished _       -> Finished ()

{-
runConveyor :: Monad m => ConveyorBody Identity m r -> r
runConveyor = \case
    OneThing _  ->
    Effect m    -> m >>= runConveyor
    Finished r  -> pure r
    -}

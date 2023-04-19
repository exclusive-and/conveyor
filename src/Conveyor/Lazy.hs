
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Lazy
-- Description  : Lazy Producer and Consumer Conveyors
-- 
module Conveyor.Lazy
    ( -- * Lazy Producers
      Conveyor
    , Signal
      -- * Lazy Consumers
    , Consumer
      -- * Conveyor Combinators
    , mapC
    , mapMC
    , takeC
    , toListC
    , sinkC
    ) where

import Conveyor.Core

import Data.Void (Void)


---------------------------------------------------------------------
-- Lazy Producers

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


---------------------------------------------------------------------
-- Lazy Consumers

-- |
--
--
type Consumer i m r = ConveyorBody ((->) i) m r


---------------------------------------------------------------------
-- Conveyor Combinators

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
      
-- |
-- Convert a producer stream to a list.
-- 
toListC :: Monad m => Conveyor a m r -> m ([a], r)
toListC = loop id where
    loop f = \case
        OneThing (x, xs) -> loop (f . (x:)) xs
        Effect   m       -> m >>= loop f
        Finished r       -> pure (f [], r)

-- |
-- Feed values from a producer stream into a consumer stream.
--
sinkC :: Monad m => Conveyor a m r -> Consumer a m r -> Sink m r
sinkC = runConsumer where
    runConsumer p = \case
        OneThing f -> runProducer f p
        Effect   m -> Effect $ continueConsuming <$> m
        Finished r -> Finished r
      where continueConsuming = runConsumer p
    
    runProducer f = \case
        OneThing (x, xs) -> runConsumer xs (f x)
        Effect   m       -> Effect $ continueProducing <$> m
        Finished r       -> Finished r
      where continueProducing = runProducer f

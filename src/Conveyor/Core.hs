
{-# LANGUAGE UndecidableInstances #-}

---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Core
-- Description  : General Structure of Types of the Conveyor Family
--
module Conveyor.Core
    ( -- * Conveyor Structure
      ConveyorBody (..)
      -- $conveyorExamples
      
      -- * Conveyor Composition
    , (|>)
    , (<|)
    , bindConveyors
    
      -- * Generalized Running
    , Sink
    , runConveyor
    ) where

import              Control.Monad (ap, liftM)
import              Control.Monad.IO.Class
import              Control.Monad.State.Class
import              Control.Monad.Trans.Class
import              Data.Functor.Const
import              Data.Void (Void)
import qualified    Data.Void as Void


---------------------------------------------------------------------
-- Conveyor Structure

-- |
-- Conveyors store sequences of values in order, one value after the
-- next. Each value is stored in a cons-cell alongside a continuation,
-- so that following values can be easily reached.
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

instance (Functor f, MonadState s m)
    => MonadState s (ConveyorBody f m)
  where    
    get   = lift get
    put   = lift . put
    state = lift . state


-- $conveyorExamples
-- 
-- = Some Example Conveyors
-- 
-- Some examples of conveyors using different sequencing functors are:
--
--  - Producer streams. These are the most basic kind of conveyors:
--    they store data as-is so that it can be moved.
--    See @"Conveyor.Lazy".'Conveyor.Lazy.Conveyor'@ and
--    @"Conveyor.Strict".'Conveyor.Strict.Conveyor'@.
--
--  - Consumer streams. Data that's been put into a consumer cannot
--    be accessed from outside the conveyor, but it can influence
--    the continuation. The only way to retrieve data from a pure
--    consumer is by running the conveyor to get the finished result.
--    See @"Conveyor.Lazy".'Conveyor.Lazy.Consumer'@.
--
--  - Hybrid streams. Hybrids combine the capabilities of producers
--    and consumers. Consumer-like steps input into the continuation
--    from upstream, and producer-like steps output data that can be
--    accessed downstream.
--    See @"Conveyor.Hybrid".'Conveyor.Hybrid.Consumer'@.


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
-- Generalized Running

-- |
-- The most general type of runnable conveyor. Sinks contain no data,
-- and so are equivalent to a free monad.
-- 
type Sink = ConveyorBody (Const Void)

-- |
-- Run a sink, using the monad action to chain effects.
-- 
runConveyor :: Monad m => Sink m r -> m r
runConveyor = \case
    OneThing o  -> Void.absurd $ getConst o
    Effect m    -> m >>= runConveyor
    Finished r  -> pure r


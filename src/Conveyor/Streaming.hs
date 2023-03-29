
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Streaming
-- Description  : Conveyor-Compatible Stepwise Streaming
--
module Conveyor.Streaming
    ( -- * Streaming Conveyors
      ConveyorS (..)
      -- * Signal-like Stream Functor
    , Of (..)
      -- * Conveyor Stream Composition
    , bindConveyorS
      -- * Streaming Combinators
    , mapS
    ) where

import qualified    Conveyor.Core as Core

import              Control.Monad (ap, liftM)
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class


---------------------------------------------------------------------
-- Streaming Conveyors

-- |
-- A streaming version of 'Core.Conveyor'. Simple and blazing fast.
--
data ConveyorS f m r
    -- |
    -- One step of the conveyor process.
    --
    -- How the constructor is interpreted depends on our choice of
    -- underlying functor. Some examples of suitable functors and
    -- their interpretations are:
    --
    --  (1) The functor @(->) i@ gives @i -> ConveyorS ((->) i) m r@,
    --      which is a consumer stream like the 'Core.Machine'
    --      constructor.
    --
    --  (2) The functor @Of i@ gives @i :> (ConveyorS (Of i) m r)@,
    --      which is a producer-based stream like the 'Core.Convey'
    --      constructor.
    -- 
    = ConveyS   (f (ConveyorS f m r))

    -- |
    -- Record an effect to be performed when running the conveyor.
    --
    | Effect    (m (ConveyorS f m r))

    -- |
    -- Indicate that the stream closed and the conveyor finished with
    -- a result.
    --
    | Finished  r


instance (Functor f, Monad m) => Functor (ConveyorS f m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance (Functor f, Monad m) => Applicative (ConveyorS f m) where
    pure  = Finished
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance (Functor f, Monad m) => Monad (ConveyorS f m) where
    return = pure
    {-# INLINE return #-}

    (>>=) = bindConveyorS
    {-# INLINE (>>=) #-}


---------------------------------------------------------------------
-- Monad Transformer Instances

instance Functor f => MonadTrans (ConveyorS f) where
    lift = Effect . fmap Finished

instance (Functor f, MonadIO m) => MonadIO (ConveyorS f m) where
    liftIO = lift . liftIO


---------------------------------------------------------------------
-- Signal-like Stream Functor

-- |
-- A simple pairing functor. When used as the underlying functor of a
-- streaming conveyor, we get something like Clash's @Signal dom@ type,
-- which is useful for hardware simulation.
--
data Of a b = !a :> b

instance Functor (Of a) where
    fmap f (a :> x) = a :> f x
    {-# INLINE fmap #-}


---------------------------------------------------------------------
-- Conveyor Stream Composition

-- |
-- Connect two streaming conveyors end-to-end.
--
-- Runs one conveyor until it produces a 'Finished' result, and then
-- passes that as an input to the next.
--
bindConveyorS
    :: (Functor f, Monad m)
    => ConveyorS f m r0
    -> (r0 -> ConveyorS f m r1)
    -> ConveyorS f m r1

bindConveyorS conveyor f = go conveyor where
    go = \case
        ConveyS  s -> ConveyS $ go <$> s
        Effect   m -> Effect $ go <$> m
        Finished r -> f r


---------------------------------------------------------------------
-- Streaming Combinators

-- |
-- 
--
mapS
    :: Monad m
    => (a -> b)
    -> ConveyorS (Of a) m r
    -> ConveyorS (Of b) m r

mapS f = loop where
    loop = \case
        ConveyS  s -> ConveyS $ go $ loop <$> s
        Effect   m -> Effect $ loop <$> m
        Finished r -> Finished r

    go (x :> xs) = f x :> xs



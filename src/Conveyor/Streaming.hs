
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Streaming
-- Description  : Conveyor-Compatible Stepwise Streaming
--
module Conveyor.Streaming
    ( -- * Streaming Conveyors
      ConveyorStream (..)
      -- * Signal-like Stream Functor
    , Of (..)
      -- * Conveyor Stream Composition
    , bindStreams
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
data ConveyorStream f m r
    -- |
    -- One step of the conveyor process.
    --
    -- How the constructor is interpreted depends on our choice of
    -- underlying functor. Some examples of suitable functors and
    -- their interpretations are:
    --
    --  (1) The functor @(->) i@ gives this constructor a field of
    --      type @i -> ConveyorStream ((->) i) m r@, which is a consumer
    --      stream like the 'Core.Machine' constructor.
    --
    --  (2) The functor @Of i@ gives this constructor a field of
    --      type @i :> (ConveyorStream (Of i) m r)@, which is a producer
    --      stream like the 'Core.Convey' constructor.
    -- 
    = OneStep   (f (ConveyorStream f m r))

    -- |
    -- Record an effect to be performed when running the conveyor.
    --
    | Effect    (m (ConveyorStream f m r))

    -- |
    -- Indicate that the stream closed and the conveyor finished with
    -- a result.
    --
    | Finished  r


instance (Functor f, Monad m) => Functor (ConveyorStream f m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance (Functor f, Monad m) => Applicative (ConveyorStream f m) where
    pure  = Finished
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance (Functor f, Monad m) => Monad (ConveyorStream f m) where
    return = pure
    {-# INLINE return #-}

    (>>=) = bindStreams
    {-# INLINE (>>=) #-}


---------------------------------------------------------------------
-- Monad Transformer Instances

instance Functor f => MonadTrans (ConveyorStream f) where
    lift = Effect . fmap Finished

instance (Functor f, MonadIO m) => MonadIO (ConveyorStream f m) where
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
bindStreams
    :: (Functor f, Monad m)
    => ConveyorStream f m r0
    -> (r0 -> ConveyorStream f m r1)
    -> ConveyorStream f m r1

bindStreams conveyor f = go conveyor where
    go = \case
        OneStep  s -> OneStep (go <$> s)
        Effect   m -> Effect  (go <$> m)
        Finished r -> f r


---------------------------------------------------------------------
-- Streaming Combinators

-- |
-- 
--
mapS
    :: Monad m
    => (a -> b)
    -> ConveyorStream (Of a) m r
    -> ConveyorStream (Of b) m r

mapS f = loop where
    loop = \case
        OneStep  s -> OneStep (go $ loop <$> s)
        Effect   m -> Effect  (     loop <$> m)
        Finished r -> Finished r

    go (x :> xs) = f x :> xs



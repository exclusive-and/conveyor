
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Streaming
-- Description  : Conveyor-Compatible Stepwise Streaming
--
module Conveyor.Streaming
    ( -- * Streaming Conveyors
      ConveyorStream (..)
      -- * Conveyor Stream Composition
    , (|>)
    , (<|)
    , bindStreams
      -- * Signal-like Stream Functor
    , Of (..)
    , ConveyorSignal
      -- * Conveyor Conversions
    , ConveyorF (..)
    , ConveyorS
    , conveyorToConveyorS
    , conveyorSToConveyor
    , conveyorToStream
    , yieldS
    , awaitS
      -- * Streaming Combinators
    , mapS
    , mapMS
    ) where

import qualified    Conveyor.Core as Core

import              Control.Monad (ap, liftM)
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Void (Void)
import qualified    Data.Void as Void


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
    -- underlying functor. Some examples of suitable functors and the
    -- corresponding interpretations of the constructor are:
    --
    --  (1) The functor @(->) i@ corresponds to steps with the type
    --      @i -> ConveyorStream ((->) i) m r@. Streams with steps
    --      of this type are consumers in the style of 'Core.Machine'.
    --
    --  (2) The functor @Of i@ corresponds to steps matching
    --      @i :> (ConveyorStream (Of i) m r)@. This pattern is a simple
    --      producer stream, similar to 'Core.Convey'.
    --
    --  (3) The functor @ConveyorF i o s u@ corresponds to the 
    --      'Core.Conveyor' type.
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
-- Conveyor Stream Composition

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
        OneStep  s -> go <$> s |> OneStep
        Effect   m -> go <$> m |> Effect
        Finished r -> f r


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


type ConveyorSignal m a = ConveyorStream (Of a) m Void


---------------------------------------------------------------------
-- Conveyor Conversions

-- |
-- A functor compatible with 'ConveyorStream' which emulates the normal
-- operation of 'Core.Conveyor'.
--
data ConveyorF i o s u a
    -- |
    -- Analogous to 'Core.Convey': this constructor conveys results
    -- to the next stage of the conveyor process.
    --
    = Convey    o a

    -- |
    -- Analogous to 'Core.Machine': this constructor accepts inputs
    -- and results from further up the process.
    --
    | Machine   (i -> a) (u -> a)

    -- |
    -- Analogous to 'Core.Spare': this constructor yields spare parts
    -- that can be reused.
    --
    | Spare     s a

instance Functor (ConveyorF i o s u) where
    fmap f = \case
        Convey o stream'
            -> Convey o $ f stream'
        Machine onInput onFinal
            -> Machine (f . onInput) (f . onFinal)
        Spare s stream'
            -> Spare s $ f stream'


type ConveyorS i o s u m r = ConveyorStream (ConveyorF i o s u) m r


yieldS :: Monad m => o -> ConveyorS i o s u m ()
yieldS o = OneStep $ Convey o $ Finished ()

awaitS :: Monad m => ConveyorS i o s u m (Maybe i)
awaitS = OneStep $ Machine onInput onFinal where
    onInput = Finished . Just
    onFinal = Finished . const Nothing


-- |
-- Convert a 'Core.Conveyor' to its analogous stream using 'ConveyorF'
-- as the underlying functor.
--
conveyorToConveyorS
    :: Monad m
    => Core.Conveyor i o s u m r
    -> ConveyorS i o s u m r

conveyorToConveyorS = go where
    go = \case
        Core.Convey o conveyor'
            -> OneStep $ Convey o $ go conveyor'
        Core.Machine onInput onFinal
            -> OneStep $ Machine (go . onInput) (go . onFinal)
        Core.ConveyorM m
            -> Effect $ go <$> m
        Core.Finished r
            -> Finished r
        Core.Spare s conveyor'
            -> OneStep $ Spare s $ go conveyor'

-- |
-- Convert a stream to its analogous 'Core.Conveyor'.
--
conveyorSToConveyor
    :: Monad m
    => ConveyorS i o s u m r
    -> Core.Conveyor i o s u m r

conveyorSToConveyor = go where
    go = \case
        OneStep (Convey o stream')
            -> Core.Convey o $ go stream'
        OneStep (Machine onInput onFinal)
            -> Core.Machine (go . onInput) (go . onFinal)
        Effect m
            -> Core.ConveyorM $ go <$> m
        Finished r
            -> Core.Finished r
        OneStep (Spare s stream')
            -> Core.Spare s $ go stream'

-- |
-- Convert a 'Core.Conveyor' to its analogous stream using 'Of' as the
-- underlying functor. Streams with 'Of' are producers, but we can
-- emulate the consumer/producer/pipe semantics of 'Core.Conveyor' with
-- functions from producers to producers.
--
-- NOTE: Producer streams do /not/ support spares! You must reuse any
--       spares /before/ trying this conversion!
--
conveyorToStream
    :: Monad m
    => Core.Conveyor i o Void u m r
    -> ConveyorStream (Of i) m u
    -> ConveyorStream (Of o) m r

conveyorToStream = flip go where
    go stream = \case
        Core.Convey o conveyor'
            -> OneStep $ o :> go stream conveyor'
        Core.Machine onInput onFinal
            -> runUpstream onInput onFinal stream
        Core.ConveyorM m
            -> Effect $ go stream <$> m
        Core.Finished r
            -> Finished r
        Core.Spare s _conveyor'
            -> Void.absurd s

    runUpstream onInput onFinal = \case
        OneStep (o :> stream')
            -> go stream' $ onInput o
        Effect m
            -> Effect $ runUpstream onInput onFinal <$> m
        Finished r
            -> go (Finished r) $ onFinal r


---------------------------------------------------------------------
-- Streaming Combinators

-- |
-- Map a function over a provider stream.
--
mapS
    :: Monad m
    => (a -> b)
    -> ConveyorStream (Of a) m r
    -> ConveyorStream (Of b) m r

mapS f = loop where
    loop = \case
        OneStep  s -> loop <$> s |> go
        Effect   m -> loop <$> m |> Effect
        Finished r -> Finished r

    go (x :> xs) = f x :> xs |> OneStep

-- |
-- Map a monadic action over a provider stream.
--
mapMS
    :: Monad m
    => (a -> m b)
    -> ConveyorStream (Of a) m r
    -> ConveyorStream (Of b) m r

mapMS f = loop where
    loop = \case
        OneStep  s -> loop <$> s |> go
        Effect   m -> loop <$> m |> Effect
        Finished r -> Finished r

    go (x :> xs) = Effect $ do
        y <- f x
        OneStep (y :> loop xs)




{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Codensity
-- Description  : Codensity-Transformed Conveyors for Better Performance
-- 
module Conveyor.Codensity
    ( -- * Codensity Conveyor
      ConveyorT (..)
      -- * Primitive Combinators
    , yield
    , await
    , machineFrame
      -- * Conveyor Fusion
    , fuse
    , (.|)
      -- * Running Conveyors
    , runConveyor
      -- * Pure Combinators
    , mapC
    , mapMaybeC
      -- * Monadic Combinators
    , mapMC
    , mapMaybeMC
    ) where

import qualified    Conveyor.Core as C

import              Control.Monad (ap, liftM, (>=>))
import              Control.Monad.IO.Class
import              Control.Monad.State.Class
import              Control.Monad.Trans.Class
import              Data.Foldable (traverse_)
import              Data.Void (Void)


---------------------------------------------------------------------
-- Codensity Transform

-- |
-- A codensity-transformed version of the main 'C.Conveyor' datatype.
-- 
newtype ConveyorT i o m r = ConveyorT
    { unConveyorT
        :: forall b. (r -> C.Conveyor i o i () m b)
        -> C.Conveyor i o i () m b
    }

instance Functor (ConveyorT i o m) where
    fmap f (ConveyorT c) = ConveyorT $ \rest -> c (rest . f)
    
instance Applicative (ConveyorT i o m) where
    pure x = ConveyorT ($ x)
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}
    (*>)   = (>>)
    {-# INLINE (*>) #-}

instance Monad (ConveyorT i o m) where
    return = pure
    
    ConveyorT f >>= g =
        ConveyorT $ \cont -> f $ \a -> unConveyorT (g a) cont


---------------------------------------------------------------------
-- Codensity Monad Transformer Instances
        
instance MonadTrans (ConveyorT i o) where
    lift m = ConveyorT $ \rest -> C.ConveyorM (rest `liftM` m)
    {-# INLINE [1] lift #-}

instance MonadIO m => MonadIO (ConveyorT i o m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadState s m => MonadState s (ConveyorT i o m) where
    get   = lift get
    put   = lift . put
    state = lift . state


---------------------------------------------------------------------
-- Codensity-Transformed Combinators

-- |
-- Place a part onto the conveyor, so that the conveyor can carry it
-- downstream for the next machine to consume.
--
yield :: o -> ConveyorT i o m ()
yield o = ConveyorT $ \rest -> C.Convey o (rest ())

{-# INLINE yield #-}

-- |
-- Await an input from an upstream conveyor.
--
-- Returns @Just i@ when it receives input @i@ from upstream. If the
-- upstream conveyor finishes, it returns @Nothing@.
--
await :: ConveyorT i o m (Maybe i)
await = ConveyorT $ \rest ->
  let
    onInput = rest . Just
    onFinal = const $ rest Nothing
  in
    C.Machine onInput onFinal

{-# INLINE [0] await #-}

-- |
-- Framework for building simple machines. Runs the same function on
-- every value in the stream. Terminates when we're out of work. 
--
machineFrame :: Monad m => (i -> ConveyorT i o m r) -> ConveyorT i o m ()
machineFrame f = ConveyorT $ \rest ->
  let
    go = C.Machine (\i -> unConveyorT (f i) (const go)) rest
  in
    go


---------------------------------------------------------------------
-- Conveyor Fusion

-- |
-- Fuse two conveyors (conveyor A and conveyor B) into one.
--
-- Runs conveyor B, feeding parts from conveyor A as inputs as needed.
-- Also runs conveyor A as needed to feed parts into conveyor B.
--
-- This is functionally identical to 'C.fuseConveyors', with two
-- important differences:
--
--  * This function automatically reuses spares, where 'C.fuseConveyors'
--    doesn't.
--
--  * Rather than doing nothing on the finished product of a process,
--    this function will pass its finished product into a continuation
--    that may do something else with it.
--
fuse
    :: Monad m
    => ConveyorT a b m ()
    -> ConveyorT b c m r
    -> ConveyorT a c m r

{-# NOINLINE fuse #-}
{-# SCC fuse #-}

fuse (ConveyorT upstream) (ConveyorT downstream) = ConveyorT $ \rest ->
  let
    runConveyorB conveyorA conveyorB = case conveyorB of
        -- Convey parts from conveyor B downstream and continue.
        C.Convey o conveyorB'
            -> C.Convey o (continueB conveyorB')
        C.Spare s conveyorB'
            -> runConveyorB (C.Convey s conveyorA) conveyorB'
        -- Let conveyor A give us the next part.
        C.Machine onInput onFinal
            -> runConveyorA onInput onFinal conveyorA
        -- Convey the finished product of this process into the
        -- next continuation.
        C.Finished r
            -> rest r
        C.ConveyorM m
            -> C.ConveyorM (liftM continueB m)
      where continueB = runConveyorB conveyorA

    runConveyorA onInput onFinal conveyorA = case conveyorA of
        -- Convey a part from conveyor A into a machine on conveyor B.
        C.Convey o conveyorA'
            -> runConveyorB conveyorA' (onInput o)
        -- Take spares off and continue.
        C.Spare s conveyorA'
            -> C.Spare s (continueA conveyorA')
        -- Run the machines that feed conveyor A and continue.
        C.Machine onInput' onFinal'
            -> C.Machine (continueA . onInput') (continueA . onFinal')
        -- If conveyor A finishes, feed the product to the appropriate
        -- machine on conveyor B.
        C.Finished r
            -> runConveyorB (C.Finished r) (onFinal r)
        C.ConveyorM m
            -> C.ConveyorM (liftM continueA m)
      where continueA = runConveyorA onInput onFinal
  in
    runConveyorB (upstream C.Finished) (downstream C.Finished)

-- |
-- Conveyor fusion operator. Synonym for 'fuse'.
--
(.|)
    :: Monad m
    => ConveyorT a b m ()
    -> ConveyorT b c m r
    -> ConveyorT a c m r

{-# INLINE (.|) #-}

(.|) = fuse


---------------------------------------------------------------------
-- Running Codensity Conveyors

runConveyor :: Monad m => ConveyorT () Void m r -> m r

{-# INLINE [0] runConveyor #-}

runConveyor (ConveyorT c) =
    C.runConveyor $ C.reuseSpares $ c C.Finished


---------------------------------------------------------------------
-- Pure Combinators

-- |
-- Apply a function to every value in a stream.
--
mapC :: Monad m => (i -> o) -> ConveyorT i o m ()
mapC f = machineFrame (yield . f)

{-# INLINE mapC #-}

-- |
-- Apply a function which returns a 'Maybe' result to every value in
-- a stream. If the function returns @Just a@, yield @a@. Otherwise
-- don't yield anything.
--
mapMaybeC :: Monad m => (i -> Maybe o) -> ConveyorT i o m ()
mapMaybeC f = machineFrame (traverse_ yield . f)

{-# INLINE mapMaybeC #-}


---------------------------------------------------------------------
-- Monadic Combinators

-- |
-- Apply a monadic function to every value in a stream.
--
mapMC :: Monad m => (i -> m o) -> ConveyorT i o m ()
mapMC f = machineFrame (lift . f >=> yield)

{-# INLINE mapMC #-}

-- |
-- Apply a monadic function which returns a 'Maybe' result to every
-- value in a stream.
--
mapMaybeMC :: Monad m => (i -> m (Maybe o)) -> ConveyorT i o m ()
mapMaybeMC f = machineFrame (lift . f >=> traverse_ yield)

{-# INLINE mapMaybeMC #-}

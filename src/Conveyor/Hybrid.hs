
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Hybrid
-- Description  : Move Parts Through a Linear Work Process
-- 
module Conveyor.Hybrid
    ( -- * Conveyor Type
      Conveyor
    , Hybrid (..)
    , pattern Convey
    , pattern Machine
    , pattern Spare
      -- * Primitive Combinators
    , yield
    , await
    , machineFrame
      -- * Conveyor Fusion
    , fuseConveyors
      -- * Running Conveyors
    , runConveyor
    , reuseSpares
      -- * Pure Combinators
    , mapC
    , mapMaybeC
      -- * Monadic Combinators
    , mapMC
    , mapMaybeMC
    ) where

import              Conveyor.Core hiding (runConveyor)

import              Control.Monad ((>=>))
import              Control.Monad.Trans.Class (lift)
import              Data.Foldable (traverse_)
import              Data.Void (Void)
import qualified    Data.Void as Void


---------------------------------------------------------------------
-- Internal Conveyor Datatype

type Conveyor i o s u = ConveyorBody (Hybrid i o s u)

-- |
-- Conveyors move data linearly through various machines.
-- 
data Hybrid i o s u a
    -- |
    -- Convey a part on the belt downstream. The two fields of the
    -- constructor are respectively:
    --
    --  (1) the part on the conveyor belt to be moved downstream; and
    --
    --  (2) the state of the belt after moving the part.
    --
    = ConveyF o a
   
    -- |
    -- Install a machine on the conveyor.
    -- 
    -- Conveyor machines support two operational modes, represented
    -- by this constructor's fields.
    -- 
    --  (1) In streaming mode, the machine accepts an input in the form
    --      of a 'Convey' constructor from further up the conveyor.
    --  
    --  (2) In final mode, the machine takes the 'Finished' return
    --      value of the upstream conveyor. It can only receive the
    --      result once, and won't get any streaming mode inputs
    --      after receiving a return value. But it can keep running
    --      independently of the upstream conveyor.
    -- 
    | MachineF (i -> a) (u -> a)

    -- |
    -- Place a spare part on the belt. Spares can be reused.
    --
    -- Similar to 'Convey', the second field represents the conveyor
    -- after the spare is removed from the belt.
    --
    | SpareF s a


pattern Convey :: o -> Conveyor i o s u m r -> Conveyor i o s u m r
pattern Convey o a = OneThing (ConveyF o a)

pattern Machine
    :: (i -> Conveyor i o s u m r)
    -> (u -> Conveyor i o s u m r)
    -> Conveyor i o s u m r
pattern Machine onInput onFinal = OneThing (MachineF onInput onFinal)

pattern Spare :: s -> Conveyor i o s u m r -> Conveyor i o s u m r
pattern Spare s a = OneThing (SpareF s a)

{-# COMPLETE Convey, Machine, Effect, Finished, Spare :: Conveyor #-}


instance Functor (Hybrid i o s u) where
    fmap f = \case
        ConveyF  o a -> ConveyF o $ f a
        MachineF i u -> MachineF (f . i) (f . u)
        SpareF   s a -> SpareF s $ f a


---------------------------------------------------------------------
-- Conveyor Combinators

-- |
-- Place a part onto the conveyor, so that the conveyor can carry it
-- downstream for the next machine to consume.
--
yield :: o -> Conveyor i o s u m ()
yield o = Convey o (Finished ())

{-# INLINE yield #-}

-- |
-- Await an input from an upstream conveyor.
--
-- Returns @Just i@ when it receives input @i@ from upstream. If the
-- upstream conveyor finishes, it returns @Nothing@.
--
await :: Conveyor i o s u m (Maybe i)
await = Machine onInput onFinal where
    onInput = Finished . Just
    onFinal = const $ Finished Nothing

{-# INLINE [0] await #-}

-- |
-- Framework for building simple machines. Runs the same function on
-- every value in the stream. Terminates when we're out of work. 
--
machineFrame
    :: Monad m => (i -> Conveyor i o s u m r) -> Conveyor i o s u m ()

machineFrame f = go where
    go = Machine (\i -> f i >> go) (const $ pure ())


---------------------------------------------------------------------
-- Hybrid Conveyor Fusion
 
-- |
-- Fuse two conveyors (conveyor A and conveyor B) into one.
--
-- Runs conveyor B, feeding parts from conveyor A as inputs as needed.
-- Also runs conveyor A as needed to feed parts into conveyor B.
--
fuseConveyors
    :: Monad m
    => Conveyor a b spare r0 m r1
    -> Conveyor b c Void r1 m r2
    -> Conveyor a c spare r0 m r2

fuseConveyors = runConveyorB where
    runConveyorB conveyorA conveyorB = case conveyorB of
        -- Convey parts from conveyor B downstream and continue.
        Convey o conveyorB'
            -> Convey o (continueB conveyorB')
        -- Let conveyor A give us the next part.
        Machine onInput onFinal
            -> runConveyorA onInput onFinal conveyorA
        -- Conveying a finished product does nothing.
        Finished r
            -> Finished r
        Effect m
            -> Effect (continueB <$> m)
        Spare s _conveyorB'
            -> Void.absurd s
      where continueB = runConveyorB conveyorA

    runConveyorA onInput onFinal conveyorA = case conveyorA of
        -- Convey a part from conveyor A into a machine on conveyor B.
        Convey o conveyorA'
            -> runConveyorB conveyorA' (onInput o)
        -- Run the machines that feed conveyor A and continue.
        Machine onInput' onFinal'
            -> Machine (continueA . onInput') (continueA . onFinal')
        -- If conveyor A finishes, feed the product to the appropriate
        -- machine on conveyor B.
        Finished r
            -> runConveyorB (Finished r) (onFinal r)
        Effect m
            -> Effect (continueA <$> m)
        -- Take spares off and continue.
        Spare s conveyorA'
            -> Spare s (continueA conveyorA')
      where continueA = runConveyorA onInput onFinal


---------------------------------------------------------------------
-- Running Conveyors

-- |
-- Runs a conveyor until it produces a finished result.
--
-- NOTE: When running a conveyor in isolation like this, it's important
-- that it doesn't accept any inputs or convey any results besides the
-- finished product. This is because there are no conveyors to feed this
-- one with inputs, and none to receive its outputs.
--
runConveyor :: Monad m => Conveyor () Void Void () m r -> m r
runConveyor conveyor = case conveyor of
    Convey    o _       -> Void.absurd o
    Machine   _ onFinal -> runConveyor (onFinal ())
    Finished  result    -> pure result
    Effect m            -> m >>= runConveyor
    Spare     s _       -> Void.absurd s

{-# SCC runConveyor #-}
{-# NOINLINE runConveyor #-}


-- |
-- Input any spare parts back into the conveyor process.
--
reuseSpares :: Monad m => Conveyor i o i u m r -> Conveyor i o s u m r
reuseSpares = go [] where
    go spares (Convey o conveyor)
        = Convey o (go spares conveyor)
    go (s : spares) (Machine onInput _)
        = go spares $ onInput s
    go [] (Machine onInput onFinal)
        = Machine (go [] . onInput) (go [] . onFinal)
    go _spares (Finished r)
        = Finished r
    go spares (Effect m)
        = Effect (go spares <$> m)
    go spares (Spare s conveyor)
        = go (s : spares) conveyor


---------------------------------------------------------------------
-- Pure Combinators

-- |
-- Apply a function to every value in a stream.
--
mapC :: Monad m => (i -> o) -> Conveyor i o s u m ()
mapC f = machineFrame (yield . f)

{-# INLINE mapC #-}

-- |
-- Apply a function which returns a 'Maybe' result to every value in
-- a stream. If the function returns @Just a@, yield @a@. Otherwise
-- don't yield anything.
--
mapMaybeC :: Monad m => (i -> Maybe o) -> Conveyor i o s u m ()
mapMaybeC f = machineFrame (traverse_ yield . f)

{-# INLINE mapMaybeC #-}


---------------------------------------------------------------------
-- Monadic Combinators

-- |
-- Apply a monadic function to every value in a stream.
--
mapMC :: Monad m => (i -> m o) -> Conveyor i o s u m ()
mapMC f = machineFrame (lift . f >=> yield)

{-# INLINE mapMC #-}

-- |
-- Apply a monadic function which returns a 'Maybe' result to every
-- value in a stream.
--
mapMaybeMC :: Monad m => (i -> m (Maybe o)) -> Conveyor i o s u m ()
mapMaybeMC f = machineFrame (lift . f >=> traverse_ yield)

{-# INLINE mapMaybeMC #-}

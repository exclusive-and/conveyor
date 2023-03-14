
---------------------------------------------------------------------
-- |
-- Module       : Conveyor.Core
-- Description  : Move Parts Through a Linear Work Process
-- 
module Conveyor.Core where

import              Control.Monad (ap, liftM, (>=>))
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Void (Void)
import qualified    Data.Void as Void


---------------------------------------------------------------------
-- Internal Conveyor Datatype

-- |
-- Conveyors move data linearly through various machines.
-- 
data Conveyor i o s u m r
    -- |
    -- Convey a part on the belt downstream. The two fields of the
    -- constructor are respectively:
    --
    --  (1) the part on the conveyor belt to be moved downstream; and
    --
    --  (2) the state of the belt after moving the part.
    --
    = Convey o (Conveyor i o s u m r)

    -- |
    -- Place a spare part on the belt. Spares can be reused.
    --
    -- Similar to 'Convey', the second field represents the conveyor
    -- after the spare is removed from the belt.
    --
    | Spare s (Conveyor i o s u m r)
    
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
    | Machine
        (i -> Conveyor i o s u m r)
        (u -> Conveyor i o s u m r)
    
    -- |
    -- The final product of this assembly stage.
    -- 
    -- This constructor indicates that everything upstream is done,
    -- terminating with this as an overall result. Any downstream
    -- machines that receive 'Finished' should not expect to receive
    -- any other parts afterwards.
    -- 
    | Finished r
    
    -- |
    -- Record a monadic action that needs to be performed when
    -- running the conveyor.
    -- 
    | ConveyorM (m (Conveyor i o s u m r))


instance Monad m => Functor (Conveyor i o s u m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance Monad m => Applicative (Conveyor i o s u m) where
    pure  = Finished
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (Conveyor i o s u m) where
    return = pure
    {-# INLINE return #-}
    (>>=)  = bindConveyors

instance MonadTrans (Conveyor i o s u) where
    lift m = ConveyorM (Finished <$> m)
    {-# INLINE [1] lift #-}
    
instance MonadIO m => MonadIO (Conveyor i o s u m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}


---------------------------------------------------------------------
-- Conveyor Composition and Fusion

-- |
-- Connect two conveyors end-to-end.
-- 
-- Runs the first conveyor until it produces a 'Finished' result, and
-- feeds that result into the input of the next conveyor function.
-- 
bindConveyors
    :: Monad m
    => Conveyor i o s u m result0
    -> (result0 -> Conveyor i o s u m result1)
    -> Conveyor i o s u m result1

bindConveyors conveyor machine = case conveyor of
    Convey o conveyor'
        -> Convey o (conveyor' >>= machine)
    Spare s conveyor'
        -> Spare s (conveyor' >>= machine)
    Machine onInput onFinal
        -> Machine (onInput >=> machine) (onFinal >=> machine)
    Finished result
        -> machine result
    ConveyorM action
        -> ConveyorM ((>>= machine) <$> action)

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
        Spare s _conveyorB'
            -> Void.absurd s
        -- Let conveyor A give us the next part.
        Machine onInput onFinal
            -> runConveyorA onInput onFinal conveyorA
        -- Conveying a finished product does nothing.
        Finished r
            -> Finished r
        ConveyorM m
            -> ConveyorM (continueB <$> m)
      where continueB = runConveyorB conveyorA
    
    runConveyorA onInput onFinal conveyorA = case conveyorA of
        -- Convey a part from conveyor A into a machine on conveyor B.
        Convey o conveyorA'
            -> runConveyorB conveyorA' (onInput o)
        -- Take spares off and continue.
        Spare s conveyorA'
            -> Spare s (continueA conveyorA')
        -- Run the machines that feed conveyor A and continue.
        Machine onInput' onFinal'
            -> Machine (continueA . onInput') (continueA . onFinal')
        -- If conveyor A finishes, feed the product to the appropriate
        -- machine on conveyor B.
        Finished r
            -> runConveyorB (Finished r) (onFinal r)
        ConveyorM m
            -> ConveyorM (continueA <$> m)
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
    Spare     s _       -> Void.absurd s
    Machine   _ onFinal -> runConveyor $ onFinal ()
    Finished  result    -> pure result
    ConveyorM m         -> m >>= runConveyor

-- |
-- Input any spare parts back into the conveyor process.
--
reuseSpares :: Monad m => Conveyor i o i u m r -> Conveyor i o s u m r
reuseSpares = go [] where
    go spares (Convey o conveyor)
        = Convey o (go spares conveyor)
    go spares (Spare s conveyor)
        = go (s : spares) conveyor
    go (s : spares) (Machine onInput _)
        = go spares $ onInput s
    go [] (Machine onInput onFinal)
        = Machine (go [] . onInput) (go [] . onFinal)
    go _spares (Finished r)
        = Finished r
    go spares (ConveyorM m)
        = ConveyorM (go spares <$> m)


---------------------------------------------------------------------
-- Conveyor Combinators

-- |
-- Yield a result onto a conveyor, to be brought downstream.
--     
yield :: o -> Conveyor i o s u m ()
yield o = Convey o (Finished ())

-- |
-- Await an input from a conveyor upstream.
-- 
await :: Conveyor i o s u m (Maybe i)
await = Machine onInput onFinal where
    onInput = Finished . Just
    onFinal = const $ Finished Nothing


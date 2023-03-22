
module Main where

import Conveyor
import Data.Functor.Identity


source :: Monad m => Int -> ConveyorT () Int m ()
source n = yield n >> source (n + 1)

takeC :: Monad m => Int -> ConveyorT i o m [i]
takeC = loop id where
    loop f count
        | count <= 0 = pure $ f []
        | otherwise  = await >>= maybe (pure $ f []) (\x -> loop (f . (x:)) (count - 1))



main :: IO ()
main = do
    let go n = yield n >> go (n + 1)

        x = runIdentity (runConveyor $ go 0 .| mapC (* 7) .| takeC 262144)

    print $ x `seq` ()


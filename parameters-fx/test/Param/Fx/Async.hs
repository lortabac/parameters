module Param.Fx.Async
  ( HasAsync,
    runAsync,
    replicateConcurrently_,
  )
where

import Control.Concurrent.Async qualified as Async
import Param.Fx

data AsyncEff

type HasAsync = HasEffect AsyncEff

runAsync :: ((HasAsync) => r) -> r
runAsync = runEffect @AsyncEff

replicateConcurrently_ :: (HasAsync) => Int -> Fx a -> Fx ()
replicateConcurrently_ n m = fxNoParam @AsyncEff $ Async.replicateConcurrently_ n (runFx m)

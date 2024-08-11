{-# LANGUAGE AllowAmbiguousTypes #-}

module Param.Fx.State
  ( -- * Untagged state effect
    HasState,
    runState,
    evalState,
    execState,
    get,
    put,
    modify,

    -- * Tagged state effect
    HasStateTagged,
    runStateTagged,
    evalStateTagged,
    execStateTagged,
    getTagged,
    putTagged,
    modifyTagged,
  )
where

import Control.Monad (void)
import Data.IORef
import Param
import Param.Fx

data StateParam (tag :: k)

-- | The context of a stateful computation
type HasState s = HasStateTagged DefaultTag s

-- | Same as 'HasState' but tagged with a unique type variable.
-- This allows multiple states to be in scope at the same time.
type HasStateTagged (tag :: k) s = HasParam (StateParam tag) (State s)

data DefaultTag

-- | Start a stateful computation.
-- It returns both the result of the computation and the final state.
runState :: forall s a. s -> ((HasState s) => Fx a) -> Fx (a, s)
runState = runStateTagged

-- | Start a stateful computation.
-- It returns the result of the computation.
evalState :: forall s a. s -> ((HasState s) => Fx a) -> Fx a
evalState = evalStateTagged

-- | Start a stateful computation.
-- It returns the final state.
execState :: forall s a. s -> ((HasState s) => Fx a) -> Fx s
execState = execStateTagged

-- | Retrieve the state
get :: forall s. (HasState s) => Fx s
get = getTagged @DefaultTag

-- | Replace the state
put :: forall s. (HasState s) => s -> Fx ()
put = putTagged @DefaultTag

-- | Modify the state
modify :: forall s. (HasState s) => (s -> s) -> Fx ()
modify = modifyTagged @DefaultTag

runStateTagged ::
  forall tag s a.
  s ->
  ((HasStateTagged tag s) => Fx a) ->
  Fx (a, s)
runStateTagged initState k = runParamFx @(StateParam tag) (mkState initState) $ do
  r <- k
  s <- readRef @tag (getRef @tag)
  pure (r, s)

evalStateTagged :: forall tag s a. s -> ((HasStateTagged tag s) => Fx a) -> Fx a
evalStateTagged s k = fst <$> runStateTagged @tag s k

execStateTagged :: forall tag s a. s -> ((HasStateTagged tag s) => Fx a) -> Fx s
execStateTagged s k = snd <$> runStateTagged @tag s k

getTagged :: forall tag s. (HasStateTagged tag s) => Fx s
getTagged = readRef @tag (getRef @tag)

putTagged :: forall tag s. (HasStateTagged tag s) => s -> Fx ()
putTagged s = fx @(StateParam tag) $ writeIORef (getRef @tag) s

modifyTagged :: forall tag s. (HasStateTagged tag s) => (s -> s) -> Fx ()
modifyTagged f = fx @(StateParam tag) $ atomicModifyIORef_ (getRef @tag) f

newtype State s = State {_ref :: IORef s}

mkState :: s -> IO (State s)
mkState s = State <$> newIORef s

readRef :: forall tag s. (HasStateTagged tag s) => IORef s -> Fx s
readRef ref = fx @(StateParam tag) $ readIORef ref

getRef :: forall tag s. (HasStateTagged tag s) => IORef s
getRef = _ref (ask @(StateParam tag))

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = void $ atomicModifyIORef' ref (\x -> (f x, ()))

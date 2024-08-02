{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StrictData #-}

module Param.Fx.State
  ( HasState,
    runState,
    evalState,
    execState,
    get,
    modify,
    put,
    HasTaggedState,
    runTaggedState,
    evalTaggedState,
    execTaggedState,
    getTagged,
    modifyTagged,
    putTagged,
  )
where

import Control.Monad (void)
import Data.IORef
import Param
import Param.Fx

data StateParam (tag :: k)

type HasTaggedState (tag :: k) s = HasParam (StateParam tag) (State s)

data DefaultTag

type HasState s = HasTaggedState DefaultTag s

runState :: forall s a. s -> ((HasState s) => Fx a) -> Fx (a, s)
runState = runTaggedState

evalState :: forall s a. s -> ((HasState s) => Fx a) -> Fx a
evalState = evalTaggedState

execState :: forall s a. s -> ((HasState s) => Fx a) -> Fx s
execState = execTaggedState

get :: forall s. (HasState s) => Fx s
get = getTagged @DefaultTag

modify :: forall s. (HasState s) => (s -> s) -> Fx ()
modify = modifyTagged @DefaultTag

put :: forall s. (HasState s) => s -> Fx ()
put = putTagged @DefaultTag

runTaggedState ::
  forall tag s a.
  s ->
  ((HasTaggedState tag s) => Fx a) ->
  Fx (a, s)
runTaggedState initState k = runParamFx @(StateParam tag) (mkState initState) $ do
  r <- k
  s <- readRef @tag (getRef @tag)
  pure (r, s)

evalTaggedState :: forall tag s a. s -> ((HasTaggedState tag s) => Fx a) -> Fx a
evalTaggedState s k = fst <$> runTaggedState @tag s k

execTaggedState :: forall tag s a. s -> ((HasTaggedState tag s) => Fx a) -> Fx s
execTaggedState s k = snd <$> runTaggedState @tag s k

getTagged :: forall tag s. (HasTaggedState tag s) => Fx s
getTagged = readRef @tag (getRef @tag)

modifyTagged :: forall tag s. (HasTaggedState tag s) => (s -> s) -> Fx ()
modifyTagged = modifyRef @tag (getRef @tag)

putTagged :: forall tag s. (HasTaggedState tag s) => s -> Fx ()
putTagged = writeRef @tag (getRef @tag)

data State s = State
  { _readRef :: IORef s -> IO s,
    _modifyRef :: IORef s -> (s -> s) -> IO (),
    _writeRef :: IORef s -> s -> IO (),
    _ref :: IORef s
  }

mkState :: s -> IO (State s)
mkState s = do
  ref <- newIORef s
  pure
    State
      { _readRef = readIORef,
        _modifyRef = \r f -> void $ atomicModifyIORef' r (\x -> (f x, ())),
        _writeRef = atomicWriteIORef,
        _ref = ref
      }

readRef :: forall tag s. (HasTaggedState tag s) => IORef s -> Fx s
readRef ref = fx @(StateParam tag) $ _readRef (ask @(StateParam tag)) ref

modifyRef :: forall tag s. (HasTaggedState tag s) => IORef s -> (s -> s) -> Fx ()
modifyRef ref f = fx @(StateParam tag) $ _modifyRef (ask @(StateParam tag)) ref f

writeRef :: forall tag s. (HasTaggedState tag s) => IORef s -> s -> Fx ()
writeRef ref s = fx @(StateParam tag) $ _writeRef (ask @(StateParam tag)) ref s

getRef :: forall tag s. (HasTaggedState tag s) => IORef s
getRef = _ref (ask @(StateParam tag))

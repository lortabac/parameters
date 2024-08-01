{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}

module Param.Fx
  ( Fx,
    runFx,
    fx,
    runParamFx,
    HasIO,
    runFxWithIO,
    embedIO,
  )
where

import GHC.Exts (WithDict (..))
import Param

-- | A restricted 'IO' type.
-- There are two ways to embed an 'IO' action into an 'Fx' one:
-- 1. Attach the action to a particular implicit parameter with 'fx'
-- 2. Embed an arbitrary 'IO' action with 'embedIO' (this requires the 'HasIO' context)
newtype Fx a = MkFx (IO a)
  deriving newtype (Functor, Applicative, Monad)

-- | Start an 'Fx' computation in which 'IO' actions can only be embedded with 'fx'
runFx :: Fx r -> IO r
runFx (MkFx k) = k

-- | Embed an 'IO' action into an 'Fx' one,
-- in a context where a given parameter is available.
-- If the parameter type is private a side-effect for that parameter can only be defined
-- inside the same module.
fx :: forall p a r. ((HasParam p a) => IO r) -> (HasParam p a) => Fx r
fx k = MkFx k

-- | A variant of 'runParam' that lets you perform IO in order to construct the parameter
runParamFx :: forall p a r. IO a -> ((HasParam p a) => Fx r) -> Fx r
runParamFx acquire k = MkFx $ do
  p <- acquire
  runParam @p p (runFx k)

-- | This class indicates that arbitrary 'IO' actions can be embedded
-- into 'Fx' ones.
class HasIO where
  embedIO' :: EmbedIO

-- | Start an 'Fx' computation in which arbitrary 'IO' actions
-- can be embedded with 'embedIO'
runFxWithIO :: ((HasIO) => Fx r) -> IO r
runFxWithIO k = withDict @HasIO (EmbedIO MkFx) (runFx k)

-- | Embed an arbitrary 'IO' action into an 'Fx' one
embedIO :: (HasIO) => IO a -> Fx a
embedIO = getEmbedIO embedIO'

-- | Internal rank-2 helper
newtype EmbedIO = EmbedIO {getEmbedIO :: forall a. IO a -> Fx a}

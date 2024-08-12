{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}

module Param.Fx
  ( -- * Fx monad
    Fx,
    runFx,

    -- * Effects that depend on parameters
    fx,
    runParamFx,

    -- * Parameterless effects
    HasEffect,
    runEffect,
    fxNoParam,

    -- * Running arbitrary IO
    HasIO,
    runFxWithIO,
    embedIO,
  )
where

import GHC.Exts (WithDict (..))
import Param

-- | A restricted 'IO' type.
-- There are three ways to embed an 'IO' action into an 'Fx' one:
--
-- 1. Attach the action to an implicit parameter with 'fx'
-- 
-- 2. Attach the action to a parameterless effect with 'fxNoParam'
--
-- 3. If the 'Fx' action has been started with 'runFxWithIO',
-- embed an arbitrary 'IO' action with 'embedIO' (this requires the 'HasIO' context)
newtype Fx a = MkFx (IO a)
  deriving newtype (Functor, Applicative, Monad)

-- | Start an 'Fx' action in which 'IO' actions can only be embedded with 'fx'
runFx :: Fx r -> IO r
runFx (MkFx k) = k

-- | Embed an 'IO' action into an 'Fx' one,
-- in a context where a given parameter is available.
fx :: forall p a r. (HasParam p a) => ((HasParam p a) => IO r) -> Fx r
fx k = MkFx k

-- | A variant of 'runParam' that lets you perform 'IO' in order to construct the parameter
runParamFx :: forall p a r. IO a -> ((HasParam p a) => Fx r) -> Fx r
runParamFx acquire k = MkFx $ do
  p <- acquire
  runParam @p p (runFx k)

-- | The context of an effect that does not depend on an implicit parameter
class HasEffect eff where
  _hasEffect :: ()

-- | Start a computation with a parameterless effect
runEffect :: forall eff r. ((HasEffect eff) => r) -> r
runEffect = withDict @(HasEffect eff) ()

-- | Embed an 'IO' action into an 'Fx' one,
-- in a context where a given parameterless effect is in scope.
fxNoParam :: forall eff r. (HasEffect eff) => ((HasEffect eff) => IO r) -> Fx r
fxNoParam k = MkFx k

-- | The context in which arbitrary 'IO' actions can be embedded
-- into 'Fx' ones (the equivalent of @MonadIO@ for this library)
class HasIO where
  embedIO' :: EmbedIO

-- | Start an 'Fx' computation in which arbitrary 'IO' actions
-- can be embedded with 'embedIO'
runFxWithIO :: ((HasIO) => Fx r) -> IO r
runFxWithIO k = withDict @HasIO (EmbedIO MkFx) (runFx k)

-- | Embed an arbitrary 'IO' action into an 'Fx' one
-- (the equivalent of @liftIO@ for this library)
embedIO :: (HasIO) => IO a -> Fx a
embedIO = getEmbedIO embedIO'

-- | Internal rank-2 helper
newtype EmbedIO = EmbedIO {getEmbedIO :: forall a. IO a -> Fx a}

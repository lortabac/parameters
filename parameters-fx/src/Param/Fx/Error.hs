{-# LANGUAGE AllowAmbiguousTypes #-}

module Param.Fx.Error
  ( -- * Untagged error effect
    HasError,
    runError,
    throwError,
    handleError,
    catchError,
    tryError,

    -- * Tagged error effect
    HasErrorTagged,
    runErrorTagged,
    throwErrorTagged,
    catchErrorTagged,
    handleErrorTagged,
    tryErrorTagged,
  )
where

import Control.Exception
import Data.Typeable (Typeable)
import GHC.Exts (Any)
import Param
import Param.Fx
import Unsafe.Coerce (unsafeCoerce)

data ErrorParam (tag :: k)

-- | The context of a computation in which errors of type 'e' may be thrown
-- and optionally caught
type HasError e = HasErrorTagged DefaultTag e

-- | Same as 'HasError' but tagged with a type variable.
-- This allows multiple error effects to be in scope at the same time.
type HasErrorTagged (tag :: k) e = HasParam (ErrorParam tag) ()

data DefaultTag

-- | Start a computation in which errors of type 'e' may be thrown
-- and optionally caught
runError :: forall e a. ((HasError e) => Fx a) -> Fx (Either e a)
runError = runErrorTagged

-- | Throw an error that may either be caught and handled inside
-- the effectful computation or cause the whole effectful computation
-- to fail if uncaught.
throwError :: forall e a. (HasError e) => e -> Fx a
throwError = throwErrorTagged @DefaultTag

-- | Catch an error inside the effectful computation
catchError ::
  forall e a.
  (HasError e) =>
  ((HasError e) => Fx a) ->
  (e -> Fx a) ->
  Fx a
catchError = catchErrorTagged

-- | Same as 'catchError' with flipped arguments
handleError ::
  forall e a.
  (HasError e) =>
  (e -> Fx a) ->
  ((HasError e) => Fx a) ->
  Fx a
handleError = handleErrorTagged

-- | Handle an error as an 'Either' inside the effectful computation
tryError ::
  forall e a.
  (HasError e) =>
  ((HasError e) => Fx a) ->
  Fx (Either e a)
tryError = tryErrorTagged

type TypeablePoly (a :: k) = (Typeable a, Typeable k)

newtype ErrorWrapper (tag :: k) = ErrorWrapper Any
  deriving (Typeable)

instance Show (ErrorWrapper tag) where
  show (ErrorWrapper _) = show "Param.Fx.Error.ErrorEx"

instance
  (Typeable tag, Typeable k) =>
  Exception (ErrorWrapper (tag :: k))

runErrorTagged ::
  forall tag e a.
  (TypeablePoly tag) =>
  ((HasErrorTagged tag e) => Fx a) ->
  Fx (Either e a)
runErrorTagged k = runParam @(ErrorParam tag) () $
  fx @(ErrorParam tag) $
    catch (Right <$> runFx k) $
      \(ErrorWrapper err :: ErrorWrapper tag) -> pure (Left (unsafeCoerce err :: e))

throwErrorTagged ::
  forall tag e a.
  (HasErrorTagged tag e, TypeablePoly tag) =>
  e ->
  Fx a
throwErrorTagged e =
  fx @(ErrorParam tag) $
    throwIO @(ErrorWrapper tag) (ErrorWrapper (unsafeCoerce e :: Any))

catchErrorTagged ::
  forall tag e a.
  (HasErrorTagged tag e, TypeablePoly tag) =>
  ((HasErrorTagged tag e) => Fx a) ->
  (e -> Fx a) ->
  Fx a
catchErrorTagged action handler = fx @(ErrorParam tag) $
  catch (runFx action) $ \(ErrorWrapper err :: ErrorWrapper tag) ->
    runFx $ handler (unsafeCoerce err :: e)

handleErrorTagged ::
  forall tag e a.
  (HasErrorTagged tag e, TypeablePoly tag) =>
  (e -> Fx a) ->
  ((HasErrorTagged tag e) => Fx a) ->
  Fx a
handleErrorTagged handler action = catchErrorTagged @tag action handler

tryErrorTagged ::
  forall tag e a.
  (HasErrorTagged tag e, TypeablePoly tag) =>
  ((HasErrorTagged tag e) => Fx a) ->
  Fx (Either e a)
tryErrorTagged action = catchErrorTagged @tag (Right <$> action) (pure . Left)

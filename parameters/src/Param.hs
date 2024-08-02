{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Param
  ( HasParam,
    RunParam,
    runParam,
    paramLocal,
    paramLocalM,
    paramAsk,
  )
where

import GHC.Base (IP (..))
import GHC.Exts (WithDict (..))
import GHC.TypeLits (Symbol)
import Param.Internal.IsParam (IsParam')

-- | The context in which an implicit parameter is available
type RunParam p a = (HasParam p a, IsParam p)

type HasParam p a = IP (IPName p) a

type IsParam p = IsParam' p 0

-- | Retrieve the value of an implicit parameter (requires a type-application)
paramAsk :: forall p a. (HasParam p a) => a
paramAsk = ip @(IPName p)

-- | Start a computation in which an implicit parameter is available
runParam :: forall p a r. a -> ((RunParam p a) => r) -> r
runParam x k = runParam' @0 @p x k

runParam' :: forall n p a r. a -> ((HasParam p a, IsParam' p n) => r) -> r
runParam' x k = withDict @(IP (IPName p) a) x (withDict @(IsParam' p n) '\0' k)

-- | Modify an implicit parameter locally
paramLocal ::
  forall p a r.
  (HasParam p a) =>
  (a -> a) ->
  ((HasParam p a) => r) ->
  r
paramLocal f = withDict @(IP (IPName p) a) (f (ip @(IPName p)))

-- | Modify an implicit parameter locally in a monadic context
paramLocalM ::
  forall p a m r.
  (HasParam p a, Monad m) =>
  (a -> m a) ->
  ((HasParam p a) => m r) ->
  m r
paramLocalM f mk = do
  r <- f (ip @(IPName p))
  k <- mk
  pure (withDict @(IP (IPName p) a) r k)

-- | A hack to be able to use 'IP' with types instead of 'Symbol's
type family IPName p = (r :: Symbol) | r -> p

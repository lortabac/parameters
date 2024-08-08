{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Param
  ( HasParam,
    runParam,
    ask,
  )
where

import GHC.Base (IP (..))
import GHC.Exts (WithDict (..))
import GHC.TypeLits (Symbol)
import Param.Internal (UnivocalParam (..))

-- | The context in which an implicit parameter is available
type HasParam p a = (IP (IPName p) a, UnivocalParam p 0)

-- | Start a computation in which an implicit parameter is available
runParam :: forall p a r. a -> ((HasParam p a) => r) -> r
runParam x k = withDict @(IP (IPName p) a) x (withDict @(UnivocalParam p 0) () k)

-- | Retrieve the value of an implicit parameter (requires a type-application)
ask :: forall p a. (HasParam p a) => a
ask = ip @(IPName p)

-- | A hack to be able to use 'IP' with types instead of 'Symbol's
type family IPName p = (r :: Symbol) | r -> p

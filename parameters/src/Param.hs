{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Param
  ( HasParam,
    runParam,
    ask,
    local,
  )
where

import GHC.Base (IP (..))
import GHC.Exts (WithDict (..))
import GHC.TypeLits (Symbol)
import Param.Internal (UnivocalParam (..))

-- | The context in which an implicit parameter is available
type HasParam p a = (HasParam' p a, UnivocalParam p 0)

type HasParam' p a = IP (IPName p) a

-- | Start a computation in which an implicit parameter is available
runParam :: forall p a r. a -> ((HasParam p a) => r) -> r
runParam x k = withDict @(HasParam' p a) x (withDict @(UnivocalParam p 0) () k)

-- | Retrieve the value of an implicit parameter.
--
-- When the plugin is enabled, a type error is raised if the parameter
-- cannot be solved unambiguously.
ask :: forall p a. (HasParam p a) => a
ask = ip @(IPName p)

-- | Modify an implicit parameter locally.
--
-- Warning: this function is provided for completeness' sake but it suffers from
-- various problems and limitations. In particular:
--
-- - The @HasParam'@ type is intentionally hidden because it
-- allows you to bypass the restrictions enforced by the plugin.
-- This means that you can't easily create abstractions on top of 'local'.
--
-- - It doesn't interact well with the dreaded @MonomorphismRestriction@.
-- To avoid surprises, set @NoMonomorphismRestriction@ in all the modules where
-- 'local' is used.
local :: forall p a r. (HasParam p a) => (a -> a) -> ((HasParam' p a) => r) -> r
local f k = withDict @(HasParam' p a) (f (ip @(IPName p))) k

-- | A hack to be able to use 'IP' with types instead of 'Symbol's
type family IPName p = (r :: Symbol) | r -> p

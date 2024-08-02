{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Param.Internal.IsParam (IsParam', OverriddenParamError) where

import GHC.TypeError
import GHC.TypeLits (Nat)

class IsParam' p (n :: Nat) where
  _isParam :: Char

class (TypeError (Text "Overridden parameter")) => OverriddenParamError where
  _overriddenParamError :: Char

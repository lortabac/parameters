{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Param.Internal
  ( UnivocalParam (..),
  )
where

import GHC.TypeLits (Nat)

-- | Magic constraint.
-- When the plugin is enabled this constraint fails if the parameter 'p'
-- has multiple definitions in scope.
class UnivocalParam p (n :: Nat) where
  _univocalParam :: ()

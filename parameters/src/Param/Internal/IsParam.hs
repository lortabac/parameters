{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Param.Internal.IsParam
  ( IsParam (..),
  )
where

import GHC.TypeLits (Nat)

class IsParam p (n :: Nat) where
  _isParam :: ()

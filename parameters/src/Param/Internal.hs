{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Param.Internal
  ( UnivocalParam (..),
  )
where

import GHC.TypeLits (Nat)

class UnivocalParam p (n :: Nat) where
  _univocalParam :: ()

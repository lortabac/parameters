{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Param.RunParam (RunParam) where

import GHC.TypeError

-- | A magical constraint that triggers a type error if the parameter is already in scope.
-- Solving this constraint requires the 'Param.Plugin' plugin.
type RunParam p = Assert (ParamIsNotInScope p) (ParamIsAlreadyInScopeErr p)

type family ParamIsNotInScope p :: Bool where
  ParamIsNotInScope _ = TypeError (Text "Implicit parameters require the Param.Plugin plugin")

type ParamIsAlreadyInScopeErr p = TypeError (Text "Parameter ‘" :<>: ShowType p :<>: Text "’ is already in scope")

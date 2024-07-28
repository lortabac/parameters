{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Param.RunParam (RunParam) where

import GHC.TypeError

type RunParam p a = Assert (ParamIsNotInScope p a) (ParamIsAlreadyInScopeErr p a)

type family ParamIsNotInScope p a :: Bool where
  ParamIsNotInScope _ _ = TypeError (Text "Implicit parameters require the Param.Plugin plugin")

type ParamIsAlreadyInScopeErr p a = TypeError (Text "Parameter ‘" :<>: ShowType p :<>: Text "’ is already in scope")

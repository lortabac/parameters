{-# LANGUAGE NamedFieldPuns #-}

module Param.Plugin (plugin) where

import Control.Monad (filterM)
import Debug.Trace
import GHC (Class, Module, PkgQual (NoPkgQual), TyCon)
import GHC.Builtin.Names (ipClassName)
import GHC.Core.Class (Class (..))
import GHC.Core.Reduction (Reduction (..))
import GHC.Core.TyCo.Rep (UnivCoProvenance (..))
import GHC.Plugins
  ( Coercion,
    Outputable,
    Plugin (..),
    Role (..),
    Type,
    defaultPlugin,
    mkModuleName,
    mkTcOcc,
    mkUnivCo,
    promotedFalseDataCon,
    promotedTrueDataCon,
    showPprUnsafe,
    splitTyConApp,
    tyConAppTyCon_maybe,
  )
import GHC.Tc.Plugin
  ( FindResult (..),
    findImportedModule,
    lookupOrig,
    tcLookupClass,
    tcLookupTyCon,
    zonkTcType,
  )
import GHC.Tc.Types
  ( TcPlugin (..),
    TcPluginM,
    TcPluginRewriteResult (..),
    TcPluginRewriter,
    TcPluginSolveResult (..),
  )
import GHC.Tc.Types.Constraint (ctPred)
import GHC.Tc.Utils.TcMType (TcType)
import GHC.Tc.Utils.TcType (mkTyConApp, mkTyConTy)
import GHC.Types.Unique.FM (UniqFM, unitUFM)

data ParamPluginState = ParamPluginState
  { paramIsNotInScopeFam :: TyCon,
    ipNameFam :: TyCon
  }

plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin = \_ -> Just runParamPlugin
    }

runParamPlugin :: TcPlugin
runParamPlugin =
  TcPlugin
    { tcPluginInit = initRunParamPlugin,
      tcPluginSolve = \_ _ _ _ -> pure $ TcPluginOk [] [],
      tcPluginRewrite = rewriteParamIsNotInScopeCt,
      tcPluginStop = \_ -> pure ()
    }

initRunParamPlugin :: TcPluginM ParamPluginState
initRunParamPlugin = do
  ipNameMdl <- lookupModule "Param.IPName"
  runParamMdl <- lookupModule "Param.RunParam"
  paramIsNotInScopeFam <- lookupFam runParamMdl "ParamIsNotInScope"
  ipNameFam <- lookupFam ipNameMdl "IPName"
  pure
    ParamPluginState
      { paramIsNotInScopeFam,
        ipNameFam
      }

rewriteParamIsNotInScopeCt :: ParamPluginState -> UniqFM TyCon TcPluginRewriter
rewriteParamIsNotInScopeCt state = unitUFM (paramIsNotInScopeFam state) (paramIsNotInScopeCtRewriter state)

paramIsNotInScopeCtRewriter :: ParamPluginState -> TcPluginRewriter
paramIsNotInScopeCtRewriter state _ givens args@[_, _, argP, _] = do
  argP' <- zonkTcType argP
  ipCts <- filterM (isIPTyConApp (ipNameFam state) argP') $ map (splitTyConApp . ctPred) givens
  if null ipCts
    then pure (TcPluginRewriteTo (reduction true) [])
    else pure (TcPluginRewriteTo (reduction false) [])
  where
    reduction b = mkTyFamAppReduction "ParamIsNotInScope" Nominal (paramIsNotInScopeFam state) args b
    true = mkTyConTy promotedTrueDataCon
    false = mkTyConTy promotedFalseDataCon
paramIsNotInScopeCtRewriter _ _ _ args = do
  tracePpr args
  error "The impossible happened! ParamIsNotIScope is called with the wrong argument count"

isIPTyConApp :: TyCon -> Type -> (TyCon, [Type]) -> TcPluginM Bool
isIPTyConApp ipNameFam wantedArgP (tyCon, givenArgP : _) = do
  ipClass <- lookupIPClass
  let wantedPTyCon = tyConAppTyCon_maybe wantedArgP
      givenPTyConApp = splitTyConApp givenArgP
  if tyCon == classTyCon ipClass
    then case (wantedPTyCon, givenPTyConApp) of
      (Just wtc, (ipNameTC, [gtc])) ->
        pure
          ( ipNameTC == ipNameFam
              && Just wtc == tyConAppTyCon_maybe gtc
          )
      _ -> pure False
    else pure False
isIPTyConApp _ _ _ = pure False

lookupModule :: String -> TcPluginM Module
lookupModule name = do
  findResult <- findImportedModule (mkModuleName name) NoPkgQual
  case findResult of
    Found _ mdl -> pure mdl
    _ -> error ("Param.Plugin couldn't find module: " <> name)

lookupFam :: Module -> String -> TcPluginM TyCon
lookupFam mdl fam = do
  famName <- lookupOrig mdl (mkTcOcc fam)
  tcLookupTyCon famName

lookupIPClass :: TcPluginM Class
lookupIPClass = tcLookupClass ipClassName

mkTyFamAppReduction ::
  -- | Name of reduction (for debugging)
  String ->
  -- | Role of reduction ('Nominal' or 'Representational')
  Role ->
  -- | Type family 'TyCon'
  TyCon ->
  -- | Type family arguments
  [TcType] ->
  -- | The type that the type family application reduces to
  TcType ->
  Reduction
mkTyFamAppReduction str role tc args ty =
  Reduction (mkPluginUnivCo str role (mkTyConApp tc args) ty) ty

mkPluginUnivCo ::
  -- | Name of equality (for the plugin's internal use, or for debugging)
  String ->
  Role ->
  -- | LHS
  TcType ->
  -- | RHS
  TcType ->
  Coercion
mkPluginUnivCo str role lhs rhs = mkUnivCo (PluginProv str) role lhs rhs

tracePpr :: (Monad m, Outputable a) => a -> m ()
tracePpr = traceM . showPprUnsafe

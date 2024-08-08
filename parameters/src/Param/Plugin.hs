{-# LANGUAGE NamedFieldPuns #-}

module Param.Plugin (plugin) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (mapMaybe)
import Data.Unique (hashUnique, newUnique)
import Debug.Trace
import GHC (Class, Module, PkgQual (NoPkgQual))
import GHC.Core.Class (Class (..))
import GHC.Plugins
  ( Outputable,
    Plugin (..),
    Type,
    defaultPlugin,
    emptyUFM,
    eqType,
    isNumLitTy,
    mkModuleName,
    mkNumLitTy,
    mkTcOcc,
    showPprUnsafe,
    splitTyConApp_maybe,
  )
import GHC.Tc.Plugin
  ( FindResult (..),
    findImportedModule,
    lookupOrig,
    tcLookupClass,
    unsafeTcPluginTcM,
  )
import GHC.Tc.Types
  ( TcPlugin (..),
    TcPluginM,
    TcPluginSolveResult (..),
    TcPluginSolver,
  )
import GHC.Tc.Types.Constraint
  ( Ct (..),
    ctEvTerm,
    ctPred,
  )
import GHC.Tc.Types.Evidence (EvTerm (..))

data ParamPluginState = ParamPluginState
  { isParamClass :: Class
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
      tcPluginSolve = checkHasParamGivens,
      tcPluginRewrite = \_ -> emptyUFM,
      tcPluginStop = \_ -> pure ()
    }

initRunParamPlugin :: TcPluginM ParamPluginState
initRunParamPlugin = do
  isParamMdl <- lookupModule "Param.Internal.IsParam"
  isParamClass <- lookupClass isParamMdl "IsParam"
  pure
    ParamPluginState
      { isParamClass
      }

checkHasParamGivens :: ParamPluginState -> TcPluginSolver
checkHasParamGivens state _ givens [] = do
  let isParam0Cts = getIsParam0Cts (isParamClass state) givens
      solved = map (\ct -> (evTermFromCt ct, ct)) isParam0Cts
  uniqueIsParam0Cts <- traverse makeIsParamCtUnique isParam0Cts
  pure $ TcPluginOk solved uniqueIsParam0Cts
checkHasParamGivens state _ givens wanteds = do
  let wantedParamTypes = getIsParamParamTypes (isParamClass state) wanteds
      isParam0CtsGivenMap = map (\ty -> (ty, getIsParamCtsForParam (isParamClass state) givens ty)) wantedParamTypes
      solvableParamTypes = mapMaybe getSolvableType isParam0CtsGivenMap
      isParamSolvableWanteds = concatMap (getIsParamCtsForParam (isParamClass state) wanteds) solvableParamTypes
      solved = map (\ct -> (evTermFromCt ct, ct)) isParamSolvableWanteds
  pure $ TcPluginOk solved []

getSolvableType :: (Type, [Ct]) -> Maybe Type
getSolvableType (ty, cts) = case cts of
  [_] -> Just ty
  _ -> Nothing

evTermFromCt :: Ct -> EvTerm
evTermFromCt CDictCan {cc_ev = ev} = ctEvTerm ev
evTermFromCt _ = error "The impossible happened: IsParam Ct is not a CDictCan"

makeIsParamCtUnique :: Ct -> TcPluginM Ct
makeIsParamCtUnique ct@CDictCan {cc_tyargs = [t, p, _]} = do
  n <- unsafeTcPluginTcM $ liftIO $ fromIntegral . hashUnique <$> newUnique
  pure ct {cc_tyargs = [t, p, mkNumLitTy n]}
makeIsParamCtUnique ct = pure ct

getIsParam0Cts :: Class -> [Ct] -> [Ct]
getIsParam0Cts isParamClass = filter (isIsParam0Ct isParamClass)

isIsParam0Ct :: Class -> Ct -> Bool
isIsParam0Ct isParamClass ct = case splitTyConApp_maybe (ctPred ct) of
  Just (ctCon, [_, _, n]) ->
    ctCon == classTyCon isParamClass
      && isNumLitTy n == Just 0
  _ -> False

getIsParamCtsForParam :: Class -> [Ct] -> Type -> [Ct]
getIsParamCtsForParam isParamClass cts p =
  filter (isIsParamCtForParam isParamClass p) cts

isIsParamCtForParam :: Class -> Type -> Ct -> Bool
isIsParamCtForParam isParamClass paramTy ct =
  case splitTyConApp_maybe (ctPred ct) of
    Just (ctCon, [_, p, _]) ->
      ctCon == classTyCon isParamClass
        && eqType paramTy p
    _ -> False

getIsParamParamTypes :: Class -> [Ct] -> [Type]
getIsParamParamTypes isParamClass =
  mapMaybe (getIsParamParamTypeFromCt isParamClass)

getIsParamParamTypeFromCt :: Class -> Ct -> Maybe Type
getIsParamParamTypeFromCt isParamClass ct =
  case splitTyConApp_maybe (ctPred ct) of
    Just (ctCon, [_, p, _]) ->
      if ctCon == classTyCon isParamClass
        then Just p
        else Nothing
    _ -> Nothing

lookupModule :: String -> TcPluginM Module
lookupModule name = do
  findResult <- findImportedModule (mkModuleName name) NoPkgQual
  case findResult of
    Found _ mdl -> pure mdl
    _ -> error ("Param.Plugin couldn't find module: " <> name)

lookupClass :: Module -> String -> TcPluginM Class
lookupClass mdl fam = do
  famName <- lookupOrig mdl (mkTcOcc fam)
  tcLookupClass famName

_tracePpr :: (Monad m, Outputable a) => a -> m ()
_tracePpr = traceM . showPprUnsafe

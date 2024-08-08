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
  { univocalParamClass :: Class
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
  internalMdl <- lookupModule "Param.Internal"
  univocalParamClass <- lookupClass internalMdl "UnivocalParam"
  pure
    ParamPluginState
      { univocalParamClass
      }

checkHasParamGivens :: ParamPluginState -> TcPluginSolver
checkHasParamGivens state _ givens [] = do
  let univocalParam0Cts = getUnivocalParam0Cts (univocalParamClass state) givens
      solved = map (\ct -> (evTermFromCt ct, ct)) univocalParam0Cts
  uniqueUnivocalParam0Cts <- traverse makeUnivocalParamCtUnique univocalParam0Cts
  pure $ TcPluginOk solved uniqueUnivocalParam0Cts
checkHasParamGivens state _ givens wanteds = do
  let wantedParamTypes = getUnivocalParamParamTypes (univocalParamClass state) wanteds
      univocalParam0CtsGivenMap =
        map
          (\ty -> (ty, getUnivocalParamCtsForParam (univocalParamClass state) givens ty))
          wantedParamTypes
      solvableParamTypes = mapMaybe getSolvableType univocalParam0CtsGivenMap
      univocalParamSolvableWanteds =
        concatMap
          (getUnivocalParamCtsForParam (univocalParamClass state) wanteds)
          solvableParamTypes
      solved = map (\ct -> (evTermFromCt ct, ct)) univocalParamSolvableWanteds
  pure $ TcPluginOk solved []

getSolvableType :: (Type, [Ct]) -> Maybe Type
getSolvableType (ty, cts) = case cts of
  [] -> Just ty
  [_] -> Just ty
  _ -> Nothing

evTermFromCt :: Ct -> EvTerm
evTermFromCt CDictCan {cc_ev = ev} = ctEvTerm ev
evTermFromCt _ = error "The impossible happened: UnivocalParam Ct is not a CDictCan"

makeUnivocalParamCtUnique :: Ct -> TcPluginM Ct
makeUnivocalParamCtUnique ct@CDictCan {cc_tyargs = [t, p, _]} = do
  n <- unsafeTcPluginTcM $ liftIO $ fromIntegral . hashUnique <$> newUnique
  pure ct {cc_tyargs = [t, p, mkNumLitTy n]}
makeUnivocalParamCtUnique ct = pure ct

getUnivocalParam0Cts :: Class -> [Ct] -> [Ct]
getUnivocalParam0Cts univocalParamClass = filter (isUnivocalParam0Ct univocalParamClass)

isUnivocalParam0Ct :: Class -> Ct -> Bool
isUnivocalParam0Ct univocalParamClass ct = case splitTyConApp_maybe (ctPred ct) of
  Just (ctCon, [_, _, n]) ->
    ctCon == classTyCon univocalParamClass
      && isNumLitTy n == Just 0
  _ -> False

getUnivocalParamCtsForParam :: Class -> [Ct] -> Type -> [Ct]
getUnivocalParamCtsForParam univocalParamClass cts p =
  filter (isUnivocalParamCtForParam univocalParamClass p) cts

isUnivocalParamCtForParam :: Class -> Type -> Ct -> Bool
isUnivocalParamCtForParam univocalParamClass paramTy ct =
  case splitTyConApp_maybe (ctPred ct) of
    Just (ctCon, [_, p, _]) ->
      ctCon == classTyCon univocalParamClass
        && eqType paramTy p
    _ -> False

getUnivocalParamParamTypes :: Class -> [Ct] -> [Type]
getUnivocalParamParamTypes univocalParamClass =
  mapMaybe (getUnivocalParamParamTypeFromCt univocalParamClass)

getUnivocalParamParamTypeFromCt :: Class -> Ct -> Maybe Type
getUnivocalParamParamTypeFromCt univocalParamClass ct =
  case splitTyConApp_maybe (ctPred ct) of
    Just (ctCon, [_, p, _]) ->
      if ctCon == classTyCon univocalParamClass
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

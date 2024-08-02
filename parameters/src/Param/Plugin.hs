{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Param.Plugin (plugin) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Unique (hashUnique, newUnique)
import Debug.Trace
import GHC (Class, Module, PkgQual (NoPkgQual), TyCon)
import GHC.Core.Class (Class (..))
import GHC.Plugins
  ( Expr (..),
    Literal (..),
    Outputable,
    Plugin (..),
    Type,
    defaultPlugin,
    emptyUFM,
    eqType,
    isNumLitTy,
    mkModuleName,
    mkNumLitTy,
    mkTcOcc,
    nonDetCmpType,
    showPprUnsafe,
    splitTyConApp,
    splitTyConApp_maybe,
  )
import GHC.Tc.Plugin
  ( FindResult (..),
    findImportedModule,
    lookupOrig,
    tcLookupClass,
    unsafeTcPluginTcM,
    zonkTcType,
  )
import GHC.Tc.Types
  ( TcPlugin (..),
    TcPluginM,
    TcPluginSolveResult (..),
    TcPluginSolver,
  )
import GHC.Tc.Types.Constraint
  ( Ct (..),
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
  isParamClass <- lookupClass isParamMdl "IsParam'"
  pure
    ParamPluginState
      { isParamClass
      }

checkHasParamGivens :: ParamPluginState -> TcPluginSolver
checkHasParamGivens state _ givens [] = do
  let isParam0Cts = getIsParam0Cts (isParamClass state) givens
      ev = EvExpr $ Lit (LitChar '\0')
      solved = map (ev,) isParam0Cts
      isParamArgs = getIsParamArgs (isParamClass state) $ map (splitTyConApp . ctPred) givens
  uniqueIsParam0Cts <- traverse makeIsParamCtUnique isParam0Cts
  args <- traverse (bitraverse zonkTcType zonkTcType) isParamArgs
  let duplicates = getDuplicateParams $ groupByFstType args
  case duplicates of
    d : _ ->
      error ("Overridden parameter: '" ++ showPprUnsafe d ++ "'")
    [] -> pure $ TcPluginOk solved uniqueIsParam0Cts
checkHasParamGivens _ _ _ _ = pure $ TcPluginOk [] []

getDuplicateParams :: Map OrdType [Type] -> [Type]
getDuplicateParams g = unOrdType <$> Map.keys (Map.filter (\ts -> length ts > 1) g)

groupByFstType :: [(Type, Type)] -> Map OrdType [Type]
groupByFstType = foldr (\(p, n) -> Map.insertWith (++) (OrdType p) [n]) mempty

newtype OrdType = OrdType {unOrdType :: Type}

instance Eq OrdType where
  OrdType x == OrdType y = eqType x y

instance Ord OrdType where
  compare (OrdType x) (OrdType y) = nonDetCmpType x y

makeIsParamCtUnique :: Ct -> TcPluginM Ct
makeIsParamCtUnique ct@CDictCan {cc_tyargs = [t, p, _]} = do
  n <- unsafeTcPluginTcM $ liftIO $ fromIntegral . hashUnique <$> newUnique
  pure ct {cc_tyargs = [t, p, mkNumLitTy n]}
makeIsParamCtUnique ct = pure ct

getIsParamArgs :: Class -> [(TyCon, [Type])] -> [(Type, Type)]
getIsParamArgs = mapMaybe . getIsParamArg

getIsParamArg :: Class -> (TyCon, [Type]) -> Maybe (Type, Type)
getIsParamArg isParamClass (tyCon, [_, param, ty]) =
  if tyCon == classTyCon isParamClass
    then Just (param, ty)
    else Nothing
getIsParamArg _ _ = Nothing

getIsParam0Cts :: Class -> [Ct] -> [Ct]
getIsParam0Cts isParamClass = filter (isIsParam0Ct isParamClass)

isIsParam0Ct :: Class -> Ct -> Bool
isIsParam0Ct isParamClass ct = case splitTyConApp_maybe (ctPred ct) of
  Just (ctCon, [_, _, n]) ->
    ctCon == classTyCon isParamClass
      && isNumLitTy n == Just 0
  _ -> False

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

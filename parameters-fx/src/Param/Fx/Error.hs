{-# OPTIONS_GHC -fplugin Param.Plugin #-}

module Param.Fx.Error () where

import Control.Exception
import Data.Dynamic (Typeable)
import Param
import Param.Fx

data ErrorParam (tag :: k) e

type HasTaggedError (tag :: k) e a = HasParam (ErrorParam tag e) ()

data ErrorWrapper (tag :: k) e = ErrorWrapper e
  deriving (Show)

instance (Typeable tag, Typeable k, Typeable e, Show e) => Exception (ErrorWrapper (tag :: k) e)

runTaggedError' ::
  forall k tag e a.
  (Typeable tag, Typeable e, Typeable k, Show e) =>
  ((HasTaggedError (tag :: k) e a) => IO a) ->
  IO (Either e a)
runTaggedError' k = undefined

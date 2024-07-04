{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Time
import Param.Fx
import Param.Fx.Time
import Param.Fx.Time.Mock
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "parameters-fx tests"
      [ testCase "Static time" $ do
          now <- getCurrentTime
          res <- liftIO $ runFx (timeTestStatic now)
          res @?= 1,
        testCase "Static time with embedded IO" $ do
          res <- liftIO $ runFxWithIO timeTestStaticIO
          res @?= 1
      ]

timeTestStatic :: UTCTime -> Fx Double
timeTestStatic now = do
  runStaticTime now 60 getMonotonicTimeMinutes

timeTestStaticIO :: (HasIO) => Fx Double
timeTestStaticIO = do
  now <- embedIO getCurrentTime
  runStaticTime now 60 getMonotonicTimeMinutes

getMonotonicTimeMinutes :: (HasTime) => Fx Double
getMonotonicTimeMinutes = fmap (/ 60) monotonicTime

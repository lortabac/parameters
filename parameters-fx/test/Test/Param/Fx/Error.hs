{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin Param.Plugin #-}

module Test.Param.Fx.Error (errorTests) where

import Param.Fx
import Param.Fx.Error
import Test.Tasty
import Test.Tasty.HUnit

errorTests :: TestTree
errorTests =
  testGroup
    "state"
    [ testCase "basic untagged no error" $ do
        res <- runFx $ runError @() (pure "foo")
        res @?= Right "foo",
      testCase "basic untagged error" $ do
        res <- runFx $ runError (throwError "bar" >> pure ())
        res @?= Left "bar",
      testCase "basic untagged multiple errors" $ do
        res <- runFx $ runError (throwError "bar" >> throwError "baz" >> pure ())
        res @?= Left "bar",
      testCase "basic tagged no error" $ do
        res <- runFx $ runErrorTagged @"err" @() (pure "foo")
        res @?= Right "foo",
      testCase "basic tagged error" $ do
        res <- runFx $ runErrorTagged @"err" (throwErrorTagged @"err" "bar" >> pure ())
        res @?= Left "bar",
      testCase "basic tagged multiple errors" $ do
        res <-
          runFx $
            runErrorTagged @"err" $ do
              _ <- throwErrorTagged @"err" "bar"
              _ <- throwErrorTagged @"err" "baz"
              pure ()
        res @?= Left "bar"
    ]

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
    "error"
    [ testCase "basic untagged no error" $ do
        res <- runFx $ runError @() (pure "foo")
        res @?= Right "foo",
      testCase "basic untagged error" $ do
        res <- runFx $ runError (throwError "bar" >> pure ())
        res @?= Left "bar",
      testCase "basic untagged multiple throws" $ do
        res <- runFx $ runError (throwError "bar" >> throwError "baz" >> pure ())
        res @?= Left "bar",
      testCase "basic untagged catch" $ do
        res <-
          runFx $
            runError @String @String $
              throwError "bar" `catchError` (\e -> pure ("caught: " ++ e))
        res @?= Right ("caught: bar"),
      testCase "basic tagged no error" $ do
        res <- runFx $ runErrorTagged @"err" @() (pure "foo")
        res @?= Right "foo",
      testCase "basic tagged error" $ do
        res <- runFx $ runErrorTagged @"err" (throwErrorTagged @"err" "bar" >> pure ())
        res @?= Left "bar",
      testCase "basic tagged multiple throws" $ do
        res <-
          runFx $
            runErrorTagged @"err" $ do
              _ <- throwErrorTagged @"err" "bar"
              _ <- throwErrorTagged @"err" "baz"
              pure ()
        res @?= Left "bar",
      testCase "basic tagged multiple errors" $ do
        res <-
          runFx $
            runErrorTagged @"err1" @String $
              runErrorTagged @"err2" @String $ do
                _ <- throwErrorTagged @"err1" "bar"
                pure ()
        res @?= Left "bar"
    ]

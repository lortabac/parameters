{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin Param.Plugin #-}

module Test.Param.Fx.State (stateTests) where

import Param.Fx
import Param.Fx.State
import Test.Tasty
import Test.Tasty.HUnit

stateTests :: TestTree
stateTests =
  testGroup
    "state"
    [ testCase "basic untagged" $ do
        res <- runFx $ runState (1 :: Int) $ do
          modify (+ 1)
          show <$> get
        res @?= ("2", 2),
      testCase "multiple modify untagged" $ do
        res <- runFx $ runState (1 :: Int) $ do
          modify (+ 1)
          modify (* 10)
          show <$> get
        res @?= ("20", 20),
      testCase "basic tagged" $ do
        res <- runFx $ runStateTagged @"state1" (1 :: Int) $ do
          modifyTagged @"state1" (+ 1)
          show <$> getTagged @"state1"
        res @?= ("2", 2),
      testCase "multiple tagged" $ do
        res <- runFx $ evalStateTagged @"state1" (1 :: Int) $ evalStateTagged @"state2" (2 :: Int) $ do
          modifyTagged @"state1" (* 10)
          modifyTagged @"state2" (* 100)
          (,) <$> getTagged @"state1" <*> getTagged @"state2"
        res @?= (10, 200)
    ]

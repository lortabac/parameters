{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fplugin Param.Plugin #-}

module Main (main) where

import Param
import Test.Tasty
import Test.Tasty.HUnit

data Foo

data Bar

main :: IO ()
main =
  defaultMain $
    testGroup
      "parameters tests"
      [ testCase "Basic param setting" $
          let res = runParam @Foo "foo" $ ask @Foo
           in res @?= "foo",
        testCase "Two parameters" $
          let res = runParam @Foo "hello" $ runParam @Bar "world" twoParams
           in res @?= "hello world",
        testCase "Two parameters inverted (order of constraints doesn't matter)" $
          let res = runParam @Bar "world" $ runParam @Foo "hello" twoParams
           in res @?= "hello world",
        testCase "Param overriding" $
          let res = runParam @Foo "hello" $ local @Foo (<> " world") $ ask @Foo
           in res @?= "hello world",
        testCase "Chris Done's terror" $
          let res = runParam @Foo "hello" terror
           in res @?= ("hello", "world")
      ]

twoParams ::
  (HasParam Bar String, HasParam Foo String) =>
  String
twoParams = ask @Foo ++ " " ++ ask @Bar

terror :: (HasParam Foo String) => (String, String)
terror =
  let result = ask @Foo
   in ( result,
        local @Foo (const "world") result
      )

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
          let res = runParam @Foo "foo" $ paramAsk @Foo
           in res @?= "foo",
        testCase "Param overriding" $
          let res = runParam @Foo "hello" $ paramLocal @Foo (<> " world") $ paramAsk @Foo
           in res @?= "hello world",
        testCase "Two parameters" $
          let res = runParam @Foo "hello" $ runParam @Bar "world" twoParams
           in res @?= "hello world",
        testCase "Two parameters inverted (order of constraints doesn't matter)" $
          let res = runParam @Bar "world" $ runParam @Foo "hello" twoParams
           in res @?= "hello world",
        testCase "Chris Done's horror" $
          let res = runParam @Foo "hello" horror
           in res @?= ("hello", "world"),
        testCase "Chris Done's terror" $
          let res = runParam @Foo "hello" terror
           in res @?= ("hello", "world")
      ]

twoParams ::
  (HasParam Bar String, HasParam Foo String) =>
  String
twoParams = paramAsk @Foo ++ " " ++ paramAsk @Bar

horror :: (HasParam Foo String) => (String, String)
horror =
  let result :: (HasParam Foo String) => String
      result = paramAsk @Foo
   in ( result,
        paramLocal @Foo (const "world") result
      )

terror :: (HasParam Foo String) => (String, String)
terror =
  let result = paramAsk @Foo
   in ( result,
        paramLocal @Foo (const "world") result
      )

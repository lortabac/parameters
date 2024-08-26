module Main where

import Param

main :: IO ()
main =
  print $
    runParam @Int True $
      runParam @Int False (ask @Int)

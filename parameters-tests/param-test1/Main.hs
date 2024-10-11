module Main where

import Param

main :: IO ()
main =
  print $
    runParam @Bool True $
      runParam @Bool False (ask @Bool)

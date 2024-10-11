module Main where

import Param

main :: IO ()
main =
  print $
    runParam @Bool True askBool

askBool :: HasParam Bool Bool => Bool
askBool = runParam @Bool False (ask @Bool)

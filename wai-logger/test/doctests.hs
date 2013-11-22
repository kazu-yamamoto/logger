module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "Network/Wai/Logger.hs"
  ]

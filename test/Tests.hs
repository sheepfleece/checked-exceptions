module Main (main) where

import           Test.Framework            (defaultMain)

import           Control.Monad.Throw.Tests

main :: IO ()
main = defaultMain
  [ Control.Monad.Throw.Tests.tests
  ]




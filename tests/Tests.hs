{-# LANGUAGE OverloadedStrings #-}

module Main where

import Swihs
import Swihs.Tests.Builtin
import Swihs.Tests.Clpfd
import Test.Tasty

main :: IO ()
main = do
  _ <- initSwipl
  useLib "clpfd"
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [builtinTests, clpfdTests]

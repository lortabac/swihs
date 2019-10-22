{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Swihs.Tests.Builtin where

import Data.List (sort)
import Swihs
import Test.Tasty
import Test.Tasty.HUnit

builtinTests :: TestTree
builtinTests =
  testGroup
    "Unification and builtin predicates"
    [ testCase "Unification succeeds on two equal atoms" $ do
        sol <- queryList $ "x" .= "x"
        sol @=? [[]],
      testCase "Unification fails on two different atoms" $ do
        sol <- queryList $ "x" .= "y"
        sol @=? [],
      testCase "Unification succeeds on two equal variables" $ do
        sol <- queryList $ "X" .= "X"
        sol @=? [[("X", FreeVar)]],
      testCase "Unification succeeds on two different variables" $ do
        sol <- queryList $ "X" .= "Y"
        sol @=? [[("X", FreeVar), ("Y", FreeVar)]],
      testCase "Fact assertion and query with one solution" $ do
        assertz $ F1 "man" "john"
        sol <- queryList $ F1 "man" "X"
        sol @=? [[("X", Ground "john")]],
      testCase "Fact assertion and query with multiple solutions" $ do
        assertz $ F1 "man" "jack"
        sol <- queryList $ F1 "man" "X"
        sort sol @=? sort [[("X", Ground "jack")], [("X", Ground "john")]],
      testCase "Fact assertion with multi-argument functor" $ do
        assertz $ F2 "loves" "jack" "lisa"
        sol <- queryList $ F2 "loves" "jack" "X"
        sol @=? [[("X", Ground "lisa")]],
      testCase "Basic rule assertion" $ do
        assertz $ F2 "loves" "john" "X" :- F1 "woman" "X"
        assertz $ F1 "woman" "lisa"
        sol <- queryList $ F2 "loves" "john" "X"
        sol @=? [[("X", Ground "lisa")]],
      testCase "Rule assertion with conjunction" $ do
        assertz $ F2 "loves" "harry" "X" :- F1 "woman" "X" .& F1 "blond" "X"
        assertz $ F1 "blond" "sarah"
        assertz $ F1 "woman" "sarah"
        sol <- queryList $ F2 "loves" "harry" "X"
        sol @=? [[("X", Ground "sarah")]],
      testCase "Non-ASCII characters in atoms" $ do
        assertz $ F1 "turkish_food" "mantı"
        assertz $ F1 "chinese_food" "春卷"
        sol <- queryOnce $ F1 "turkish_food" "X" .& F1 "chinese_food" "Y"
        sol @=? Just [("X", Ground "mantı"), ("Y", Ground "春卷")],
      testCase "Non-ASCII characters in strings" $ do
        assertz $ F1 "turkish_food_str" (String "mantı")
        assertz $ F1 "chinese_food_str" (String "春卷")
        sol <- queryOnce $ F1 "turkish_food_str" "X" .& F1 "chinese_food_str" "Y"
        sol @=? Just [("X", Ground (String "mantı")), ("Y", Ground (String "春卷"))]
    ]

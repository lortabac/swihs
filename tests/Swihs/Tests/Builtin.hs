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
        sol <- queryBool $ "X" .= "X"
        sol @=? True,
      testCase "Unification succeeds on two different variables" $ do
        sol <- queryBool $ "X" .= "Y"
        sol @=? True,
      testCase "Fact assertion and query with one solution" $ do
        assertz $ F1 "man" "john"
        sol <- queryList $ F1 "man" "X"
        sol @=? [[("X", "john")]],
      testCase "Fact assertion and query with multiple solutions" $ do
        assertz $ F1 "man" "jack"
        sol <- queryList $ F1 "man" "X"
        sort sol @=? sort [[("X", "jack")], [("X", "john")]],
      testCase "Fact assertion with multi-argument functor" $ do
        assertz $ F2 "loves" "jack" "lisa"
        sol <- queryList $ F2 "loves" "jack" "X"
        sol @=? [[("X", "lisa")]],
      testCase "Basic rule assertion" $ do
        assertz $ F2 "loves" "john" "X" :- F1 "woman" "X"
        assertz $ F1 "woman" "lisa"
        sol <- queryList $ F2 "loves" "john" "X"
        sol @=? [[("X", "lisa")]],
      testCase "Rule assertion with conjunction" $ do
        assertz $ F2 "loves" "harry" "X" :- F1 "woman" "X" .& F1 "blond" "X"
        assertz $ F1 "blond" "sarah"
        assertz $ F1 "woman" "sarah"
        sol <- queryList $ F2 "loves" "harry" "X"
        sol @=? [[("X", "sarah")]],
      testCase "Non-ASCII characters in atoms" $ do
        assertz $ F1 "turkish_food" "mantı"
        assertz $ F1 "chinese_food" "春卷"
        sol <- queryOnce $ F1 "turkish_food" "X" .& F1 "chinese_food" "Y"
        sol @=? Just [("X", "mantı"), ("Y", "春卷")],
      testCase "Non-ASCII characters in strings" $ do
        assertz $ F1 "turkish_food_str" (String "mantı")
        assertz $ F1 "chinese_food_str" (String "春卷")
        sol <- queryOnce $ F1 "turkish_food_str" "X" .& F1 "chinese_food_str" "Y"
        sol @=? Just [("X", String "mantı"), ("Y", String "春卷")],
      testCase "Reading back atoms" $ do
        sol <- queryOnce $ "X" .= "atom"
        fmap (! "X") sol @=? Just "atom",
      testCase "Reading back strings" $ do
        sol <- queryOnce $ "X" .= String "string"
        fmap (! "X") sol @=? Just (String "string"),
      testCase "Reading back integers" $ do
        sol <- queryOnce $ "X" .= 1
        fmap (! "X") sol @=? Just (Number 1),
      testCase "Reading back compound terms of arity 1" $ do
        sol <- queryOnce $ "X" .= F1 "compound" "f1"
        fmap (! "X") sol @=? Just (F1 "compound" "f1"),
      testCase "Reading back compound terms of arity 2" $ do
        sol <- queryOnce $ "X" .= F2 "compound" "f1" "f2"
        fmap (! "X") sol @=? Just (F2 "compound" "f1" "f2"),
      testCase "Reading back compound terms of arity 3" $ do
        sol <- queryOnce $ "X" .= F3 "compound" "f1" "f2" "f3"
        fmap (! "X") sol @=? Just (F3 "compound" "f1" "f2" "f3"),
      testCase "Reading back compound terms of arity 4" $ do
        sol <- queryOnce $ "X" .= F4 "compound" "f1" "f2" "f3" "f4"
        fmap (! "X") sol @=? Just (F4 "compound" "f1" "f2" "f3" "f4"),
      testCase "Reading back compound terms of arity 5" $ do
        sol <- queryOnce $ "X" .= F5 "compound" "f1" "f2" "f3" "f4" "f5"
        fmap (! "X") sol @=? Just (F5 "compound" "f1" "f2" "f3" "f4" "f5"),
      testCase "Reading back compound terms of arity 6" $ do
        sol <- queryOnce $ "X" .= F6 "compound" "f1" "f2" "f3" "f4" "f5" "f6"
        fmap (! "X") sol @=? Just (F6 "compound" "f1" "f2" "f3" "f4" "f5" "f6"),
      testCase "Reading back compound terms of arity 7" $ do
        sol <- queryOnce $ "X" .= F7 "compound" "f1" "f2" "f3" "f4" "f5" "f6" "f7"
        fmap (! "X") sol @=? Just (F7 "compound" "f1" "f2" "f3" "f4" "f5" "f6" "f7"),
      testCase "Reading back compound terms of arity 8" $ do
        sol <- queryOnce $ "X" .= F8 "compound" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8"
        fmap (! "X") sol @=? Just (F8 "compound" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8"),
      testCase "Reading back compound terms of arity 9" $ do
        sol <- queryOnce $ "X" .= F9 "compound" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9"
        fmap (! "X") sol @=? Just (F9 "compound" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9")
    ]

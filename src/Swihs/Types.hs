{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Swihs.Types where

import Data.Char
import Data.Int (Int64)
import Data.Map (Map)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts (IsList (..))
import Swihs.C

-- | Haskell representation of a Prolog term
data Term
  = Atom Text
  | Int Int64
  | Double Double
  | String Text
  | Var String
  | WildCard
  | Nil
  | (:|) Term Term
  | (:-) Term Term
  | F1 Text Term
  | F2 Text Term Term
  | F3 Text Term Term Term
  | F4 Text Term Term Term Term
  | F5 Text Term Term Term Term Term
  | F6 Text Term Term Term Term Term Term
  | F7 Text Term Term Term Term Term Term Term
  | F8 Text Term Term Term Term Term Term Term Term
  | F9 Text Term Term Term Term Term Term Term Term Term
  deriving (Eq, Ord, Show)

infix 1 :-

infixr 5 :|

instance IsString Term where
  fromString "_" = WildCard
  fromString name@('_' : _) = Var name
  fromString name@(c : _) = if isUpper c then Var name else Atom (Text.pack name)
  fromString name = Atom (Text.pack name)

instance Num Term where

  (+) = F2 "+"

  (-) = F2 "-"

  (*) = F2 "*"

  fromInteger = Int . fromInteger

instance Fractional Term where

  (/) = F2 "/"

  fromRational = Double . fromRational

instance IsList Term where

  type Item Term = Term

  fromList [] = Nil
  fromList (x : xs) = x :| fromList xs

  toList Nil = []
  toList (x :| xs) = x : toList xs
  toList t = error ("The term '" ++ show t ++ "' is not a list")

-- | Build a compound term that lacks the last argument normally expected
--   by its functor
partial :: (Term -> Term) -> Term
partial p = case p WildCard of
  Atom _ -> error "Cannot partially apply an atom"
  Int _ -> error "Cannot partially apply an integer"
  Double _ -> error "Cannot partially apply a double"
  String _ -> error "Cannot partially apply a string"
  Var _ -> error "Cannot partially apply a variable"
  WildCard -> error "Cannot partially apply a wild card"
  Nil -> error "Cannot partially apply nil"
  (:|) _ _ -> error "Cannot partially apply cons"
  (:-) t1 _ -> F1 ":-" t1
  F1 name _ -> Atom name
  F2 name t1 _ -> F1 name t1
  F3 name t1 t2 _ -> F2 name t1 t2
  F4 name t1 t2 t3 _ -> F3 name t1 t2 t3
  F5 name t1 t2 t3 t4 _ -> F4 name t1 t2 t3 t4
  F6 name t1 t2 t3 t4 t5 _ -> F5 name t1 t2 t3 t4 t5
  F7 name t1 t2 t3 t4 t5 t6 _ -> F6 name t1 t2 t3 t4 t5 t6
  F8 name t1 t2 t3 t4 t5 t6 t7 _ -> F7 name t1 t2 t3 t4 t5 t6 t7
  F9 name t1 t2 t3 t4 t5 t6 t7 t8 _ -> F8 name t1 t2 t3 t4 t5 t6 t7 t8

data Query = Query Query_ (Map String TermRef)

data QueryTerm = QueryTerm Predicate TermRef (Map String TermRef)

type Functor1 = Term -> Term

type Functor2 = Term -> Term -> Term

type Functor3 = Term -> Term -> Term -> Term

type Functor4 = Term -> Term -> Term -> Term -> Term

type Functor5 = Term -> Term -> Term -> Term -> Term -> Term

type Functor6 = Term -> Term -> Term -> Term -> Term -> Term -> Term

type Functor7 = Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term

type Functor8 = Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term

type Functor9 = Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term

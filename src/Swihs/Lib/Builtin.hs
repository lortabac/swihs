{-# LANGUAGE OverloadedStrings #-}

module Swihs.Lib.Builtin where

import Swihs.Types

(.&) :: Functor2
(.&) = F2 ","

infixr 2 .&

(.=) :: Functor2
(.=) = F2 "="

infix 3 .=

(./=) :: Functor2
(./=) = F2 "\\="

infix 3 ./=

cut :: Term
cut = Atom "!"

is :: Functor2
is = F2 "is"

var :: Functor1
var = F1 "var"

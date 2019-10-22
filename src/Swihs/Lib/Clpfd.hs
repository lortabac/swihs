{-# LANGUAGE OverloadedStrings #-}

module Swihs.Lib.Clpfd where

import Swihs.Types

(#=) :: Functor2
(#=) = F2 "#="

infix 3 #=

(#/=) :: Functor2
(#/=) = F2 "#/="

infix 3 #/=

(#>=) :: Functor2
(#>=) = F2 "#>="

infix 3 #>=

(#=<) :: Functor2
(#=<) = F2 "#=<"

infix 3 #=<

(#>) :: Functor2
(#>) = F2 "#>"

infix 3 #>

(#<) :: Functor2
(#<) = F2 "#<"

infix 3 #<

in_ :: Functor2
in_ = F2 "in"

infix 3 `in_`

(...) :: Functor2
(...) = F2 ".."

infix 4 ...

label :: Functor1
label = F1 "label"

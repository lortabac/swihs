{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Swihs.C.Types where

import Data.Void
import Foreign

newtype Atom_ = Atom_ WordPtr deriving (Storable)

newtype Functor_ = Functor_ WordPtr deriving (Storable)

newtype Module = Module (Ptr Void) deriving (Storable)

noModule :: Module
noModule = Module nullPtr

newtype Predicate = Predicate (Ptr Void) deriving (Storable)

newtype TermRef = TermRef WordPtr deriving (Eq, Show, Storable)

nextTermRef :: TermRef -> TermRef
nextTermRef (TermRef w) = TermRef $ ptrToWordPtr (plusPtr (wordPtrToPtr w) 1)

newtype Query_ = Query_ WordPtr deriving (Storable)

newtype Fid = Fid WordPtr deriving (Storable)

data TermType
  = TVariable
  | TAtom
  | TInteger
  | TFloat
  | TString
  | TTerm
  | TNil
  | TListPair
  deriving (Eq, Show)

instance Enum TermType where

  toEnum 1 = TVariable
  toEnum 2 = TAtom
  toEnum 3 = TInteger
  toEnum 5 = TFloat
  toEnum 6 = TString
  toEnum 7 = TTerm
  toEnum 8 = TNil
  toEnum 10 = TListPair
  toEnum _ = error "Unknown TermType"

  fromEnum TVariable = 1
  fromEnum TAtom = 2
  fromEnum TInteger = 3
  fromEnum TFloat = 5
  fromEnum TString = 6
  fromEnum TTerm = 7
  fromEnum TNil = 8
  fromEnum TListPair = 10

newtype CInt64 = CInt64 Int64 deriving (Eq, Num, Ord, Show, Storable)

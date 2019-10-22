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
  | TNil
  | TListPair
  deriving (Eq, Show)

instance Enum TermType where

  toEnum 1 = TVariable
  toEnum 2 = TAtom
  toEnum 3 = TInteger
  toEnum 4 = TFloat
  toEnum 7 = TNil
  toEnum 9 = TListPair
  toEnum _ = error "Unknown TermType"

  fromEnum TVariable = 1
  fromEnum TAtom = 2
  fromEnum TInteger = 3
  fromEnum TFloat = 4
  fromEnum TNil = 7
  fromEnum TListPair = 9

newtype CInt64 = CInt64 Int64 deriving (Eq, Num, Ord, Show, Storable)

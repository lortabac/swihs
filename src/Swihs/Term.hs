module Swihs.Term
  ( getTerm,
  )
where

import Control.Applicative (liftA2)
import Data.Scientific
import Swihs.C
import Swihs.Types

-- | Read back a Prolog term into its Haskell representation
getTerm :: TermRef -> IO (Ground Term)
getTerm ref = do
  t <- termType ref
  case t of
    TAtom -> do
      a <- getAtom ref
      Ground . Atom <$> atomChars a
    TVariable -> pure FreeVar
    TInteger -> do
      i <- getInt64 ref
      pure $ Ground (Number (fromIntegral i))
    TFloat -> do
      r <- getFloat ref
      pure $ Ground (Number (fromFloatDigits r))
    TString -> do
      s <- getStringChars ref
      pure $ Ground (String s)
    TNil -> do
      _ <- getNil ref
      pure $ Ground Nil
    TListPair -> do
      href <- newTermRef
      tref <- newTermRef
      _ <- getList ref href tref
      x <- getTerm href
      xs <- getTerm tref
      pure $ liftA2 (:|) x xs

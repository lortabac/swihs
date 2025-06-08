module Swihs.Term
  ( getTerm,
  )
where

import Swihs.C
import Swihs.Types

-- | Read back a Prolog term into its Haskell representation
getTerm :: TermRef -> IO Term
getTerm ref = do
  t <- termType ref
  case t of
    TAtom -> do
      a <- getAtom ref
      Atom <$> atomChars a
    TVariable -> Var <$> getVarName ref
    TInteger -> do
      i <- getInt64 ref
      pure $ Int i
    TFloat -> do
      r <- getFloat ref
      pure $ Double r
    TString -> do
      s <- getStringChars ref
      pure $ String s
    TTerm -> do
      (a, arity) <- getCompoundNameArity ref
      name <- atomChars a
      case arity of
        1 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          pure $ F1 name arg1
        2 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          pure $ F2 name arg1 arg2
        3 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          pure $ F3 name arg1 arg2 arg3
        4 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          ref4 <- getArg 4 ref
          arg4 <- getTerm ref4
          pure $ F4 name arg1 arg2 arg3 arg4
        5 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          ref4 <- getArg 4 ref
          arg4 <- getTerm ref4
          ref5 <- getArg 5 ref
          arg5 <- getTerm ref5
          pure $ F5 name arg1 arg2 arg3 arg4 arg5
        6 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          ref4 <- getArg 4 ref
          arg4 <- getTerm ref4
          ref5 <- getArg 5 ref
          arg5 <- getTerm ref5
          ref6 <- getArg 6 ref
          arg6 <- getTerm ref6
          pure $ F6 name arg1 arg2 arg3 arg4 arg5 arg6
        7 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          ref4 <- getArg 4 ref
          arg4 <- getTerm ref4
          ref5 <- getArg 5 ref
          arg5 <- getTerm ref5
          ref6 <- getArg 6 ref
          arg6 <- getTerm ref6
          ref7 <- getArg 7 ref
          arg7 <- getTerm ref7
          pure $ F7 name arg1 arg2 arg3 arg4 arg5 arg6 arg7
        8 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          ref4 <- getArg 4 ref
          arg4 <- getTerm ref4
          ref5 <- getArg 5 ref
          arg5 <- getTerm ref5
          ref6 <- getArg 6 ref
          arg6 <- getTerm ref6
          ref7 <- getArg 7 ref
          arg7 <- getTerm ref7
          ref8 <- getArg 8 ref
          arg8 <- getTerm ref8
          pure $ F8 name arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8
        9 -> do
          ref1 <- getArg 1 ref
          arg1 <- getTerm ref1
          ref2 <- getArg 2 ref
          arg2 <- getTerm ref2
          ref3 <- getArg 3 ref
          arg3 <- getTerm ref3
          ref4 <- getArg 4 ref
          arg4 <- getTerm ref4
          ref5 <- getArg 5 ref
          arg5 <- getTerm ref5
          ref6 <- getArg 6 ref
          arg6 <- getTerm ref6
          ref7 <- getArg 7 ref
          arg7 <- getTerm ref7
          ref8 <- getArg 8 ref
          arg8 <- getTerm ref8
          ref9 <- getArg 9 ref
          arg9 <- getTerm ref9
          pure $ F9 name arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
        n -> error ("Unsupported term arity: " ++ show n)
    TNil -> do
      _ <- getNil ref
      pure Nil
    TListPair -> do
      href <- newTermRef
      tref <- newTermRef
      _ <- getList ref href tref
      x <- getTerm href
      xs <- getTerm tref
      pure (x :| xs)

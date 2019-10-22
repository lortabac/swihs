{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Swihs.Builder
  ( buildQueryTerm,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific
import Data.Text (Text)
import Swihs.C
import Swihs.Types

type BuilderT a = StateT (Map String TermRef) IO a

buildQueryTerm :: Term -> IO QueryTerm
buildQueryTerm t = do
  ((p, args), varMap) <- runStateT (buildQueryTerm' t) mempty
  pure $ QueryTerm p args varMap

buildQueryTerm' :: Term -> BuilderT (Predicate, TermRef)
buildQueryTerm' (Atom name) = liftIO $ do
  a <- newAtom name
  f <- newFunctor a 0
  args <- newTermRefs 0
  p <- pred_ f noModule
  pure (p, args)
buildQueryTerm' (F1 name t1) = buildCompQueryTerm name [t1]
buildQueryTerm' (F2 name t1 t2) = buildCompQueryTerm name [t1, t2]
buildQueryTerm' (F3 name t1 t2 t3) = buildCompQueryTerm name [t1, t2, t3]
buildQueryTerm' (F4 name t1 t2 t3 t4) = buildCompQueryTerm name [t1, t2, t3, t4]
buildQueryTerm' (F5 name t1 t2 t3 t4 t5) = buildCompQueryTerm name [t1, t2, t3, t4, t5]
buildQueryTerm' (F6 name t1 t2 t3 t4 t5 t6) = buildCompQueryTerm name [t1, t2, t3, t4, t5, t6]
buildQueryTerm' (F7 name t1 t2 t3 t4 t5 t6 t7) = buildCompQueryTerm name [t1, t2, t3, t4, t5, t6, t7]
buildQueryTerm' (F8 name t1 t2 t3 t4 t5 t6 t7 t8) = buildCompQueryTerm name [t1, t2, t3, t4, t5, t6, t7, t8]
buildQueryTerm' (F9 name t1 t2 t3 t4 t5 t6 t7 t8 t9) = buildCompQueryTerm name [t1, t2, t3, t4, t5, t6, t7, t8, t9]
buildQueryTerm' _ = error "Invalid query"

buildCompQueryTerm :: Text -> [Term] -> BuilderT (Predicate, TermRef)
buildCompQueryTerm name ts = do
  a <- liftIO $ newAtom name
  f <- liftIO $ newFunctor a (length ts)
  args <- termRefs ts
  p <- liftIO $ pred_ f noModule
  pure (p, args)

putTerm :: TermRef -> Term -> BuilderT ()
putTerm ref t = case t of
  Atom name -> liftIO (newAtom name >>= putAtom ref)
  Number n -> liftIO $ case floatingOrInteger n of
    Left r -> void $ putFloat ref r
    Right i -> void $ putInt64 ref i
  String str -> liftIO $ void $ putStringChars ref str
  Var name -> do
    varMap <- get
    case Map.lookup name varMap of
      Just refInMap -> void $ liftIO (unify refInMap ref)
      Nothing -> modify' (Map.insert name ref)
  WildCard -> pure ()
  Nil -> void $ liftIO (putNil ref)
  (:|) x xs -> do
    href <- termRef x
    putTerm ref xs
    void $ liftIO (consList ref href ref)
  (:-) t1 t2 -> putCompTerm ref ":-" [t1, t2]
  F1 name t1 -> putCompTerm ref name [t1]
  F2 name t1 t2 -> putCompTerm ref name [t1, t2]
  F3 name t1 t2 t3 -> putCompTerm ref name [t1, t2, t3]
  F4 name t1 t2 t3 t4 -> putCompTerm ref name [t1, t2, t3, t4]
  F5 name t1 t2 t3 t4 t5 -> putCompTerm ref name [t1, t2, t3, t4, t5]
  F6 name t1 t2 t3 t4 t5 t6 -> putCompTerm ref name [t1, t2, t3, t4, t5, t6]
  F7 name t1 t2 t3 t4 t5 t6 t7 -> putCompTerm ref name [t1, t2, t3, t4, t5, t6, t7]
  F8 name t1 t2 t3 t4 t5 t6 t7 t8 -> putCompTerm ref name [t1, t2, t3, t4, t5, t6, t7, t8]
  F9 name t1 t2 t3 t4 t5 t6 t7 t8 t9 -> putCompTerm ref name [t1, t2, t3, t4, t5, t6, t7, t8, t9]

putCompTerm :: TermRef -> Text -> [Term] -> BuilderT ()
putCompTerm ref name ts = do
  a <- liftIO $ newAtom name
  f <- liftIO $ newFunctor a (length ts)
  args <- termRefs ts
  liftIO $ void (consFunctorV ref f args)

termRef :: Term -> BuilderT TermRef
termRef t = do
  ref <- liftIO newTermRef
  putTerm ref t
  pure ref

termRefs :: [Term] -> BuilderT TermRef
termRefs [t] = termRef t
termRefs terms@(t : ts) = do
  ref <- liftIO $ newTermRefs (length terms)
  putTerm ref t
  putNextTerms ts ref
  pure ref
  where
    putNextTerms (x : xs) prevRef = do
      let newRef = nextTermRef prevRef
      putTerm newRef x
      putNextTerms xs newRef
    putNextTerms [] _ = pure ()
termRefs [] = error "termRefs: empty list of terms"

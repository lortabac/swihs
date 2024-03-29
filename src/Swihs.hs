{-# LANGUAGE OverloadedStrings #-}

module Swihs
  ( runSwipl,
    initSwipl,
    useModule,
    useModule',
    useLibrary,
    ensureLoaded,
    notrace,
    asserta,
    assertz,
    (!),
    module Swihs.Lib.Builtin,
    module Swihs.Query,
    module Swihs.Types,
  )
where

import Control.Monad (void)
import Data.Map ((!))
import Data.Text (Text)
import qualified Swihs.C as C
import Swihs.Lib.Builtin
import Swihs.Query
import Swihs.Types

runSwipl :: IO a -> IO a
runSwipl f = do
  _ <- initSwipl
  f

initSwipl :: IO Bool
initSwipl = C.initialise

useModule :: Text -> IO ()
useModule str = useModule' (String str)

useModule' :: Term -> IO ()
useModule' t =
  void $ queryBool $
    F1 "use_module" t

useLibrary :: Text -> IO ()
useLibrary lib = useModule' (F1 "library" (Atom lib))

ensureLoaded :: Text -> IO ()
ensureLoaded file =
  void
    $ queryBool
    $ F1 "ensure_loaded" (String file)

notrace :: IO ()
notrace = void $ queryBool $ Atom "notrace"

asserta :: Term -> IO ()
asserta t =
  void $ queryBool $
    F1 "asserta" t

assertz :: Term -> IO ()
assertz t =
  void $ queryBool $
    F1 "assertz" t

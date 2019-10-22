{-# LANGUAGE ForeignFunctionInterface #-}

module Swihs.C.Query where

import Foreign
import Foreign.C
import Swihs.C.Types

foreign import ccall "PL_pred" pl_pred :: Functor_ -> Module -> IO Predicate

pred_ :: Functor_ -> Module -> IO Predicate
pred_ = pl_pred

foreign import ccall "PL_open_query" pl_open_query :: Module -> CInt -> Predicate -> TermRef -> IO Query_

defaultFlags :: CInt
defaultFlags = 2

openQuery :: Module -> Predicate -> TermRef -> IO Query_
openQuery ctx = pl_open_query ctx defaultFlags

foreign import ccall "PL_next_solution" pl_next_solution :: Query_ -> IO CInt

nextSolution :: Query_ -> IO Bool
nextSolution q = toBool <$> pl_next_solution q

foreign import ccall "PL_cut_query" pl_cut_query :: Query_ -> IO CInt

cutQuery :: Query_ -> IO Bool
cutQuery q = toBool <$> pl_cut_query q

foreign import ccall "PL_close_query" pl_close_query :: Query_ -> IO CInt

closeQuery :: Query_ -> IO Bool
closeQuery q = toBool <$> pl_close_query q

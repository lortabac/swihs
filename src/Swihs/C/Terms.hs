{-# LANGUAGE ForeignFunctionInterface #-}

module Swihs.C.Terms where

import Control.Monad ((<=<))
import Data.Text (Text)
import Foreign
import Foreign.C
import Swihs.C.Text
import Swihs.C.Types

foreign import ccall "PL_new_atom" pl_new_atom :: CString -> IO Atom_

newAtom :: Text -> IO Atom_
newAtom = flip useTextAsCString pl_new_atom

foreign import ccall "PL_atom_chars" pl_atom_chars :: Atom_ -> IO CString

atomChars :: Atom_ -> IO Text
atomChars = packCStringAsText <=< pl_atom_chars

foreign import ccall "PL_new_functor" pl_new_functor :: Atom_ -> CInt -> IO Functor_

newFunctor :: Atom_ -> Int -> IO Functor_
newFunctor a arity = pl_new_functor a (fromIntegral arity)

foreign import ccall "PL_functor_name" pl_functor_name :: Functor_ -> IO Atom_

functorName :: Functor_ -> IO Atom_
functorName = pl_functor_name

foreign import ccall "PL_functor_arity" pl_functor_arity :: Functor_ -> IO CSize

functorArity :: Functor_ -> IO Int
functorArity f = fromIntegral <$> pl_functor_arity f

foreign import ccall "PL_new_term_ref" pl_new_term_ref :: IO TermRef

newTermRef :: IO TermRef
newTermRef = pl_new_term_ref

foreign import ccall "PL_new_term_refs" pl_new_term_refs :: CInt -> IO TermRef

newTermRefs :: Int -> IO TermRef
newTermRefs n = pl_new_term_refs (fromIntegral n)

foreign import ccall "PL_put_variable" pl_put_variable :: TermRef -> IO ()

putVariable :: TermRef -> IO ()
putVariable = pl_put_variable

foreign import ccall "PL_put_atom" pl_put_atom :: TermRef -> Atom_ -> IO ()

putAtom :: TermRef -> Atom_ -> IO ()
putAtom = pl_put_atom

foreign import ccall "PL_put_int64" pl_put_int64 :: TermRef -> CInt64 -> IO CInt

putInt64 :: TermRef -> Int64 -> IO Bool
putInt64 t i = toBool <$> pl_put_int64 t (CInt64 i)

foreign import ccall "PL_put_float" pl_put_float :: TermRef -> CDouble -> IO CInt

putFloat :: TermRef -> Double -> IO Bool
putFloat t r = toBool <$> pl_put_float t (CDouble r)

foreign import ccall "PL_put_string_chars" pl_put_string_chars :: TermRef -> CString -> IO CInt

putStringChars :: TermRef -> Text -> IO Bool
putStringChars t str = toBool <$> useTextAsCString str (pl_put_string_chars t)

foreign import ccall "PL_put_functor" pl_put_functor :: TermRef -> Functor_ -> IO CInt

putFunctor :: TermRef -> Functor_ -> IO Bool
putFunctor t f = toBool <$> pl_put_functor t f

foreign import ccall "PL_cons_functor_v" pl_cons_functor_v :: TermRef -> Functor_ -> TermRef -> IO CInt

consFunctorV :: TermRef -> Functor_ -> TermRef -> IO Bool
consFunctorV t f args = toBool <$> pl_cons_functor_v t f args

foreign import ccall "PL_put_nil" pl_put_nil :: TermRef -> IO CInt

putNil :: TermRef -> IO Bool
putNil t = toBool <$> pl_put_nil t

foreign import ccall "PL_cons_list" pl_cons_list :: TermRef -> TermRef -> TermRef -> IO CInt

consList :: TermRef -> TermRef -> TermRef -> IO Bool
consList ref h t = toBool <$> pl_cons_list ref h t

foreign import ccall "PL_get_atom" pl_get_atom :: TermRef -> Ptr Atom_ -> IO CInt

getAtom :: TermRef -> IO Atom_
getAtom t = alloca $ \p_a -> do
  _ <- pl_get_atom t p_a
  a <- peek p_a
  pure a

foreign import ccall "PL_get_int64" pl_get_int64 :: TermRef -> Ptr CInt64 -> IO CInt

getInt64 :: TermRef -> IO Int64
getInt64 t = alloca $ \p_i -> do
  _ <- pl_get_int64 t p_i
  CInt64 i <- peek p_i
  pure i

foreign import ccall "PL_get_float" pl_get_float :: TermRef -> Ptr CDouble -> IO CInt

getFloat :: TermRef -> IO Double
getFloat t = alloca $ \p_r -> do
  _ <- pl_get_float t p_r
  CDouble r <- peek p_r
  pure r

foreign import ccall "PL_get_string" pl_get_string_chars :: TermRef -> Ptr CString -> Ptr CSize -> IO CInt

getStringChars :: TermRef -> IO Text
getStringChars t =
  alloca $ \p_str ->
    alloca $ \p_len -> do
      _ <- pl_get_string_chars t p_str p_len
      cstr <- peek p_str
      packCStringAsText cstr

foreign import ccall "PL_get_chars" pl_get_chars :: TermRef -> Ptr CString -> CUInt -> IO CInt

getVarName :: TermRef -> IO String
getVarName t =
  alloca $ \p_str -> do
    _ <- pl_get_chars t p_str 0x0020
    cstr <- peek p_str
    peekCString cstr

foreign import ccall "PL_get_compound_name_arity" pl_get_compound_name_arity :: TermRef -> Ptr Atom_ -> Ptr CSize -> IO CInt

getCompoundNameArity :: TermRef -> IO (Atom_, Int)
getCompoundNameArity t =
  alloca $ \p_name ->
    alloca $ \p_arity -> do
      _ <- pl_get_compound_name_arity t p_name p_arity
      name <- peek p_name
      arity <- peek p_arity
      pure (name, fromIntegral arity)

foreign import ccall "PL_get_functor" pl_get_functor :: TermRef -> Ptr Functor_ -> IO CInt

getFunctor :: TermRef -> IO Functor_
getFunctor t = alloca $ \p_f -> do
  _ <- pl_get_functor t p_f
  f <- peek p_f
  pure f

foreign import ccall "PL_get_nil" pl_get_nil :: TermRef -> IO CInt

getNil :: TermRef -> IO Bool
getNil t = toBool <$> pl_get_nil t

foreign import ccall "PL_get_list" pl_get_list :: TermRef -> TermRef -> TermRef -> IO CInt

getList :: TermRef -> TermRef -> TermRef -> IO Bool
getList l h t = toBool <$> pl_get_list l h t

foreign import ccall "PL_get_arg" pl_get_arg :: CSize -> TermRef -> TermRef -> IO ()

getArg :: Int -> TermRef -> IO TermRef
getArg n t = do
  ref <- newTermRef
  _ <- pl_get_arg (fromIntegral n) t ref
  pure ref

foreign import ccall "PL_unify" pl_unify :: TermRef -> TermRef -> IO CInt

unify :: TermRef -> TermRef -> IO Bool
unify t1 t2 = toBool <$> pl_unify t1 t2

foreign import ccall "PL_is_ground" pl_is_ground :: TermRef -> IO CInt

isGround :: TermRef -> IO Bool
isGround t = toBool <$> pl_is_ground t

foreign import ccall "PL_term_type" pl_term_type :: TermRef -> IO CInt

termType :: TermRef -> IO TermType
termType t = toEnum . fromIntegral <$> pl_term_type t

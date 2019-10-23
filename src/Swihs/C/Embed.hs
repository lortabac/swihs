{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Swihs.C.Embed where

import Foreign
import Foreign.C
import Swihs.C.Types

#include "HsBaseConfig.h"

foreign import ccall unsafe "getProgArgv"
    getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall "PL_initialise" pl_initialise :: CInt -> Ptr CString -> IO Int

initialise :: IO Bool
initialise = do
    (argc, argv) <- getArgs
    toBool <$> pl_initialise argc argv

foreign import ccall "PL_is_initialised" pl_is_initialised :: Ptr CInt -> Ptr (Ptr CString) -> IO Int

isInitialised :: IO Bool
isInitialised =
    alloca $ \p_argc ->
    alloca $ \p_argv ->
        toBool <$> pl_is_initialised p_argc p_argv

foreign import ccall "PL_halt" pl_halt :: CInt -> IO CInt

halt :: Int -> IO Bool
halt code = toBool <$> pl_halt (fromIntegral code)

foreign import ccall "PL_toplevel" pl_toplevel :: IO CInt

toplevel :: IO Bool
toplevel = toBool <$> pl_toplevel

foreign import ccall "PL_open_foreign_frame" pl_open_foreign_frame :: IO Fid

openForeignFrame :: IO Fid
openForeignFrame = pl_open_foreign_frame

foreign import ccall "PL_close_foreign_frame" pl_close_foreign_frame :: Fid -> IO ()

closeForeignFrame :: Fid -> IO ()
closeForeignFrame = pl_close_foreign_frame

foreign import ccall "PL_discard_foreign_frame" pl_discard_foreign_frame :: Fid -> IO ()

discardForeignFrame :: Fid -> IO ()
discardForeignFrame = pl_discard_foreign_frame

foreign import ccall "PL_rewind_foreign_frame" pl_rewind_foreign_frame :: Fid -> IO ()

rewindForeignFrame :: Fid -> IO ()
rewindForeignFrame = pl_rewind_foreign_frame

foreign import ccall "PL_clear_exception" pl_clear_exception :: IO ()

clearException :: IO ()
clearException = pl_clear_exception

getArgs :: IO (CInt, Ptr CString)
getArgs = do
    let argCount = 3
    p_argc <- malloc :: IO (Ptr CInt)
    p_p_argv <- malloc :: IO (Ptr (Ptr CString))
    getProgArgv p_argc p_p_argv
    p_argv <- peek p_p_argv
    p_args <- reallocArray p_argv argCount
    quiet  <- newCString "--quiet"
    noSigs <- newCString "--no-signals"
    pokeElemOff p_args 1 quiet
    pokeElemOff p_args 2 noSigs
    pure (fromIntegral argCount, p_args)

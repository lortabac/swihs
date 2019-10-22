module Swihs.C.Text where

import qualified Data.ByteString.Char8 as Char8
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C

useTextAsCString :: Text -> (CString -> IO a) -> IO a
useTextAsCString txt f = Char8.useAsCString bs f
  where
    bs = Text.encodeUtf8 txt

packCStringAsText :: CString -> IO Text
packCStringAsText cstr = Text.decodeUtf8 <$> Char8.packCString cstr

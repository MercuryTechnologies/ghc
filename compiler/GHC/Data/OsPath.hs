{-# LANGUAGE CPP #-}
module GHC.Data.OsPath
  (
  -- * OsPath initialisation and transformation
    OsPath
  , OsString
  , encodeUtf
  , decodeUtf
  , unsafeDecodeUtf
  , unsafeEncodeUtf
  , os
  -- * Common utility functions
  , (</>)
  , (<.>)
  , splitSearchPath
  , splitExtension
  , isRelative
  , makeRelative
  , normalise
  , dropTrailingPathSeparator
  , takeDirectory
  , OS.isSuffixOf
  , OS.drop
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  , createDirectoryIfMissing
  , pprOsPath
  , fastStringToOsPath
  )
  where

import GHC.Prelude

import GHC.Data.FastString (FastString)
#if defined(mingw32_HOST_OS)
import GHC.Data.FastString (unpackFS)
#else
import GHC.Data.FastString (fastStringToShortByteString)
import System.OsString.Internal.Types (OsString (..), PosixString (..))
#endif
import GHC.Utils.Misc (HasCallStack)
import GHC.Utils.Outputable qualified as Outputable
import GHC.Utils.Panic (panic)

import System.OsPath
import qualified System.OsString as OS (isSuffixOf, drop)
import System.Directory.OsPath (doesDirectoryExist, doesFileExist, getDirectoryContents, createDirectoryIfMissing)

import System.Directory.Internal (os)

-- | Decode an 'OsPath' to 'FilePath', throwing an 'error' if decoding failed.
-- Prefer 'decodeUtf' and gracious error handling.
unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf p =
  either (\err -> panic $ "Failed to decodeUtf \"" ++ show p ++ "\", because: " ++ show err) id (decodeUtf p)

pprOsPath :: HasCallStack => OsPath -> Outputable.SDoc
pprOsPath = Outputable.text . unsafeDecodeUtf

-- | The path a 'FastString' holds, as an 'OsPath'.
--
-- On POSIX an 'OsPath' is the path's bytes, and a 'FastString' keeps its
-- characters as UTF-8 bytes, so this takes those bytes as they are, without
-- unpacking the 'FastString'. They are the path's bytes for every path that
-- has no NUL and no lone surrogate, which a 'FastString' encodes differently.
-- So it is a key for paths that all come from 'FastString's, rather than a
-- way to the file itself. On Windows an 'OsPath' is UTF-16, and the
-- 'FastString' is unpacked and encoded.
fastStringToOsPath :: FastString -> OsPath
#if defined(mingw32_HOST_OS)
fastStringToOsPath = unsafeEncodeUtf . unpackFS
#else
fastStringToOsPath = OsString . PosixString . fastStringToShortByteString
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_SlategreyStrictMosaic (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/runner/Advent-of-Code/.config/cabal/bin"
libdir     = "/home/runner/Advent-of-Code/.config/cabal/lib/x86_64-linux-ghc-8.10.6/SlategreyStrictMosaic-0.1.0.0-inplace-SlategreyStrictMosaic"
dynlibdir  = "/home/runner/Advent-of-Code/.config/cabal/lib/x86_64-linux-ghc-8.10.6"
datadir    = "/home/runner/Advent-of-Code/.config/cabal/share/x86_64-linux-ghc-8.10.6/SlategreyStrictMosaic-0.1.0.0"
libexecdir = "/home/runner/Advent-of-Code/.config/cabal/libexec/x86_64-linux-ghc-8.10.6/SlategreyStrictMosaic-0.1.0.0"
sysconfdir = "/home/runner/Advent-of-Code/.config/cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SlategreyStrictMosaic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SlategreyStrictMosaic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SlategreyStrictMosaic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SlategreyStrictMosaic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SlategreyStrictMosaic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SlategreyStrictMosaic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

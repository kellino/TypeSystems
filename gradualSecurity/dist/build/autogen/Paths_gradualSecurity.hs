{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_gradualSecurity (
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

bindir     = "/home/david/.cabal/bin"
libdir     = "/home/david/.cabal/lib/x86_64-linux-ghc-8.0.2/gradualSecurity-0.1.0.0-Atdh9SAiqYBEkDzo0unXEl"
dynlibdir  = "/home/david/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/david/.cabal/share/x86_64-linux-ghc-8.0.2/gradualSecurity-0.1.0.0"
libexecdir = "/home/david/.cabal/libexec"
sysconfdir = "/home/david/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gradualSecurity_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gradualSecurity_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gradualSecurity_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gradualSecurity_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gradualSecurity_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gradualSecurity_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

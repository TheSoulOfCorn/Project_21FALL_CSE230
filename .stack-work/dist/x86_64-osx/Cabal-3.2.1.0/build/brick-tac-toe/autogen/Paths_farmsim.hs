{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_farmsim (
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

bindir     = "/Users/yuki/Developer/Project_21FALL_CSE230/.stack-work/install/x86_64-osx/39c8b84ccf4569e92d40569edc29a19aa55697b5afdc6d5f8ad9ea7ab9ad5450/8.10.7/bin"
libdir     = "/Users/yuki/Developer/Project_21FALL_CSE230/.stack-work/install/x86_64-osx/39c8b84ccf4569e92d40569edc29a19aa55697b5afdc6d5f8ad9ea7ab9ad5450/8.10.7/lib/x86_64-osx-ghc-8.10.7/farmsim-0.1.0.0-7Kh1ofGqgWjB9Oq5e4OaBw-brick-tac-toe"
dynlibdir  = "/Users/yuki/Developer/Project_21FALL_CSE230/.stack-work/install/x86_64-osx/39c8b84ccf4569e92d40569edc29a19aa55697b5afdc6d5f8ad9ea7ab9ad5450/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/yuki/Developer/Project_21FALL_CSE230/.stack-work/install/x86_64-osx/39c8b84ccf4569e92d40569edc29a19aa55697b5afdc6d5f8ad9ea7ab9ad5450/8.10.7/share/x86_64-osx-ghc-8.10.7/farmsim-0.1.0.0"
libexecdir = "/Users/yuki/Developer/Project_21FALL_CSE230/.stack-work/install/x86_64-osx/39c8b84ccf4569e92d40569edc29a19aa55697b5afdc6d5f8ad9ea7ab9ad5450/8.10.7/libexec/x86_64-osx-ghc-8.10.7/farmsim-0.1.0.0"
sysconfdir = "/Users/yuki/Developer/Project_21FALL_CSE230/.stack-work/install/x86_64-osx/39c8b84ccf4569e92d40569edc29a19aa55697b5afdc6d5f8ad9ea7ab9ad5450/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "farmsim_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "farmsim_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "farmsim_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "farmsim_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "farmsim_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "farmsim_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

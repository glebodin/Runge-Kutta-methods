{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Runge_Kutta (
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

bindir     = "/Users/gleb/Desktop/programming/differ/.stack-work/install/x86_64-osx/6141f220f201199dfa37d8821a9710ef6152391fe07ebcb1f1551501468e4bd2/8.6.5/bin"
libdir     = "/Users/gleb/Desktop/programming/differ/.stack-work/install/x86_64-osx/6141f220f201199dfa37d8821a9710ef6152391fe07ebcb1f1551501468e4bd2/8.6.5/lib/x86_64-osx-ghc-8.6.5/Runge-Kutta-0.1.0.0-Cuuy0JX0hZ7CfPJhHT4L31-Runge-Kutta"
dynlibdir  = "/Users/gleb/Desktop/programming/differ/.stack-work/install/x86_64-osx/6141f220f201199dfa37d8821a9710ef6152391fe07ebcb1f1551501468e4bd2/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/gleb/Desktop/programming/differ/.stack-work/install/x86_64-osx/6141f220f201199dfa37d8821a9710ef6152391fe07ebcb1f1551501468e4bd2/8.6.5/share/x86_64-osx-ghc-8.6.5/Runge-Kutta-0.1.0.0"
libexecdir = "/Users/gleb/Desktop/programming/differ/.stack-work/install/x86_64-osx/6141f220f201199dfa37d8821a9710ef6152391fe07ebcb1f1551501468e4bd2/8.6.5/libexec/x86_64-osx-ghc-8.6.5/Runge-Kutta-0.1.0.0"
sysconfdir = "/Users/gleb/Desktop/programming/differ/.stack-work/install/x86_64-osx/6141f220f201199dfa37d8821a9710ef6152391fe07ebcb1f1551501468e4bd2/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Runge_Kutta_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Runge_Kutta_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Runge_Kutta_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Runge_Kutta_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Runge_Kutta_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Runge_Kutta_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

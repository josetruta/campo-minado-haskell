{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_campo_minado (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/jose/.cabal/bin"
libdir     = "/home/jose/.cabal/lib/x86_64-linux-ghc-9.4.8/campo-minado-0.1.0.0-inplace-campo-minado"
dynlibdir  = "/home/jose/.cabal/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/jose/.cabal/share/x86_64-linux-ghc-9.4.8/campo-minado-0.1.0.0"
libexecdir = "/home/jose/.cabal/libexec/x86_64-linux-ghc-9.4.8/campo-minado-0.1.0.0"
sysconfdir = "/home/jose/.cabal/etc"

getBinDir     = catchIO (getEnv "campo_minado_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "campo_minado_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "campo_minado_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "campo_minado_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "campo_minado_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "campo_minado_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

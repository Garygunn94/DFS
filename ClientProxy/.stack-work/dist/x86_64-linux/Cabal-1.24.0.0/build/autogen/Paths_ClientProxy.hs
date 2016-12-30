{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ClientProxy (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ggunn/DFS/ClientProxy/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/bin"
libdir     = "/home/ggunn/DFS/ClientProxy/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/lib/x86_64-linux-ghc-8.0.1/ClientProxy-0.1.0.0-3QL8PIu2wS0IX3UFcV3mIo"
datadir    = "/home/ggunn/DFS/ClientProxy/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/share/x86_64-linux-ghc-8.0.1/ClientProxy-0.1.0.0"
libexecdir = "/home/ggunn/DFS/ClientProxy/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/libexec"
sysconfdir = "/home/ggunn/DFS/ClientProxy/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ClientProxy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ClientProxy_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ClientProxy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ClientProxy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ClientProxy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

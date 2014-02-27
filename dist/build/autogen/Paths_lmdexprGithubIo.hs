module Paths_lmdexprGithubIo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/saku/project/lmdexpr.github.io/cabal-dev//bin"
libdir     = "/home/saku/project/lmdexpr.github.io/cabal-dev//lib/lmdexprGithubIo-0.1.0.0/ghc-7.6.3"
datadir    = "/home/saku/project/lmdexpr.github.io/cabal-dev//share/i386-linux-ghc-7.6.3/lmdexprGithubIo-0.1.0.0"
libexecdir = "/home/saku/project/lmdexpr.github.io/cabal-dev//libexec"
sysconfdir = "/home/saku/project/lmdexpr.github.io/cabal-dev//etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lmdexprGithubIo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lmdexprGithubIo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lmdexprGithubIo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lmdexprGithubIo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lmdexprGithubIo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

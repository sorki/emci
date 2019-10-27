
module Emci.Util where

import Control.Monad (forM, unless)
import qualified Data.Text
import qualified Filesystem.Path.CurrentOS
import System.FilePath

import Polysemy
import Polytype
import Emci.Types
import Emci.Effects

sysPathToText :: Filesystem.Path.CurrentOS.FilePath -> Data.Text.Text
sysPathToText = Data.Text.pack . Filesystem.Path.CurrentOS.encodeString

findHookExe :: Members '[Error String, Nix, Filesystem] r => Sem r FilePath
findHookExe = do
  result <- nixBuildAttr "haskellPackages.git-post-receive-zre"
  findBin "git-post-receive-zre" result

findBin :: Members '[Filesystem, Error String] r => String -> FilePath -> Sem r FilePath
findBin exeName resultDir = do
  let pth = resultDir </> "bin" </> exeName
  e <- isFile pth
  unless e $ throw "Binary not found"
  return pth

mkdirNow :: Members '[Filesystem, Time] r => FilePath -> Sem r FilePath
mkdirNow parent = do
  dirName <- isoFmtNow
  mkdir (parent </> dirName)
  return dirName

forAllRepos :: Member (Input Project) r
            => (Repo -> Sem r a)
            -> Sem r [a]
forAllRepos act = do
  Project{..} <- input
  forM projRepos act



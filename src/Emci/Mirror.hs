module Emci.Mirror (
    installHook
  , setupMirrors
  , update
  ) where

import Control.Monad (when, unless)
import qualified Data.Text

import Polysemy
import Polytype

import System.FilePath

import Emci.Effects
import Emci.Types
import Emci.Util

installHook
  :: Members '[Error String, Filesystem] r
  => FilePath
  -> FilePath
  -> Sem r ()
installHook hookExePath repoPath = do
  let hookPath = repoPath </> "hooks" </> "post-receive"
  exists <- isPath hookPath
  when exists $ remove hookPath
  symlink hookExePath hookPath

setupMirrors
  :: Members '[ Git
              , Input (Config FilePath)
              , Input Project
              , Error String
              , Nix
              , Filesystem ] r
  => Sem r ()
setupMirrors = do
  Project{..} <- input
  Config{..} <- input

  mkdir cfgUpstreamsPath
  mkdir cfgMirrorsPath

  hookExePath <- findHookExe

  void $ forAllRepos $ \r -> do
    let name = Data.Text.unpack $ repoName r
        upstreamRepoPath = cfgUpstreamsPath </> name
        mirrorRepoPath = cfgMirrorsPath </> name

    hasUpstream <- isDir upstreamRepoPath
    unless hasUpstream $ do
      clone (Data.Text.unpack $ repoLocation r) upstreamRepoPath ["--mirror"]
      addRemote upstreamRepoPath "mirror" mirrorRepoPath
    hasMirror <- isDir mirrorRepoPath
    unless hasMirror $
      clone (cfgUpstreamsPath </> name) mirrorRepoPath ["--mirror"]

    installHook hookExePath mirrorRepoPath
    setConfig mirrorRepoPath "emci" "project" (Data.Text.unpack projName)

update
  :: Members '[ Git
              , Input (Config FilePath)
              , Input Project] r
  => Sem r ()
update = do
  Config{..} <- input
  void $ forAllRepos $ \r -> do
    let upstreamPath = cfgUpstreamsPath </> (Data.Text.unpack $ repoName r)

    fetch upstreamPath ["--prune", "origin"]
    push upstreamPath ["--prune", "--mirror", "mirror"]

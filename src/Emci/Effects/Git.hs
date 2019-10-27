
module Emci.Effects.Git where

import Polysemy
import Polysemy.Error
import Polytype

import Data.Kind (Type)

import Colog.Core.IO
import Colog.Polysemy.Effect
import Emci.Effects.Process

import qualified Data.Git
import qualified Data.Git.Monad
import qualified Data.Git.Repository
import qualified Filesystem.Path.CurrentOS

data Git (m :: Type -> Type) (a :: Type) where
  Clone :: String -> FilePath -> [String] -> Git m ()
  Fetch :: FilePath -> [String] -> Git m ()
  Push :: FilePath -> [String] -> Git m ()
  AddRemote :: FilePath -> String -> String -> Git m ()
  DelRemote :: FilePath -> String -> Git m ()
  GetConfig :: FilePath -> String -> String -> Git m (Maybe String)
  SetConfig :: FilePath -> String -> String -> String -> Git m ()
  RunGitM :: FilePath -> Data.Git.Monad.GitM a -> Git m (Either String a)

makeSem ''Git

runGitAsProcess
  :: Members '[ Async
              , Log ProcLogItem
              , Resource
              , Error String
              , Embed IO] r
  => Sem (Git ': r) a
  -> Sem (Process ': r) a
runGitAsProcess = reinterpret $ \case
  Clone url fp args -> void $
    runLoggedProcess "git" (["clone"] ++ args ++ [url, fp])
  Fetch fp args -> void $
    runLoggedProcess "git" (["-C", fp, "fetch"] ++ args)
  Push fp args -> void $
    runLoggedProcess "git" (["-C", fp, "push"] ++ args)
  AddRemote fp name target -> void $
    runLoggedProcess "git" ["-C", fp, "remote", "add", name, target]
  DelRemote fp name -> void $
    runLoggedProcess "git" ["-C", fp, "remote", "remove", name]
  GetConfig fp section varName -> embed $
    Data.Git.withRepo (Filesystem.Path.CurrentOS.decodeString fp) $ \g -> Data.Git.Repository.configGet g section varName
  SetConfig fp section varName newValue -> void $
    runLoggedProcess "git" ["-C", fp, "config", section ++ "." ++ varName, newValue]
  RunGitM fp act -> embed $
    Data.Git.Monad.withRepo (Filesystem.Path.CurrentOS.decodeString fp) act

gitExample :: IO (Either String ())
gitExample = do
  runFinal
  . asyncToIOFinal
  . embedToFinal @IO
  . runLogAction logPrint
  . resourceToIOFinal
  . runError @String
  . runProcessIO
  . runGitAsProcess $ do
      clone "http://github.com/sorki/emci" "/tmp/emci" ["--mirror"]
      setConfig "/tmp/emci" "emci" "project" "soMeta"

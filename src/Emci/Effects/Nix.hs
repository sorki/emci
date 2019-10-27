module Emci.Effects.Nix where

import Polysemy
import Polysemy.Error
import Polytype

import Data.Kind (Type)

import System.Exit (ExitCode(..))
import qualified System.IO

import Colog.Core.IO
import Colog.Polysemy.Effect
import Emci.Effects.Process

data Nix (m :: Type -> Type) (a :: Type) where
  NixBuildAttr :: String -> Nix m FilePath

makeSem ''Nix

runNixAsProcess :: Members
                 '[ Async
                  , Resource
                  , Embed IO] r
                => Sem (Nix ': r) a
                -> Sem (Error String ': Process ': r) a
runNixAsProcess = reinterpret2 $ \case
  NixBuildAttr attr ->
    withProcess "nix-build" ["<nixpkgs>", "--no-out-link", "-A", attr] $ \(_i, o, e, h) -> do
      r <- waitProcess h
      case r of
        ExitSuccess -> do
          so <- embed $ System.IO.hGetContents o
          return $ last $ lines so
        ExitFailure _ -> do
          se <- embed $ System.IO.hGetContents e
          so <- embed $ System.IO.hGetContents o

          throw $ "Can't nix build attr " ++ attr ++ se ++ so

runNixAsLoggedProcess :: Members
                 '[ Async
                  , Log ProcLogItem
                  , Resource
                  , Embed IO] r
                => Sem (Nix ': r) a
                -> Sem (Error String ': Process ': r) a
runNixAsLoggedProcess = reinterpret2 $ \case
  NixBuildAttr attr -> do
    pLog <-
      captureLogs
      $ runLoggedProcess "nix-build" ["<nixpkgs>", "--no-out-link", "-A", attr]

    case last pLog of
      ProcLogItem _ ProcLogStdout l -> return l
      x -> throw $ "Unexpected item in last part line of process output" ++ show x

nixExample :: IO (Either String FilePath)
nixExample = do
  runFinal
  . asyncToIOFinal
  . embedToFinal @IO
  . runLogAction logPrint
  . resourceToIOFinal
  . runProcessIO
  . runError @String
  . runNixAsLoggedProcess $ do
      nixBuildAttr "haskellPackages.git-post-receive-zre"

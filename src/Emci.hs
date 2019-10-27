module Emci (
    runEmci
  , module Emci.Checkout
  , module Emci.Mirror
  ) where

import Control.Exception (IOException)
import Data.Text (Text)

import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall

import Polysemy
import Polytype

import Colog.Core.Action (LogAction(..))
import Colog.Polysemy.Effect

import Emci.Checkout
import Emci.Effects
import Emci.Mirror
import Emci.Pretty
import Emci.Types

runInputFromDhall
  :: Members '[Filesystem, Embed IO] r
  => Maybe String
  -> Sem (Input (Config FilePath) ': Input Project ': r) a
  -> Sem r a
runInputFromDhall mCfgFp a = do
  let cfgFp = case mCfgFp of
        Nothing -> "./project.dhall"
        Just c -> c
  (proj :: Project) <- embed $ Dhall.input Dhall.auto (Data.Text.pack cfgFp)
  home <- fmap Data.Text.pack homeDir
  runInputConst proj .
    runInputConst (fmap (Data.Text.unpack . Data.Text.replace "~" home) $ projConf proj) $ a

runEmci
  :: Maybe FilePath
  -> Sem '[ Nix
          , Git
          , Error String
          , Resource
          , Log ProcLogItem
          , Log Text
          , Input (Config FilePath)
          , Input (Project)
          , Filesystem
          , Error IOException
          , Embed IO
          , Async
          , Final IO
          ] a
  -> IO ()

runEmci mCfgFile act = do
  void . runFinal
  . asyncToIOFinal
  . embedToFinal @IO
  . runError @IOException
  . filesystemToTextIO
  . runInputFromDhall mCfgFile
  . runLogAction (LogAction $ Data.Text.IO.putStrLn)
  . mapLog renderPrettyProcTerm
  . resourceToIOFinal
  . runError @String
  . runProcessIO
  . runGitAsProcess
  . runProcessIO
  . runError @String
  . runNixAsLoggedProcess
  $ act

module Emci.Checkout (
    createCheckout
  ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text

import Polysemy
import Polytype

import System.FilePath

import Emci.Effects
import Emci.Types
import Emci.Util

createCheckout
  :: Members '[ Git
              , Input (Config FilePath)
              , Input Project
              , Log Text
              , Error String
              , Filesystem ] r
  => Sem r ()
createCheckout = do
  Config{..} <- input

  mkdir cfgCheckoutsPath

  void $ forAllRepos $ \r -> do
    let name = Data.Text.unpack $ repoName r
        targetPath = cfgCheckoutsPath </> name

    exists <- isDir targetPath
    unless exists $ do
      clone (Data.Text.unpack $ toSSHLocation $ repoLocation r) targetPath []

      writable <- try $ push targetPath ["--dry-run"]
      case writable of
        Right _ -> return ()
        Left _ -> do
          logs "Repo not writable, changing origin to original location"
          delRemote targetPath "origin"
          addRemote targetPath "origin" (Data.Text.unpack $ repoLocation r)

toSSHLocation :: Text -> Text
toSSHLocation http = case Data.Text.stripPrefix "https://github.com/" http of
  Nothing -> http
  Just rest -> "git@github.com:" <> rest

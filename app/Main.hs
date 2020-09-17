{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.IO.Class
import Options.Applicative

import Emci
import Emci.Effects.Git

data Command =
    MirrorCommand MirrorCommands
  | CheckoutCommand CheckoutCommands
  deriving (Eq, Show, Ord)

data MirrorCommands =
    MirrorInit
  | MirrorUpdate
  deriving (Eq, Show, Ord)

data CheckoutCommands =
    CheckoutStatus
  | CheckoutCreate
  deriving (Eq, Show, Ord)

data Options = Options {
    cmd     :: Command
  , cfgFile :: Maybe FilePath
  } deriving (Show)

parseMirrorCommand = MirrorCommand <$> subparser
  (   command "init" (info (pure MirrorInit) ( progDesc "Init mirror" ))
   <> command "update" (info (pure MirrorUpdate) ( progDesc "Update mirror" ))
  )

parseCheckoutCommand = CheckoutCommand <$> subparser
  (   command "create" (info (pure CheckoutCreate) ( progDesc "Create new project checkout" ))
   <> command "status" (info (pure CheckoutStatus) ( progDesc "Show status of all checked-out repositories" ))
  )

cmdParser :: Parser Command
cmdParser = subparser
  (   command "mirror" (info (helper <*> parseMirrorCommand) ( progDesc "Mirror management commands" ))
   <> command "checkout" (info (helper <*> parseCheckoutCommand) ( progDesc "Checkout management commands" ))
  )

optParser :: Parser Options
optParser = Options
  <$> cmdParser
  <*> (optional $
        strOption (
             long "config"
          <> short 'c'
          <> metavar "CONFIG-FILE"))

main = execParser opts >>= \Options{..} -> runEmci cfgFile $ do
  case cmd of
    MirrorCommand mc -> case mc of
      MirrorInit -> setupMirrors
      MirrorUpdate -> update

    CheckoutCommand cc -> case cc of
      CheckoutCreate -> createCheckout
--      CheckoutStatus
    c -> error $ "Command not implemented :( " ++ show c
  where
      opts = info (helper <*> optParser)
        ( fullDesc
       <> progDesc "emci CLI"
       <> header "emci - The Embedded CI" )

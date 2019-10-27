{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Emci.Types where

import Dhall

data Flag = CI | Deployment | Generated
  deriving (Eq, Ord, Show, Generic)

data Repo = Repo {
    repoName     :: Text
  , repoLocation :: Text
  , repoBranch   :: Maybe Text
  , repoFlags    :: [Flag]
  } deriving (Eq, Ord, Show, Generic)

data Project = Project {
    projName   :: Text -- default or Maybe??
  , projConf   :: Config Text
  , projRepos  :: [Repo]
  } deriving (Eq, Ord, Show, Generic)

data Config fp = Config {
    cfgMirrorsPath   :: fp
  , cfgUpstreamsPath :: fp
  , cfgSnapshotsPath :: fp
  , cfgBackupsPath   :: fp
  , cfgCheckoutsPath :: fp
  } deriving (Eq, Functor, Show, Ord, Generic)

instance FromDhall (Config Text)
instance FromDhall Flag
instance FromDhall Repo
instance FromDhall Project

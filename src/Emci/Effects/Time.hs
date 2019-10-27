
module Emci.Effects.Time where

import Polysemy

import Data.Kind (Type)
import Data.Time.Clock
import Data.Time.Format

data Time (m :: Type -> Type) (a :: Type) where
  Now :: Time m UTCTime

makeSem ''Time

timeToIO :: Member (Embed IO) r
         => Sem (Time ': r) a
         -> Sem r a
timeToIO = interpret $ \case
  Now -> embed $ getCurrentTime

isoFmtNow :: Member Time r => Sem r String
isoFmtNow = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$> now

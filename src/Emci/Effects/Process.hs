module Emci.Effects.Process where

import Polysemy
import Polysemy.Error
import Polytype

import qualified Streaming.Prelude

import qualified System.IO
import System.Exit (ExitCode(..))

import Colog.Core.IO
import Colog.Polysemy.Effect

data ProcLogType = ProcLogStdout | ProcLogStderr
  deriving (Eq, Ord, Show)

data ProcLogItem = ProcLogItem {
    procLogName :: String
  , procLogType :: ProcLogType
  , procLogData :: String
  }
  deriving (Eq, Ord, Show)

runLoggedProcess
  :: Members '[ Async
              , Log ProcLogItem
              , Process
              , Resource
              , Error String
              , Embed IO] r
  => String
  -> [String]
  -> Sem r ()
runLoggedProcess p args =
  withProcess p args $ \(_i, o, e, h) -> do
      let logger handle typ =
            async $ runInputViaStream (Streaming.Prelude.fromHandle handle) $ do
            let loop = input >>= \case
                  Nothing -> return ()
                  Just x -> logs (ProcLogItem (unwords (p:args)) typ x) >> loop
            loop

      lo <- logger o ProcLogStdout
      le <- logger e ProcLogStderr
      r <- waitProcess h
      throwOnExitFailure r
      _ <- await lo
      _ <- await le
      return ()

throwOnExitFailure :: Member (Error String) r => ExitCode -> Sem r ()
throwOnExitFailure (ExitFailure e) = throw $ "Process failed with exit code: " ++ show e
throwOnExitFailure _ = return ()

-- Duplicate @Log x@ effect as @Tagget t (Output x)@ effect
taggedLogOutput :: forall t x r a
    . (Member (Tagged t (Output x)) r, Member (Log x) r)
     => Sem r a
     -> Sem r a
taggedLogOutput = intercept @(Log x) $ \case
  Log x -> logs x >> (tag @t $ output x)

-- Duplicate @Log x@ effect as @Tagget t (Log x)@ effect
taggedLogLog :: forall t x r a
    . (Member (Tagged t (Log x)) r, Member (Log x) r)
     => Sem r a
     -> Sem r a
taggedLogLog = intercept @(Log x) $ \case
  Log x -> logs x >> (tag @t $ logs x)

logOutputFile :: Members '[Embed IO, Resource] r
              => FilePath
              -> Sem (Output ProcLogItem ': r) a
              -> Sem r a
logOutputFile f rest = do
  bracket
    (embed $ System.IO.openFile f System.IO.WriteMode)
    (embed . System.IO.hClose)
    \h -> interpret (\case
      Output (ProcLogItem procAndArgs p s) -> embed $ System.IO.hPutStrLn h $ show procAndArgs ++ show p ++ " " ++ s
    ) rest

mapLog :: forall a b r x . Members '[Log b] r
  => (a -> b)
  -> Sem (Log a ': r) x
  -> Sem r x
mapLog f = interpret $ \case
  (Log x) -> logs @b (f x)

captureLogs
  :: Members '[Embed IO, Log b] r
  => Sem (Tagged "proc" (Output b) ': r) a
  -> Sem r [b]
captureLogs =
    fmap fst
  . outputToIOMonoid pure
  . untag @"proc"
  . taggedLogOutput @"proc"

procExample :: IO (Either String ())
procExample = do
  runFinal
  . asyncToIOFinal
  . embedToFinal @IO
  . runLogAction logPrint
  . resourceToIOFinal
  . runError
  . runProcessIO
  $ runLoggedProcess "echo" ["yow"]

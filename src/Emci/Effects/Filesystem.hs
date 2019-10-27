
module Emci.Effects.Filesystem where

import Control.Exception (IOException)

import Polysemy
import Polysemy.Error

import Data.Kind (Type)
import Data.Text (Text)

import qualified Data.Text.IO
import qualified System.Directory
import qualified System.Posix


data Filesystem' (fp :: Type) (stringy :: Type) (m :: Type -> Type) (a :: Type) where
  ReadFile :: fp -> Filesystem' fp stringy m stringy
  WriteFile :: fp -> stringy -> Filesystem' fp stringy m ()
  Remove :: fp -> Filesystem' fp stringy m ()
  Symlink :: fp -> fp -> Filesystem' fp stringy m ()
  Mkdir :: fp -> Filesystem' fp stringy m ()
  IsDir :: fp -> Filesystem' fp stringy m Bool
  IsFile :: fp -> Filesystem' fp stringy m Bool
  IsLink :: fp -> Filesystem' fp stringy m Bool
  IsPath :: fp -> Filesystem' fp stringy m Bool
  ListDir :: fp -> Filesystem' fp stringy m [fp]
  HomeDir :: Filesystem' fp stringy m fp
  CurrentDir :: Filesystem' fp stringy m fp
  ChangeDir :: fp -> Filesystem' fp stringy m ()
  CanonicalizePath :: fp -> Filesystem' fp stringy m fp
  -- FileStatus
  -- SymbolicLinkStatus

type Filesystem = Filesystem' FilePath Text

makeSem_ ''Filesystem'

readFile :: forall fp stringy r
          . Member (Filesystem' fp stringy) r
         => fp
         -> Sem r stringy

writeFile :: forall fp stringy r
           . Member (Filesystem' fp stringy) r
          => fp
          -> stringy
          -> Sem r ()

remove :: forall fp stringy r
        . Member (Filesystem' fp stringy) r
        => fp
        -> Sem r ()

symlink :: forall fp stringy r
         . Member (Filesystem' fp stringy) r
        => fp
        -> fp
        -> Sem r ()

mkdir :: forall fp stringy r
       . Member (Filesystem' fp stringy) r
      => fp
      -> Sem r ()


isDir :: forall fp stringy r
       . Member (Filesystem' fp stringy) r
      => fp
      -> Sem r Bool

isFile :: forall fp stringy r
        . Member (Filesystem' fp stringy) r
       => fp
       -> Sem r Bool

isLink :: forall fp stringy r
        . Member (Filesystem' fp stringy) r
       => fp
       -> Sem r Bool

isPath :: forall fp stringy r
        . Member (Filesystem' fp stringy) r
       => fp
       -> Sem r Bool

listDir :: forall fp stringy r
         . Member (Filesystem' fp stringy) r
        => fp
        -> Sem r [fp]

homeDir :: forall fp stringy r
         . Member (Filesystem' fp stringy) r
        => Sem r fp

currentDir :: forall fp stringy r
            . Member (Filesystem' fp stringy) r
           => Sem r fp

changeDir :: forall fp stringy r
           . Member (Filesystem' fp stringy) r
          => fp
          -> Sem r ()

canonicalizePath :: forall fp stringy r
                  . Member (Filesystem' fp stringy) r
                 => fp
                 -> Sem r fp

filesystemToTextIO
  :: Members  '[Embed IO, Error IOException] r
  => Sem (Filesystem' FilePath Text ': r) a
  -> Sem r a
filesystemToTextIO = interpret $ \case
  ReadFile fp         -> fromException $ Data.Text.IO.readFile fp
  WriteFile fp x      -> fromException $ Data.Text.IO.writeFile fp x
  Remove fp           -> fromException $ System.Directory.removePathForcibly fp
  Symlink a b         -> fromException $ System.Posix.createSymbolicLink a b
  Mkdir fp            -> fromException $ System.Directory.createDirectoryIfMissing True fp
  IsDir fp            -> exceptionAsFalse $ fmap System.Posix.isDirectory $ System.Posix.getFileStatus fp
  IsFile fp           -> exceptionAsFalse $ fmap System.Posix.isRegularFile $ System.Posix.getFileStatus fp
  IsLink fp           -> exceptionAsFalse $ fmap System.Posix.isSymbolicLink $ System.Posix.getSymbolicLinkStatus fp
  IsPath fp           -> fromException $ System.Directory.doesPathExist fp

  ListDir fp          -> fromException $ System.Directory.listDirectory fp
  HomeDir             -> fromException $ System.Directory.getHomeDirectory
  CurrentDir          -> fromException $ System.Directory.getCurrentDirectory
  ChangeDir fp        -> fromException $ System.Directory.setCurrentDirectory fp
  CanonicalizePath fp -> embed $ System.Directory.canonicalizePath fp
  where
    exceptionAsFalse act = do
      r <- runError $ fromException @IOException $ act
      case r of
        Left _ -> return False
        Right res -> return res


cd :: Member (Filesystem' fp txt) r => fp -> Sem r ()
cd = changeDir

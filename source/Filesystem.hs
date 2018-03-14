
module Filesystem where

import Core
import Control.Monad.Except
import Control.Monad.Catch
import Path
import Path.IO
import Data.Typeable

import qualified Data.ByteString as ByteString

newtype NonExPath = NonExPath FilePath
  deriving (Show)

newtype ExPath t = ExPath { trustEx :: Path Abs t}
  deriving (Show)

data FileF a = FileF (Path Abs File) a

data FileException =
      PathDoesNotExist String
    | FileDoesNotExist String
    | DirDoesNotExist  String
    | PathShouldNotExist String
    deriving (Typeable)

instance Show FileException where
    show (PathDoesNotExist p)   = "File or directory '" ++ show p ++ "' does not exist."
    show (FileDoesNotExist p)   = "File " ++ show p ++ " does not exist."
    show (DirDoesNotExist  p)   = "Directory " ++ show p ++ " does not exist."
    show (PathShouldNotExist p) = "Path '" ++ p ++ "' was expected to be non-existant, but it does exist."
instance Exception FileException

nonExPath :: (MonadIO m, MonadThrow m) => FilePath -> m NonExPath
nonExPath p =
  do
      file <- parseAbsFile p
      dir  <- parseAbsDir p

      bFile <- doesFileExist file
      bDir  <- doesDirExist dir

      case bFile || bDir of
        False -> return $ NonExPath p
        True  -> throwM $ PathShouldNotExist p

exPath :: (MonadIO m, MonadThrow m) => FilePath -> m (Either (ExPath File) (ExPath Dir))
exPath p =
  do
      base <- getCurrentDir

      file <- (base </>) <$> (parseRelFile p)
      dir  <- (base </>) <$> (parseRelDir p)

      bFile <- doesFileExist file
      bDir  <- doesDirExist dir

      case (bFile, bDir) of
        (True, False) -> return (Left $ ExPath file)
        (False, True) -> return (Right $ ExPath dir)
        _             -> throwM (PathDoesNotExist p)


exFile :: (MonadIO m, MonadThrow m) => Path Abs File -> m (ExPath File)
exFile p =
  do
      bExists <- doesFileExist p
      if bExists
        then (return (ExPath p))
        else (throwM $ FileDoesNotExist (p->>toFilePath))

exDir :: (MonadIO m, MonadThrow m) => Path Abs Dir -> m (ExPath Dir)
exDir p =
  do
      bExists <- doesDirExist p
      if bExists
        then (return (ExPath p))
        else (throwM (DirDoesNotExist (p->>toFilePath)))

safeReadFile :: Path Abs File -> PartIO (ByteString.ByteString)
safeReadFile p =
  do
      bExists <- doesFileExist p
      if bExists
        then (lift $ ByteString.readFile (toFilePath p))
        else (throwM (FileDoesNotExist (p->>toFilePath)))



lockfilePath :: MonadThrow m => Path d File -> m (Path d File)
lockfilePath = (<.> "lock")

lockdirPath :: MonadThrow m => Path d Dir -> m (Path d Dir)
lockdirPath = rename (++ ".lock")
  where
    rename :: MonadThrow m => (String -> String) -> Path b Dir -> m (Path b Dir)
    rename f d = (d->>parent </>) <$> parseRelDir (f (d->>dirname->>toFilePath))

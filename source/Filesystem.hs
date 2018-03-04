
module Filesystem where

import Core
import Control.Monad.Except
import Path
import Path.IO

import qualified Data.ByteString as ByteString

newtype NonExPath = NonExPath FilePath
  deriving (Show)

newtype ExPath t = ExPath { unver :: Path Abs t}
  deriving (Show)

data FileF a = FileF (Path Abs File) a

nonExPath :: FilePath -> PartIO NonExPath
nonExPath p =
  do
      file <- parseAbsFile p
      dir  <- parseAbsDir p

      bFile <- doesFileExist file
      bDir  <- doesDirExist dir

      case bFile || bDir of
        False -> return $ NonExPath p
        True  -> throwError $ "Path '" ++ p ++ "' was expected to be non-existant, but it does exist."

exPath :: FilePath -> PartIO (Either (ExPath File) (ExPath Dir))
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
        _             -> throwError ("File or directory '" ++ show p ++ "' does not exist.")


exFile :: Path Abs File -> PartIO (ExPath File)
exFile p =
  do
      bExists <- doesFileExist p
      if bExists
        then (return (ExPath p))
        else (throwError ("File " ++ show p ++ " does not exist."))

exDir :: Path Abs Dir -> PartIO (ExPath Dir)
exDir p =
  do
      bExists <- doesDirExist p
      if bExists
        then (return (ExPath p))
        else (throwError ("Directory " ++ show p ++ " does not exist."))

safeReadFile :: Path Abs File -> PartIO (ByteString.ByteString)
safeReadFile p =
  do
      bExists <- doesFileExist p
      if bExists
        then (lift $ ByteString.readFile (toFilePath p))
        else (throwError ("File " ++ show p ++ " does not exist."))

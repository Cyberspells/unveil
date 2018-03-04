
module Filesystem where

import Core
import Control.Monad.Except
import Path
import Path.IO

import qualified Data.ByteString as ByteString


newtype ExPath t = ExPath (Path Abs t)
  deriving (Show)

data FileF a = FileF (Path Abs File) a


checkPath :: FilePath -> PartIO (Either (ExPath File) (ExPath Dir))
checkPath p =
  do
      base <- lift $ getCurrentDir

      file <- (base </>) <$> (lift $ parseRelFile p)
      dir  <- (base </>) <$> (lift $ parseRelDir p)

      bFile <- lift $ doesFileExist file
      bDir  <- lift $ doesDirExist dir

      case (bFile, bDir) of
        (True, False) -> return (Left $ ExPath file)
        (False, True) -> return (Right $ ExPath dir)
        _             -> throwError ("File or directory '" ++ show p ++ "' does not exist.")


exDir :: Path Abs Dir -> PartIO (ExPath Dir)
exDir p =
  do
      bExists <- lift $ doesDirExist p
      if bExists
        then (return (ExPath p))
        else (throwError ("Directory " ++ show p ++ " does not exist."))

safeReadFile :: Path Abs File -> PartIO (ByteString.ByteString)
safeReadFile p =
  do
      bExists <- lift $ doesFileExist p
      if bExists
        then (lift $ ByteString.readFile (toFilePath p))
        else (throwError ("File " ++ show p ++ " does not exist."))

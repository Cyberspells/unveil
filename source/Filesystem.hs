
module Filesystem where

import Core
import Control.Monad.Except
import Path
import Path.IO

import qualified Data.ByteString as ByteString


newtype ExPath b t = ExPath (Path b t)
  deriving (Show)

data FileF a = FileF (Path Abs File) a




exDir :: Path Abs Dir -> ExceptT String IO (ExPath Abs Dir)
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

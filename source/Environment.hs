
module Environment
  (
      readEnvironment
  )
  where

import Control.Monad.Except
import Control.Monad.Error.Class
import Path
import Path.IO
import Filesystem
import Core
import Data.Aeson.TH
import Data.Yaml

import qualified Data.ByteString as ByteString


data Environment = Environment
  {
      config :: Config
  }
  deriving (Show)

readEnvironment :: PartIO Environment
readEnvironment = Environment <$> readConfig

readConfig :: PartIO Config
readConfig = (liftEither . decodeEither) =<< content
  where
    file :: IO (Path Abs File)
    file = getUserDocsDir >>= (return . (</> [relfile|unveil.yaml|]))

    content :: PartIO ByteString.ByteString
    content = lift file >>= safeReadFile


----------------------------------
-- configuration

data Config = Config
  {
      unveilPath :: String
  }
  deriving (Show)
$(deriveJSON defaultOptions ''Config)

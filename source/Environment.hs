
module Environment
  (
      readEnvironment,
      verifyConfig
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


data VerConfig = VerConfig
  {
      verUnveilPath :: ExPath Abs Dir
  }
  deriving (Show)

verifyConfig :: Config -> PartIO VerConfig
verifyConfig = fmap VerConfig . (exDir <=< (lift . parseAbsDir . unveilPath))


------------------------------------------------
-- Build environment
data Environment = Environment
  {
      config :: VerConfig
  }
  deriving (Show)

readEnvironment :: PartIO Environment
readEnvironment = Environment <$> (verifyConfig =<< readConfig)


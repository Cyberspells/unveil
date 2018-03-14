
module Environment.Config
  (
      VerConfig (..),
      readConfig
  )
where

-- unveil modules
import Core
import Filesystem

-- extenal modules
import Path
import Path.IO
import Data.Yaml (decodeEither)
import qualified Data.ByteString as ByteString

-- template haskell
import Data.Aeson.TH




-----------------------------------------------------------------
-- configuration

data Config = Config
  {
      theaterPath :: String
  }
  deriving (Show)
$(deriveJSON defaultOptions ''Config)


data VerConfig = VerConfig
  {
      verTheaterPath :: ExPath Dir
  }
  deriving (Show)


-----------------------------------------------------------------
-- Interface

readConfig :: PartIO VerConfig
readConfig =
  let
      ----------------------------------------------
      -- Read the configuration file from disk

      file :: IO (Path Abs File)
      file = getUserDocsDir >>= (return . (</> [relfile|unveil.yaml|]))

      content :: PartIO ByteString.ByteString
      content = lift file >>= safeReadFile

      ----------------------------------------------
      -- Create the corresponding Haskell object
      -- of type `Config`
      config :: PartIO Config
      config = content >>= (decodeEither ´ liftEither)

      ----------------------------------------------
      -- Verify the Config. Check whether the
      -- given paths exist
      verConfig :: PartIO VerConfig
      verConfig = do

          config        <- config
          verTheaterDir <- parseAbsDir (config->>theaterPath) >>= exDir

          return $ VerConfig verTheaterDir
  in
    verConfig


module Environment
  (
      readInitState,
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
import Data.Yaml (decodeEither)

import Options.Applicative
import Data.Semigroup ((<>))

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
      verUnveilPath :: ExPath Dir
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


------------------------------------------------
-- Command
data Command = Command
  {
      action :: Action,
      file   :: String
  }
  deriving (Show)

data Action = Open | Close
  deriving (Show)

commandP :: Parser Command
commandP = Command <$> actionP <*> fileP
  where
    actionP :: Parser Action
    actionP = flag Open Close
              (
                  long "close" <>
                  short 'c' <>
                  help "Whether to close the given FILE (default: open)"
              )

    fileP = argument str
            (
                metavar "FILE"
            )

commandInfo :: ParserInfo Command
commandInfo = info (commandP <**> helper)
              (
                  fullDesc <>
                  progDesc "Encrypt & decrypt files" <>
                  header "/// unveil ///"
              )

data VerCommand = VerCommand
  {
      verAction :: Action,
      verTarget :: Either (ExPath File) (ExPath Dir)
  }
  deriving (Show)

readCommand :: PartIO Command
readCommand = lift $ execParser commandInfo

verCommand :: Command -> PartIO VerCommand
verCommand (Command a f) = VerCommand a <$> checkPath f

readInitState :: PartIO (Environment, VerCommand)
readInitState = (,) <$> readEnvironment <*> (verCommand =<< readCommand)

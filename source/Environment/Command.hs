
module Environment.Command
  (
      VerCommand (..),
      Action (..),
      readCommand
  )
where

-- own modules
import Core
import Filesystem

-- external modules
import Path
import Options.Applicative
import Data.Semigroup ((<>))


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


readCommand :: PartIO VerCommand
readCommand =
  do
      (Command a f) <- lift $ execParser commandInfo
      VerCommand a <$> exPath f


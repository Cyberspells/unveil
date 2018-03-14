
module Environment
  (
      Environment (..),
      -- VerConfig (..),
      -- VerCommand (..),
      -- VerObject (..),
      -- VerMapping (..),
      readEnvironment
  )
  where

-- own modules
import Core
import Environment.Command
import Environment.Config
import Environment.Theater



------------------------------------------------
-- The execution environment

data Environment = Environment
  {
      command :: VerCommand,
      config  :: VerConfig,
      theater :: VerTheater
  }

readEnvironment :: PartIO Environment
readEnvironment = do
    command <- readCommand
    config  <- readConfig
    theater <- readTheater (config->>verTheaterPath)
    return (Environment command config theater)



module Main where

import Control.Monad.Except
import Filesystem
import Core

import Environment
import Environment.Command
import Environment.Config
import Action

main :: IO ()
main = runExceptT (readEnvironment >>= dispatch) >>= checkresult
  where
    dispatch :: (Environment) -> PartIO ()
    dispatch Environment
      {
          command = VerCommand Open (Left file),
          config,
          theater
      }
      = openFile (config->>verTheaterPath) file >> return ()
    -- dispatch (env, VerCommand Open (Left file)) = lift $ openFile file (env->>config->>verTheater)
    -- dispatch (env, _) = throwError "This action is not implemented."


    checkresult :: Show a => Either String a -> IO ()
    checkresult (Left s) = putStrLn $ "Error: " ++ s
    checkresult (Right a) = putStrLn $ "Done: " ++ show a



-- read configuration


-- config :: PartIO (ExPath Abs File)
-- config = 




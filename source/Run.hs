
module Run where

import Prelude hiding (putStrLn)
import Control.Monad.Except
import Filesystem
import Data.ByteString.Char8 (putStrLn, pack)
import Data.ByteString.UTF8 (fromString)
import Data.Monoid

import Core
import Environment
import Environment.Command
import Environment.Config
import Action

run :: IO ()
run = do
    runExceptT (readEnvironment >>= _dispatch) >>= _checkresult
  where
    _dispatch :: (Environment) -> PartIO ()
    _dispatch Environment
      {
          command = VerCommand Open (Left file),
          config,
          theater
      }
      = openFile (config->>verTheaterPath) file >> return ()
    -- dispatch (env, VerCommand Open (Left file)) = lift $ openFile file (env->>config->>verTheater)
    -- dispatch (env, _) = throwError "This action is not implemented."


    _checkresult :: Show a => Either String a -> IO ()
    _checkresult (Left s) = putStrLn $ "Error: " <> pack s
    _checkresult (Right a) = putStrLn $ "Done: " <> pack (show a)



-- read configuration


-- config :: PartIO (ExPath Abs File)
-- config = 



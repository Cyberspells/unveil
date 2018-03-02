
module Main where

import Control.Monad.Except
import Filesystem
import Core

import Environment

main :: IO ()
main = runExceptT readEnvironment >>= checkresult
  where checkresult :: Show a => Either String a -> IO ()
        checkresult (Left s) = putStrLn $ "Error: " ++ s
        checkresult (Right a) = putStrLn $ "Done: " ++ show a



-- read configuration


-- config :: PartIO (ExPath Abs File)
-- config = 




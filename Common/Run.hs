
module Run
  (
      run
  )
  where

import qualified CLI
import qualified UI

import System.IO.HiddenChar

run :: IO ()
run =
  do
      UI.test2

loop :: IO ()
loop =
  do
      putStrLn =<< fmap pure getHiddenChar
      loop


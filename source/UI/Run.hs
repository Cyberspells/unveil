
module UI.Run
  (
      run
  )
  where

import qualified UI.CLI as CLI
import qualified UI.UI as UI

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


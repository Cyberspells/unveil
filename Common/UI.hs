
module UI
  (
      test2
      -- showSize,
  )
  where


import Obscura.Projection
import Obscura.Components

import Data.Text

import Control.Lens

import System.IO
import System.Console.ANSI


data State = State
  {
      _title :: Text,
      _testContent :: Text
  }
makeLenses ''State



-- data App a = App
--   {
--       keyHandler :: Char -> a -> a
--   }

-- data TextBox = TextBox String

-- class Draw a where
--     draw :: a ->














-- new, only getter


initialState = State "hello" "this is my text\nand another one which is to long for this world\n\n\n\n\n\n\n hello?\n\nIs anyone there?\n\n\n\n Just kidding!"

stateP = verticalLayout
           (addBorder (displayText testContent))
           (displayText title)


test2 = clearScreen >> runProjection stateP initialState




-- noFormat :: Show a => Getter a Text
-- noFormat = to $ \a -> pack $ show a






module Obscura.Render
  (
      Render(..)
  )
  where

import Obscura.Types

import Data.Text

import Control.Lens

import System.Console.ANSI





class Render t where
    render :: (Frame, t) -> Code

data FormD = FormD Bool Text

instance Render FormD where
    render (Frame p s, (FormD b t)) = setSGR [SetSwapForegroundBackground b] >> renderText (Frame p s, t)

instance Render Text where
    render frame = setSGR [Reset] >> renderText frame



renderText :: (Frame, Text) -> Code
renderText (Frame position size, text) = mapM_ putLine $ Prelude.zip [ Point x (position^.y) | x <- [(position^.x) .. (position^.x) + (size^.x)]] (Data.Text.lines text)
  where
    putLine :: (Point, Text) -> Code
    putLine (p, l) = setCursorPositionP p >> putStr cleanLine
      where
        cleanLine = Prelude.take (size^.y) (unpack l ++ repeat ' ')


setCursorPositionP :: Point -> Code
setCursorPositionP (Point i j) = setCursorPosition i j

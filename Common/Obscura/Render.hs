
module Obscura.Render
  (
      Render(..),
      renderBorder
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
renderText (Frame position size, text) = mapM_ putLine $ Prelude.zip [ Point x (position^.y) | x <- [(position^.x) .. (position^.x) + (size^.x) - 1]] (Data.Text.lines text)
  where
    putLine :: (Point, Text) -> Code
    putLine (p, l) = setCursorPositionP p >> putStr cleanLine
      where
        cleanLine = Prelude.take (size^.y - 1) (unpack l ++ repeat ' ')


setCursorPositionP :: Point -> Code
setCursorPositionP (Point i j) = setCursorPosition i j

renderBorder :: Frame -> Code
renderBorder (Frame pos size) =
  do
      setSGR [SetSwapForegroundBackground True]
      horizontal pos (size^.y)
      vertical pos (size^.y)
      vertical (pos & y +~ (size^.y)) (size^.x)
      horizontal (pos & x +~ (size^.x)) (size^.y)


  where
    horizontal :: Point -> Int -> Code
    horizontal pos length =
      do
          setCursorPositionP pos
          putStr (Prelude.replicate length ' ')

    vertical :: Point -> Int -> Code
    vertical pos length = 
      let points =
            [ Point i (pos^.y) | i <- [pos^.x .. pos^.x + length - 1]]
          putPoint p =
            setCursorPositionP p >> putStr " "

      in mapM_ putPoint points

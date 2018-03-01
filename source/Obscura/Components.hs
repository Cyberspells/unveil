
module Obscura.Components
  (
      displayText,
      displayList,
      addBorder,
      verticalLayout
  )
  where

import Obscura.Types
import Obscura.Render
import Obscura.Projection

import Data.Text
import Data.Monoid

import Control.Lens



verticalLayout :: Projection x -> Projection x -> Projection x
verticalLayout p q = (p & draw %~ (. frame1)) <> (q & draw %~ (. frame2))
  where
    frame1 = _1 %~ (\(Frame position (Point h w)) -> Frame position (Point (h - 2) w))
    frame2 = _1 %~ (\(Frame (Point x y) (Point h w)) -> Frame (Point h y) (Point 1 w))


addBorder :: Projection x -> Projection x
addBorder p = (p & draw %~ (. frameChange)) <> border
  where
    frameChange = _1 <>~ (Frame (Point 1 1) (Point (-1) (-1)))
    border = Projection
             {
                 _draw = \(frame, _) -> renderBorder frame,
                 _key = \_ -> id
             }

displayText :: Render a => Getter x a -> Projection x
displayText l = Projection
            {
                _draw = \(frame, x) -> render (frame, (x^.l)),
                _key = \_ -> id
            }

displayList :: Show a => Getter x [a] -> Projection x
displayList l = displayText (l.to listStrings)
  where
    listStrings :: Show a => [a] -> Text
    listStrings = intercalate "\n" . fmap (pack . show)


{-
Helper
-}
appendCode :: Getter (Frame, x) Code -> Getter (Frame, x) Code -> Getter (Frame, x) Code
appendCode l1 l2 = to $ \f -> f^.l1 >> f^.l2

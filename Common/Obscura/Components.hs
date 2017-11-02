
module Obscura.Components
  (
      renderBox,
      listBox,
      vertical_ms_box
  )
  where

import Obscura.Types
import Obscura.Render
import Obscura.Projection

import Data.Text
import Data.Monoid

import Control.Lens



vertical_ms_box :: Projection x -> Projection x -> Projection x
vertical_ms_box p q = (p & draw %~ (. frame1)) <> (q & draw %~ (. frame2))
  where
    frame1 = _1 %~ (\(Frame position (Point h w)) -> Frame position (Point (h - 2) w))
    frame2 = _1 %~ (\(Frame (Point x y) (Point h w)) -> Frame (Point h y) (Point 1 w))


renderBox :: Render a => Getter x a -> Projection x
renderBox l = Projection
            {
                _draw = \(frame, x) -> render (frame, (x^.l)),
                _key = \_ -> id
            }

listBox :: Show a => Getter x [a] -> Projection x
listBox l = renderBox (l.to listStrings)
  where
    listStrings :: Show a => [a] -> Text
    listStrings = intercalate "\n" . fmap (pack . show)


{-
Helper
-}
appendCode :: Getter (Frame, x) Code -> Getter (Frame, x) Code -> Getter (Frame, x) Code
appendCode l1 l2 = to $ \f -> f^.l1 >> f^.l2

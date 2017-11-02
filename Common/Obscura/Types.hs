
module Obscura.Types
  (
      Code,
      Point(..), x, y,
      Frame(..), topLeft, size,
      Projection(..), draw, key
  )
  where

import Control.Lens

import Data.Monoid


type Code = IO ()




data Point = Point
  {
      _x :: Int,
      _y :: Int
  }
makeLenses ''Point


data Frame = Frame
  {
      _topLeft :: Point,
      _size :: Point
  }
makeLenses ''Frame



data Projection a = Projection
  {
      _draw :: (Frame, a) -> Code,
      _key :: Char -> a -> a
  }
makeLenses ''Projection


instance Monoid (Projection a) where
    mempty = Projection
             {
                 _draw = \_ -> pure (),
                 _key = \_ -> id
             }
    mappend p q = Projection
                  {
                      _key = \k -> (_key p) k . (_key q) k,
                      _draw = (_draw p) <> (_draw q)
                  }

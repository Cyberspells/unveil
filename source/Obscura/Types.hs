
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

instance Monoid Point where
    mempty = Point 0 0
    mappend (Point x y) (Point a b) = Point (x + a) (y + b)


data Frame = Frame
  {
      _topLeft :: Point,
      _size :: Point
  }
makeLenses ''Frame

instance Monoid Frame where
    mempty = Frame mempty mempty
    mappend (Frame p s) (Frame q t) = Frame (p <> q) (s <> t)



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


module Obscura.Projection
  (
      runProjection
  )
  where

import System.IO
import System.IO.HiddenChar
import qualified System.Console.Terminal.Size as Size

import Data.Monoid
import Control.Lens

import Obscura.Types

{-
Main function for running a screen.
-}
runProjection :: Projection a -> a -> IO ()
runProjection w a =
  do
      s <- getTerminalSize
      case s of
        Nothing -> putStrLn "Error: Could not get terminal size."
        Just psize  -> do
            let frame = (Frame (Point 0 0) psize, a)
            (w^.draw) frame
            hFlush stdout
            _ <- getHiddenChar
            return ()


getTerminalSize :: IO (Maybe Point)
getTerminalSize = (fmap . fmap) f Size.size
  where
    f :: Size.Window Int -> Point
    f (Size.Window x y) = Point x y


-- appendCode :: (Frame, x) Code -> Getter (Frame, x) Code -> Getter (Frame, x) Code
-- appendCode l1 l2 = to $ \f -> f^.l1 >> f^.l2

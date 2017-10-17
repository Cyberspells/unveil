
module UI
  (
      test2,
      showSize,
      runWindow
  )
  where

import System.IO
import System.Console.ANSI
import System.IO.HiddenChar
import qualified System.Console.Terminal.Size as Size

import Control.Lens

import Data.Text

data State = State
  {
      _title :: Text,
      _value :: Int
  }
makeLenses ''State


-- data App a = App
--   {
--       keyHandler :: Char -> a -> a
--   }

-- data TextBox = TextBox String

-- class Draw a where
--     draw :: a ->


data Point = Point
  {
      _x :: Int,
      _y :: Int
  }
makeLenses ''Point

data Frame a = Frame
  {
      _topLeft :: Point,
      _content :: a
  }
makeLenses ''Frame

data Draw a = Draw (IO ()) a

runWindow :: Int -> (Frame a -> Draw a) -> a -> IO a
runWindow 0 _ a = return a
runWindow i f a =
    do
      let (Draw code a') = f (Frame (Point (i*2) i) a)
      let i' = i - 1
      code
      hFlush stdout
      _ <- getHiddenChar
      runWindow i' f a'




data Window a = Window
  {
      _draw :: Fold (Frame a) Code,
      _key :: Char -> a -> a
  }



type D a = Draw a
type F a = Frame a


type Code = IO ()



runWindow2 :: (Fold (F a) Code) -> a -> IO ()
runWindow2 l a =
  do
      let frame = (Frame (Point 0 0) a)
      frame^.l
      hFlush stdout
      _ <- getHiddenChar
      return ()

-- new, only getter

-- double :: Fold 

class Format t where
    format :: t -> Code

data FormD = FormD Text Bool

instance Format FormD where
    format (FormD t b) = setSGR [SetSwapForegroundBackground b] >> putStr (show t)

instance Format Text where
    format t = setSGR [Reset] >> putStr (show t)



double :: Fold (F x) Code -> Fold (F x) Code -> Fold (F x) Code
double l1 l2 = folding f
  where
    f (Frame (Point i j) x) = [foldOf l1 (Frame (Point i (j + 2)) x)] ++ [foldOf l2 (Frame (Point (i+1) (j+2)) x)]


textBox :: Format a => Getter x a -> Window x
textBox l = Window
            {
                _draw = to $ \frame -> setCursorPositionW (frame^.topLeft) >> format (frame^.content^.l),
                _key = \_ -> id
            }


setCursorPositionW (Point i j) = setCursorPosition i j



listBox :: Show a => Getter x [a] -> Window x
listBox l = textBox $ l.listStrings


listStrings :: Show a => Getter [a] Text
listStrings = to $ \xs -> (intercalate "\n" $ fmap tshow xs)

tshow :: Show a => a -> Text
tshow a = pack . show $ a



test2 = clearScreen >> runWindow2 (double (textBox title) num) (State "hello" 1)

num :: Fold (F State) Code
num = double (textBox (value.noFormat)) (textBox (value.noFormat))


noFormat :: Show a => Getter a Text
noFormat = to $ \a -> pack $ show a


-- test :: String
-- test = setTitleCode "Security"

showSize :: IO ()
showSize = putStrLn =<< fmap show s
  where
    s :: IO (Maybe (Size.Window Int))
    s = Size.size


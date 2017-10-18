
module UI
  (
      test2,
      -- showSize,
      runWindow2
  )
  where

import System.IO
import System.Console.ANSI
import System.IO.HiddenChar
import qualified System.Console.Terminal.Size as Size

import Control.Lens

import Data.Text
import qualified Data.Text.IO as TIO

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


data Point = Point
  {
      _x :: Int,
      _y :: Int
  }
makeLenses ''Point

data Frame a = Frame
  {
      _topLeft :: Point,
      _size :: Point,
      _content :: a
  }
makeLenses ''Frame

data Draw a = Draw (IO ()) a


type Code = IO ()

data Window a = Window
  {
      _draw :: Getter (Frame a) Code,
      _key :: Char -> a -> a
  }



type D a = Draw a
type F a = Frame a





runWindow2 :: Window a -> a -> IO ()
runWindow2 w a =
  do
      size <- getTerminalSize
      case size of
        Nothing -> putStrLn "Error: Could not get terminal size."
        Just psize  -> do
            let frame = (Frame (Point 1 1) psize a)
            frame ^. _draw w
            hFlush stdout
            _ <- getHiddenChar
            return ()

-- new, only getter


class Format t where
    format :: Frame t -> Code

data FormD = FormD Bool Text

instance Format FormD where
    format (Frame p s (FormD b t)) = setSGR [SetSwapForegroundBackground b] >> boxPutText (Frame p s t)

instance Format Text where
    format frame = setSGR [Reset] >> boxPutText frame

boxPutText :: Frame Text -> Code
boxPutText (Frame position size text) = mapM_ putLine $ Prelude.zip [ Point x (position^.y) | x <- [(position^.x) .. (position^.x) + (size^.x)]] (Data.Text.lines text)
  where
    putLine :: (Point, Text) -> Code
    putLine (p, l) = setCursorPositionW p >> TIO.putStr l
      where
        cleanLine = Prelude.take (size^.y) (unpack l ++ repeat ' ')


double :: Window x -> Window x -> Window x
double w1 w2 =
  Window
  {
      _key = \k -> (_key w1) k . (_key w2) k,
      _draw = ((to frame1) . (_draw w1)) `appendCode` ((to frame2) . (_draw w2))
  }
  where
    frame1 = mapFrame (\(position, Point h w) -> (position, Point (h - 2) w))
    frame2 = mapFrame (\(Point x y, Point h w) -> (Point h y, Point 1 w))

mapFrame :: ((Point,Point) -> (Point,Point)) -> Frame x -> Frame x
mapFrame f (Frame position size x) = uncurry Frame (f (position,size)) x

appendCode :: Getter (F x) Code -> Getter (F x) Code -> Getter (F x) Code
appendCode l1 l2 = to $ \f -> f^.l1 >> f^.l2


textBox :: Format a => Getter x a -> Window x
textBox l = Window
            {
                _draw = to $ \(Frame position size x) -> format (Frame position size (x^.l)),
                _key = \_ -> id
            }


setCursorPositionW (Point i j) = setCursorPosition i j



listBox :: Show a => Getter x [a] -> Window x
listBox l = textBox $ l.listStrings


listStrings :: Show a => Getter [a] Text
listStrings = to $ \xs -> (intercalate "\n" $ fmap tshow xs)

tshow :: Show a => a -> Text
tshow a = pack . show $ a



test2 = clearScreen >> runWindow2 (double (textBox testContent) (textBox title)) (State "hello" "this is my text\nand another one which is to long for this world\n\n\n\n\n\n\n hello?\n\nIs anyone there?\n\n\n\n Just kidding!")

-- num :: Getter (F State) Code
-- num = double (textBox (value.noFormat)) (textBox (value.noFormat))





noFormat :: Show a => Getter a Text
noFormat = to $ \a -> pack $ show a


-- test :: String
-- test = setTitleCode "Security"

getTerminalSize :: IO (Maybe Point)
getTerminalSize = (fmap . fmap) f Size.size
  where
    f :: Size.Window Int -> Point
    f (Size.Window x y) = Point x y


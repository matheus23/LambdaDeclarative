module Main where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import Graphics.Declarative.Tracked
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Gtk.KeyboardInput

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import Data.Maybe (fromJust)
import FRP.Behaviour
import Control.Automaton
import Widget
import Util

main :: IO ()
main = runFormFocusableWidget (0.5, 0.5) $ mapState snd $ textLineFocusable "Philipp"


data TextLine = Line { onLeft :: String, onRight :: String }

instance Show TextLine where
  show (Line left right) = reverse left ++ "|" ++ right

toString :: TextLine -> String
toString (Line left right) = reverse left ++ right

-- cursor on the right
fromString :: String -> TextLine
fromString str = Line (reverse str) ""

removeChar :: TextLine -> TextLine
removeChar (Line (x:left) right) = Line left right
removeChar tl = tl

deleteChar :: TextLine -> TextLine
deleteChar (Line left (x:right)) = Line left right
deleteChar tl = tl

addChar :: Char -> TextLine -> TextLine
addChar c (Line left right) = Line (c:left) right

moveLeft :: TextLine -> TextLine
moveLeft (Line (c:left) right) = Line left (c:right)
moveLeft tl = tl

moveRight :: TextLine -> TextLine
moveRight (Line left (c:right)) = Line (c:left) right
moveRight tl = tl

textLineFocusable :: String -> Widget GtkEvent (String, Bool -> Form) (Maybe GtkEvent)
textLineFocusable content = mapState render $ textLine content
  where
    style = font "monospace" 8
    render line = (toString line, renderToForm line)
    renderToForm line True = text style $ show line
    renderToForm line False = text style $ toString line

textLine :: String -> Widget GtkEvent TextLine (Maybe GtkEvent)
textLine content = foldW (fromString content) step
  where
    step (KeyPress (Special Backspace)) tl = (removeChar tl, Nothing)
    step (KeyPress (Special ArrRight)) tl = (moveRight tl, Nothing)
    step (KeyPress (Special ArrLeft)) tl = (moveLeft tl, Nothing)
    step (KeyPress (Letter c)) tl = (addChar c tl, Nothing)
    step e tl = (tl, Just e)

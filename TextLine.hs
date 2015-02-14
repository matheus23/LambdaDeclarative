module TextLine where

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

data TextLine = Line { onLeft :: String, onRight :: String }

instance Show TextLine where
  show (Line left right) = reverse left ++ "|" ++ right

toString :: TextLine -> String
toString (Line left right) = reverse left ++ right

-- cursor on the right
fromString :: String -> TextLine
fromString str = Line (reverse str) ""

removeChar :: TextLine -> Maybe TextLine
removeChar (Line (x:left) right) = Just $ Line left right
removeChar _ = Nothing

deleteChar :: TextLine -> Maybe TextLine
deleteChar (Line left (x:right)) = Just $ Line left right
deleteChar _ = Nothing

addChar :: Char -> TextLine -> TextLine
addChar c (Line left right) = Line (c:left) right

moveLeft :: TextLine -> Maybe TextLine
moveLeft (Line (c:left) right) = Just $ Line left (c:right)
moveLeft _ = Nothing

moveRight :: TextLine -> Maybe TextLine
moveRight (Line left (c:right)) = Just $ Line (c:left) right
moveRight _ = Nothing

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
    action a object event = case a object of
      Just sth -> (sth, Nothing)
      Nothing  -> (object, Just event)
    step e@(KeyPress (Special Backspace)) tl = action removeChar tl e
    step e@(KeyPress (Special ArrRight)) tl = action moveRight tl e
    step e@(KeyPress (Special ArrLeft)) tl = action moveLeft tl e
    step e@(KeyPress (Special Delete)) tl = action deleteChar tl e
    step e@(KeyPress (Letter c)) tl = (addChar c tl, Nothing)
    step e tl = (tl, Just e)

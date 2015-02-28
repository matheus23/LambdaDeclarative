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
import ReactBox
import Util

data TextLine = Line { onLeft :: String, onRight :: String }

instance Show TextLine where
  show (Line left right) = reverse left ++ "|" ++ right

toString :: TextLine -> String
toString (Line left right) = reverse left ++ right

-- cursor on the right
fromString :: String -> TextLine
fromString str = Line "" str

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

textLineFocusable :: TextStyle -> String -> ReactBox GtkEvent (String, Bool -> Form) (Maybe GtkEvent)
textLineFocusable style content = mapState render $ textLine content
  where
    render line = (toString line, renderToForm line)
    renderToForm line True = text style $ show line
    renderToForm line False = text style $ toString line

textLine :: String -> ReactBox GtkEvent TextLine (Maybe GtkEvent)
textLine content = steppingBox (fromString content) step
  where
    step e@(KeyPress (Special Backspace)) (Line (_:left) right) = (Line left right, Nothing)
    step e@(KeyPress (Special ArrRight)) (Line left (c:right)) = (Line (c:left) right, Nothing)
    step e@(KeyPress (Special ArrLeft)) (Line (c:left) right) = (Line left (c:right), Nothing)
    step e@(KeyPress (Special Delete)) (Line left (_:right)) = (Line left right, Nothing)
    step e@(KeyPress (Letter c)) tl = (addChar c tl, Nothing)
    step e tl = (tl, Just e)

module Main where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Gtk.KeyboardInput

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import Data.List (intersperse)
import FRP.Behaviour
import Control.Automaton
import ReactBox
import Util
import TextLine
import FocusForm
import WidgetUtilities

main :: IO ()
main = runFormWidget (0.5, 0.5) $ mapState focused $ mathExprWidget $ Add [Num 2, Var "a", Var "b", Mul [Num 4, Var "n", Var "y"]]

data MathExpr
  = Add [MathExpr]
  | Mul [MathExpr]
  | Num Int
  | Var String

mathExprWidget :: MathExpr -> GtkWidget FocusForm
mathExprWidget (Num numb) = mapState (mapFocusedForm addFocusBorder) $ mapState snd $ numWidget generalFont numb
mathExprWidget (Var name) = mapState (mapFocusedForm addFocusBorder) $ mapState snd $ textEdit generalFont name
mathExprWidget (Mul exprs) = mapState renderFocus $ focusWrapper ((== focusKey), (== unfocusKey)) True $ listWidget1 renderMul $ map mathExprWidget exprs
mathExprWidget (Add exprs) = mapState renderFocus $ focusWrapper ((== focusKey), (== unfocusKey)) True $ listWidget1 renderAdd $ map mathExprWidget exprs

renderFocus :: (Bool, GtkWidget FocusForm) -> FocusForm
renderFocus (innerFocused, widget) = applyIf (not innerFocused) (mapFocusedForm addFocusBorder) $ fromFocus innerFocused $ boxValue widget

addFocusBorder :: Form -> Form
addFocusBorder = addBorder (solid lightBlue) 4

addBorder :: LineStyle -> Double -> Form -> Form
addBorder lstyle padding form = form `atopBorderless` border
  where border = outlined lstyle $ roundedRectangleFromBB padding $ Border.getBoundingBox $ getBorder $ padded padding form

renderAdd :: Form -> Form -> Form
renderAdd addend1 addend2 = append Vec2.right $ map centeredVert [addend1, plusSign, addend2]
  where plusSign = text generalFont " + "

renderMul :: Form -> Form -> Form
renderMul factor1 factor2 = append Vec2.right $ map centeredVert [factor1, mulSign, factor2]
  where
    space = centeredVert $ text generalFont " "
    mulSign = append Vec2.right [space, filled (textColor generalFont) $ circle (fontSize generalFont / 8), space]

focusKey, unfocusKey :: GtkEvent
focusKey   = KeyPress $ Special Return
unfocusKey = KeyPress $ Special Escape

generalFont :: TextStyle
generalFont = font "monospace" 14 -- "sir monospace"


applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f = f
applyIf False f = id

atopBorderless :: Physical2D a => Bordered a -> Bordered a -> Bordered a
atopBorderless (Bordered border graphicA) (Bordered _ graphicB) = Bordered border $ graphicA `atop` graphicB

textEdit :: TextStyle -> String -> Widget GtkEvent (String, FocusForm)
textEdit style content = mapState render $ focusWrapper ((== focusKey), (== unfocusKey)) False $ textLine content
  where
    render (hasFocus, widget) = (str, fromFocus hasFocus rendered)
      where (str, rendered) = renderLine $ boxValue widget
    renderLine line@(Line left right) = (toString line, FocusForm (withCursor (reverse left) right) (text style $ toString line))
    cursor = collapseBorder $ alignVert 0 $ filled grey $ rectangle (fontSize style / 10) $ graphicHeight $ text style "|"
    withCursor leftOfCursor rightOfCursor = withBackground $ append Vec2.right [text greyStyle leftOfCursor, cursor, text greyStyle rightOfCursor]
    withBackground form = form `atopBorderless` (filled (0.2, 0.2, 0.2) $ roundedRectangleFromBB padding $ Border.getBoundingBox $ getBorder $ padded padding form)
    padding = 2 + fontSize style / 10
    greyStyle = style { textColor = grey }

numWidget :: TextStyle -> Int -> Widget GtkEvent (Int, FocusForm)
numWidget style number = mapState render $ focusWrapper ((== focusKey), (== unfocusKey)) False $ steppingBox number step
  where
    render (hasFocus, widget) = (number, fromFocus hasFocus rendered)
      where
        number = boxValue widget
        rendered = FocusForm (renderNumAndArrows number) (centeredHoriz $ text style $ show number)

    arrowUp size = filled (0.4, 0.4, 0.4) $ noBorder $ closedPath $ pathPoint (-size, 0) `lineConnect` pathPoint (0, -size) `lineConnect` pathPoint (size, 0)
    arrowDown size = scale (1, -1) $ arrowUp size
    renderNumAndArrows number = append Vec2.down [append Vec2.up [txt, arrowUp size], arrowDown size]
      where
        size = 0.4 * fontSize style
        txt = centeredHoriz $ text style $ show number

    step (KeyPress (Special ArrUp)) num = (num+1, Nothing)
    step (KeyPress (Special ArrDown)) num = (num-1, Nothing)
    step e num = (num, Just e)

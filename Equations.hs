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
main = runFormWidget (0.5, 0.5) $ mapState focused $ mathExprWidget $ Add [Var "a", Var "b", Mul [Var "n", Var "y"]]

data MathExpr
  = Add [MathExpr]
  | Mul [MathExpr]
  | Var String

mathExprWidget :: MathExpr -> GtkWidget FocusForm
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
    focusKey = (KeyPress (Special Return))
    unfocusKey = (KeyPress (Special Escape))
    render (hasFocus, widget) = (str, fromFocus hasFocus rendered)
      where (str, rendered) = renderLine $ boxValue widget
    renderLine line@(Line left right) = (toString line, FocusForm (withCursor (reverse left) right) (text style $ toString line))
    cursor = collapseBorder $ alignVert 0 $ filled grey $ rectangle (fontSize style / 10) $ graphicHeight $ text style "|"
    withCursor leftOfCursor rightOfCursor = withBackground $ append Vec2.right [text greyStyle leftOfCursor, cursor, text greyStyle rightOfCursor]
    withBackground form = form `atopBorderless` (filled (0.2, 0.2, 0.2) $ roundedRectangleFromBB padding $ Border.getBoundingBox $ getBorder $ padded padding form)
    padding = 2 + fontSize style / 10
    greyStyle = style { textColor = grey }

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
import Widget
import Util
import TextLine

main :: IO ()
main = runFormWidget (0.5, 0.5) $ mapState focused $ mathExprWidget $ Add [Var "a", Var "b", Mul [Var "n", Var "y"]]

data MathExpr
  = Add [MathExpr]
  | Mul [MathExpr]
  | Var String

data FocusList a = Focus [a] a [a]

focusList :: [a] -> FocusList a
focusList (x:ls) = Focus [] x ls

focusLeftmost :: FocusList a -> FocusList a
focusLeftmost (Focus (x:left) mid right) = focusLeftmost $ Focus left x (mid:right)
focusLeftmost focus = focus

focusRightmost :: FocusList a -> FocusList a
focusRightmost (Focus left mid (x:right)) = focusRightmost $ Focus (mid:left) x right
focusRightmost focus = focus

renderAddition :: [Form] -> Form
renderAddition addends = append Vec2.right $ intersperse plusSign (map centeredVert addends)
  where plusSign = centeredVert $ text generalFont " + "

renderMultiplication :: [Form] -> Form
renderMultiplication addends = append Vec2.right $ intersperse plusSign (map centeredVert addends)
  where
    space = centeredVert $ text generalFont " "
    plusSign = append Vec2.right [space, filled (textColor generalFont) $ circle (fontSize generalFont / 8), space]

addBorder :: LineStyle -> Double -> Form -> Form
addBorder lstyle padding form = form `atopBorderless` border
  where border = outlined lstyle $ roundedRectangleFromBB padding $ Border.getBoundingBox $ getBorder $ padded padding form

addFocusBorder = addBorder (solid lightBlue) 4

mathExprWidget :: MathExpr -> Widget GtkEvent SchroedForm (Maybe GtkEvent)
mathExprWidget (Var name) = mapState (mapFocusedSchroed addFocusBorder) $ mapState snd $ textEdit generalFont name
mathExprWidget (Add exprs) = mapState render $ foldW (False, focusList $ map mathExprWidget exprs) step
  where
    render (selectionMode, Focus left mid right) = SchroedForm
      { focused = applyIf selectionMode addFocusBorder $ renderAddition $ renderedLeft ++ [focusedMid] ++ renderedRight
      , unfocused = renderAddition $ renderedLeft ++ [unfocused renderedMid] ++ renderedRight }
      where
        renderedLeft = map (unfocused . valueW) $ reverse left
        renderedRight = map (unfocused . valueW) right
        renderedMid = valueW mid
        focusedMid = if not selectionMode then focused renderedMid else unfocused renderedMid
    step (KeyPress (Special ArrLeft)) (True, focus) = ((False, focusLeftmost focus), Nothing)
    step (KeyPress (Special ArrRight)) (True, focus) = ((False, focusRightmost focus), Nothing)
    step e (selectionMode, Focus left foc right) = case runW foc e of
      (newFoc, Just e) -> stepAfter e (selectionMode, Focus left newFoc right)
      (newFoc, Nothing) -> ((selectionMode, Focus left newFoc right), Nothing)
    stepAfter (KeyPress (Special ArrLeft)) (False, Focus (x:left) mid right) = ((False, Focus left x (mid:right)), Nothing)
    stepAfter (KeyPress (Special ArrRight)) (False, Focus left mid (x:right)) = ((False, Focus (mid:left) x right), Nothing)
    stepAfter (KeyPress (Special Escape)) (False, focus) = ((True, focus), Nothing)
    stepAfter e@(KeyPress (Special Escape)) (True, focus) = ((False, focus), Just e)
    stepAfter e state = (state, Just e)
mathExprWidget (Mul exprs) = mapState render $ foldW (False, focusList $ map mathExprWidget exprs) step
  where
    render (selectionMode, Focus left mid right) = SchroedForm
      { focused = applyIf selectionMode addFocusBorder $ renderMultiplication $ renderedLeft ++ [focusedMid] ++ renderedRight
      , unfocused = renderMultiplication $ renderedLeft ++ [unfocused renderedMid] ++ renderedRight }
      where
        renderedLeft = map (unfocused . valueW) $ reverse left
        renderedRight = map (unfocused . valueW) right
        renderedMid = valueW mid
        focusedMid = if not selectionMode then focused renderedMid else unfocused renderedMid
    step (KeyPress (Special ArrLeft)) (True, focus) = ((False, focusLeftmost focus), Nothing)
    step (KeyPress (Special ArrRight)) (True, focus) = ((False, focusRightmost focus), Nothing)
    step e (selectionMode, Focus left foc right) = case runW foc e of
      (newFoc, Just e) -> stepAfter e (selectionMode, Focus left newFoc right)
      (newFoc, Nothing) -> ((selectionMode, Focus left newFoc right), Nothing)
    stepAfter (KeyPress (Special ArrLeft)) (False, Focus (x:left) mid right) = ((False, Focus left x (mid:right)), Nothing)
    stepAfter (KeyPress (Special ArrRight)) (False, Focus left mid (x:right)) = ((False, Focus (mid:left) x right), Nothing)
    stepAfter (KeyPress (Special Escape)) (False, focus) = ((True, focus), Nothing)
    stepAfter e@(KeyPress (Special Escape)) (True, focus) = ((False, focus), Just e)
    stepAfter e state = (state, Just e)

generalFont :: TextStyle
generalFont = font "monospace" 14 -- "sir monospace"

data SchroedForm = SchroedForm { focused :: Form, unfocused :: Form }

instance Physical2D SchroedForm where
  move = mapSchroed . move
  rotate = mapSchroed . rotate
  scale = mapSchroed . scale
  atop = combineSchroed atop
  empty = SchroedForm empty empty

combineSchroed :: (Form -> Form -> Form) -> SchroedForm -> SchroedForm -> SchroedForm
combineSchroed f (SchroedForm f1 u1) (SchroedForm f2 u2) = SchroedForm (f f1 f2) (f u1 u2)

mapSchroed :: (Form -> Form) -> SchroedForm -> SchroedForm
mapSchroed f (SchroedForm foc unfoc) = SchroedForm (f foc) (f unfoc)

mapFocusedSchroed :: (Form -> Form) -> SchroedForm -> SchroedForm
mapFocusedSchroed f (SchroedForm foc unfoc) = SchroedForm (f foc) unfoc

mapUnfocusedSchroed :: (Form -> Form) -> SchroedForm -> SchroedForm
mapUnfocusedSchroed f (SchroedForm foc unfoc) = SchroedForm foc (f unfoc)

getIsFocused :: Bool -> SchroedForm -> Form
getIsFocused True (SchroedForm f _) = f
getIsFocused False (SchroedForm _ u) = u

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f = f
applyIf False f = id

atopBorderless :: Physical2D a => Bordered a -> Bordered a -> Bordered a
atopBorderless (Bordered border graphicA) (Bordered _ graphicB) = Bordered border $ graphicA `atop` graphicB

textEdit :: TextStyle -> String -> Widget GtkEvent (String, SchroedForm) (Maybe GtkEvent)
textEdit style content = mapState render $ focusWrapper (KeyPress (Special Return)) (KeyPress (Special Escape)) $ textLine content
  where
    render (hasFocus, widget) = (str, if hasFocus
          then SchroedForm (focused rendered) (unfocused rendered)
          else SchroedForm (unfocused rendered) (unfocused rendered))
      where (str, rendered) = renderLine $ valueW widget
    renderLine line@(Line left right) = (toString line, SchroedForm (withCursor (reverse left) right) (text style $ toString line))
    cursor = collapseBorder $ alignVert 0 $ filled grey $ rectangle (fontSize style / 10) $ graphicHeight $ text style "|"
    withCursor leftOfCursor rightOfCursor = withBackground $ append Vec2.right [text greyStyle leftOfCursor, cursor, text greyStyle rightOfCursor]
    withBackground form = form `atopBorderless` (filled (0.2, 0.2, 0.2) $ roundedRectangleFromBB padding $ Border.getBoundingBox $ getBorder $ padded padding form)
    padding = 2 + fontSize style / 10
    greyStyle = style { textColor = grey }

focusWrapper :: Eq e => e -> e -> Widget e s (Maybe e) -> Widget e (Bool, Widget e s (Maybe e)) (Maybe e)
focusWrapper keyFocus keyUnfocus widget = foldW (False, widget) step
  where
    step e (False, widget)
      | e == keyFocus = ((True, widget), Nothing)
      | otherwise     = ((False, widget), Just e)
    step e (True, widget) = case returnedE of
      Just event | event == keyUnfocus -> ((False, newWidget), Nothing)
      otherwise                        -> ((True, newWidget), returnedE)
      where (newWidget, returnedE) = runW widget e

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
import FRP.Behaviour
import Control.Automaton
import Widget
import Util
import TextLine

main :: IO ()
main = runFormWidget (0.5, 0.5) $ mapState (centeredHoriz . focused . snd) $ tupleW exampleTuple

data Tuple = Tuple Tuple Tuple | Leaf String

data Focus = LeftFocus | RightFocus deriving (Eq)

data SchroedForm = SchroedForm { focused :: Form, unfocused :: Form }

exampleTuple = Tuple (Tuple (Leaf "hi") (Leaf "this")) (Tuple (Leaf "is") (Tuple (Leaf "phil,") (Leaf "yeah!")))

foldTuple :: (String -> String -> String) -> Tuple -> String
foldTuple _ (Leaf str) = str
foldTuple f (Tuple left right) = f (foldTuple f left) (foldTuple f right)

concatWords :: String -> String -> String
concatWords a b = a ++ " " ++ b

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

getFocused :: Focus -> (a, a) -> a
getFocused LeftFocus (left, _) = left
getFocused RightFocus (_, right) = right

modifyFocused :: Focus -> (a -> a) -> (a, a) -> (a, a)
modifyFocused LeftFocus f (left, right) = (f left, right)
modifyFocused RightFocus f (left, right) = (left, f right)

setFocused :: Focus -> a -> (a, a) -> (a, a)
setFocused focus v = modifyFocused focus $ const v

renderTuple :: Form -> Form -> Form
renderTuple left right = append Vec2.right
    [ padded 2 $ centeredVert $ text style "("
    , left
    , padded 2 $ centeredVert $ text style ","
    , right
    , padded 2 $ centeredVert $ text style ")"]
  where style = font "" 10

addStringBelow :: RGB -> String -> Form -> Form
addStringBelow color stringBelow upper = append Vec2.down
    [ centeredHoriz $ upper
    , padded 5 midline
    , centeredHoriz $ lower ]
  where
    lower = text (font "serif" 8) stringBelow
    midline = filled color $ rectangle (max (graphicWidth upper) (graphicWidth lower) - 10) 2

addSthBelow :: (Double -> Form) -> Form -> Form
addSthBelow below upper = append Vec2.down
    [ centeredHoriz $ upper
    , centeredHoriz $ padded 5 formBelow ]
  where formBelow = below (graphicWidth upper - 10)

addBorder :: LineStyle -> Form -> Form
addBorder ls form = border `atop` form
  where border = outlined ls $ rectangleFromBB $ Border.getBoundingBox $ getBorder form

renderBorder :: Bool -> SchroedForm -> SchroedForm
renderBorder True (SchroedForm focused unfocused) = SchroedForm (addBorder (solid red) focused) unfocused
renderBorder False form = form

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f = f
applyIf False f = id

textLineSchroed :: String -> Widget GtkEvent (String, SchroedForm) (Maybe GtkEvent)
textLineSchroed content = mapState render $ textLine content
  where
    style = font "monospace" 8
    render line@(Line left right) = (toString line, SchroedForm (withCursor (reverse left) right) (text style $ toString line))
    cursor = collapseBorder $ alignVert 0 $ filled black $ rectangle 1.3 $ graphicHeight $ text style "|"
    withCursor leftOfCursor rightOfCursor = append Vec2.right [text style leftOfCursor, cursor, text style rightOfCursor]

tupleFocusSchroed :: Focus -> (Form -> Form -> Form) -> SchroedForm -> SchroedForm -> SchroedForm
tupleFocusSchroed LeftFocus combine left right
  = SchroedForm
      { focused   = combine (focused left) (unfocused right)
      , unfocused = combine (unfocused left) (unfocused right) }
tupleFocusSchroed RightFocus combine left right
  = SchroedForm
      { focused   = combine (unfocused left) (focused right)
      , unfocused = combine (unfocused left) (unfocused right) }

unfocus :: SchroedForm -> SchroedForm
unfocus schroedForm = schroedForm { focused = unfocused schroedForm }


tupleW :: Tuple -> Widget GtkEvent (Tuple, SchroedForm) (Maybe GtkEvent)
tupleW (Leaf str) = mapState (\(txt, form) -> (Leaf txt, mapSchroed centeredVert form)) $ textLineSchroed str
tupleW (Tuple leftT rightT) = mapState render $ focusWrapper focusKey unfocusKey $ foldW (LeftFocus, (tupleW leftT, tupleW rightT)) step
  where
    focusKey = KeyPress $ Special Return
    unfocusKey = KeyPress $ Special Escape
    render (isInnerFocused, widget) = (Tuple leftT rightT, rendered)
      where
        {-
        renderer focused = applyIf (focused && not isInnerFocused) (addStringBelow value) $ renderTuple leftRender rightRender
        renderFocused renderer = SchroedForm (renderer True) (renderer False)
        leftRender = getIsFocused (isInnerFocused && focus == LeftFocus) leftForm
        rightRender = getIsFocused (isInnerFocused && focus == RightFocus) rightForm
        -}
        value = foldTuple concatWords $ Tuple leftT rightT
        rendered = mapSchroed addValue $ renderFocus isInnerFocused $ applyIf (not isInnerFocused) unfocus $ tupleFocusSchroed focus renderTuple leftForm rightForm
        addValue = addSthBelow (const $ text (font "serif" 8) value)
        (leftT, leftForm) = valueW leftW
        (rightT, rightForm) = valueW rightW
        (focus, (leftW, rightW)) = valueW widget
    step e (focus, widgets) = ((newFocus, newWidgets), mayEvent2)
      where
        (newWidgets, mayEvent) = runInner focus widgets e
        (newFocus, mayEvent2) = postProcess handleFocusChange (focus, mayEvent)

    runInner focus widgets e = (setFocused focus newWidget widgets, mayEvent)
      where (newWidget, mayEvent) = runW (getFocused focus widgets) e

    postProcess f (state, Just event) = f state event
    postProcess f (state, Nothing)    = (state, Nothing)

    handleFocusChange RightFocus (KeyPress (Special ArrLeft)) = (LeftFocus, Nothing)
    handleFocusChange LeftFocus (KeyPress (Special ArrRight)) = (RightFocus, Nothing)
    handleFocusChange focus e = (focus, Just e)

{-tupleW (Tuple leftT rightT) = mapState render $ foldW (False, LeftFocus, (tupleW leftT, tupleW rightT)) step
  where
    render (isInnerFocused, focus, (leftW, rightW)) = (Tuple leftT rightT, rendered)
      where
        rendered = SchroedForm (applyIf (not isInnerFocused) (addStringBelow $ foldTuple concatWords $ Tuple leftT rightT) r) r where r = renderTuple leftRender rightRender
        leftRender = getIsFocused (isInnerFocused && focus == LeftFocus) leftForm
        rightRender = getIsFocused (isInnerFocused && focus == RightFocus) rightForm
        (leftT, leftForm) = valueW leftW
        (rightT, rightForm) = valueW rightW
    step e (False, focus, widgets)
      | e == (KeyPress (Special Return)) = ((True, focus, widgets), Nothing)
      | otherwise = ((False, focus, widgets), Just e)
    step e (True, focus, widgets) = case runW (getFocused focus widgets) e of
      (newWidget, Just (KeyPress (Special Escape))) -> ((False, focus, setFocused focus newWidget widgets), Nothing)
      (newWidget, Just (KeyPress (Special ArrLeft))) -> ((True, LeftFocus, setFocused focus newWidget widgets), Nothing)
      (newWidget, Just (KeyPress (Special ArrRight))) -> ((True, RightFocus, setFocused focus newWidget widgets), Nothing)
      (newWidget, mayEvent) -> ((True, focus, setFocused focus newWidget widgets), mayEvent)
-}


renderFocus isInnerFocused innerForm = mapUnfocusedSchroed addUnfocusedLineBelow $ mapFocusedSchroed addLineBelow innerForm
  where
    addLineBelow
      | isInnerFocused = addUnfocusedLineBelow
      | otherwise      = addSthBelow (horizLine orange)

    addUnfocusedLineBelow = addSthBelow (horizLine grey)

    horizLine color width = filled color $ rectangle width 2

focusWrapper :: Eq e => e -> e -> Widget e s (Maybe e) -> Widget e (Bool, Widget e s (Maybe e)) (Maybe e)
focusWrapper keyFocus keyUnfocus widget = foldW (True, widget) step
  where
    step e (False, widget)
      | e == keyFocus = ((True, widget), Nothing)
      | otherwise     = ((False, widget), Just e)
    step e (True, widget) = case returnedE of
      Just event | event == keyUnfocus -> ((False, newWidget), Nothing)
      otherwise                        -> ((True, newWidget), returnedE)
      where (newWidget, returnedE) = runW widget e

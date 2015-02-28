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

main :: IO ()
--main = runFormWidget (0.5, 0.5) $ mapState focused $ mathExprWidget $ Add [Var "a", Var "b", Mul [Var "n", Var "y"]]
main = runFormWidget (0.5, 0.5) $ mapState (centeredHoriz . focused) $ listWidget combine plusWidget $ map stringWidget ["Hello", "World", "awesome", "!"]
  where
    stringWidget str = mapState (mapFocusedForm focusRend . snd) $ textEdit style str
    style = generalFont
    combine form1 form2 = append Vec2.right [form1, text style " , ", form2]
    focusRend form = form `atopBorderless` (outlined (solid red) $ rectangleFromBB $ Border.getBoundingBox $ getBorder $ padded 5 form)
    plusWidget = condReplace replacement $ functionBox (const plusWValue) Just plusWValue
    plusWValue = mapBothForms (alignHV (0, 0)) $ FocusForm plusFocused plusUnfocused
    plusSign col = filled col (rectangle 18 6) `atop` filled col (rectangle 6 18)
    plusUnfocused = plusSign (0.2, 0.2, 0.2) `atop` outlined (solid (0.2, 0.2, 0.2)) (rectangle 30 30)
    plusFocused = plusSign (1, 1, 1) `atop` filled (0.2, 0.2, 0.2) (rectangle 30 30)
    replacement (KeyPress (Special Return)) = Just $ (toupleWidget combine FocusLeft (stringWidget "Replace me!", plusWidget), Nothing)
    replacement _ = Nothing

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

type GtkWidget s = Widget GtkEvent s

focusListWidget :: FocusList (GtkWidget s) -> GtkWidget (Bool, FocusList (GtkWidget s))
focusListWidget initList = steppingBox (False, initList) step
  where
    step (KeyPress (Special ArrLeft)) (True, focus) = ((False, focusLeftmost focus), Nothing)
    step (KeyPress (Special ArrRight)) (True, focus) = ((False, focusRightmost focus), Nothing)
    step e (selectionMode, Focus left foc right) = case runBox foc e of
      (newFoc, Just e) -> stepAfter e (selectionMode, Focus left newFoc right)
      (newFoc, Nothing) -> ((selectionMode, Focus left newFoc right), Nothing)
    stepAfter (KeyPress (Special ArrLeft)) (False, Focus (x:left) mid right) = ((False, Focus left x (mid:right)), Nothing)
    stepAfter (KeyPress (Special ArrRight)) (False, Focus left mid (x:right)) = ((False, Focus (mid:left) x right), Nothing)
    stepAfter (KeyPress (Special Escape)) (False, focus) = ((True, focus), Nothing)
    stepAfter e state = (state, Just e)

renderListExpr :: ([Form] -> Form) -> (Bool, FocusList (GtkWidget FocusForm)) -> FocusForm
renderListExpr formCombiner (selectionMode, Focus left mid right) = FocusForm
  { focused = applyIf selectionMode addFocusBorder $ formCombiner $ renderedLeft ++ [focusedMid] ++ renderedRight
  , unfocused = formCombiner $ renderedLeft ++ [unfocused renderedMid] ++ renderedRight }
  where
    renderedLeft = map (unfocused . boxValue) $ reverse left
    renderedRight = map (unfocused . boxValue) right
    renderedMid = boxValue mid
    focusedMid = if not selectionMode then focused renderedMid else unfocused renderedMid

mathExprWidget :: MathExpr -> ReactBox GtkEvent FocusForm (Maybe GtkEvent)
mathExprWidget (Var name) = mapState (mapFocusedForm addFocusBorder) $ mapState snd $ textEdit generalFont name
mathExprWidget (Add exprs) = mapState (renderListExpr renderAddition) $ focusListWidget $ focusList $ map mathExprWidget exprs
mathExprWidget (Mul exprs) = mapState (renderListExpr renderMultiplication) $ focusListWidget $ focusList $ map mathExprWidget exprs

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

focusWrapper :: (e -> Bool, e -> Bool) -> Bool -> Widget e s -> Widget e (Bool, Widget e s)
focusWrapper (isKeyFocus, isKeyUnfocus) innerFocused widget = steppingBox (innerFocused, widget) step
  where
    step e (False, widget)
      | isKeyFocus e = ((True, widget), Nothing)
      | otherwise    = ((False, widget), Just e)
    step e (True, widget) = case returnedE of
      Just event | isKeyUnfocus event -> ((False, newWidget), Nothing)
      otherwise                       -> ((True, newWidget), returnedE)
      where (newWidget, returnedE) = runBox widget e

data FocusSide = FocusLeft | FocusRight deriving (Show, Eq, Ord)

getFocused :: FocusSide -> (a, a) -> a
getFocused FocusLeft (left, _) = left
getFocused FocusRight (_, right) = right

modifyFocused :: FocusSide -> (a -> a) -> (a, a) -> (a, a)
modifyFocused FocusLeft f (left, right) = (f left, right)
modifyFocused FocusRight  f (left, right) = (left, f right)

setFocused :: FocusSide -> a -> (a, a) -> (a, a)
setFocused focus v = modifyFocused focus $ const v

biFocusWrapper :: (e -> Bool, e -> Bool) -> FocusSide -> (Widget e s, Widget e s) -> Widget e (FocusSide, (Widget e s, Widget e s))
biFocusWrapper (isKeyLeft, isKeyRight) initFocusSide widgets = steppingBox (initFocusSide, widgets) step
  where
    step e (focus, widgets) = stepAfter returnedE (focus, setFocused focus newWidget widgets)
      where (newWidget, returnedE) = runBox (getFocused focus widgets) e
    stepAfter returnedE (FocusLeft,  widgets) = case returnedE of
      Just event | isKeyRight event -> ((FocusRight, widgets), Nothing)
      anythingElse                  -> ((FocusLeft,  widgets), anythingElse)
    stepAfter returnedE (FocusRight, widgets) = case returnedE of
      Just event | isKeyLeft event  -> ((FocusLeft,  widgets), Nothing)
      anythingElse                  -> ((FocusRight, widgets), anythingElse)

toupleWidget :: (Form -> Form -> Form) -> FocusSide -> (GtkWidget FocusForm, GtkWidget FocusForm) -> GtkWidget FocusForm
toupleWidget formCombiner initSide widgets = mapState render $ biFocusWrapper ((== leftKey), (== rightKey)) initSide widgets
  where
    leftKey = KeyPress $ Special ArrLeft
    rightKey = KeyPress $ Special ArrRight
    render (focus, (widgetL, widgetR)) = combineFocusForms formCombiner renderL renderR
      where
        renderL = fromFocus (focus == FocusLeft)  $ boxValue widgetL
        renderR = fromFocus (focus == FocusRight) $ boxValue widgetR

listWidget :: (Form -> Form -> Form) -> GtkWidget FocusForm -> [GtkWidget FocusForm] -> GtkWidget FocusForm
listWidget combine nullWidget widgets = foldr (curry $ toupleWidget combine FocusLeft) nullWidget widgets

listWidget1 :: (Form -> Form -> Form) -> [GtkWidget FocusForm] -> GtkWidget FocusForm
listWidget1 combine widgets = foldr1 (curry $ toupleWidget combine FocusLeft) widgets

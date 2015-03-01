module WidgetUtilities where

import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Gtk.KeyboardInput

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import ReactBox
import FocusForm
import Util

type GtkWidget s = Widget GtkEvent s

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

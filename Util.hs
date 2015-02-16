module Util where

import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Physical2D

import FRP.Behaviour
import Widget

import qualified Data.Vec2 as Vec2

widgetToBehaviour :: Widget i s o -> Behaviour i s
widgetToBehaviour widget = Behaviour (valueW widget) step
  where step input = widgetToBehaviour $ fst $ runW widget input

runFormBehaviour :: (Double, Double) -> Behaviour GtkEvent Form -> IO ()
runFormBehaviour align behaviour = do
  runFormProgram align behaviour step
    where step input currentBehaviour = do
            let newBehaviour = runEvent currentBehaviour input
            return (newBehaviour, value $ newBehaviour)

runFormWidget :: (Double, Double) -> Widget GtkEvent Form any -> IO ()
runFormWidget align widget = runFormBehaviour (0, 0) $ draggableForm $ windowAligned align $ widgetToBehaviour widget

runFormFocusableWidget :: (Double, Double) -> Widget GtkEvent (Bool -> Form) any -> IO ()
runFormFocusableWidget align widget = runFormWidget align $ mapState ($ True) widget

windowSize :: Behaviour GtkEvent (Int, Int)
windowSize = filterMapE filterResize $ remember (0, 0)
  where
    filterResize (Resize size) = Just size
    filterResize _ = Nothing

windowAligned :: (Double, Double) -> Behaviour GtkEvent Form -> Behaviour GtkEvent Form
windowAligned alignment formB = merge alignWindow windowSize formB
  where alignWindow (w, h) form = move (fromIntegral w / 2, fromIntegral h / 2) form

draggableForm :: Behaviour GtkEvent Form -> Behaviour GtkEvent Form
draggableForm formB = merge move (mouseDrag (0, 0)) formB

data DragState = NotDragging | Dragging (Double, Double) deriving (Show, Eq)

mouseDrag :: (Double, Double) -> Behaviour GtkEvent (Double, Double)
mouseDrag initialPos = mapValue snd $ fold (NotDragging, initialPos) step
  where
    step (ButtonPress pos RightButton) (NotDragging, dragPos) = (Dragging pos, dragPos)
    step (MouseMove pos) (Dragging dragStartPos, _) = (Dragging dragStartPos, pos `Vec2.add` (dragStartPos `Vec2.to` pos))
    step (ButtonRelease pos RightButton) (Dragging dragStartPos, dragPos) = (NotDragging, pos `Vec2.add` (dragStartPos `Vec2.to` pos))
    step _ state = state

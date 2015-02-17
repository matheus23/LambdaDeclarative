module Util where

import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Physical2D

import FRP.Behaviour
import Widget

import qualified Data.Vec2 as Vec2
import Data.Vec2 as Vec2

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
draggableForm formB = merge move (easing 0.8 $ mouseDrag (0, 0)) formB

easing :: Double -> Behaviour GtkEvent Vec2 -> Behaviour GtkEvent Vec2
easing weight behaviour = mapValue snd $ fold (behaviour, value behaviour) ease
  where 
    ease Tick (b, actual) = (newB, (Vec2.scale weight actual) `Vec2.add` (Vec2.scale (1-weight) $ value newB)) where newB = runEvent b Tick
    ease e (b, actual) = (newB, actual) where newB = runEvent b e

data DragState = NotDragging | Dragging Vec2 Vec2 deriving (Show, Eq)

mouseDrag :: Vec2 -> Behaviour GtkEvent Vec2
mouseDrag initialPos = mapValue snd $ fold (NotDragging, initialPos) step
  where
    step (ButtonPress pos RightButton)   (NotDragging, dragPos) = (Dragging pos dragPos, dragPos)
    step (MouseMove pos)                 (Dragging dragStartPos prevDragPos, _) = (Dragging dragStartPos prevDragPos, prevDragPos `Vec2.add` (dragStartPos `Vec2.to` pos))
    step (ButtonRelease pos RightButton) (Dragging dragStartPos prevDragPos, _) = (NotDragging, prevDragPos `Vec2.add` (dragStartPos `Vec2.to` pos))
    step _ state = state

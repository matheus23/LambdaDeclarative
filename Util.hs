module Util where

import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Cairo.Form

import FRP.Behaviour
import Widget

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
runFormWidget align widget = runFormBehaviour align $ widgetToBehaviour widget

runFormFocusableWidget :: (Double, Double) -> Widget GtkEvent (Bool -> Form) any -> IO ()
runFormFocusableWidget align widget = runFormWidget align $ mapState ($ True) widget

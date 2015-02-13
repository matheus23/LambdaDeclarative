module Widget where

import FRP.Behaviour

data Widget i v o = Widget { valueW :: v, runW :: i -> (Widget i v o, o) }

foldW :: s -> (i -> s -> (s, o)) -> Widget i s o
foldW initialState stateF = Widget initialState step
  where step input = let (newState, output) = stateF input initialState
                      in (foldW newState stateF, output)

mapState :: (a -> b) -> Widget i a o -> Widget i b o
mapState f widget = Widget (f $ valueW widget) $ \event -> let (newW, out) = runW widget event in (mapState f newW, out)

mapOutput :: (a -> b) -> Widget i s a -> Widget i s b
mapOutput f widget = Widget (valueW widget) $ \event -> let (newW, out) = runW widget event in (mapOutput f newW, f out)

substituteOutput :: o' -> Widget i a o -> Widget i a o'
substituteOutput = mapOutput . const

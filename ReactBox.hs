module ReactBox where

import FRP.Behaviour

data ReactBox i v o = ReactBox { boxValue :: v, runBox :: i -> (ReactBox i v o, o) }

constantBox :: v -> o -> ReactBox i v o
constantBox value out = ReactBox value $ \_ -> (constantBox value out, out)

functionBox :: (i -> v) -> (i -> o) -> v -> ReactBox i v o
functionBox produceValue produceOut value = ReactBox value step
  where step i = (functionBox produceValue produceOut $ produceValue i, produceOut i)

steppingBox :: s -> (i -> s -> (s, o)) -> ReactBox i s o
steppingBox initialState stateF = ReactBox initialState step
  where step input = let (newState, output) = stateF input initialState
                      in (steppingBox newState stateF, output)

mapState :: (a -> b) -> ReactBox i a o -> ReactBox i b o
mapState f box = ReactBox (f $ boxValue box) $ \input -> let (newB, out) = runBox box input in (mapState f newB, out)

mapOutput :: (a -> b) -> ReactBox i s a -> ReactBox i s b
mapOutput f box = ReactBox (boxValue box) $ \input -> let (newB, out) = runBox box input in (mapOutput f newB, f out)

substituteOutput :: o' -> ReactBox i a o -> ReactBox i a o'
substituteOutput = mapOutput . const

-- "conditional replacement"
condReplace :: (i -> Maybe (ReactBox i v o, o)) -> ReactBox i v o -> ReactBox i v o
condReplace cond otherwiseBox = ReactBox (boxValue otherwiseBox) step
  where step input = cond input `orElse` (condReplace cond newB, out)
          where (newB, out) = runBox otherwiseBox input

wrap :: (ReactBox i v o -> i -> ReactBox i v o) -> ReactBox i v o -> ReactBox i v o
wrap wrapper box = ReactBox (boxValue box) step
  where step input = (wrap wrapper $ newB, out) where (newB, out) = runBox (wrapper box input) input

orElse :: Maybe a -> a -> a
orElse (Just sth) _ = sth
orElse Nothing a = a

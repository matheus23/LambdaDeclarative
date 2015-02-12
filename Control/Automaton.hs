module Control.Automaton where

import Control.Applicative

-- from https://github.com/leonidas/codeblog/blob/master/2012/2012-01-08-streams-coroutines.md
--  and https://github.com/evancz/automaton/blob/master/Automaton.elm

newtype Automaton i o = Automaton { runA :: i -> (Automaton i o, o) }

fromFunction :: (i -> o) -> Automaton i o
fromFunction f = Automaton $ \input -> (fromFunction f, f input)

instance Functor (Automaton i) where
  fmap f (Automaton step) = Automaton $ \input -> let (nAut, out) = step input in (fmap f nAut, f out)

instance Applicative (Automaton i) where
  pure x = Automaton $ const (pure x, x)
  autf <*> autx = Automaton $ \input ->
    let (autf', f) = runA autf input
        (autx', x) = runA autx input
     in (autf' <*> autx', f x)

-- Hides state:
loop :: s -> Automaton (i, s) (o, s) -> Automaton i o
loop state aut = Automaton $ \input ->
    let (newAut, (output, newState)) = runA aut (input, state) in (loop newState newAut, output)

loopF :: s -> (i -> s -> (s, o)) -> Automaton i o
loopF state stateF = Automaton $ \input ->
    let (newState, output) = stateF input state in (loopF newState stateF, output)

-- Doesn't hide state, gives output and state to function which produces output:
loopMapState :: (o -> s -> u) -> s -> Automaton (i, s) (o, s) -> Automaton i u
loopMapState stateMapF state aut = Automaton $ \input ->
    let (newAut, (output, newState)) = runA aut (input, state) in (loopMapState stateMapF newState newAut, stateMapF output newState)

-- Outputs the state
loopStateOut :: s -> Automaton (i, s) s -> Automaton i s
loopStateOut state aut = Automaton $ \input ->
    let (newAut, newState) = runA aut (input, state) in (loopStateOut newState newAut, newState)

(>>>) :: Automaton i inner -> Automaton inner o -> Automaton i o
(>>>) aut1 aut2 = Automaton $ \firstInput ->
    let (aut1', nextInput)   = runA aut1 firstInput
        (aut2', finalResult) = runA aut2 nextInput
    in  (aut1' >>> aut2', finalResult)

(<<<) :: Automaton inner o -> Automaton i inner -> Automaton i o
(<<<) = flip (>>>)

mapInputFirst :: Automaton a b -> Automaton (b, i) o -> Automaton (a, i) o
mapInputFirst aut after = Automaton $ \(a, input) ->
    let (newAut, newInput) = runA aut a
        (newAfter, output) = runA after (newInput, input)
     in (mapInputFirst newAut newAfter, output)

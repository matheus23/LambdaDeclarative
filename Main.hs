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

main :: IO ()
main = runFormBehaviour (0.5, 0.5) $ widgetToBehaviour $ mapState (centeredHV . ($ True)) $ lambdaW exampleLam

runFormBehaviour :: (Double, Double) -> Behaviour GtkEvent Form -> IO ()
runFormBehaviour align behaviour = do
  runFormProgram align behaviour step
    where step input currentBehaviour = do
            let newBehaviour = runEvent currentBehaviour input
            return (newBehaviour, value $ newBehaviour)

data LamExpr
  = Var String
  | Lambda String LamExpr
  | App LamExpr LamExpr

renderVar :: String -> Form
renderVar name = text varStyle name
  where varStyle = defaultTextStyle { fontSize = 10, fontFamily = "monospace" }

renderLambda :: String -> Form -> Form
renderLambda varName inner = append Vec2.right [lambda, gap 5 0, var, gap 5 0, arrow, gap 5 0, inner]
  where
    lambda = text defaultTextStyle { fontSize = 10 } "λ"
    arrow = text defaultTextStyle { fontSize = 10 } "→"
    var = renderVar varName

renderApp :: Form -> Form -> Form
renderApp function argument = append Vec2.right [addParen function, gap 10 0, addParen argument]

addParen :: Form -> Form
addParen form = append Vec2.right [text paren "(", form, text paren ")"]
  where paren = defaultTextStyle { fontSize = 10 }

exampleLam :: LamExpr
exampleLam = App lambdaNot lambdaTrue
  where
    lambdaNot = Lambda "p" $ Lambda "a" $ Lambda "b" $ App (App (Var "p") (Var "b")) $ Var "a"
    lambdaTrue = Lambda "a" $ Lambda "b" $ Var "a"


data Focus = LeftFocus | RightFocus

data Widget i v o = Widget { valueW :: v, runW :: i -> (Widget i v o, o) }

foldW :: s -> (i -> s -> (s, o)) -> Widget i s o
foldW initialState stateF = Widget initialState step
  where step input = let (newState, output) = stateF input initialState
                      in (foldW newState stateF, output)

mapState :: (a -> b) -> Widget i a o -> Widget i b o
mapState f widget = Widget (f $ valueW widget) $ \event -> let (newW, out) = runW widget event in (mapState f newW, out)

addBorder :: LineStyle -> Form -> Form
addBorder ls form = border `atop` form
  where border = outlined ls $ rectangleFromBB $ Border.getBoundingBox $ getBorder form

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f = f
applyIf False f = id

lambdaW :: LamExpr -> Widget GtkEvent (Bool -> Form) (Maybe GtkEvent)
lambdaW (Var name) = Widget (\focused -> applyIf focused (addBorder (solid red)) $ renderVar name) $ \event -> (lambdaW (Var name), Just event)
lambdaW (Lambda var inner) = mapState render $ foldW (False, lambdaW inner) step
  where
    render (editMode, innerW) focused = applyIf (not editMode && focused) (addBorder (solid red)) $ renderLambda var $ valueW innerW (editMode && focused)
    step e (False, innerW)
      | e == KeyPress (Special ArrUp) = ((True, innerW), Nothing)
      | otherwise                     = ((False, innerW), Just e)
    step e (True, innerW) = case runW innerW e of
      (newInnerW, Just (KeyPress (Special Escape))) -> ((False, newInnerW), Nothing)
      (newInnerW, Just e) -> ((True, newInnerW), Just e)
      (newInnerW, Nothing) -> ((True, newInnerW), Nothing)
lambdaW (App func arg) = mapState render $ foldW (False, LeftFocus, lambdaW func, lambdaW arg) step
  where
    render (editMode, LeftFocus, funcW, argW) focused = applyIf (not editMode && focused) (addBorder (solid red)) $ renderApp (valueW funcW (editMode && focused)) (valueW argW False)
    render (editMode, RightFocus, funcW, argW) focused = applyIf (not editMode && focused) (addBorder (solid red)) $ renderApp (valueW funcW False) (valueW argW (editMode && focused))
    step e (False, editMode, funcW, argW)
      | e == KeyPress (Special ArrUp) = ((True, editMode, funcW, argW), Nothing)
      | otherwise                     = ((False, editMode, funcW, argW), Just e)
    step e (True, LeftFocus, funcW, argW) = case runW funcW e of
      (newFuncW, Just (KeyPress (Special Escape))) -> ((False, LeftFocus, newFuncW, argW), Nothing)
      (newFuncW, Just (KeyPress (Special ArrRight))) -> ((True, RightFocus, newFuncW, argW), Nothing)
      (newFuncW, Just e) -> ((True, LeftFocus, newFuncW, argW), Just e)
      (newFuncW, Nothing) -> ((True, LeftFocus, newFuncW, argW), Nothing)
    step e (True, RightFocus, funcW, argW) = case runW argW e of
      (newArgW, Just (KeyPress (Special Escape))) -> ((False, RightFocus, funcW, newArgW), Nothing)
      (newArgW, Just (KeyPress (Special ArrLeft))) -> ((True, LeftFocus, funcW, newArgW), Nothing)
      (newArgW, Just e) -> ((True, RightFocus, funcW, newArgW), Just e)
      (newArgW, Nothing) -> ((True, RightFocus, funcW, newArgW), Nothing)

widgetToBehaviour :: Widget i s o -> Behaviour i s
widgetToBehaviour widget = Behaviour (valueW widget) step
  where step input = widgetToBehaviour $ fst $ runW widget input

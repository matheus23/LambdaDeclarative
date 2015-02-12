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
    lambdaTrue = Lambda "t" $ Lambda "f" $ Var "t"


data Focus = LeftFocus | RightFocus

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
      (newInnerW, Just (KeyPress (Special ArrDown))) -> ((False, newInnerW), Nothing)
      (newInnerW, Just e) -> ((True, newInnerW), Nothing)
      (newInnerW, Nothing) -> ((True, newInnerW), Nothing)
lambdaW (App func arg) = mapState render $ foldW (False, LeftFocus, lambdaW func, lambdaW arg) step
  where
    render (editMode, LeftFocus, funcW, argW) focused = applyIf (not editMode && focused) (addBorder (solid red)) $ renderApp (valueW funcW (editMode && focused)) (valueW argW False)
    render (editMode, RightFocus, funcW, argW) focused = applyIf (not editMode && focused) (addBorder (solid red)) $ renderApp (valueW funcW False) (valueW argW (editMode && focused))
    step e (False, editMode, funcW, argW)
      | e == KeyPress (Special ArrUp) = ((True, editMode, funcW, argW), Nothing)
      | otherwise                     = ((False, editMode, funcW, argW), Just e)
    step e (True, LeftFocus, funcW, argW) = case runW funcW e of
      (newFuncW, Just (KeyPress (Special ArrDown))) -> ((False, LeftFocus, newFuncW, argW), Nothing)
      (newFuncW, Just (KeyPress (Special ArrRight))) -> ((True, RightFocus, newFuncW, argW), Nothing)
      (newFuncW, Just e) -> ((True, LeftFocus, newFuncW, argW), Nothing)
      (newFuncW, Nothing) -> ((True, LeftFocus, newFuncW, argW), Nothing)
    step e (True, RightFocus, funcW, argW) = case runW argW e of
      (newArgW, Just (KeyPress (Special ArrDown))) -> ((False, RightFocus, funcW, newArgW), Nothing)
      (newArgW, Just (KeyPress (Special ArrLeft))) -> ((True, LeftFocus, funcW, newArgW), Nothing)
      (newArgW, Just e) -> ((True, RightFocus, funcW, newArgW), Nothing)
      (newArgW, Nothing) -> ((True, RightFocus, funcW, newArgW), Nothing)

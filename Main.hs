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
--import Data.Maybe (fromJust)
import Control.Automaton
import Widget
import Util
import TextLine
import LambdaCalculus

main :: IO ()
main = runFormBehaviour (0.5, 0.5) $ widgetToBehaviour $ mapState (centeredHV . ($ True)) $ lambdaW exampleLam

{-
data LamExpr
  = Var String
  | Lam String LamExpr
  | App LamExpr LamExpr deriving (Show, Eq)

data LValue
  = Func String LamExpr
  | Number Int deriving (Show, Eq)

type Env
  = [(String, LValue)]-- deriving (Show, Eq)

eval :: Env -> LamExpr -> LValue
eval env (Var str) = case lookup str env of
  Just value -> value
  Nothing -> error $ "Can't find variable: " ++ str ++ ", env: " ++ show env
eval env (Lam str body) = Func str body
eval env (App func arg) = case eval env func of
  Func var body -> eval ((var, eval env arg):env) body
  Number n -> error "Can't apply Function to number!"
-}

renderLambda :: Form -> Form -> Form
renderLambda var inner = append Vec2.right [lambda, gap 5 0, var, gap 5 0, arrow, gap 5 0, inner]
  where
    lambda = text defaultTextStyle { fontSize = 10 } "λ"
    arrow = text defaultTextStyle { fontSize = 10 } "→"

renderApp :: Form -> Form -> Form
renderApp function argument = append Vec2.right [addParen function, gap 10 0, addParen argument]

addParen :: Form -> Form
addParen form = append Vec2.right [text paren "(", form, text paren ")"]
  where paren = defaultTextStyle { fontSize = 10 }

exampleLam :: LamExpr
exampleLam = App lambdaNot lambdaTrue
  where
    lambdaNot = Lam "p" $ Lam "a" $ Lam "b" $ App (App (Var "p") (Var "b")) $ Var "a"
    lambdaTrue = Lam "t" $ Lam "f" $ Var "t"


data Focus = LeftFocus | RightFocus

addBorder :: LineStyle -> Form -> Form
addBorder ls form = border `atop` form
  where border = outlined ls $ rectangleFromBB $ Border.getBoundingBox $ getBorder form

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f = f
applyIf False f = id

lambdaW :: LamExpr -> Widget GtkEvent (Bool -> Form) (Maybe GtkEvent)
lambdaW (Var name) = mapState snd $ textLineFocusable name
lambdaW (Lam var inner) = mapState render $ foldW (False, textLineFocusable var, lambdaW inner) step
  where
    render (editMode, varW, innerW) focused = applyIf (not editMode && focused) (addBorder (solid red)) $ renderLambda ((snd $ valueW varW) (not editMode && focused)) (valueW innerW (editMode && focused))
    step e (False, varW, innerW)
      | e == KeyPress (Special ArrUp) = ((True, varW, innerW), Nothing)
      | otherwise                     = ((False, newVarW, innerW), mayEvent) where (newVarW, mayEvent) = runW varW e
    step e (True, varW, innerW) = case runW innerW e of
      (newInnerW, Just (KeyPress (Special ArrDown))) -> ((False, varW, newInnerW), Nothing)
      (newInnerW, Just e) -> ((True, varW, newInnerW), Nothing)
      (newInnerW, Nothing) -> ((True, varW, newInnerW), Nothing)
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

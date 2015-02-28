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
import ReactBox
import Util
import TextLine
import LambdaCalculus

main :: IO ()
main = runFormBehaviour (0.5, 0.5) $ widgetToBehaviour $ mapState (centeredHoriz . ($ True) . snd) $ lambdaW exampleLam

textStyle :: TextStyle
textStyle = defaultTextStyle { fontSize = 18 }

textStyleM :: TextStyle
textStyleM = textStyle { fontFamily = "monospace" }

renderLambda :: Form -> Form -> Form
renderLambda var inner = append Vec2.right [lambda, gap 5 0, var, gap 5 0, arrow, gap 5 0, inner]
  where
    lambda = text textStyle "λ"
    arrow = text textStyle "→"

renderApp :: Form -> Form -> Form
renderApp function argument = addParen $ append Vec2.right [function, gap 10 0, argument]

addParen :: Form -> Form
addParen form = append Vec2.right [text textStyle "(", form, text textStyle ")"]

exampleLam :: LamExpr
exampleLam = App notLam trueLam

data Focus = LeftFocus | RightFocus

addBorder :: LineStyle -> Form -> Form
addBorder ls form = border `atop` form
  where border = outlined ls $ rectangleFromBB $ Border.getBoundingBox $ getBorder form

addStringBelow :: String -> Form -> Form
addStringBelow stringBelow upper = append Vec2.down
    [ centeredHoriz $ upper
    , padded 5 midline
    , centeredHoriz $ lower ]
  where
    lower = text (font "serif" 8) stringBelow
    midline = filled grey $ rectangle (max (graphicWidth upper) (graphicWidth lower) - 10) 2

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f = f
applyIf False f = id

textLineFoc :: TextStyle -> String -> ReactBox GtkEvent (String, Bool -> Form) (Maybe GtkEvent)
textLineFoc style content = mapState render $ textLine content
  where
    render line = (toString line, renderForm line)
    renderForm (Line left right) True = withCursor (reverse left) right
    renderForm line False = text style $ toString line
    cursor = collapseBorder $ alignVert 0 $ filled black $ rectangle 1.3 $ graphicHeight $ text style "|"
    withCursor leftOfCursor rightOfCursor = append Vec2.right [text style leftOfCursor, cursor, text style rightOfCursor]

lambdaW :: LamExpr -> ReactBox GtkEvent (Env -> (LamExpr, Bool -> Form)) (Maybe GtkEvent)
lambdaW (Var name) = mapState (\(str, form) -> (Var str, form)) $ textLineFoc textStyleM name
lambdaW (Lam var inner) = mapState render $ steppingBox (False, textLineFoc textStyleM var, lambdaW inner) step
  where
    render (innerHasFocus, varW, innerW) env focused = applyIf selfFocus (addStringBelow lam . addBorder (solid red)) $ renderLambda ((snd $ boxValue varW) selfFocus) (snd $ boxValue innerW (innerHasFocus && focused))
      where
        selfFocus = not innerHasFocus && focused
        lam = show $ eval
    step e (False, varW, innerW)
      | e == KeyPress (Special ArrUp) = ((True, varW, innerW), Nothing)
      | otherwise                     = ((False, newVarW, innerW), mayEvent) where (newVarW, mayEvent) = runBox varW e
    step e (True, varW, innerW) = case runBox innerW e of
      (newInnerW, Just (KeyPress (Special ArrDown))) -> ((False, varW, newInnerW), Nothing)
      (newInnerW, Just e) -> ((True, varW, newInnerW), Nothing)
      (newInnerW, Nothing) -> ((True, varW, newInnerW), Nothing)
lambdaW (App func arg) = mapState render $ steppingBox (False, LeftFocus, lambdaW func, lambdaW arg) step
  where
    render (innerHasFocus, LeftFocus, funcW, argW) focused = applyIf selfFocus (addStringBelow lam . addBorder (solid red)) $ renderApp (boxValue funcW (innerHasFocus && focused)) (boxValue argW False)
      where selfFocus = not innerHasFocus && focused
    render (innerHasFocus, RightFocus, funcW, argW) focused = applyIf selfFocus (addStringBelow lam . addBorder (solid red)) $ renderApp (boxValue funcW False) (boxValue argW (innerHasFocus && focused))
      where selfFocus = not innerHasFocus && focused
    step e (False, innerHasFocus, funcW, argW)
      | e == KeyPress (Special ArrUp) = ((True, innerHasFocus, funcW, argW), Nothing)
      | otherwise                     = ((False, innerHasFocus, funcW, argW), Just e)
    step e (True, LeftFocus, funcW, argW) = case runBox funcW e of
      (newFuncW, Just (KeyPress (Special ArrDown))) -> ((False, LeftFocus, newFuncW, argW), Nothing)
      (newFuncW, Just (KeyPress (Special ArrRight))) -> ((True, RightFocus, newFuncW, argW), Nothing)
      (newFuncW, Just e) -> ((True, LeftFocus, newFuncW, argW), Nothing)
      (newFuncW, Nothing) -> ((True, LeftFocus, newFuncW, argW), Nothing)
    step e (True, RightFocus, funcW, argW) = case runBox argW e of
      (newArgW, Just (KeyPress (Special ArrDown))) -> ((False, RightFocus, funcW, newArgW), Nothing)
      (newArgW, Just (KeyPress (Special ArrLeft))) -> ((True, LeftFocus, funcW, newArgW), Nothing)
      (newArgW, Just e) -> ((True, RightFocus, funcW, newArgW), Nothing)
      (newArgW, Nothing) -> ((True, RightFocus, funcW, newArgW), Nothing)

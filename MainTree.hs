module Main where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import Graphics.Declarative.Tracked
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Gtk.KeyboardInput

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import Data.Maybe (fromJust)
import FRP.Behaviour
import Control.Automaton
import ReactBox
import Util

main :: IO ()
main = runFormBehaviour (0.5, 0.5) $ mapValue centeredHV $ widgetToBehaviour $ mapState ($ True) $ treeW exampleTree

data Tree a = Node a (Tree a) (Tree a) | Empty

data TreeStructure = SuperNode | SubLeft | SubRight deriving (Show, Eq, Ord)

exampleTree :: Tree Int
exampleTree =
  Node 5
    (Node 10 Empty Empty)
    (Node 4
      (Node 1 Empty Empty)
      (Node 2
        Empty
        (Node 3 Empty Empty)))

treeNode :: Form -> Form -> Form -> Form
treeNode node leftTree rightTree = removeTracking trackedTree `atop` conLines
  where
    subtrees = centeredHV $ append Vec2.right [addOriginTracking SubLeft leftTree, addOriginTracking SubRight rightTree]
    trackedTree = append Vec2.down [addOriginTracking SuperNode node, subtrees]
    superPos = fromJust $ getTrackedPos SuperNode trackedTree
    leftPos = fromJust $ getTrackedPos SubLeft trackedTree
    rightPos = fromJust $ getTrackedPos SubRight trackedTree
    blackLine start end = outlined (solid black) $ noBorder $ openPath $ pathPoint start `lineConnect` pathPoint end
    conLines = blackLine superPos leftPos `atop` blackLine superPos rightPos

drawNode :: TreeStructure -> RGB -> Int -> Form
drawNode ts col n = append Vec2.down [centeredHV $ text style $ show n, centeredHV $ text small $ show ts] `atop` (padded 10 $ filled col $ circle 25)
  where
    style = defaultTextStyle { fontSize = 16, fontFamily = "monospace" }
    small = defaultTextStyle { fontSize = 8, fontFamily = "monospace" }

treeEmpty :: Bool -> Form
treeEmpty True = padded 10 $ outlined (solid red) (circle 25) `atop` filled white (circle 25)
treeEmpty False = padded 10 $ outlined (solid black) (circle 25) `atop` filled white (circle 25)

type FocusWidget = ReactBox GtkEvent (Bool -> Form) (Maybe GtkEvent)

treeW :: Tree Int -> FocusWidget
treeW Empty = ReactBox treeEmpty step
  where
    step (KeyPress (Letter 'n')) = (treeW $ Node 0 Empty Empty, Nothing)
    step event = (treeW Empty, Just event)
treeW (Node num leftTree rightTree) = appWidget (SuperNode, treeW leftTree, treeW rightTree)
  where
    render (SuperNode, leftW, rightW) focused = treeNode (drawNode SuperNode (if focused then lightBlue else grey) num) (boxValue leftW False) (boxValue rightW False)
    render (SubLeft, leftW, rightW) focused = treeNode (drawNode SubLeft grey num) (boxValue leftW focused) (boxValue rightW False)
    render (SubRight, leftW, rightW) focused = treeNode (drawNode SubRight grey num) (boxValue leftW False) (boxValue rightW focused)
    appWidget :: (TreeStructure, FocusWidget, FocusWidget) -> FocusWidget
    appWidget state = ReactBox (render state) $ step state
      where
        step (SuperNode, leftW, rightW) e
          | e == KeyPress (Special ArrLeft)   = (appWidget (SubLeft, leftW, rightW), Nothing)
          | e == KeyPress (Special ArrRight)  = (appWidget (SubRight, leftW, rightW), Nothing)
          | e == KeyPress (Special Backspace) = (treeW Empty, Nothing)
          | otherwise = (appWidget (SuperNode, leftW, rightW), Just e)
        step (SubLeft, leftW, rightW) e = case runBox leftW e of
          (newLeftW, Just (KeyPress (Special ArrUp))) -> (appWidget (SuperNode, newLeftW, rightW), Nothing)
          (newLeftW, mayEvent) -> (appWidget (SubLeft, newLeftW, rightW), mayEvent)
        step (SubRight, leftW, rightW) e = case runBox rightW e of
          (newRightW, Just (KeyPress (Special ArrUp))) -> (appWidget (SuperNode, leftW, newRightW), Nothing)
          (newRightW, mayEvent) -> (appWidget (SubRight, leftW, newRightW), mayEvent)

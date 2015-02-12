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
import Widget

main :: IO ()
--main = showFormWindow (0.5, 0.5) $ filled red $ circle 100
main = runFormBehaviour (0.5, 0.5) $ mapValue centeredHV $ widgetToBehaviour $ mapState ($ True) $ treeW exampleTree

runFormBehaviour :: (Double, Double) -> Behaviour GtkEvent Form -> IO ()
runFormBehaviour align behaviour = do
  runFormProgram align behaviour step
    where step input currentBehaviour = do
            let newBehaviour = runEvent currentBehaviour input
            return (newBehaviour, value $ newBehaviour)

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
treeEmpty True = padded 10 $ outlined (solid red) $ circle 25
treeEmpty False = padded 10 $ outlined (solid black) $ circle 25

treeW :: Tree Int -> Widget GtkEvent (Bool -> Form) (Maybe GtkEvent)
treeW Empty = Widget treeEmpty step
  where
    step (KeyPress (Letter 'n')) = (treeW $ Node 0 Empty Empty, Nothing)
    step event = (treeW Empty, Just event)
treeW (Node num leftTree rightTree) = mapState render $ foldW (SuperNode, treeW leftTree, treeW rightTree) step
  where
    render (SuperNode, leftW, rightW) focused = treeNode (drawNode SuperNode (if focused then lightBlue else grey) num) (valueW leftW False) (valueW rightW False)
    render (SubLeft, leftW, rightW) focused = treeNode (drawNode SubLeft grey num) (valueW leftW focused) (valueW rightW False)
    render (SubRight, leftW, rightW) focused = treeNode (drawNode SubRight grey num) (valueW leftW False) (valueW rightW focused)
    step e (SuperNode, leftW, rightW)
      | e == KeyPress (Special ArrLeft)  = ((SubLeft, leftW, rightW), Nothing)
      | e == KeyPress (Special ArrRight) = ((SubRight, leftW, rightW), Nothing)
      | otherwise = ((SuperNode, leftW, rightW), Just e)
    step e (SubLeft, leftW, rightW) = case runW leftW e of
      (newLeftW, Just (KeyPress (Special ArrUp))) -> ((SuperNode, newLeftW, rightW), Nothing)
      (newLeftW, mayEvent) -> ((SubLeft, newLeftW, rightW), mayEvent)
    step e (SubRight, leftW, rightW) = case runW rightW e of
      (newRightW, Just (KeyPress (Special ArrUp))) -> ((SuperNode, leftW, newRightW), Nothing)
      (newRightW, mayEvent) -> ((SubRight, leftW, newRightW), mayEvent)

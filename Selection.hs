module Selection where

import Data.Maybe (maybe)

data Selection a = Selection { leftOf :: [a], selection :: [a], rightOf :: [a] } deriving (Eq, Show)

toString :: Selection Char -> String
toString (Selection left sel right) = reverse left ++ sel ++ right

toStringWithCursor :: Selection Char -> String
toStringWithCursor (Selection left [] right) = reverse left ++ "|" ++ right
toStringWithCursor (Selection left sel right) = reverse left ++ "[" ++ sel ++ "]" ++ right

fromListCursorLeft :: [a] -> Selection a
fromListCursorLeft ls = Selection [] [] ls

fromListCursorRight :: [a] -> Selection a
fromListCursorRight ls = Selection (reverse ls) [] []

unMay :: (a -> Maybe a) -> a -> a
unMay f a = maybe a id (f a)

cursorLeftMay :: Selection a -> Maybe (Selection a)
cursorLeftMay (Selection [] [] _)              = Nothing
cursorLeftMay (Selection (x:left) [] right)    = Just $ Selection left [] (x:right)
cursorLeftMay (Selection left selection right) = Just $ Selection left [] (selection ++ right)

cursorRightMay :: Selection a -> Maybe (Selection a)
cursorRightMay (Selection _ [] [])              = Nothing
cursorRightMay (Selection left [] (x:right))    = Just $ Selection (x:left) [] right
cursorRightMay (Selection left selection right) = Just $ Selection (reverse selection ++ left) [] right

selectLeftMay :: Selection a -> Maybe (Selection a)
selectLeftMay (Selection [] _ _)             = Nothing
selectLeftMay (Selection (x:left) sel right) = Just $ Selection left (x:sel) right

selectRightMay :: Selection a -> Maybe (Selection a)
selectRightMay (Selection _ _ [])             = Nothing
selectRightMay (Selection left sel (x:right)) = Just $ Selection left (sel ++ [x]) right

jumpLeftmostMay :: Selection a -> Maybe (Selection a)
jumpLeftmostMay (Selection [] [] _)              = Nothing
jumpLeftmostMay (Selection left selection right) = Just $ Selection [] [] (reverse left ++ selection ++ right)

jumpRightmostMay :: Selection a -> Maybe (Selection a)
jumpRightmostMay (Selection _ [] [])              = Nothing
jumpRightmostMay (Selection left selection right) = Just $ Selection (reverse (selection ++ right) ++ left) [] []

removeLeftMay :: Selection a -> Maybe ([a], Selection a)
removeLeftMay (Selection [] [] _)              = Nothing
removeLeftMay (Selection (x:left) [] right)    = Just ([x], Selection left [] right)
removeLeftMay (Selection left sel right)       = Just (sel, Selection left [] right)

removeRightMay :: Selection a -> Maybe ([a], Selection a)
removeRightMay (Selection _ [] [])              = Nothing
removeRightMay (Selection left [] (x:right))    = Just ([x], Selection left [] right)
removeRightMay (Selection left sel right)       = Just (sel, Selection left [] right)

removeLeft :: Selection a -> Selection a
removeLeft = unMay $ fmap snd . removeLeftMay

removeRight :: Selection a -> Selection a
removeRight = unMay $ fmap snd . removeRightMay

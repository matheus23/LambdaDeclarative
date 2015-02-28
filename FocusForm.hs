module FocusForm where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Cairo.Form

data FocusForm = FocusForm { focused :: Form, unfocused :: Form }

instance Physical2D FocusForm where
  move = mapBothForms . move
  rotate = mapBothForms . rotate
  scale = mapBothForms . scale
  atop = combineFocusForms atop
  empty = FocusForm empty empty

combineFocusForms :: (Form -> Form -> Form) -> FocusForm -> FocusForm -> FocusForm
combineFocusForms f (FocusForm f1 u1) (FocusForm f2 u2) = FocusForm (f f1 f2) (f u1 u2)

mapBothForms :: (Form -> Form) -> FocusForm -> FocusForm
mapBothForms f (FocusForm foc unfoc) = FocusForm (f foc) (f unfoc)

mapFocusedForm :: (Form -> Form) -> FocusForm -> FocusForm
mapFocusedForm f (FocusForm foc unfoc) = FocusForm (f foc) unfoc

mapUnfocusedForm :: (Form -> Form) -> FocusForm -> FocusForm
mapUnfocusedForm f (FocusForm foc unfoc) = FocusForm foc (f unfoc)

fromFocus :: Bool -> FocusForm -> FocusForm
fromFocus True  form = FocusForm { focused = focused form,   unfocused = unfocused form }
fromFocus False form = FocusForm { focused = unfocused form, unfocused = unfocused form }

applyFocusRenderer :: (Form -> Form) -> Bool -> FocusForm -> FocusForm
applyFocusRenderer func hasFocus form = fromFocus hasFocus (mapFocusedForm func form)

getIsFocused :: Bool -> FocusForm -> Form
getIsFocused True (FocusForm f _) = f
getIsFocused False (FocusForm _ u) = u

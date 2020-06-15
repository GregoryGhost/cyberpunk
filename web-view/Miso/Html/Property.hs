{-# LANGUAGE OverloadedStrings #-}

module Miso.Html.Property where

import Miso
import Miso.String

role_ :: MisoString -> Attribute action
role_ = textProp "role"

tabIndex_ :: Int -> Attribute action
tabIndex_ = intProp "tabindex"

ariaSelected_ :: Bool -> Attribute action
ariaSelected_ = boolProp "aria-selected"

ariaHidden_ :: Bool -> Attribute action
ariaHidden_ = boolProp "aria-hidden"
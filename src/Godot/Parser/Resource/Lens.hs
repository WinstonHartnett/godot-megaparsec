{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Godot.Parser.Resource.Lens where

import           Control.Lens

import           Godot.Parser.Resource

makeLenses ''GodotValue

makeFields ''GodotSection

makeFields ''TscnDescriptor

makeFields ''TscnParsed

makeFields ''GdnsDescriptor

makeFields ''GdnsParsed

makeFields ''GodotParsed

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Godot.Parser.Resource.Lens where

import           Control.Lens

import           Godot.Parser.Resource

makeLenses ''GodotValue

makeLenses ''GodotSection

makeLenses ''TscnDescriptor

makeLenses ''TscnParsed

makeLenses ''GdnsDescriptor

makeLenses ''GdnsParsed

makeLenses ''OtherDescriptor

makeLenses ''OtherParsed

makeLenses ''GodotParsed

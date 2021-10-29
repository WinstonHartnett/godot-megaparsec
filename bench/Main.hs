{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad         (unless)

import           Criterion.Main

import           Data.Either           (fromRight, isRight)
import qualified Data.Text.IO          as T

import           Godot.Parser.Resource

import qualified Text.Megaparsec       as P

main :: IO ()
main = do
  let parseFile f = fromRight undefined . P.parse tscnParser "" <$> T.readFile f
  defaultMain [ bench "EntityMap" $ whnfIO (parseFile "bench/EntityMap.tscn")
              , bench "Geography" $ whnfIO (parseFile "bench/Geography.tscn")
              , bench "BigFile" $ whnfIO (parseFile "bench/BigFile.tscn")]
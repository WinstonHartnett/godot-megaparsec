{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.Text.IO as T
import Godot.Parser.Resource
import qualified Text.Megaparsec as P

main :: IO ()
main = do
  let parseFile f = do
        txt <- T.readFile f
        case P.parse parsedP "" txt of
          Right p -> pure p
          Left e -> putStrLn (P.errorBundlePretty e) >> undefined
  defaultMain
    [ bench "EntityMap" $ whnfIO (parseFile "bench/EntityMap.tscn")
    , bench "Geography" $ whnfIO (parseFile "bench/Geography.tscn")
    , bench "MarketActor" $ whnfIO (parseFile "bench/MarketActor.tscn")
    , bench "Other" $ whnfIO (parseFile "bench/Other.other")
    , bench "BigFile" $ whnfIO (parseFile "bench/BigFile.tscn")
    ]

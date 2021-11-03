{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main

import           Data.Either           (fromRight)
import qualified Data.Text.IO          as T

import           Godot.Parser.Resource

import qualified Text.Megaparsec       as P

main :: IO ()
main = do
  let parseFile f =
        fromRight (error $ "Unable to parse " <> f) . P.parse godotParser ""
        <$> T.readFile f
  defaultMain [ bench "EntityMap" $ whnfIO (parseFile "bench/EntityMap.tscn")
              , bench "Geography" $ whnfIO (parseFile "bench/Geography.tscn")
              , bench "MarketActor" $ whnfIO (parseFile "bench/MarketActor.tscn")
              , bench "Other" $ whnfIO (parseFile "bench/Other.other")
              , bench "BigFile" $ whnfIO (parseFile "bench/BigFile.tscn")]

module Main where

import System.IO
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as S
import Debug.Trace

redistribute :: S.Seq Int -> Int -> Int -> S.Seq Int
redistribute seq index 0 = seq
redistribute seq index rem | index == (length seq) = redistribute seq 0 rem
redistribute seq index rem = let 
    el = S.index seq index
    seq' = S.update index (el + 1) seq
    in redistribute seq' (index + 1) (rem - 1)

detect :: S.Seq Int -> Map.Map (S.Seq Int) Int -> Int 
detect seq counts = let
  maxEl = maximum seq
  maxIndex = length $ S.takeWhileL (< maxEl) seq
  seq' = S.update maxIndex 0 seq
  result = redistribute seq' (maxIndex + 1) maxEl
  in case Map.member result counts of
    True -> 1 + Map.size counts
    False -> traceShow result $ detect result $ Map.insert result 1 counts

solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  let list = S.fromList $ fmap (\x -> read x :: Int) $ concat $ fmap words $ lines input
  print list
  print $ detect list Map.empty

main :: IO ()
-- main = solve "006_test.txt"
main = solve "006_1_input.txt"


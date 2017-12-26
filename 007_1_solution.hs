module Main where

import System.IO
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as S
import Debug.Trace

parseLine :: String -> (String, (Int, Maybe [String]))
parseLine input = let
    (key, _:i') = break isSpace input
    (_:i'') = i'    
    num = takeWhile (/= ')') i''

    -- add the pointers to other words if exist
    = case break (== '>') input of
      
    (_, _:)
  in


solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  let list = S.fromList $ fmap (\x -> read x :: Int) $ concat $ fmap words $ lines input
  print list
  print $ detect list Map.empty

main :: IO ()
main = solve "007_test.txt"
-- main = solve "006_1_input.txt"


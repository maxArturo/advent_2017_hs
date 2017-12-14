module Main where

import System.IO
import Data.List
import qualified Data.Map as Map

isDistinct :: [String] -> Bool
isDistinct p = length fullMap == length p
  where
  fullMap = foldl (\x y -> Map.insert y 1 x) Map.empty p 

solve :: String -> IO ()
solve fileName = do
    input <- readFile fileName 
    print $ length $ filter (==True) $ fmap isDistinct $ fmap (\x -> fmap sort x) $ fmap words $ lines input

main :: IO ()
main = solve "004_input.txt"


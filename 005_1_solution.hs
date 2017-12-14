module Main where

import System.IO
import Data.List

countSteps :: Int -> Int -> [Int] -> Int
countSteps n index l | index >= length l || index < 0 = n
countSteps n index l = countSteps n' index' l'
  where
    (pre, el:post) = splitAt index l
    index' = l!!index + index
    l' = pre ++ (el + 1) : post
    n' = n + 1


solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  print $ countSteps 0 0 $ fmap (\x -> read x :: Int) $ lines input

main :: IO ()
main = solve "005_1_input.txt"


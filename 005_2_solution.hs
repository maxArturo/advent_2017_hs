module Main where

import System.IO
import Data.List
import qualified Data.Sequence as S

countSteps :: Int -> Int -> S.Seq Int -> Int
countSteps n index l | index >= length l || index < 0 = n
countSteps n index l = countSteps n' index' l'
  where
    el = S.index l index
    index' = el + index
    el' = if el >= 3 then el - 1 else el + 1
    l' = S.update index el' l 
    n' = n + 1

solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  print $ countSteps 0 0 $ S.fromList $ fmap (\x -> read x :: Int) $ lines input

main :: IO ()
main = solve "005_1_input.txt"


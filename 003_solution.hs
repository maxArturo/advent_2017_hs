module Main where

import System.IO
import Data.String
import Data.Char
-- import Debug.Trace

delta :: Int -> Int -> Int -> Int
delta num base curr 
  | num > curr + base = delta num base (curr + base)
delta num base curr = num - (curr + (div base 2))

solve :: Int -> Int
solve input = let
    init_base = floor . sqrt . fromIntegral $ input
    base = case init_base `rem` 2 of 
      0 -> init_base - 1
      _ -> init_base
  in 
     (div base 2) + 1 + (delta input (base + 1) (base ^ 2))

main :: IO ()
main = do 
  print $ solve 289326


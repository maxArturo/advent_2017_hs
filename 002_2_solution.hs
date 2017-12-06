module Main where

import System.IO
import Data.String
import Data.List
-- import Debug.Trace

findDiv :: [Integer] -> Integer -> Integer
findDiv _ a | a /= 0 = a
findDiv [] _ = 0
findDiv (x:xs) _ = case find (\n -> x `rem` n == 0) xs of
  Nothing -> findDiv xs 0
  Just a  -> findDiv [] $ div x a
  
getDiff :: [String] -> Integer
getDiff l = findDiv (reverse $ sort digits) 0
  where
    stringToInt x = read x :: Integer
    digits = fmap stringToInt l 

solve :: String -> IO ()
solve fileName = do
    input <- readFile fileName 
    let rows = filter (\x -> x /= []) (fmap words $ lines input)
    print $ foldl (\x y ->  x + getDiff y) 0 rows 

main :: IO ()
main = solve "002_input.txt"


module Main where

import System.IO
import Data.String
import Data.Char

getDiff :: [String] -> Int
getDiff l = max - min
  where
    stringToInt (x:_) =  digitToInt x
    digits = fmap stringToInt l 
    max = maximum digits
    min = minimum digits

solve :: String -> IO ()
solve fileName = do
    input <- readFile fileName 
    let rows = filter (\x -> x /= []) (fmap words $ lines input)
    print $ foldl (\x y -> x + getDiff y) 0 rows 

main :: IO ()
main = solve "002_input.txt"


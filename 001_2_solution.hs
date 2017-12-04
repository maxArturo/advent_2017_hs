module Main where
import System.IO
-- import Debug.Trace
import Data.Char
import Data.List

trim = unwords . words

isMatching :: String -> Int -> Bool
isMatching (x:xs) len | x == xs!!(len - 1) = True
isMatching _ _ = False 

findMatching :: String -> [Int]
findMatching str = fmap digitToInt (findMatching' (concat [str, str]) "" (div (length str) 2))

findMatching' :: String -> String -> Int -> String
findMatching' "" ans _ = ans
findMatching' (x:xs) ans len | length (x:xs) == len * 2 = findMatching' "" ans len
findMatching' (x:xs) ans len | isMatching (x:xs) len = findMatching' xs (x:ans) len
findMatching' (x:xs) ans len = findMatching' xs ans len

solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  let output = findMatching $ trim input
  print $ foldl (\x y -> x + y) 0 output

main :: IO ()
main = solve "001_input.txt"


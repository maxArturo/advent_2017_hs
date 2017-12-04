module Main where
import System.IO
-- import Debug.Trace
import Data.Char

trim = unwords . words

isMatching :: String -> Bool
isMatching (x:y:_) | x == y = True
isMatching _ = False 

findMatching :: String -> [Int]
findMatching str = fmap digitToInt (findMatching' (last str:str) "")

findMatching' :: String -> String -> String
findMatching' "" ans = ans
findMatching' (x:xs) ans | isMatching (x:xs) = findMatching' xs (x:ans)
findMatching' (x:xs) ans = findMatching' xs ans

solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  let output = findMatching $ trim input
  print $ foldl (\x y -> x + y) 0 output

main :: IO ()
main = solve "001_input.txt"


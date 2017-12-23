module Main where

import System.IO
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as S

redistribute :: S.Seq Int -> Int -> Int -> S.Seq Int
redistribute seq index rem | rem == 0 = seq
redistribute seq index rem = case length seq of 
  index -> redistribute seq 0 rem
  otherwise          -> let 
    el = S.index seq index
    seq' = S.update index (el + 1) seq
    in redistribute seq' (index + 1) (rem - 1)

solve :: String -> IO ()
solve fileName = do
  input <- readFile fileName 
  let list = S.fromList $ fmap (\x -> read x :: Int) $ concat $ fmap words $ lines input
  print list
  print $ redistribute (S.fromList [0, 2, 0, 0]) 3 7

main :: IO ()
main = solve "006_test.txt"


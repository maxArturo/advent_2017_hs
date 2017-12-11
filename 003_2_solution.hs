module Main where
import qualified Data.Map as Map
import Data.Maybe
import Data.List
-- import System.IO
-- import Data.String
-- import Data.Map
-- import Debug.Trace

up    (a, b) = (a, b + 1)
down  (a, b) = (a, b - 1)
left  (a, b) = (a - 1, b)
right (a, b) = (a + 1, b)
directions = [right, up, left, down]

day3Gen = scanl (\c f -> f c) (0,0) $ concat $ zipWith replicate steps (cycle directions)
    where
        steps = [1..] >>= replicate 2

getValue :: Num a => (Integer, Integer) -> Map.Map (Integer, Integer) a -> a
getValue position table = sum $ mapMaybe (\f -> Map.lookup (f position) table) dir
    where 
        dir = directions ++ [\(a,b) -> (a + 1, b + 1), \(a,b) -> (a - 1, b + 1), \(a,b) -> (a + 1, b - 1), \(a,b) -> (a - 1, b - 1)]


setValue table coord = 
    let x = getValue coord table
    in (Map.insert coord x table, x)

day3Part2 = find (> 289326) $ snd $ mapAccumL setValue (Map.singleton (0,0) 1) $ drop 1 day3Gen

main :: IO ()
main = do 
  print $ day3Gen !! ( 289326 - 1)
  print $ take 10 day3Gen
  print $ day3Part2



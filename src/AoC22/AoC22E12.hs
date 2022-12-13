module AoC22E12 (hillClimbingAlgorithm) where

import Data.Char (ord)
import Data.List (delete, elemIndex, find, minimumBy)
import Data.Maybe (fromJust, isJust, listToMaybe, maybeToList)
import Helpers (printHeader, printResult, readData)

type Key = (Int, Int)

data Node = Node
  { key :: Key,
    label :: Char,
    height :: Int,
    prev :: Maybe Key,
    dist :: Int,
    done :: Bool
  }
  deriving (Show)

type Map = [[Node]]

hillClimbingAlgorithm :: IO ()
hillClimbingAlgorithm = do
  sampleContent <- readData "data-s22e12-sample.txt"
  content <- readData "data-s22e12.txt"

  let sampleMap = parseInput sampleContent
  let theMap = parseInput content

  printHeader "2022 Day 12: Hill Climbing Algorithm"

  printResult "Shortest sample path from start to top" 31 $ shortestPathToTop sampleMap
  printResult "Shortest path from start to top" 490 $ shortestPathToTop theMap

  printResult "Shortest sample path from bottom to top" 29 $ shortestPathDown sampleMap
  printResult "Shortest path from bottom to top" 488 $ shortestPathDown theMap

shortestPathToTop :: Map -> Int
shortestPathToTop m =
  let result = shortestPathUp $ setDist m (fromJust (findLabel 'S' m)) 0
      end = findLabel 'E' result
      node = getNode result $ fromJust end
   in dist node

shortestPathDown :: Map -> Int
shortestPathDown m =
  let result = shortestPathBack $ setDist m (fromJust (findLabel 'E' m)) 0
      as = filter (\n -> 'a' == label n) $ concat result
      dists = map dist as
   in minimum dists

getNode :: Map -> Key -> Node
getNode m (r, c) = m !! r !! c

findLabel :: Char -> Map -> Maybe Key
findLabel l m =
  let mn = find (\n -> label n == l) $ concat m
   in listToMaybe $ map key $ maybeToList mn

setPrev :: Map -> Key -> Key -> Map
setPrev m k k' = updateNode m k (\n -> n {prev = Just k'})

setDist :: Map -> Key -> Int -> Map
setDist m k d = updateNode m k (\n -> n {dist = d})

setDone :: Map -> Key -> Map
setDone m k = updateNode m k (\n -> n {done = True})

updateNode :: Map -> Key -> (Node -> Node) -> Map
updateNode m k@(kr, kc) f = zipWith processRow [0 ..] m
  where
    processRow row mr
      | kr == row = zipWith processNode [0 ..] mr
      | otherwise = mr

    processNode col n
      | col == kc = f n
      | otherwise = n

parseInput :: String -> Map
parseInput = zipWith processLine [0 ..] . lines
  where
    processLine row = zipWith (processCoord row) [0 ..]

    processCoord row col c =
      Node
        { key = (row, col),
          label = c,
          height = charToHeight c,
          prev = Nothing,
          dist = (maxBound :: Int) - 1, -- XXX: Here be dragons
          done = False
        }

    charToHeight c = case c of
      'S' -> 0
      'E' -> ord 'z' - ord 'a'
      c -> ord c - ord 'a'

shortestPathUp :: Map -> Map
shortestPathUp = dijkstra (compareHeights (\uh vh -> uh + 1 >= vh))

shortestPathBack :: Map -> Map
shortestPathBack = dijkstra (compareHeights (\uh vh -> vh + 1 >= uh))

compareHeights f m u v = f (height $ getNode m u) (height $ getNode m v)

dijkstra :: (Map -> Key -> Key -> Bool) -> Map -> Map
dijkstra isReachable m0 = shortestPath' m0 $ map key $ concat m0
  where
    shortestPath' :: Map -> [Key] -> Map
    shortestPath' m1 [] = m1
    shortestPath' m1 q =
      -- XXX: Use heap
      let u = minimumBy (\a b -> compare (dist $ getNode m1 a) (dist $ getNode m1 b)) q
          us = delete u q
       in shortestPath' (process u) us
      where
        process u = setDone (processDestinations m1 u $ validDestinations u) u

        validDestinations :: Key -> [Key]
        validDestinations u@(r, c) =
          filter isValid [(r -1, c), (r + 1, c), (r, c -1), (r, c + 1)]
          where
            isValid v = isInRange v && not (isDone v) && isReachable m1 u v
            isDone v = done $ getNode m1 v
            isInRange (r, c) = 0 <= r && r < length m1 && 0 <= c && c < length (head m1)

processDestinations m _ [] = m
processDestinations m u (v : vs) =
  let dist' = 1 + dist (getNode m u)
      m' = if dist' < dist (getNode m v) then acceptPath dist' else m
   in processDestinations m' u vs
  where
    acceptPath d = setDist (setPrev m v u) v d

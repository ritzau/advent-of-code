{-# LANGUAGE LambdaCase #-}

module Common
  ( solvePart1,
    solvePart2,
    Direction (..),
    Instruction (..),
    parseInput,
  )
where

import qualified Data.Set as Set
import Data.List (foldl')
import Data.List.Split (splitOn)

-- | Cardinal directions
data Direction = North | East | South | West
  deriving (Show, Eq)

-- | Turn 90 degrees clockwise
turnRight :: Direction -> Direction
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

-- | Turn 90 degrees counter-clockwise
turnLeft :: Direction -> Direction
turnLeft = \case
  North -> West
  West -> South
  South -> East
  East -> North

-- | Get the (dx, dy) movement vector for this direction
delta :: Direction -> (Int, Int)
delta = \case
  North -> (0, 1)
  East -> (1, 0)
  South -> (0, -1)
  West -> (-1, 0)

-- | A single movement instruction
data Instruction = Instruction
  { turn :: Char, -- 'L' or 'R'
    blocks :: Int
  }
  deriving (Show, Eq)

-- | Parse the input into a list of instructions
parseInput :: String -> [Instruction]
parseInput input =
  map parseInstruction $ splitOn ", " (trim input)
  where
    parseInstruction (t : rest) = Instruction t (read rest)
    parseInstruction [] = error "Empty instruction"
    trim = reverse . dropWhile (`elem` " \n\r\t") . reverse . dropWhile (`elem` " \n\r\t")

-- | Solve part 1: Find Manhattan distance to final position
solvePart1 :: String -> Int
solvePart1 input =
  let instructions = parseInput input
      (x, y, _) = foldl' step (0, 0, North) instructions
   in abs x + abs y
  where
    step (x, y, dir) instr =
      let newDir = case turn instr of
            'R' -> turnRight dir
            'L' -> turnLeft dir
            _ -> error "Invalid turn"
          (dx, dy) = delta newDir
       in (x + dx * blocks instr, y + dy * blocks instr, newDir)

-- | Solve part 2: Find Manhattan distance to first location visited twice
solvePart2 :: String -> Int
solvePart2 input =
  let instructions = parseInput input
      result = go instructions (0, 0) North (Set.singleton (0, 0))
   in case result of
        Just (x, y) -> abs x + abs y
        Nothing -> 0
  where
    go [] _ _ _ = Nothing
    go (instr : rest) (x, y) dir visited =
      let newDir = case turn instr of
            'R' -> turnRight dir
            'L' -> turnLeft dir
            _ -> error "Invalid turn"
          (dx, dy) = delta newDir
          positions = [(x + dx * i, y + dy * i) | i <- [1 .. blocks instr]]
       in case findFirst positions visited of
            Just pos -> Just pos
            Nothing ->
              let newVisited = foldl' (flip Set.insert) visited positions
                  (newX, newY) = last positions
               in go rest (newX, newY) newDir newVisited

    findFirst [] _ = Nothing
    findFirst (pos : rest) visited =
      if Set.member pos visited
        then Just pos
        else findFirst rest (Set.insert pos visited)

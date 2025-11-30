module AoC22E14 (simulateRegolithSand) where

import Data.List (replicate)
import Data.Maybe (fromJust, isNothing)
import Debug.Trace (traceShowId)
import Helpers (printHeader, printResult, readData, split)

simulateRegolithSand = do
  printHeader "2022 Day 14: Regolith Reservoir"

  sampleContent <- readData "data-s22e14-sample.txt"
  content <- readData "data-s22e14.txt"

  let sampleScan = parseRegolithReservoir sampleContent
  let scan = parseRegolithReservoir content

  printResult "Sample units before falling out" 24 $
    countUntilFallingOut sampleScan

  -- printResult "Units before falling out" 696 $
  --   countUntilFallingOut scan

  printResult "Sample count until filled" 93 $
    countUntilFilled sampleScan

  -- printResult "Count until filled" 23610 $
  --   countUntilFilled scan

countUntilFallingOut :: Scan -> Int
countUntilFallingOut = simulateUntilFallingOut

simulateUntilFallingOut :: Scan -> Int
simulateUntilFallingOut s0 = bar' $ simulateUnit (s0, Falling (500, 0))
  where
    bar' (s, Falling p) = undefined
    bar' (s, Resting p) = 1 + simulateUntilFallingOut s
    bar' (s, Outside p) = 0

countUntilFilled :: Scan -> Int
countUntilFilled = simulateUntilFilled 0

simulateUntilFilled :: Int -> Scan -> Int
simulateUntilFilled n s0 =
  let (s, p) = simulateUnit2 (s0, Falling (500, 0))
   in if p == Resting (500, 0)
        then n + 1
        else simulateUntilFilled (n + 1) s

data Material = Air | Rock | Sand
  deriving (Eq, Show)

data Scan = Scan
  { minX :: Int,
    maxY :: Int,
    slice :: [[Material]]
  }
  deriving (Show)

type Position = (Int, Int)

data SandPosition = Falling Position | Resting Position | Outside Position
  deriving (Eq, Show)

simulateUnit :: (Scan, SandPosition) -> (Scan, SandPosition)
simulateUnit sp@(s, p) = case p of
  Falling _ -> simulateUnit $ stepSandUnit sp
  _ -> sp

stepSandUnit :: (Scan, SandPosition) -> (Scan, SandPosition)
stepSandUnit (s, Falling p@(px, py)) =
  let pDown = (px, py + 1)
      pLeft = (px -1, py + 1)
      pRight = (px + 1, py + 1)
      mDown = getMaterial s pDown
      mLeft = getMaterial s pLeft
      mRight = getMaterial s pRight
   in if isNothing mDown
        then moveOut
        else
          if fromJust mDown == Air
            then moveTo pDown
            else
              if isNothing mLeft
                then moveOut
                else
                  if fromJust mLeft == Air
                    then moveTo pLeft
                    else
                      if isNothing mRight
                        then moveOut
                        else
                          if fromJust mRight == Air
                            then moveTo pRight
                            else place p
  where
    moveOut = (setMaterial s Air p, Outside p)
    moveTo to = (s, Falling to)
    place at = (setMaterial s Sand at, Resting at)

stepSandUnit (_, _) = undefined

simulateUnit2 :: (Scan, SandPosition) -> (Scan, SandPosition)
simulateUnit2 sp@(s, p) = case p of
  Falling _ -> simulateUnit2 $ stepSandUnit2 sp
  _ -> sp

stepSandUnit2 (s, Falling p@(px, py)) =
  let pDown = (px, py + 1)
      pLeft = (px -1, py + 1)
      pRight = (px + 1, py + 1)
      mDown = getMaterial s pDown
      mLeft = getMaterial s pLeft
      mRight = getMaterial s pRight
   in if py + 1 == maxY s + 2
        then place p
        else
          if fromJust mDown == Air
            then moveTo pDown
            else
              if fromJust mLeft == Air
                then moveTo pLeft
                else
                  if fromJust mRight == Air
                    then moveTo pRight
                    else place p
  where
    place at = (setMaterial s Sand at, Resting at)
    moveTo to = (s, Falling to)

stepSandUnit2 (_, _) = undefined

parseRegolithReservoir :: String -> Scan
parseRegolithReservoir content =
  let ls = map parseLine $ lines $ content
      xs = map fst $ concat ls
      ys = map snd $ concat ls
      minX = minimum xs
      maxX = maximum xs
      maxY = maximum ys
      s0 = Scan (minX - maxY) maxY (replicate (maxY + 2) (replicate (maxX - minX + 1 + 2 * maxY) Air))
   in setMaterial (drawLines s0 ls) Sand (500, 0)
  where
    parseLine = map parsePosition . filter (/= "->") . words

    parsePosition = toPosition . map read . split ','

    toPosition [x, y] = (x, y)
    toPosition _ = undefined

    drawLines s (l : ls) = drawLines (drawLineSegments s l) ls
    drawLines s _ = s

    drawLineSegments s (p : p' : ps) =
      drawLineSegments (drawLine s Rock p p') (p' : ps)
    drawLineSegments s _ = s

scanWidth :: Scan -> Int
scanWidth = length . head . slice

scanHeight :: Scan -> Int
scanHeight = length . slice

inScan :: Scan -> Position -> Bool
inScan s (x, y) =
  let x' = x - minX s
   in 0 <= x' && 0 <= y && y < scanHeight s && x' < scanWidth s

getMaterial :: Scan -> Position -> Maybe Material
getMaterial s (x, y)
  | inScan s (x, y) = Just (slice s !! y !! (x - minX s))
  | otherwise = Nothing

setMaterial :: Scan -> Material -> Position -> Scan
setMaterial s m (x, y) = s {slice = zipWith processLine [0 ..] $ slice s}
  where
    processLine :: Int -> [Material] -> [Material]
    processLine i l
      | i == y = zipWith processPosition [0 ..] l
      | otherwise = l

    processPosition :: Int -> Material -> Material
    processPosition j m'
      | j + minX s == x = m
      | otherwise = m'

drawLine :: Scan -> Material -> Position -> Position -> Scan
drawLine s0 m p@(x, y) p'@(x', y')
  | x == x' = drawVerticalLine (y' - y)
  | y == y' = drawHorizontalLine (x' - x)
  | otherwise = undefined
  where
    drawVerticalLine :: Int -> Scan
    drawVerticalLine l
      | l < 0 = drawVerticalLine' s0 (x, y + l) (1 - l)
      | otherwise = drawVerticalLine' s0 p (l + 1)

    drawVerticalLine' :: Scan -> Position -> Int -> Scan
    drawVerticalLine' s (x, y) 0 = s
    drawVerticalLine' s (x, y) l =
      drawVerticalLine' (setMaterial s m (x, y)) (x, y + 1) (l - 1)

    drawHorizontalLine l
      | l < 0 = drawHorizontalLine' s0 (x + l, y) (1 - l)
      | otherwise = drawHorizontalLine' s0 p (l + 1)

    drawHorizontalLine' s (x, y) 0 = s
    drawHorizontalLine' s (x, y) l =
      drawHorizontalLine' (setMaterial s m (x, y)) (x + 1, y) (l - 1)

renderRegolithReservoir :: Scan -> String
renderRegolithReservoir =
  unlines . map (map toMark) . slice
  where
    toMark m = case m of
      Air -> '.'
      Rock -> '#'
      Sand -> 'o'

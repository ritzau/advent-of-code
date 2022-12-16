module AoC22E15 (beaconExclusionZone) where

import Data.Char (isDigit)
import Data.List (break, stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Helpers (printHeader, printResult, readData)

type Position = (Int, Int)

data Sensor = Sensor
  { position :: Position,
    beacon :: Position,
    dist :: Int
  }
  deriving (Show)

data Cell = EmptyCell | SensorCell | BeaconCell | CoveredCell
  deriving (Eq, Show)

beaconExclusionZone = do
  printHeader "2022 Day 15: Beacon Exclusion Zone"

  sampleContent <- readData "data-s22e15-sample.txt"
  content <- readData "data-s22e15.txt"

  let sampleSensors = parseBeaconExclusionZone sampleContent
  let sensors = parseBeaconExclusionZone content

  printResult "Count covered sample cells" 26 $ countCoveredCells sampleSensors 10
  printResult "Count covered cells" 5688618 $ countCoveredCells sensors 2000000

  printResult "Sample tuning frequency" 56000011 $ 
    fromJust $ tuningFrequency sampleSensors 20
  printResult "Sample tuning frequency" 12625383204261 $ 
    fromJust $ tuningFrequency sensors 4000000

countCoveredCells :: [Sensor] -> Int -> Int
countCoveredCells sensors l =
  let minX = minimum $ map (\s -> fst (position s) - dist s) sensors
      maxX = maximum $ map (\s -> fst (position s) + dist s) sensors
   in length $ filter (== CoveredCell) $ scanLine sensors [minX .. maxX] l

tuningFrequency sensors maxValue =
  tuningFrequency' $ findEmpty' sensors [0 .. maxValue] maxValue
  where
    findEmpty' :: [Sensor] -> [Int] -> Int -> Maybe Position
    findEmpty' sensors (row : rs) lastX = maybeKeepLooking $ findEmpty sensors lastX row 0
      where
        maybeKeepLooking Nothing = findEmpty' sensors rs lastX
        maybeKeepLooking p = p
    findEmpty' _ _ _ = Nothing

    tuningFrequency' :: Maybe Position -> Maybe Int
    tuningFrequency' Nothing = Nothing
    tuningFrequency' (Just (x, y)) = Just (4000000 * x + y)

findEmpty :: [Sensor] -> Int -> Int -> Int -> Maybe Position
findEmpty sensors maxX y x = findEmpty' $ findSensor sensors (x, y)
  where
    findEmpty' :: Maybe Sensor -> Maybe Position
    findEmpty' Nothing = Just (x, y)
    findEmpty' (Just s) =
      let x' = nextCol s y
       in if x' > maxX then Nothing else findEmpty sensors maxX y x'

findSensor (sensor : ss) pos =
  let Sensor spos _ sdist = sensor
      cdist = taxiDist pos spos
   in if cdist <= sdist then Just sensor else findSensor ss pos
findSensor _ _ = Nothing

nextCol (Sensor (sx, sy) _ sdist) y = 1 + sx + sdist - abs (y - sy)

scanLine :: [Sensor] -> [Int] -> Int -> [Cell]
scanLine sensors cols line =
  let ss = filter (\(Sensor (_, y) _ d) -> abs (line - y) <= d) sensors
   in map (\c -> getCell ss (c, line)) cols

renderLine :: [Sensor] -> [Int] -> Int -> String
renderLine sensors cols line = map renderCell $ scanLine sensors cols line

getCell :: [Sensor] -> Position -> Cell
getCell sensors p = fromMaybe (inRange sensors p) (onSensorOrBeacon sensors p)
  where
    onSensorOrBeacon ((Sensor spos bpos _) : ss) p
      | p == spos = Just SensorCell
      | p == bpos = Just BeaconCell
      | otherwise = onSensorOrBeacon ss p
    onSensorOrBeacon [] _ = Nothing

    inRange ((Sensor spos bpos sdist) : ss) p =
      let cdist = taxiDist p spos
       in if cdist <= sdist then CoveredCell else inRange ss p
    inRange _ _ = EmptyCell

renderCell c = case c of
  EmptyCell -> '.'
  SensorCell -> 'S'
  BeaconCell -> 'B'
  CoveredCell -> '#'

taxiDist :: Position -> Position -> Int
taxiDist (x, y) (x', y') = abs (x - x') + abs (y - y')

parseBeaconExclusionZone :: String -> [Sensor]
parseBeaconExclusionZone = map parseSensor . lines
  where
    parseSensor line =
      let (rawPos, rawBeacon) = break (== ':') line
          pos = fromJust $ stripPrefix "Sensor at " rawPos
          beacon = fromJust $ stripPrefix ": closest beacon is at " rawBeacon
          spos = (parseCoord pos)
          bpos = (parseCoord beacon)
       in Sensor spos bpos (taxiDist spos bpos)
      where
        parseCoord s =
          let (rawX, rawY) = break (== ',') s
              x = fromJust $ stripPrefix "x=" rawX
              y = fromJust $ stripPrefix ", y=" rawY
           in (read x, read y)

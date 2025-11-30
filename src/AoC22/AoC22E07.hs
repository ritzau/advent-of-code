module AoC22E07 (noSpaceLeftOnDevice) where

import Data.List (find, intercalate, isPrefixOf, replicate, sort)
import Data.Maybe (fromJust)
import Helpers (printHeader, printResult, readData)

data Command = ChangeDirectory String | List
  deriving (Show)

data File = File String Int | Dir String
  deriving (Show)

type CommandOutput = (Command, [File])

type Path = [String]

type DirRecord = (Path, [File])

noSpaceLeftOnDevice :: IO ()
noSpaceLeftOnDevice = do
  sampleContent <- readData "data-s22e07-sample.txt"
  content <- readData "data-s22e07.txt"

  let sampleInput = parseInput sampleContent
  let input = parseInput content

  printHeader "2022 Day 7: No Space Left On Device"
  printResult "Sum small samples" 95437 $ sumOfSmallDirectories sampleInput
  printResult "Sum small directories" 1297683 $ sumOfSmallDirectories input
  printResult "Target sample directory size" 24933642 $ selectDeletionTarget sampleInput
  printResult "Target directory size" 5756764 $ selectDeletionTarget input

sumOfSmallDirectories :: [CommandOutput] -> Int
sumOfSmallDirectories =
  sum . filter (<= 100000) . map snd . du . processCommands

selectDeletionTarget :: [CommandOutput] -> Int
selectDeletionTarget fs =
  let
    usage = sort $ map snd $ du $ processCommands fs
  in
    fromJust $ find (>= last usage + 30000000 - 70000000) usage

parseInput :: String -> [CommandOutput]
parseInput = parseLines . lines
  where
    parseLines :: [String] -> [CommandOutput]
    parseLines [] = []
    parseLines (l : ls)
      | "$ " `isPrefixOf` l = parseLines' l ls
      | otherwise = undefined

    parseLines' :: String -> [String] -> [CommandOutput]
    parseLines' cmdLine ls =
      let (output, tail) = break ("$ " `isPrefixOf`) ls
          cmd = parseCommand cmdLine
       in (cmd, parseOutput cmd output) : parseLines tail

    parseCommand :: String -> Command
    parseCommand l =
      let ws = drop 1 $ words l
          cmd = head ws
       in case cmd of
            "cd" -> ChangeDirectory (ws !! 1)
            "ls" -> List
            _ -> undefined

    parseOutput :: Command -> [String] -> [File]
    parseOutput cmd output = case cmd of
      ChangeDirectory _ -> []
      List -> map parseListLine output

    parseListLine :: String -> File
    parseListLine l = parse (words l)
      where
        parse [sizeType, name] = case sizeType of
          "dir" -> Dir name
          _ -> File name (read sizeType)
        parse _ = undefined

processCommands :: [CommandOutput] -> [DirRecord]
processCommands [] = []
processCommands cs = processCommands' [] cs
  where
    processCommands' :: Path -> [CommandOutput] -> [DirRecord]
    processCommands' pwd [] = []
    processCommands' pwd (c : cs) =
      let (pwd', records) = processCommand pwd c
       in records ++ processCommands' pwd' cs

    processCommand :: Path -> CommandOutput -> (Path, [DirRecord])
    processCommand pwd (cmd, params) = case cmd of
      ChangeDirectory dir -> (changeDirectory pwd dir, [])
      List -> (pwd, [(pwd, params)])

    changeDirectory :: Path -> String -> Path
    changeDirectory pwd dir = case dir of
      "/" -> []
      ".." -> drop 1 pwd
      n -> n : pwd

du :: [DirRecord] -> [(Path, Int)]
du = snd . du' []
  where
    du' :: Path -> [DirRecord] -> (Int, [(Path, Int)])
    du' path fs =
      let (total, list) = du'' (fromJust $ lookup path fs)
       in (total, (path, total) : list)
      where
        du'' :: [File] -> (Int, [(Path, Int)])
        du'' [] = (0, [])
        du'' (f : fss) =
          let (rsz, rest) = du'' fss
           in case f of
                File n sz -> (sz + rsz, rest)
                Dir n -> combine (rsz, rest) (du' (n : path) fs)

        combine (sza, resta) (szb, restb) = (sza + szb, resta ++ restb)

-- Bonus

tree :: [DirRecord] -> Path -> Int -> [String]
tree fs path indent =
  let rec = lookup path fs
   in tree' rec
  where
    tree' :: Maybe [File] -> [String]
    tree' (Just []) = []
    tree' (Just (f : fss)) =
      let indentString = replicate (2 * indent) ' '
       in case f of
            File n sz -> (indentString ++ fileDesc f) : tree' (Just fss)
            Dir n -> ((indentString ++ fileDesc f) : tree fs (n : path) (indent + 1)) ++ tree' (Just fss)
    tree' Nothing = undefined

    fileDesc :: File -> String
    fileDesc (File n sz) = n ++ " (file size=" ++ show sz ++ ")"
    fileDesc (Dir n) = n ++ " (dir)"
